----------------------------------------------------------------------
--  Rules.Instantiations - Package body                             --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Controller  is  free software;  you can redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from the units  of this program, or if you  link this unit with --
--  other files  to produce  an executable, this  unit does  not by --
--  itself cause the resulting executable  to be covered by the GNU --
--  General  Public  License.   This  exception  does  not  however --
--  invalidate any  other reasons why the executable  file might be --
--  covered by the GNU Public License.                              --
--                                                                  --
--  This  software is  distributed  in  the hope  that  it will  be --
--  useful,  but WITHOUT  ANY  WARRANTY; without  even the  implied --
--  warranty  of  MERCHANTABILITY   or  FITNESS  FOR  A  PARTICULAR --
--  PURPOSE.                                                        --
----------------------------------------------------------------------

-- ASIS
with
  Asis.Elements,
  Asis.Declarations,
  Asis.Expressions;

-- Ada
with
  Ada.Strings.Wide_Unbounded,
  Ada.Unchecked_Deallocation;

-- Adalog
with
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Instantiations is
   use Framework;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   type Generic_Parameters is array (Positive range <>) of Entity_Specification;
   type Generic_Parameter_List is access Generic_Parameters;

   type Instance_Info (Nb_Param : Positive) is
      record
         Loc    : Location;
         Values : Generic_Parameters (1 .. Nb_Param);
      end record;
   package Instance_Info_List is new Linear_Queue (Instance_Info);

   type Instantiation_Context (Has_Repeated : Boolean) is new Basic_Rule_Context with
      record
         Values : Generic_Parameter_List;
         Places : Framework.Language.Shared_Keys.Places_Set;
         case Has_Repeated is
            when False =>
               null;
            when True =>
               All_Instances : Instance_Info_List.Queue;
         end case;
      end record;
   procedure Clear (Context : in out Instantiation_Context);

   Rule_Uses : Context_Store;

   ----------
   -- Free --
   ----------

   procedure Free is
      new Ada.Unchecked_Deallocation (Generic_Parameters, Generic_Parameter_List);

   -----------
   -- Image --
   -----------

   function Image (Values : in Generic_Parameters) return Wide_String is
      -- Precondition: Values /= null
      use Ada.Strings.Wide_Unbounded;

      Dummy : Unbounded_Wide_String := Null_Unbounded_Wide_String;
   begin
      Append (Dummy, "(");
      Append (Dummy, Image (Values (Values'First)));

      for I in Positive range Values'First + 1 .. Values'Last loop
         Append (Dummy, ", ");
         Append (Dummy, Image (Values (I)));
      end loop;

      Append (Dummy, ")");

      return To_Wide_String (Dummy);
   end Image;

   ---------------
   -- Add_Value --
   ---------------

   procedure Add_Value (Values    : in out Generic_Parameter_List;
                        Has_Equal : in out Boolean;
                        Value     : in     Entity_Specification)
   is
      New_Values : Generic_Parameter_List;
   begin
      if Values = null then
         New_Values := new Generic_Parameters' ((1 => Value));
      else
         New_Values := new Generic_Parameters' (Values.all & Value);
      end if;

      Free (Values);
      Values := New_Values;

      if Entity_Specification_Kind (Value) = Equal then
         Has_Equal := True;
      end if;
   end Add_Value;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Framework.Language.Shared_Keys, Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter 1     : {<location>} <Generic name>");
      User_Message ("Parameter 2 .. N: <Entity name> | <> | = (optional)");
      Scope_Places_Utilities.Help_On_Modifiers (Header => "<location>      :");
      User_Message ("Control generic instantiations of specified units, either all of them");
      User_Message ("or those made with the indicated actual parameters.");
      User_Message ("Optionally, control is restricted to instantiations appearing at indicated locations");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Framework.Language.Shared_Keys, Instance_Info_List;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "At least one parameter required");
      end if;

      declare
         Places         : constant Places_Set             := Get_Places_Set_Modifiers;
         Generic_Name   : constant Entity_Specification   := Get_Entity_Parameter;
         Generic_Params :          Generic_Parameter_List := null;
         Has_Equal      :          Boolean                := False;
      begin
         while Parameter_Exists loop
            Add_Value (Generic_Params, Has_Equal, Get_Entity_Parameter (Allow_Extended => True));
         end loop;

         if Has_Equal then
            Associate (Rule_Uses,
                       Generic_Name,
                       Instantiation_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                              Has_Repeated  => True,
                                              Values        => Generic_Params,
                                              Places        => Places,
                                              All_Instances => Empty_Queue),
                       Additive => True);
         else
            Associate (Rule_Uses,
                       Generic_Name,
                       Instantiation_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                              Has_Repeated => False,
                                              Values       => Generic_Params,
                                              Places       => Places),
                       Additive => True);
         end if;
         Rule_Used := True;
      exception
         when Already_In_Store =>
            Parameter_Error (Rule_Id, "this combination of parameters already specified for " & Image (Generic_Name));
      end;
   end Add_Control;

   -----------
   -- Clear --
   -----------

   procedure Clear (Context : in out Instantiation_Context) is
   begin
      Free (Context.Values);
   end Clear;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Clear (Rule_Uses);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Rule_Uses);
   end Prepare;

   ----------------------
   -- Is_Corresponding --
   ----------------------

   function Is_Corresponding (Value      : in Entity_Specification;
                              Definition : in Asis.Definition) return Boolean is
      use Asis, Asis.Elements, Asis.Declarations;

      Declaration : constant Asis.Declaration := Enclosing_Element (Definition);

      Dummy_Definition : Asis.Definition;
   begin
      if Entity_Specification_Kind (Value) /= Regular_Id then
         -- Box or Equal
         return True;
      end if;

      case Declaration_Kind (Declaration) is
         when An_Ordinary_Type_Declaration
           | A_Task_Type_Declaration
           | A_Protected_Type_Declaration
           | A_Private_Type_Declaration
           | A_Private_Extension_Declaration
           | A_Subtype_Declaration
           | A_Formal_Type_Declaration
           =>
            Dummy_Definition := Names (Corresponding_First_Subtype (Declaration))(1);

         when others =>
            Dummy_Definition := Definition;
      end case;

      return Matches (Dummy_Definition, Value);
   end Is_Corresponding;

   -----------
   -- Match --
   -----------

   function Match (Actual_Part : in Asis.Association_List;
                   Values      : in Generic_Parameters) return Boolean is
      use Asis, Asis.Elements, Asis.Expressions;

      Parameter    : Expression;
      Definition   : Asis.Definition;
      Values_Index : Natural         := Values'First;
   begin
      for I in Actual_Part'Range loop
         Parameter := Actual_Parameter (Actual_Part (I));

         case Expression_Kind (Parameter) is
            when An_Identifier =>
               Definition := Corresponding_Name_Definition (Parameter);

            when A_Selected_Component =>
               Definition := Corresponding_Name_Definition (Selector (Parameter));

            when others =>
               -- An arithmetic expression for example, not much we can do with it
               return False;
         end case;

         if not Is_Corresponding (Values (Values_Index), Definition) then
            return False;
         end if;

         -- Safety if there are too many parameters specified by user:
         exit when Values_Index = Values'Last;

         Values_Index := Values_Index + 1;
      end loop;

      return True;
   end Match;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Instantiation : in Asis.Declaration) is
      use Asis.Declarations;

      function Make_Info (Origin : Generic_Parameters;
                          Using  : Asis.Association_List)
                          return Instance_Info
      is
         use Asis.Expressions;
         use Thick_Queries;
         Result : Instance_Info := (Origin'Length, Get_Location (Instantiation), Origin);
      begin
         for I in Result.Values'Range loop
            -- We know that both Origin and Result have 'First = 1, so we can use the same index for both
            if Entity_Specification_Kind (Result.Values (I)) = Equal then
               Result.Values (I) := Value (Full_Name_Image (Actual_Parameter (Using (I)), With_Profile => True));
            end if;
            exit when I > Using'Length;
            -- Security if (user provided) type list is longer than expected by generic
         end loop;

         return Result;
      end Make_Info;

      procedure Process_Context (Context : in Root_Context'Class; Finished : out Boolean) is
         use Framework.Language.Shared_Keys, Framework.Reports;
      begin
         if Context = No_Matching_Context then
            Finished := True;
            return;
         end if;
         Finished := False;

         declare
            use Scope_Places_Utilities, Utilities;
            Good_Context : Instantiation_Context := Instantiation_Context (Context);
         begin
            if not Is_Applicable (Good_Context.Places) then
               return;
            end if;

            if Good_Context.Values = null then
               Report (Rule_Id,
                       Good_Context,
                       Get_Location (Instantiation),
                       Image (Good_Context.Places, Default => Everywhere)
                       & "instantiation of "
                       & To_Title (Last_Matching_Name (Rule_Uses)));

            elsif Good_Context.Has_Repeated then
               declare
                  use Instance_Info_List;
                  Actual_Part : constant Asis.Association_List := Generic_Actual_Part (Instantiation,
                                                                                       Normalized => True);
                  Current     : Cursor;
                  Found       : Boolean := False;
               begin
                  Current := First (Good_Context.All_Instances);
                  while Has_Element (Current) loop
                     if Match (Actual_Part, Fetch (Current).Values) then
                        Found := True;
                        Report (Rule_Id,
                                Good_Context,
                                Get_Location (Instantiation),
                                Image (Good_Context.Places, Default => Everywhere)
                                       & "instantiation of "
                                       & To_Title (Last_Matching_Name (Rule_Uses))
                                       & " with " & Image (Fetch (Current).Values)
                                       & " already provided at " & Image (Fetch (Current).Loc));
                     end if;
                     Current := Next (Current);
                  end loop;
                  if not Found and Match (Actual_Part, Good_Context.Values.all) then
                     Append (Good_Context.All_Instances, Make_Info (Good_Context.Values.all, Actual_Part));
                     Update (Rule_Uses, Good_Context);
                  end if;
               end;

            else
               declare
                  Actual_Part : constant Asis.Association_List := Generic_Actual_Part (Instantiation,
                                                                                       Normalized => True);
               begin
                  if Match (Actual_Part, Good_Context.Values.all) then
                     Report (Rule_Id,
                             Good_Context,
                             Get_Location (Instantiation),
                             Image (Good_Context.Places, Default => Everywhere)
                                    & "instantiation of "
                                    & To_Title (Last_Matching_Name (Rule_Uses))
                                    & " with " & Image (Good_Context.Values.all));
                  end if;
               end;
            end if;
         end;
      end Process_Context;

      Finished : Boolean;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Process_Context (Matching_Context (Rule_Uses, Generic_Unit_Name (Instantiation)), Finished);
      while not Finished loop
         Process_Context (Next_Matching_Context (Rule_Uses), Finished);
      end loop;
   end Process_Instantiation;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Instantiations;
