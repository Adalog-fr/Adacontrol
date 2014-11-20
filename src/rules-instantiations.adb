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

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Elements,
  Asis.Declarations,
  Asis.Expressions;

-- Adalog
with
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language.Shared_Keys;

package body Rules.Instantiations is
   use Asis, Framework, Framework.Control_Manager;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   type Generic_Parameters is array (List_Index range <>) of Entity_Specification;
   No_Parameters : constant Generic_Parameters (1 .. 0) := (others => Value ("Junk"));

   type Instance_Info (Nb_Param : List_Index) is
      record
         Loc    : Location;
         Values : Generic_Parameters (1 .. Nb_Param);
      end record;
   package Instance_Info_List is new Linear_Queue (Instance_Info);

   type Instantiation_Context (Nb_Values : ASIS_Natural; Has_Repeated : Boolean) is new Basic_Rule_Context with
      record
         Values : Generic_Parameters (1 .. Nb_Values);
         Places : Framework.Language.Shared_Keys.Places_Set;
         case Has_Repeated is
            when False =>
               null;
            when True =>
               All_Instances : Instance_Info_List.Queue;
         end case;
      end record;

   Rule_Uses : Context_Store;
   package Rule_Uses_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Rule_Uses);

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

      for I in List_Index range Values'First + 1 .. Values'Last loop
         Append (Dummy, ", ");
         Append (Dummy, Image (Values (I)));
      end loop;

      Append (Dummy, ")");

      return To_Wide_String (Dummy);
   end Image;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Framework.Language.Shared_Keys, Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control generic instantiations of specified units, either all of them");
      User_Message ("or those made with the indicated actual parameters.");
      User_Message ("Optionally, control is restricted to instantiations appearing at indicated locations");
      User_Message;
      User_Message ("Parameter(1): {<location>} <Generic name>");
      User_Message ("Parameter(2..): <Entity name> | <category> | <> | = (optional)");
      Scope_Places_Utilities.Help_On_Modifiers (Header => "<location>:");
      User_Message ("<category>: ()      | access   | array | delta  | digits | mod |");
      User_Message ("            private |protected | range | record | tagged | task");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Framework.Language.Shared_Keys;

      function Build_Context (Places : Places_Set) return Instantiation_Context is
         use Instance_Info_List;
      begin
         if not Parameter_Exists then
            return (Basic.New_Context (Ctl_Kind, Ctl_Label) with
                    Nb_Values    => 0,
                    Has_Repeated => False,
                    Values       => No_Parameters,
                    Places       => Places);
         end if;

         declare
            Spec : Entity_Specification := Get_Entity_Parameter (Allow_Extended => True);
            Rest : constant Instantiation_Context := Build_Context (Places);
         begin
            if Spec = Value ("()") then
               Spec := Value ("ENUM");
            end if;

            if Rest.Has_Repeated or Entity_Specification_Kind (Spec) = Equal then
               return (Basic_Rule_Context (Rest) with
                       Nb_Values     => Rest.Nb_Values + 1,
                       Has_Repeated  => True,
                       Values        => Spec & Rest.Values,
                       Places        => Rest.Places,
                       All_Instances => Empty_Queue);
            else
               return (Basic_Rule_Context (Rest) with
                       Nb_Values    => Rest.Nb_Values + 1,
                       Has_Repeated => False,
                       Values       => Spec & Rest.Values,
                       Places       => Rest.Places);
            end if;
         end;
      end Build_Context;

   begin   -- Add_Control
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "At least one parameter required");
      end if;

      declare
         Places         : constant Places_Set           := Get_Places_Set_Modifiers;
         Generic_Name   : constant Entity_Specification := Get_Entity_Parameter;
      begin
         Associate (Rule_Uses,
                    Generic_Name,
                    Build_Context (Places),
                    Additive => True);
         Rule_Used := True;
      exception
         when Already_In_Store =>
            Parameter_Error (Rule_Id, "this combination of parameters already specified for " & Image (Generic_Name));
      end;
   end Add_Control;

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

   function Is_Corresponding (Specification : in Entity_Specification;
                              Name          : in Asis.Defining_Name) return Boolean
   is
      use Asis.Elements, Asis.Expressions;
      use Framework.Language.Shared_Keys, Utilities, Thick_Queries;

      Declaration  : Asis.Declaration;
      Is_Attribute : Boolean := False;
   begin
      if Entity_Specification_Kind (Specification) in Box .. Equal then
         return True;
      end if;

      case Expression_Kind (Name) is
         when An_Identifier | A_Selected_Component =>
            null;
         when An_Attribute_Reference =>
            case Attribute_Kind (Name) is
               when A_Base_Attribute | A_Class_Attribute =>
                  -- The only ones that are names
                  Is_Attribute := True;
               when others =>
                  return False;
            end case;
         when others =>
            -- An arithmetic expression for example, matches nothing but <> and =
            return False;
      end case;

      if Matches (Specification, Name) then
         return True;
      end if;

      -- No direct match here

      if Is_Attribute then
         return False;
      end if;

      -- Special case if Name designates a type: try category
      -- (+ first named subtype if it is a subtype)
      Declaration := Corresponding_Name_Declaration (Simple_Name (Name));
      case Declaration_Kind (Declaration) is
         when An_Ordinary_Type_Declaration
            | A_Task_Type_Declaration
            | A_Protected_Type_Declaration
            | A_Private_Type_Declaration
            | A_Private_Extension_Declaration
            | A_Formal_Type_Declaration
            | An_Incomplete_Type_Declaration
              =>
            null;
         when A_Subtype_Declaration =>
            if Matches (Specification, First_Subtype_Name (Name)) then
               return True;
            end if;
         when others =>
            -- Not a type or subtype
            -- (includes Not_An_Element if Name is class wide or predefined)
            return False;
      end case;

      -- Here we have a type or subtype
      if Image (Type_Category (Declaration)) = To_Upper (Image (Specification)) then
         return True;
      end if;

      return False;
   end Is_Corresponding;

   -----------
   -- Match --
   -----------

   function Match (Actual_Part : in Asis.Association_List;
                   Values      : in Generic_Parameters) return Boolean
   is
      use Asis.Expressions;

      Values_Index : List_Index := Values'First;
   begin
      for I in Actual_Part'Range loop
         if not Is_Corresponding (Values (Values_Index), Actual_Parameter (Actual_Part (I))) then
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

      Iter : Context_Iterator := Rule_Uses_Iterator.Create;

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

      procedure Process_Context is
         use Framework.Language.Shared_Keys, Framework.Reports;
         use Scope_Places_Utilities, Utilities;
         Good_Context : Instantiation_Context := Instantiation_Context (Value (Iter));
      begin
         if not Is_Applicable (Good_Context.Places) then
            return;
         end if;

         if Good_Context.Nb_Values = 0 then
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
               if not Found and Match (Actual_Part, Good_Context.Values) then
                  Append (Good_Context.All_Instances, Make_Info (Good_Context.Values, Actual_Part));
                  Update (Rule_Uses, Good_Context);
               end if;
            end;

         else
            declare
               Actual_Part : constant Asis.Association_List := Generic_Actual_Part (Instantiation,
                                                                                    Normalized => True);
            begin
               if Match (Actual_Part, Good_Context.Values) then
                  Report (Rule_Id,
                          Good_Context,
                          Get_Location (Instantiation),
                          Image (Good_Context.Places, Default => Everywhere)
                          & "instantiation of "
                          & To_Title (Last_Matching_Name (Rule_Uses))
                          & " with " & Image (Good_Context.Values));
               end if;
            end;
         end if;
      end Process_Context;

   begin  -- Process_Instantiation
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Reset (Iter, Generic_Unit_Name (Instantiation), All_Extensions);
      while not Is_Exhausted (Iter) loop
         Process_Context;
         Next (Iter);
      end loop;
   end Process_Instantiation;

begin  -- Rules.Instantiations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Instantiations;
