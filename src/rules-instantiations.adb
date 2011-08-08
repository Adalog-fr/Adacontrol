----------------------------------------------------------------------
--  Rules.Instantiations - Package body                             --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Code Cheker  is free software;  you can redistribute  it and/or --
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
  Utilities,
  Thick_Queries;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Instantiations is
   use Framework;

   Rule_Used : Boolean := False;

   type Generic_Parameters is array (Positive range <>)
     of Entity_Specification;

   type Generic_Parameter_List is access Generic_Parameters;

   type Instantiation_Context is new Simple_Context with
      record
         Count  : Natural;
         Values : Generic_Parameter_List;
      end record;

   Rule_Uses : Context_Store;

   ----------
   -- Free --
   ----------

   procedure Free is
      new Ada.Unchecked_Deallocation (Generic_Parameters, Generic_Parameter_List);

   -----------
   -- Image --
   -----------

   function Image (Values : in Generic_Parameter_List) return Wide_String is
      -- Precondition: Values /= null
      use Ada.Strings.Wide_Unbounded, Utilities;

      Dummy     : Unbounded_Wide_String := Null_Unbounded_Wide_String;
   begin
      Append (Dummy, "(");

      for I in Values'Range loop
         if I /= Values'First then
            Append (Dummy, ", ");
         end if;

         if Values (I).Is_Box then
            Append (Dummy, "<>");
         else
            Append (Dummy, To_Title (To_Wide_String (Values (I).Specification)));
         end if;
      end loop;

      Append (Dummy, ")");

      return To_Wide_String (Dummy);
   end Image;

   ---------------
   -- Add_Value --
   ---------------

   procedure Add_Value (Values : in out Generic_Parameter_List; Value : in Entity_Specification) is
      New_Values : Generic_Parameter_List;
   begin
      if Values = null then
         New_Values := new Generic_Parameters' ((1 => Value));
      else
         New_Values := new Generic_Parameters' (Values.all & Value);
      end if;

      Free (Values);
      Values := New_Values;
   end Add_Value;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter 1     : Generic name");
      User_Message ("Parameter 2 .. N: Entity name (Optional)");
      User_Message ("This rule can be used to check/search for generic instantiations,");
      User_Message ("either all of them or those made with the given entities");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;
   begin
      if not Parameter_Exists then
         Parameter_Error ("At least one parameter required for rule " & Rule_Id);
      end if;

      declare
         Generic_Name   : constant Entity_Specification   := Get_Entity_Parameter;
         Generic_Params :          Generic_Parameter_List := null;
      begin
         while Parameter_Exists loop
            Add_Value (Generic_Params, Get_Entity_Parameter);
         end loop;

         Associate (Rule_Uses,
                    Generic_Name,
                    Instantiation_Context'(Rule_Type,
                                           To_Unbounded_Wide_String (Label),
                                           0,
                                           Generic_Params),
                    Additive => True);
         Rule_Used := True;
      end;
   end Add_Use;

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
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Elements, Asis.Declarations;
      use Utilities, Thick_Queries;

      Declaration : constant Asis.Declaration := Enclosing_Element (Definition);

      Dummy_Definition : Asis.Definition;
   begin
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

      return To_Wide_String (Value.Specification)
        = To_Upper (Full_Name_Image (Dummy_Definition));
   end Is_Corresponding;

   -----------
   -- Match --
   -----------

   function Match (Actual_Part : in Asis.Association_List;
                   Values      : in Generic_Parameter_List) return Boolean is
      use Asis, Asis.Elements, Asis.Expressions;

      Parameter    : Expression;
      Definition   : Asis.Definition;
      Values_Index : Natural         := Values'First;
   begin
      for I in Actual_Part'Range loop
         Parameter := Actual_Parameter (Actual_Part (I));

         if not Values (Values_Index).Is_Box then
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

      procedure Process_Context (Context : Rule_Context'Class; Finished : out Boolean) is
         use Asis, Framework.Reports, Ada.Strings.Wide_Unbounded;
      begin
         if Context = No_Matching_Context then
            Finished := True;
            return;
         end if;
         Finished := False;

         declare
            use Utilities;
            Good_Context : Instantiation_Context := Instantiation_Context (Context);
         begin
            if Good_Context.Values = null then
               Good_Context.Count := Good_Context.Count + 1;
               Update (Rule_Uses, Good_Context);
               Report (Rule_Id,
                       To_Wide_String (Good_Context.Rule_Label),
                       Good_Context.Rule_Type,
                       Get_Location (Instantiation),
                       "instantiation of """ & To_Title (Last_Matching_Name (Rule_Uses))
                         & """ (" & Natural'Wide_Image (Good_Context.Count) & ")");
            else
               declare
                  Actual_Part : constant Asis.Association_List
                    := Generic_Actual_Part (Instantiation, Normalized => True);
               begin
                  if Match (Actual_Part, Good_Context.Values) then
                     Good_Context.Count := Good_Context.Count + 1;
                     Update (Rule_Uses, Good_Context);
                     Report (Rule_Id,
                             To_Wide_String (Good_Context.Rule_Label),
                             Good_Context.Rule_Type,
                             Get_Location (Instantiation),
                             "instantiation of """ & To_Title (Last_Matching_Name (Rule_Uses))
                               & """ (" & Natural'Wide_Image (Good_Context.Count) & ")"
                               & " with " & Image (Good_Context.Values));
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
                                     Help    => Help'Access,
                                     Prepare => Prepare'Access,
                                     Add_Use => Add_Use'Access);
end Rules.Instantiations;
