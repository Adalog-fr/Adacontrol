----------------------------------------------------------------------
--  Rules.Max_Primitives - Package body                             --
--                                                                  --
--  This software is (c) Adalog 2004-2019.                          --
--  The Ada Controller is  free software; you can  redistribute  it --
--  and/or modify it under  terms of the GNU General Public License --
--  as published by the Free Software Foundation; either version 2, --
--  or (at your option) any later version. This unit is distributed --
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
----------------------------------------------------------------------

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Utilities,
  Thick_Queries,
  Scope_Manager;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Symbol_Table;

-- ASIS
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Iterator,
  Asis.Set_Get;

use Utilities;

pragma Elaborate (Framework.Language);

package body Rules.Max_Primitives is
   use Ada.Strings.Wide_Unbounded, Asis, Framework, Framework.Language, Thick_Queries;

   -- Algorithm:
   --
   -- Count the number of primitives.

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   type SR_Visibility is (PV_Visible, PV_Total);
   package Visibility_Flag_Utilities is new Flag_Utilities (Flags => SR_Visibility, Prefix => "PV_");

   type Primitives_Filter is (PF_Tagged, PF_Untagged);
   package Filter_Modifiers is new Modifier_Utilities (Modifiers => Primitives_Filter,
                                                       Prefix    => "PF_");
   subtype Filter_Set is Filter_Modifiers.Modifier_Set;

   Ctl_Labels : array (Control_Kinds, SR_Visibility) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Maximum    : array (Control_Kinds, SR_Visibility, Primitives_Filter) of Asis.ASIS_Natural :=
     (others => (others => (others => Asis.ASIS_Natural'Last)));

   package Types_Table is new Framework.Symbol_Table.Data_Access (ASIS_Natural);

   procedure Post_Process_Spec (Visibility : SR_Visibility);


   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control types that have more primitive operations than the indicated maximum");
      User_Message;
      Visibility_Flag_Utilities.Help_On_Flags ("Parameter (1):");
      User_Message ("Parameter (2): [tagged | untagged] <max allowed primitives>");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Filter_Modifiers;
      Visibility : SR_Visibility;
      Filters    : Filter_Set;
      Bound      : ASIS_Natural;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "Maximum value required");
      end if;

      Visibility := Visibility_Flag_Utilities.Get_Flag_Parameter (Allow_Any => False);

      Filters := Filter_Modifiers.Get_Modifier_Set;

      if Filters = Filter_Modifiers.Empty_Set then
         Filters := Filter_Modifiers.Full_Set;
      end if;

      Bound := Get_Integer_Parameter (Min => 0, Max => Asis.ASIS_Natural'Last - 1);

      for Pf in Primitives_Filter loop
         if Filters (Pf) then
            if Maximum (Ctl_Kind, Visibility, Pf) /= Asis.ASIS_Natural'Last then
               Parameter_Error (Rule_Id, "Maximum primitives already given for "
                                & Primitives_Filter'Wide_Image (Pf)
                                & ' ' & Control_Kinds'Wide_Image (Ctl_Kind));
            end if;
            Maximum (Ctl_Kind, Visibility, Pf) := Bound;
         end if;
      end loop;

      Ctl_Labels (Ctl_Kind, Visibility) := To_Unbounded_Wide_String (Ctl_Label);
      Rule_Used := True;
   exception
      when Constraint_Error =>
         Parameter_Error (Rule_Id, "maximum value negative or too big");
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
            Maximum := (others => (others => (others => Asis.ASIS_Natural'Last)));
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -------------------
   -- Add_Primitive --
   -------------------

   procedure Add_Primitive (Declaration : Asis.Declaration; Nb_Primitives : ASIS_Integer) is
      use Asis.Declarations;
   begin
      Types_Table.Store (Names (Declaration) (1),
                         Types_Table.Fetch (Names (Declaration) (1), 0) + Nb_Primitives);
   end Add_Primitive;


   ------------------------------
   -- Process_Type_Declaration --
   ------------------------------

   procedure Process_Type_Declaration (Decl : Asis.Declaration) is
      use Asis.Declarations, Asis.Definitions, Asis.Elements;
   begin
      if not Rule_Used or
        Declaration_Kind (Decl) in An_Incomplete_Type_Declaration | A_Tagged_Incomplete_Type_Declaration
      then
         return;
      end if;

      declare
         Def   : constant Asis.Definition := Type_Declaration_View (Decl);
         Value : ASIS_Natural             := 0;
      begin
         case Definition_Kind (Def) is
            when A_Private_Extension_Definition =>
               Value := Implicit_Inherited_Subprograms (Def)'Length;
            when A_Type_Definition | A_Formal_Type_Definition =>
               case Type_Kind (Def) is
                  when A_Derived_Type_Definition | A_Derived_Record_Extension_Definition |
                       An_Interface_Type_Definition =>
                     Value := Implicit_Inherited_Subprograms (Def)'Length;
                  when others =>
                     null;
               end case;
               case Formal_Type_Kind (Def) is
                  when A_Formal_Derived_Type_Definition =>
                     Value := Implicit_Inherited_Subprograms (Def)'Length;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Add_Primitive (Decl, Value);
      end;
   end Process_Type_Declaration;


   ------------------------------------
   -- Process_Subprogram_Declaration --
   ------------------------------------

   procedure Process_Subprogram_Declaration (Decl : Asis.Declaration) is
   begin
      if not Rule_Used then
         return;
      end if;

      for Declaration : Asis.Declaration of Corresponding_Primitive_Types (Decl) loop
         Add_Primitive (Declaration, 1);
      end loop;
   end Process_Subprogram_Declaration;


   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Decl : Asis.Declaration) is
      use Asis.Declarations, Asis.Elements, Asis.Iterator;
      Ignored         : Traverse_Control := Continue;
      Controlled_Inst : Null_State;

      procedure Pre_Operation  (Element       :        Asis.Element;
                                Control       : in out Traverse_Control;
                                In_Controlled : in out Null_State);

      procedure Traverse is new Traverse_Element (Null_State, Pre_Operation, Null_State_Procedure);

      procedure Pre_Operation  (Element       :        Asis.Element;
                                Control       : in out Traverse_Control;
                                In_Controlled : in out Null_State)
      is
         pragma Unreferenced (Control, In_Controlled);
      begin
         case Element_Kind (Element) is
            when A_Declaration =>
               case Declaration_Kind (Element) is
                  when A_Type_Declaration =>
                     Process_Type_Declaration (Element);
                  when A_Procedure_Declaration | A_Procedure_Body_Stub | A_Null_Procedure_Declaration
                     | A_Function_Declaration  | A_Function_Body_Stub  | An_Expression_Function_Declaration
                     =>
                     Process_Subprogram_Declaration (Element);
                  when A_Package_Instantiation | A_Procedure_Instantiation | A_Function_Instantiation =>
                     Process_Instantiation (Element);
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
      end Pre_Operation;
   begin  -- Process_Instantiation
      if not Rule_Used then
         return;
      end if;

      Rules_Manager.Enter (Rule_Id);

      declare
         Inst_Spec   : constant Asis.Declaration := Corresponding_Declaration (Decl);
         Inst_Body   : constant Asis.Declaration := Corresponding_Body (Decl);
      begin
         Scope_Manager.Enter_Scope (Inst_Spec);
         if Declaration_Kind (Inst_Spec) = A_Package_Instantiation then
            for Declaration : Asis.Element of Visible_Part_Declarative_Items (Inst_Spec) loop
               Traverse (Declaration, Ignored, Controlled_Inst);
            end loop;
            Process_Private_Part;
            for Declaration : Asis.Element of Private_Part_Declarative_Items (Inst_Spec) loop
               Traverse (Declaration, Ignored, Controlled_Inst);
            end loop;
         else
            Traverse (Inst_Spec, Ignored, Controlled_Inst);
         end if;
         Post_Process_Spec (PV_Total);
         Scope_Manager.Exit_Scope (Inst_Spec);

         if not Is_Nil (Inst_Body) then
            Scope_Manager.Enter_Scope (Inst_Body);
            if Declaration_Kind (Inst_Spec) = A_Package_Body_Declaration then
               for Declaration : Asis.Element of Visible_Part_Declarative_Items (Inst_Body) loop
                  Traverse (Declaration, Ignored, Controlled_Inst);
               end loop;
               Process_Private_Part;
               for Declaration : Asis.Element of Private_Part_Declarative_Items (Inst_Body) loop
                  Traverse (Declaration, Ignored, Controlled_Inst);
               end loop;
            else
               Traverse (Inst_Body, Ignored, Controlled_Inst);
            end if;
            Scope_Manager.Exit_Scope (Inst_Body);
         end if;
      end;
   end Process_Instantiation;


   --------------------------
   -- Process_Private_Part --
   --------------------------

   procedure Process_Private_Part is
   begin
      Post_Process_Spec (PV_Visible);
   end Process_Private_Part;


   -----------------------
   -- Post_Process_Spec --
   -----------------------

   procedure Post_Process_Spec (Visibility : SR_Visibility) is
      ------------
      -- Action --
      ------------

      procedure Action (Entity : Asis.Defining_Name; Content_Value : in out ASIS_Natural) is
         use Asis.Elements, Asis.Set_Get;

         function Is_Set (Control : Control_Kinds; Filter : Primitives_Filter) return Boolean is
         begin
            return Maximum (Control, Visibility, Filter) /= Asis.ASIS_Natural'Last;
         end Is_Set;

         procedure Do_Report (Ctl_Kind : Control_Kinds; Filter : Primitives_Filter; Is_From_Instance : Boolean := False)
         is
            use Framework.Locations, Framework.Reports;
            Good_Entity : Asis.Definition := Entity;
         begin
            if Visibility = PV_Total then
               Good_Entity := Corresponding_Full_Type_Declaration (Enclosing_Element (Entity));
            end if;
            Report (Rule_Id   => Rule_Id,
                    Ctl_Label => To_Wide_String (Ctl_Labels (Ctl_Kind, Visibility)),
                    Ctl_Kind  => Ctl_Kind,
                    Loc       => (if Is_From_Instance then Get_Location (Ultimate_Enclosing_Instantiation (Good_Entity))
                                  else Get_Location (Good_Entity)),
                    Msg       => (if Ctl_Kind = Count then ""
                                  else "Number of " & (if Visibility = PV_Visible then "visible" else "total") &
                                    " primitives of """ & Full_Name_Image (Entity) & """ is more than " &
                                    ASIS_Integer_Img (Maximum (Ctl_Kind, Visibility, Filter)) &
                                    " (" & ASIS_Integer_Img (Content_Value) & ")"));
         end Do_Report;

         Filter  : constant Primitives_Filter := (if Is_Tagged (Entity) then PF_Tagged else PF_Untagged);
         Element : constant Asis.Declaration  := Enclosing_Element (Entity);
      begin -- Action
         if Is_Set (Check, Filter) and Content_Value > Maximum (Check, Visibility, Filter) then
            if Is_From_Instance (Element) then
               Do_Report (Check, Filter, True);
            else
               Do_Report (Check, Filter);
            end if;

         elsif Is_Set (Search, Filter) and Content_Value > Maximum (Search, Visibility, Filter) then
            if Is_From_Instance (Element) then
               Do_Report (Search, Filter, True);
            else
               Do_Report (Search, Filter);
            end if;
         end if;

         if Is_Set (Count, Filter) and Content_Value > Maximum (Count, Visibility, Filter) then
            if Is_From_Instance (Element) then
               Do_Report (Count, Filter, True);
            else
               Do_Report (Count, Filter);
            end if;
         end if;
      end Action;

      procedure Iterate_Types_Table is new Types_Table.On_Every_Entity_From_Scope (Action => Action);
   begin  -- Post_Process_Spec
      if not Rule_Used then
         return;
      end if;

      Iterate_Types_Table (Framework.Symbol_Table.Declaration_Scope);
   end Post_Process_Spec;


   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Process_Scope_Exit (Scope : Asis.Declaration) is
      use Asis.Elements;
   begin
      case Declaration_Kind (Scope) is
         when A_Package_Declaration | A_Generic_Package_Declaration =>
            Post_Process_Spec (PV_Total);

         when others =>
            null;
      end case;
   end Process_Scope_Exit;

begin  -- Rules.Max_Primitives
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Max_Primitives;
