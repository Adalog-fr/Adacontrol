----------------------------------------------------------------------
--  Rules.Renaming_Declarations - Package body                      --
--                                                                  --
--  This software  is (c) Adalog  2004-2021.                        --
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
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Control_Manager,
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Locations,
  Framework.Rules_Manager,
  Framework.Reports;

---------------------
-- Rules.Renamings --
---------------------

package body Rules.Renaming_Declarations is
   use Framework, Framework.Control_Manager, Framework.Language;

   -- Algorithm
   --
   -- The subrules correspond to the various kinds of renamings, except the subrules "object" and "function_call"
   -- that both correspond to An_Object_Renaming_Declaration, depending on the initial value.
   --
   -- Locations are treated using the services provided in Language.Shared_Keys
   --
   -- Filters are stored in an array of type Filter_State, and can take the values Positive, Negative, and None
   -- For filters provided in a control, it is Positive if provided without "not", Negative if provided with "not",
   --   and None if not provided.
   -- When a renaming declaration is encountered, the proper subrule is identified, then an Applicable_Filter is
   --   computed, with values of Positive if the filter applies to the declaration, Negative if the filter could apply
   --   but does not, and None if the filter is not applicable to the declaration. For example, if the user specifies:
   --      Check renaming_declarations (not operator all)
   --   it would be stupid to report all object renaming declarations, since these cannot be operators.
   --   The Applicable_Filter is initialized from a static table, then adjusted according to the applicable subrule.
   --
   -- The control is reported neither the user filter and the Applicable_Filter is None, and the values are the same
   -- (subjet to the location being appropriate).

   type Subrules is (SR_All, SR_Object, SR_Function_Call, SR_Procedure, SR_Function, SR_Package, SR_Exception);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "SR_");
   subtype True_Subrules is Subrules range Subrules'Succ (SR_All) .. Subrules'Last;
   -- SR_All is not a real subrule

   type Filters is (F_As_Body,      F_As_Operator, F_Generic,  F_Identical,
                    F_Library_Unit, F_Operator,    F_Renaming, F_Synonym);
   package Filter_Modifier_Utilities is new Modifier_Utilities (Filters, "F_");

   type Filter_Status is (Positive, Negative, None);
   type Filter_State is array (Filters) of Filter_Status;
   No_Filter : constant Filter_State := (others => None);

   -- Initial set of applicable filters: Negative if applicable, None if not applicable
   Initial_Filter : constant array (True_Subrules) of Filter_State :=
     (SR_Object        => (F_Identical   | F_Renaming    | F_Synonym                                    => Negative,
                           F_As_Body     | F_As_Operator | F_Generic      | F_Library_Unit | F_Operator => None),

      SR_Function_Call => (F_Renaming                                                                   => Negative,
                           F_As_Body     | F_As_Operator | F_Generic      | F_Identical    | F_Library_Unit |
                           F_Operator    | F_Synonym                                                    => None),

      SR_Procedure     => (F_As_Body     | F_Generic     | F_Identical    | F_Library_Unit | F_Renaming |
                           F_Synonym                                                                    => Negative,
                           F_As_Operator | F_Operator                                                   => None),

      SR_Function      => (F_As_Body     | F_As_Operator | F_Generic      | F_Identical    | F_Library_Unit |
                           F_Operator    | F_Renaming    | F_Synonym                                    => Negative),

      SR_Package       => (F_Generic     | F_Identical   | F_Library_Unit | F_Renaming     | F_Synonym  => Negative,
                           F_As_Body     | F_As_Operator | F_Operator                                   => None),

      SR_Exception     => (F_Identical   | F_Renaming    | F_Synonym                                    => Negative,
                           F_As_Body     | F_As_Operator | F_Generic      | F_Library_Unit | F_Operator => None));

   type Renaming_Context is new Basic_Rule_Context with
      record
         Locations      : Framework.Language.Shared_Keys.Places_Set;
         Subrule_Filter : Filter_State;
      end record;

   type Usage_Flags is array (Subrules) of Boolean;
   No_Rule_Used   : constant Usage_Flags := (others => False);
   All_Rules_Used : constant Usage_Flags := (others => True);
   Rule_Used : Usage_Flags := No_Rule_Used;
   Save_Used : Usage_Flags;
   Usage     : Context_Store;
   package Usage_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Usage);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of Ada declarations");
      User_Message;
      User_Message ("Parameter(s): {<location>} {<filter>} <subrule>");
      Subrules_Flag_Utilities.       Help_On_Flags        (Header => "<subrule>:");
      Framework.Language.Shared_Keys.Help_On_Scope_Places (Header => "<location>:");
      Filter_Modifier_Utilities.     Help_On_Modifiers    (Header => "<filter>:");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind  : in Control_Kinds) is
      use Framework.Language.Shared_Keys;

      Loc     : Places_Set;
      Status  : Filter_Status;
      Filter  : Filters;
      State   : Filter_State;
      Subrule : Subrules;
      Found   : Boolean;

      use Filter_Modifier_Utilities, Subrules_Flag_Utilities;
   begin
      if not Parameter_Exists then
         Rule_Used := All_Rules_Used;
         for Sr in True_Subrules loop
            Associate (Usage,
                       Value (Subrules'Wide_Image (Sr)),
                       Renaming_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Everywhere, No_Filter),
                       Additive => True);
         end loop;
         return;
      end if;

      while Parameter_Exists loop
         Loc := Get_Places_Set_Modifiers (Rule_Id, Allow_All => False);

         State := No_Filter;
         On_Filters : loop
            if Get_Modifier ("NOT") then
               Status := Negative;
            else
               Status := Positive;
            end if;
            Get_Modifier (Filter, Found, Full_Set);
            if Found then
               State (Filter) := Status;
            elsif Status = Negative then
               Parameter_Error (Rule_Id, """not"" must be followed by filter");
            else
               exit;
            end if;
         end loop On_Filters;

         Subrule := Get_Flag_Parameter (Allow_Any => False);

         if Subrule = SR_All then
            Rule_Used := All_Rules_Used;
            for Sr in True_Subrules loop
               Associate (Usage,
                          Value (Subrules'Wide_Image (Sr)),
                          Renaming_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Loc, State),
                          Additive => True);
            end loop;
         else
            Rule_Used (Subrule) := True;
            Associate (Usage,
                       Value (Subrules'Wide_Image (Subrule)),
                       Renaming_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Loc, State),
                       Additive => True);
         end if;
      end loop;

   exception
      when Already_In_Store =>
         Parameter_Error (Rule_Id, "Subrule already given");
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := No_Rule_Used;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Name : Asis.Defining_Name; Subrule : Subrules; Applicable_Filter : Filter_State) is

      procedure Check (Context : Renaming_Context) is
         use Ada.Strings.Wide_Unbounded;
         use  Framework.Language.Shared_Keys, Framework.Locations, Framework.Reports;
         use  Filter_Modifier_Utilities, Subrules_Flag_Utilities, Utilities;

         Message : Unbounded_Wide_String;
      begin
         if not Is_Applicable (Context.Locations) then
            return;
         end if;

         Append (Message, Image (Context.Locations, Default => Everywhere));
         for F in Filters loop
            if Context.Subrule_Filter (F) /= None then
               if Applicable_Filter (F) = None or Applicable_Filter (F) /= Context.Subrule_Filter (F) then
                  return;
               end if;
               if Applicable_Filter (F) = Negative then
                  Append (Message, "not ");
               end if;
               Append (Message, Image (F, Lower_Case));
               Append (Message, ' ');
            end if;
         end loop;
         Report (Rule_Id,
                 Context,
                 Get_Location (Name),
                 To_Wide_String (Message) & Image (Subrule, Lower_Case) & ' '  & "renaming");
      end Check;

      Iter : Context_Iterator := Usage_Iterator.Create;
   begin  -- Do_Report
      Reset (Iter, Value (Subrules'Wide_Image (Subrule)));
      if Is_Exhausted (Iter) then
         return;
      end if;

      while not Is_Exhausted (Iter) loop
         Check (Renaming_Context (Value (Iter)));
         Next (Iter);
      end loop;
   end Do_Report;

   ----------------------------------
   -- Process_Renaming_Declaration --
   ----------------------------------

   procedure Process_Renaming_Declaration (Decl : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;

      Subrule       : Subrules;
      Filter        : Filter_State;
      Name          : Asis.Defining_Name;
      Target_Entity : Asis.Element;
      Target_Decl   : Asis.Element;
   begin
      if Rule_Used = No_Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Name := Names (Decl) (1);      -- No multiple names in renamings

      -- Get the target, without conversions, qualification, etc.
      -- We don't need the real target, but something part of the renaming that tells
      -- us where the renamed entity is declared, and whether it involves a function call.
      -- Beware of implicit dereferences!
      Target_Entity := Renamed_Entity (Decl);
      loop
         case Expression_Kind (Target_Entity) is
            when An_Identifier | An_Operator_Symbol =>
               Target_Decl := Corresponding_Name_Declaration (Target_Entity);
               exit;
            when A_Type_Conversion | A_Qualified_Expression =>
               Target_Entity := Converted_Or_Qualified_Expression (Target_Entity);
            when A_Selected_Component =>
               if Is_Expanded_Name (Target_Entity) then
                  Target_Entity := Selector (Target_Entity);
               else                                           -- it's a component
                  Target_Entity := Prefix (Target_Entity);
                  if Is_Access_Expression (Target_Entity) then -- implicit dereference
                     Target_Decl := Nil_Element;
                     exit;
                  end if;
               end if;
            when An_Indexed_Component | A_Slice =>
               Target_Entity := Prefix (Target_Entity);
               if Is_Access_Expression (Target_Entity) then -- implicit dereference
                  Target_Decl := Nil_Element;
                  exit;
               end if;
            when An_Explicit_Dereference =>
               Target_Decl := Nil_Element;
               exit;
            when An_Attribute_Reference =>
               Target_Decl := Nil_Element;
               exit;
            when others =>
               -- Various kinds of expressions, including A_Function_Call: treat as function calls
               Target_Entity := Nil_Element;
               Target_Decl   := Nil_Element;
               exit;
         end case;
      end loop;

      -- Set the appropriate subrule and corresponding possible filters.
      -- Filters are set to Positive if matched, to Negative if not matched,
      --    and to None if not appropriate for the subrule
      case A_Renaming_Declaration (Declaration_Kind (Decl)) is
         when An_Object_Renaming_Declaration =>
            if Is_Nil (Target_Entity) then
               Subrule := SR_Function_Call;
            else
               Subrule := SR_Object;
            end if;
            Filter := Initial_Filter (Subrule);
         when An_Exception_Renaming_Declaration =>
            Subrule := SR_Exception;
            Filter  := Initial_Filter (Subrule);
         when A_Package_Renaming_Declaration =>
            Subrule := SR_Package;
            Filter  := Initial_Filter (Subrule);
         when A_Procedure_Renaming_Declaration =>
            Subrule := SR_Procedure;
            Filter  := Initial_Filter (Subrule);
            if not Is_Equal (Decl, Corresponding_Declaration (Decl)) then
               Filter (F_As_Body) := Positive;
            end if;
         when A_Function_Renaming_Declaration =>
            Subrule := SR_Function;
            Filter  := Initial_Filter (Subrule);
            if not Is_Equal (Decl, Corresponding_Declaration (Decl)) then
               Filter (F_As_Body) := Positive;
            end if;
            if Expression_Kind (Target_Entity) = An_Operator_Symbol then
               Filter (F_Operator) := Positive;
            end if;
            if Defining_Name_Kind (Name) = A_Defining_Operator_Symbol then
               Filter (F_As_Operator) := Positive;
            end if;
         when A_Generic_Package_Renaming_Declaration =>
            Subrule := SR_Package;
            Filter  := Initial_Filter (Subrule);
            Filter (F_Generic) := Positive;
         when A_Generic_Procedure_Renaming_Declaration =>
            Subrule := SR_Procedure;
            Filter  := Initial_Filter (Subrule);
            Filter (F_Generic) := Positive;
            Filter (F_As_Body) := None;
         when A_Generic_Function_Renaming_Declaration =>
            Subrule := SR_Function;
            Filter  := Initial_Filter (Subrule);
            Filter (F_Generic) := Positive;
            Filter (F_As_Body) := None;
      end case;

      if Subrule /= SR_Function_Call then   -- no filter appropriate for SR_Function call
         if not Is_Nil (Target_Decl) then
            if Declaration_Kind (Target_Decl) in A_Renaming_Declaration then
               Filter (F_Renaming) := Positive;
            end if;

            if Is_Nil (Enclosing_Element (Target_Decl)) then
               Filter (F_Library_Unit) := Positive;
            elsif Is_Equal (Enclosing_Element (Target_Decl), Enclosing_Element (Decl)) then
               Filter (F_Synonym) := Positive;
            end if;
         end if;

         if Expression_Kind (Target_Entity) in An_Identifier | An_Operator_Symbol
           and then To_Upper (Defining_Name_Image (Name)) = To_Upper (Name_Image (Target_Entity))
         then
            Filter (F_Identical) := Positive;
         end if;
      end if;

      Do_Report (Name, Subrule, Filter);
   end Process_Renaming_Declaration;

begin  -- Rules.Renaming_Declarations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Renaming_Declarations;
