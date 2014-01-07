----------------------------------------------------------------------
--  Rules.No_Operator_Usage - Package body                          --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2007. The Ada --
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
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Symbol_Table;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.No_Operator_Usage is
   use Framework, Framework.Control_Manager;

   -- Algorithm
   --
   -- Nothing fancy here: during traversal we note how the type is used by an entry in a symbol table.
   -- At scope exit, corresponding messages are issued.
   --
   -- Since we allow various combinations of filters, we keep in a list the parameters of the controls,
   -- but since we don't allow two identical controls, the length of the list is bounded by
   -- (# of filters) * (# of observed)

   type Observed is (Logical, Indexing);
   package Observed_Flag_Utilities is new Framework.Language.Flag_Utilities (Observed);

   type Filters is (F_Used, F_Not, F_Ignore, F_Report);
   package Filters_Modifiers_Utilities is new Framework.Language.Modifier_Utilities (Filters, Prefix => "F_");

   -- Max number of possible distinct controls:
   Max_Controls : constant := (Filters'Pos (Filters'Last) + 1) * (Observed'Pos (Observed'Last) + 1);
   type Observed_Filters is array (Observed) of Filters;
   type Control_Parameters is
      record
         Observed_Filter : Observed_Filters;
         Context         : Basic_Rule_Context;
      end record;
   Given_Controls : array (Asis.List_Index range 1 .. Max_Controls) of Control_Parameters;
   Rule_Used : Asis.ASIS_Natural := 0;
   Save_Used : Asis.ASIS_Natural;

   type Operator_Class is (Arithmetic, Logical, Indexing); -- Indexing is a bit of a stretch as an operator...
   type Operator_Usage is array (Operator_Class) of Boolean;
   package Type_Usage is new Framework.Symbol_Table.Data_Access (Operator_Usage);


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Observed_Flag_Utilities, Filters_Modifiers_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control integer types where no arithmetic operators are used");
      User_Message;
      User_Message ("Parameters: [<filter>] <observed>");
      Help_On_Modifiers ("<filter>  : ", Extra_Value => "");
      Help_On_Flags ("<observed>: ");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Observed_Flag_Utilities, Filters_Modifiers_Utilities;
      Filter1, Filter2   : Filters;
      Subrule1, Subrule2 : Observed;
      SF : Observed_Filters;
   begin
      if Parameter_Exists then
         Filter1  := Get_Modifier (Required => False);
         Subrule1 := Get_Flag_Parameter (Allow_Any => False);

         if Parameter_Exists then
            Filter2  := Get_Modifier (Required => False);
            Subrule2 := Get_Flag_Parameter (Allow_Any => False);
         else
            Filter2 := F_Ignore;
            case Subrule1 is
               when Logical =>
                  Subrule2 := Indexing;
               when Indexing =>
                  Subrule2 := Logical;
            end case;
         end if;

         if Subrule1 = Subrule2 then
            Parameter_Error (Rule_Id, "same <observed> given twice in control");
         end if;

      else
         Filter1  := F_Ignore;
         Subrule1 := Logical;
         Filter2  := F_Ignore;
         Subrule2 := Indexing;
      end if;
      SF (Subrule1) := Filter1;
      SF (Subrule2) := Filter2;

      -- Check if already there, but allow same value being specified twice if one (and only one) is "count"
      for C in Asis.List_Index range 1 .. Rule_Used loop
         if Given_Controls (C).Observed_Filter = SF
           and then (Given_Controls (C).Context.Ctl_Kind = Count) = (Ctl_Kind = Count)
         then
            Parameter_Error (Rule_Id, "combination of parameters already given");
         end if;
      end loop;

      Rule_Used                  := Rule_Used + 1; -- No need to check since it allows all possible combinations
      Given_Controls (Rule_Used) := (SF, Basic.New_Context (Ctl_Kind, Ctl_Label));
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := 0;
            Type_Usage.Clear;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := 0;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      -- Clean remaining data at scope 0 from previous run
      Type_Usage.Clear;
   end Prepare;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      -- Call Process_Scope_Exit to handle types declared in library package specifications
      -- (i.e. we are somehow exiting from scope 0)
      Process_Scope_Exit;
   end Finalize;

   -----------------------------
   -- Process_Type_Definition --
   -----------------------------

   procedure Process_Type_Definition (Definition : in Asis.Definition) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements;

      Good_Def : Asis.Definition;
      Tmp_Usage    : Operator_Usage;
      Name     : Asis.Expression;
   begin
      if Rule_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Good_Def := Definition;

      -- Get rid of derived types
      if Type_Kind (Good_Def) = A_Derived_Type_Definition then
         Good_Def := Type_Declaration_View (Corresponding_Root_Type (Good_Def));
      end if;

      if Type_Kind (Good_Def) not in A_Signed_Integer_Type_Definition .. A_Modular_Type_Definition then
         return;
      end if;

      Name      := Names (Enclosing_Element (Definition)) (1);
      Tmp_Usage := Type_Usage.Fetch (Name, Default => (others => False));
      -- If it was already there, don't change it
      -- If not, Usage has the initial value
      -- => In all cases, don't change it (but update in case it was not there)
      Type_Usage.Store (Name, Tmp_Usage);
   end Process_Type_Definition;

   ----------------------
   -- Process_Operator --
   ----------------------

   procedure Process_Operator (Oper : in Asis.Expression) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries;

      E     : Asis.Element;
      Class : Operator_Class;
   begin
      if Rule_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Operator_Kind (Oper) is
         when An_And_Operator
            | An_Or_Operator
            | An_Xor_Operator
              =>
            Class := Logical;
         when A_Unary_Plus_Operator
            | A_Unary_Minus_Operator
            | A_Plus_Operator
            | A_Minus_Operator
            | A_Multiply_Operator
            | A_Divide_Operator
            | A_Mod_Operator
            | A_Rem_Operator
            | An_Exponentiate_Operator
            | An_Abs_Operator
              =>
            Class := Arithmetic;
         when others =>
            return;
      end case;

      -- Here we have an appropriate operator, find which type it operates on.
      -- Go up to the function call, but beware that the name of the
      -- function may be composite.
      E := Enclosing_Element (Oper);
      while Expression_Kind (E) = A_Selected_Component loop
         E := Enclosing_Element (E);
      end loop;

      -- Only usage as function calls are (currently) considered,
      -- other cases (operator used as actual in instantiation) are ignored.
      -- Whether this is the right thing to do is not obvious, but it is definitely
      -- simpler to implement. We'll see if users complain...
      if Is_Dispatching_Call (E) then
         -- Certainly not operating on an integer type!
         return;
      elsif Expression_Kind (E) = A_Function_Call then
         declare
            Parameters : constant Asis.Element_List := Function_Call_Parameters (E);
         begin
            E := A4G_Bugs.Corresponding_Expression_Type (Actual_Parameter (Parameters (1)));

            -- Annoying cases:
            -- A string litteral will return a nil element for E
            -- A universal value will return a declaration, however there is
            -- not much we can do with it. We recognize universal values by the
            -- fact that the declaration has no enclosing element. If someone knows
            -- a better way...
            if (Is_Nil (E) or else Is_Nil (Enclosing_Element (E)))
              and Parameters'Length > 1
            then
               -- Try with the other parameter
               E := A4G_Bugs.Corresponding_Expression_Type (Actual_Parameter (Parameters (2)));
            end if;

            if Is_Nil (E)
              or else Is_Nil (Enclosing_Element (E))
              or else (Declaration_Kind (E) = An_Ordinary_Type_Declaration
                       and then Type_Kind (Type_Declaration_View (E)) = A_Root_Type_Definition)
            then
               -- All operands universal or equivalent => give up
               -- (anyway, it's a language defined operator)
               return;
            end if;
         end;
         E := Corresponding_First_Subtype (E);

         -- Here, E is the declaration of the type the operator operates on.
         if Type_Kind (Type_Declaration_View (Ultimate_Type_Declaration (E)))
            not in A_Signed_Integer_Type_Definition .. A_Modular_Type_Definition
         then
            return;
         end if;

         declare
            Type_Name : constant Asis.Expression := Names (E) (1);
            U         : Operator_Usage := Type_Usage.Fetch (Type_Name, Default => (others => False));
         begin
            U (Class) := True;
            Type_Usage.Store (Type_Name, U);
         end;
      end if;
   end Process_Operator;


   ------------------------------
   -- Process_Array_Definition --
   ------------------------------

   procedure Process_Array_Definition (Definition : in Asis.Definition) is
      use Asis.Declarations, Asis.Elements;
      use Thick_Queries;
   begin
      if Rule_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Subtypes   : constant Asis.Defining_Name_List := Index_Subtypes_Names (Definition);
         Type_Name  : Asis.Defining_Name;
         U          : Operator_Usage;
      begin
         for S in Subtypes'Range loop
            if Type_Category (Subtypes (S), Follow_Derived => True) in Integer_Types then
               Type_Name    := Names (Corresponding_First_Subtype (Enclosing_Element (Subtypes (S))))(1);
               U            := Type_Usage.Fetch (Type_Name, Default => (others => False));
               U (Indexing) := True;
               Type_Usage.Store (Type_Name, U);
            end if;
         end loop;
      end;
   end Process_Array_Definition;


   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Report_One (Entity : Asis.Defining_Name; Operator_Used : in out Operator_Usage) is

      procedure Do_Report (Param : Control_Parameters) is
         use Ada.Strings.Wide_Unbounded;
         use Reports, Thick_Queries, Utilities;
         Extra : Unbounded_Wide_String := Null_Unbounded_Wide_String;
      begin
         case Param.Observed_Filter (Logical) is
            when F_Used =>
               if not Operator_Used (Logical) then
                  return;
               end if;
               Append (Extra, ", logical operator");
            when F_Not =>
               if Operator_Used (Logical) then
                  return;
               end if;
               Append (Extra, ", no logical operator");
            when F_Report =>
               Append (Extra, Choose (Operator_Used (Logical),  ", ", ", no ") & "logical operator");
            when F_Ignore =>
               null;
         end case;

         case Param.Observed_Filter (Indexing) is
            when F_Used =>
               if not Operator_Used (Indexing) then
                  return;
               end if;
               Append (Extra, ", indexing");
            when F_Not =>
               if Operator_Used (Indexing) then
                  return;
               end if;
               Append (Extra, ", no indexing");
            when F_Report =>
               Append (Extra, Choose (Operator_Used (Indexing),  ", ", ", no ") & "indexing");
            when F_Ignore =>
               null;
         end case;

         Report (Rule_Id,
                 Param.Context,
                 Get_Location (Entity),
                 "type " & Full_Name_Image (Entity) & " uses no arithmetic operator" & To_Wide_String (Extra));
      end Do_Report;

   begin   -- Report_One
      if Operator_Used (Arithmetic) then
         return;
      end if;

      for C in Asis.List_Index range 1 .. Rule_Used loop
         Do_Report (Given_Controls (C));
      end loop;
   end Report_One;

   procedure Report_All is new Type_Usage.On_Every_Entity_From_Scope (Report_One);

   procedure Process_Scope_Exit is
      use Framework.Symbol_Table;
   begin
      if Rule_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Report_All (Visibility);
   end Process_Scope_Exit;

begin  -- Rules.No_Operator_Usage
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB         => Help'Access,
                                     Add_Control_CB  => Add_Control'Access,
                                     Command_CB      => Command'Access,
                                     Prepare_CB      => Prepare'Access,
                                     Finalize_CB     => Finalize'Access);
end Rules.No_Operator_Usage;
