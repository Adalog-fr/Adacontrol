----------------------------------------------------------------------
--  Rules.No_Operator_Usage - Package body                          --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2007.           --
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

-- AdaControl
with
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Symbol_Table;
pragma Elaborate (Framework.Language);

package body Rules.No_Operator_Usage is
   use Framework, Framework.Control_Manager, Framework.Language.Shared_Keys;

   -- Algorithm
   --
   -- Nothing fancy here: during traversal we note how the type is used by an entry in a symbol table.
   -- At scope exit, corresponding messages are issued.
   --
   -- Since we allow various combinations of filters, we keep in a list the parameters of the controls,
   -- but since we don't allow two identical controls, the length of the list is bounded by
   -- (# of filters) * (# of observed)

   type Observed is (Relational, Logical, Indexing);
   package Observed_Flag_Utilities is new Framework.Language.Flag_Utilities (Observed);

   type Filters is (F_Used, F_Not, F_Ignore, F_Report);
   package Filters_Modifiers_Utilities is new Framework.Language.Modifier_Utilities (Filters, Prefix => "F_");

   -- Max number of possible distinct controls:
   Max_Controls : constant := (Filters'Pos (Filters'Last) + 1) * (Observed'Pos (Observed'Last) + 1);
   type Observed_Filters is array (Observed) of Filters;
   type Control_Parameters is
      record
         Category        : Categories;
         Observed_Filter : Observed_Filters;
         Context         : Basic_Rule_Context;
      end record;
   Given_Controls : array (Asis.List_Index range 1 .. Max_Controls) of Control_Parameters;
   Rule_Used : Asis.ASIS_Natural := 0;
   Save_Used : Asis.ASIS_Natural;

   type Operator_Class is (Arithmetic, Relational, Logical, Indexing);
   -- Indexing is a bit of a stretch as an operator...
   type Operator_Usage is array (Operator_Class) of Boolean;
   type Type_Info is
      record
         Category : Categories;
         Usage    : Operator_Usage;
      end record;
   package Type_Usage is new Framework.Symbol_Table.Data_Access (Type_Info);

   Expected_Categories : constant Categories_Set := Integer_Set;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Observed_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control integer types where no arithmetic operators are used");
      User_Message;
      User_Message ("Parameter(1): [<category>] [<filter>] <observed> (optional)");
      User_Message ("Parameter(2..): [<filter>] <observed>");
      Help_On_Categories (Expected => Expected_Categories);
      Filters_Modifiers_Utilities.Help_On_Modifiers ("<filter>: ", Extra_Value => "");
      Help_On_Flags ("<observed>: ");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Observed_Flag_Utilities, Filters_Modifiers_Utilities, Categories_Utilities;
      Cat     : Categories := Cat_Any;
      Filter  : Filters;
      Subrule : Observed;
      SF      : Observed_Filters := (others => F_Ignore);
      Given   : array (Observed) of Boolean := (others => False);
   begin
      if Parameter_Exists then
         -- The error message is more accurate if we allow any category to Get_Modifier and then check explicitely
         Cat := Get_Modifier (Required => False);  -- Returns Cat_Any by default
         if Cat /= Cat_Any and then Cat not in Integer_Categories then
            Parameter_Error (Rule_Id, "Only ""mod"" and ""range"" allowed as categories");
         end if;
         while Parameter_Exists loop
            Filter  := Get_Modifier (Required => False);
            Subrule := Get_Flag_Parameter (Allow_Any => False);

            if Given (Subrule) then
               Parameter_Error (Rule_Id, "same <observed> given twice in control");
            end if;
            SF    (Subrule) := Filter;
            Given (Subrule) := True;
         end loop;
      end if;

      -- Check if already there, but allow same value being specified twice if one (and only one) is "count"
      for Cp : Control_Parameters of Given_Controls (1 .. Rule_Used) loop
         if Cp.Observed_Filter = SF
           and then (Cp.Category = Cat_Any
                     or else Cat = Cat_Any
                     or else Cp.Category = Cat)
           and then (Cp.Context.Ctl_Kind = Count) = (Ctl_Kind = Count)
         then
            Parameter_Error (Rule_Id, "combination of parameters already given");
         end if;
      end loop;

      Rule_Used                  := Rule_Used + 1; -- No need to check since it allows all possible combinations
      Given_Controls (Rule_Used) := (Cat, SF, Basic.New_Context (Ctl_Kind, Ctl_Label));
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
      Process_Scope_Exit (Asis.Nil_Element);
   end Finalize;

   ------------------
   -- Update_Store --
   ------------------

   procedure Update_Store (T : Asis.Declaration; Class : Operator_Class) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Thick_Queries;

      Decl : Asis.Declaration;
   begin
      Decl := A4G_Bugs.Corresponding_First_Subtype (T);

      if Type_Kind (Type_Declaration_View (Ultimate_Type_Declaration (Decl)))
         not in A_Signed_Integer_Type_Definition .. A_Modular_Type_Definition
      then
         return;
      end if;

      declare
         Type_Name : constant Asis.Expression := Names (Decl) (1);
         TI        : Type_Info                := Type_Usage.Fetch (Type_Name, Default => (Cat_Any, (others => False)));
      begin
         TI.Usage (Class) := True;
         Type_Usage.Store (Type_Name, TI);
      end;
   end Update_Store;

   -----------------------------
   -- Process_Type_Definition --
   -----------------------------

   procedure Process_Type_Definition (Definition : in Asis.Definition) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements;

      Good_Def  : Asis.Definition;
      Tmp_Usage : Type_Info;
      Name      : Asis.Expression;
      Kind      : Type_Kinds;
      subtype Integer_Kinds is Type_Kinds range A_Signed_Integer_Type_Definition .. A_Modular_Type_Definition;
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

      Kind := Type_Kind (Good_Def);
      if Kind not in Integer_Kinds then
         return;
      end if;

      Name      := Names (Enclosing_Element (Definition)) (1);
      Tmp_Usage := Type_Usage.Fetch (Name, Default => (Cat_Any, (others => False)));
      -- If it was already there, don't change it (but update category)
      -- If not, Usage has the initial value
      -- => In all cases, don't change Usage (but update needed anyway)
      case Integer_Kinds'(Kind) is
         when A_Signed_Integer_Type_Definition =>
            Tmp_Usage.Category := Cat_Range;
         when A_Modular_Type_Definition =>
            Tmp_Usage.Category := Cat_Mod;
      end case;
      Type_Usage.Store (Name, Tmp_Usage);
   end Process_Type_Definition;

   ----------------------
   -- Process_Operator --
   ----------------------

   procedure Process_Operator (Oper : in Asis.Expression) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;

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
         when A_Less_Than_Operator
            | A_Less_Than_Or_Equal_Operator
            | A_Greater_Than_Operator
            | A_Greater_Than_Or_Equal_Operator
            =>
            Class := Relational;
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

         -- Here, E is the declaration of the type the operator operates on.
         Update_Store (E, Class);
      end if;
   end Process_Operator;

   ------------------------
   -- Process_Membership --
   ------------------------

   procedure Process_Membership (Test : in Asis.Expression) is
   -- Like Process_Operator, but for membership tests
   -- Class is Relational
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;

      Decl : Asis.Declaration;
   begin
      if Rule_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Decl := A4G_Bugs.Corresponding_Expression_Type (Membership_Test_Expression (Test));

      if Is_Nil (Decl)
        or else Is_Nil (Enclosing_Element (Decl))
        or else (Declaration_Kind (Decl) = An_Ordinary_Type_Declaration
                 and then Type_Kind (Type_Declaration_View (Decl)) = A_Root_Type_Definition)
      then
         -- Expression is universal or equivalent => give up
         -- We could try harder to get the type from the RHS list, but constructs such as
         -- "5 in T'Range" are unlikely
         return;
      end if;

      Update_Store (Decl, Relational);
   end Process_Membership;

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
         Type_Name : Asis.Defining_Name;
         TI        : Type_Info;
      begin
         for N : Asis.Defining_Name of Index_Subtypes_Names (Definition) loop
            if Type_Category (N, Follow_Derived => True) in Integer_Types then
               Type_Name           := Names (A4G_Bugs.Corresponding_First_Subtype (Enclosing_Element (N)))(1);
               TI                  := Type_Usage.Fetch (Type_Name, Default => (Cat_Any, (others => False)));
               TI.Usage (Indexing) := True;
               Type_Usage.Store (Type_Name, TI);
            end if;
         end loop;
      end;
   end Process_Array_Definition;


   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Report_One (Entity : Asis.Defining_Name; Info : in out Type_Info) is

      procedure Do_Report (Param : Control_Parameters) is
         use Ada.Strings.Wide_Unbounded;
         use Framework.Locations, Framework.Reports, Thick_Queries, Utilities;
         Extra : Unbounded_Wide_String := Null_Unbounded_Wide_String;
      begin
         if Param.Category /= Cat_Any and then Param.Category /= Info.Category then
            return;
         end if;

         case Param.Observed_Filter (Relational) is
            when F_Used =>
               if not Info.Usage (Relational) then
                  return;
               end if;
               Append (Extra, ", relational operator");
            when F_Not =>
               if Info.Usage (Relational) then
                  return;
               end if;
               Append (Extra, ", no relational operator");
            when F_Report =>
               Append (Extra, Choose (Info.Usage (Relational),  ", ", ", no ") & "relational operator");
            when F_Ignore =>
               null;
         end case;

         case Param.Observed_Filter (Logical) is
            when F_Used =>
               if not Info.Usage (Logical) then
                  return;
               end if;
               Append (Extra, ", logical operator");
            when F_Not =>
               if Info.Usage (Logical) then
                  return;
               end if;
               Append (Extra, ", no logical operator");
            when F_Report =>
               Append (Extra, Choose (Info.Usage (Logical),  ", ", ", no ") & "logical operator");
            when F_Ignore =>
               null;
         end case;

         case Param.Observed_Filter (Indexing) is
            when F_Used =>
               if not Info.Usage (Indexing) then
                  return;
               end if;
               Append (Extra, ", indexing");
            when F_Not =>
               if Info.Usage (Indexing) then
                  return;
               end if;
               Append (Extra, ", no indexing");
            when F_Report =>
               Append (Extra, Choose (Info.Usage (Indexing),  ", ", ", no ") & "indexing");
            when F_Ignore =>
               null;
         end case;

         Report (Rule_Id,
                 Param.Context,
                 Get_Location (Entity),
                 "type " & Full_Name_Image (Entity) & " uses no arithmetic operator" & To_Wide_String (Extra));
      end Do_Report;

   begin   -- Report_One
      if Info.Usage (Arithmetic) then
         return;
      end if;

      for Cont : Control_Parameters of Given_Controls (1 .. Rule_Used) loop
         Do_Report (Cont);
      end loop;
   end Report_One;

   procedure Report_All is new Type_Usage.On_Every_Entity_From_Scope (Report_One);

   procedure Process_Scope_Exit (Scope : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Framework.Symbol_Table;
   begin
      if Rule_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if not Is_Nil (Scope) then   -- Nil when finalizing
         -- Process at scope exit of body if any. Only (non-generic) packages have an optional body
         if Declaration_Kind (Scope) in A_Procedure_Declaration        | A_Function_Declaration
                                      | A_Single_Task_Declaration      | A_Task_Type_Declaration
                                      | A_Single_Protected_Declaration | A_Protected_Type_Declaration
                                      | A_Generic_Declaration
         then
            return;
         end if;
         if Declaration_Kind (Scope) = A_Package_Declaration and then not Is_Nil (Corresponding_Body (Scope)) then
            return;
         end if;
      end if;

      Report_All (Visibility_Scope);
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
