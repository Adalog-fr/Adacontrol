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

-- ASIS
with
  Asis.Declarations,
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
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Symbol_Table;
pragma Elaborate (Framework.Language);

package body Rules.No_Operator_Usage is
   use Framework;

   type Operator_Class is (Arithmetic, Logical);

   type Filter_Kind is (None, Logical);
   package Filter_Flag_Utilities is new Framework.Language.Flag_Utilities (Filter_Kind);

   type Usage_Flags is array (Filter_Kind) of Boolean;

   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;

   type Usage_Contexts is array (Filter_Kind) of Basic_Rule_Context;
   Usage : Usage_Contexts;

   type Operator_Usage is array (Operator_Class) of Boolean;
   package Type_Usage is new Framework.Symbol_Table.Data_Access (Operator_Usage);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Filter_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags ("Parameter:");
      User_Message ("Control integer types where no operators, except as indicated, are used");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language, Filter_Flag_Utilities;
      Key : Filter_Kind;
   begin
      if Parameter_Exists then
         Key := Get_Flag_Parameter (Allow_Any => False);

         if Rule_Used (Key) then
            Parameter_Error (Rule_Id, "rule can be specified only once for each parameter");
         end if;

         Rule_Used (Key) := True;
         Usage (Key)     := Basic.New_Context (Rule_Type, Label);
      else
         if Rule_Used /= Usage_Flags'(others => False) then
            Parameter_Error (Rule_Id, "rule can be specified only once for each parameter");
         end if;

         Rule_Used := Usage_Flags'(others => True);
         Usage     := Usage_Contexts'(others => Basic.New_Context (Rule_Type, Label));
      end if;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => False);
            Type_Usage.Clear;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
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
      use Asis, Asis.Declarations, Asis.Elements;

      Good_Def : Asis.Definition;
      Tmp_Usage    : Operator_Usage;
      Name     : Asis.Expression;
   begin
      if Rule_Used = Usage_Flags'(others => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Good_Def := Definition;

      -- Get rid of derived types
      if Type_Kind (Good_Def) = A_Derived_Type_Definition then
         Good_Def := Type_Declaration_View (A4G_Bugs.Corresponding_Root_Type (Good_Def));
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
      if Rule_Used = Usage_Flags'(others => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Operator_Kind (Oper) is
         when An_And_Operator
            | An_Or_Operator
            | An_Xor_Operator
              =>
            if not Rule_Used (Logical) then
               return;
            end if;
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

         -- Here, E is the declaration of the type the operator operates on.
         if Type_Kind (Type_Declaration_View (Ultimate_Type_Declaration (E)))
            not in A_Signed_Integer_Type_Definition .. A_Modular_Type_Definition
         then
            return;
         end if;

         declare
            Type_Name : constant Asis.Expression := Names (E) (1);
            U         : Operator_Usage := Type_Usage.Fetch (Oper, Default => (others => False));
         begin
            U (Class) := True;
            Type_Usage.Store (Type_Name, U);
         end;
      end if;
   end Process_Operator;

   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Report_One (Entity : Asis.Defining_Name; Operator_Used : in out Operator_Usage) is
      use Reports, Thick_Queries;
   begin
      if Operator_Used (Arithmetic) then
         return;
      end if;

      if Operator_Used (Logical) then
         if Rule_Used (Logical) then
            Report (Rule_Id,
                    Usage (Logical),
                    Get_Location (Entity),
                    "only logical operator(s) used with type " & Full_Name_Image (Entity));
         end if;
      else
         if Rule_Used (None) then
            Report (Rule_Id,
                    Usage (None),
                    Get_Location (Entity),
                    "no operator used with type " & Full_Name_Image (Entity));
         end if;
      end if;
   end Report_One;

   procedure Do_Report is new Type_Usage.On_Every_Entity_From_Scope (Report_One);

   procedure Process_Scope_Exit is
   begin
      if Rule_Used = Usage_Flags'(others => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Do_Report;
   end Process_Scope_Exit;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB     => Help'Access,
                                     Add_Use_CB  => Add_Use'Access,
                                     Command_CB  => Command'Access,
                                     Prepare_CB  => Prepare'Access,
                                     Finalize_CB => Finalize'Access);
end Rules.No_Operator_Usage;
