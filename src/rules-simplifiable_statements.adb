----------------------------------------------------------------------
--  Rules.Simplifiable_Statement - Package body                     --
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
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Simplifiable_Statements is
   use Framework;

   type Subrules is (Stmt_Block, Stmt_Handler,     Stmt_If,   Stmt_If_For_Case,
                     Stmt_Loop,  Stmt_Nested_Path, Stmt_Null, Stmt_Dead);

   package Subrule_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "STMT_");
   use Subrule_Flags_Utilities;

   type Usage_Flags is array (Subrules) of Boolean;
   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;
   Usage     : array (Subrules) of Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      Help_On_Flags ("Parameter(s):");
      User_Message  ("Control Ada statements that can be made simpler");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language;
      Stmt    : Subrules;

   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Stmt := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Stmt) then
               Parameter_Error (Rule_Id, "statement already given: " & Image (Stmt));
            end if;

            Rule_Used (Stmt) := True;
            Usage (Stmt)     := Basic.New_Context (Rule_Type, Label);
         end loop;
      else
         Rule_Used := (others => True);
         Usage     := (others => Basic.New_Context (Rule_Type, Label));
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
            Rule_Used  := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ---------------------------
   -- Is_Breaking_Statement --
   ---------------------------

   function Is_Breaking_Statement (S : Asis.Statement) return Boolean is
      use Asis, Asis.Elements, Asis.Statements;
      use Thick_Queries, Utilities;
      SP : Asis.Expression;
   begin
      case Statement_Kind (S) is
         when A_Raise_Statement
            | A_Requeue_Statement
            | A_Requeue_Statement_With_Abort
            | A_Return_Statement
            | A_Goto_Statement
              =>
            return True;
         when An_Exit_Statement =>
            return Is_Nil (Exit_Condition (S))
              or else Static_Expression_Value_Image (Exit_Condition (S)) = "1";
         when A_Procedure_Call_Statement =>
            SP := Called_Simple_Name (S);
            if Is_Nil (SP) then
               -- a dynamic subprogram
               return False;
            end if;
            declare
               SP_Name : constant Wide_String := To_Upper (Full_Name_Image (SP));
            begin
               return SP_Name = "ADA.EXCEPTIONS.RAISE_EXCEPTION";
               -- Since Reraise_Occurrence (Null_Occurrence) does nothing, Reraise_Occurrence cannot
               -- be considered a breaking statement
            end;
         when others =>
            return False;
      end case;
   end Is_Breaking_Statement;


   -----------
   -- Check --
   -----------

   Not_Appropriate_For_Case : exception;

   procedure Check_Condition (Expr : Asis.Expression; Pivot : in out Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Utilities, Thick_Queries;

      procedure Update_Pivot (Var : Asis.Expression) is
         Good_Var : Asis.Expression := Var;
      begin
         while Expression_Kind (Good_Var) = A_Parenthesized_Expression loop
            Good_Var := Expression_Parenthesized (Good_Var);
         end loop;

         if Is_Nil (Pivot) then
            if Expression_Type_Kind (Good_Var) not in An_Enumeration_Type_Definition .. A_Modular_Type_Definition then
               -- Not a discrete type
               raise Not_Appropriate_For_Case;
            end if;
            Pivot := Good_Var;
         elsif Variables_Proximity (Pivot, Good_Var) /= Same_Variable then
            raise Not_Appropriate_For_Case;
         end if;
      end Update_Pivot;
   begin
      case Expression_Kind (Expr) is
         when Not_An_Expression =>
            Failure ("Not an expression in traverse", Expr);

         when A_Function_Call =>
            declare
               Func_Name : Asis.Expression := Prefix (Expr);
               Decl      : Asis.Declaration;
            begin
               if Expression_Kind (Func_Name) = A_Selected_Component then
                  Func_Name := Selector (Func_Name);
               end if;
               case Expression_Kind (Func_Name) is
                  when An_Operator_Symbol =>
                     -- Check that the operator is the real one, not some user-defined function
                     -- For predefined operations, either there is no "fake" declaration and
                     -- Corresponding_Name_Declaration returns Nil_Element (GNAT case), or the
                     -- Declaration_Origin is An_Implicit_Predefined_Declaration.
                     Decl := Corresponding_Name_Declaration (Func_Name);

                     if not Is_Nil (Decl)
                       and then Declaration_Origin (Decl) /= An_Implicit_Predefined_Declaration
                     then
                        raise Not_Appropriate_For_Case;
                     end if;

                     case Operator_Kind (Func_Name) is
                        when Not_An_Operator =>
                           Failure ("Wrong operator", Func_Name);

                        when A_Not_Operator =>
                           -- Traverse parameter only (not the function name)
                           Check_Condition (Actual_Parameter (Function_Call_Parameters (Expr) (1)), Pivot);

                        when An_And_Operator
                          | An_Or_Operator
                          | An_Xor_Operator
                          =>
                           declare
                              Params : constant Asis.Association_List := Function_Call_Parameters (Expr);
                           begin
                              for I in Params'Range loop
                                 Check_Condition (Actual_Parameter (Params (I)), Pivot);
                              end loop;
                           end;

                        when An_Equal_Operator
                          | A_Not_Equal_Operator
                          | A_Less_Than_Operator
                          | A_Less_Than_Or_Equal_Operator
                          | A_Greater_Than_Operator
                          | A_Greater_Than_Or_Equal_Operator
                          =>
                           declare
                              Param_List : constant Asis.Association_List := Function_Call_Parameters (Expr);
                              Param      : Asis.Expression;
                           begin
                              Param := Actual_Parameter (Param_List (1));
                              if Static_Expression_Value_Image (Param) = "" then
                                 -- Left operand not static, assume it is a variable
                                 Update_Pivot (Param);
                                 if Static_Expression_Value_Image (Actual_Parameter (Param_List (2))) = "" then
                                    -- Left operand is the good variable, but right is not static
                                    raise Not_Appropriate_For_Case;
                                 end if;
                              else
                                 -- Left operand is static value, right must be variable
                                 Update_Pivot (Actual_Parameter (Param_List (2)));
                              end if;
                           end;

                        when others =>
                           -- Arithmetic operators
                           -- If we encounter these during normal traversal, it is not as part
                           -- of a comparison
                           raise Not_Appropriate_For_Case;
                     end case;

                  when An_Identifier   -- A user-defined function
                    | An_Attribute_Reference
                    =>
                     -- If we encounter these during normal traversal, it is not as part
                     -- of a comparison
                     raise Not_Appropriate_For_Case;

                  when others =>
                     Failure ("Wrong function name", Func_Name);
               end case;
            end;

         when An_And_Then_Short_Circuit
           | An_Or_Else_Short_Circuit
           =>
            Check_Condition (Short_Circuit_Operation_Left_Expression (Expr), Pivot);
            Check_Condition (Short_Circuit_Operation_Right_Expression (Expr), Pivot);

         when A_Parenthesized_Expression =>
            Check_Condition (Expression_Parenthesized (Expr), Pivot);

         when An_In_Range_Membership_Test
           | A_Not_In_Range_Membership_Test
           =>
            if Discrete_Constraining_Lengths (Membership_Test_Range (Expr)) = (1 => Not_Static) then
               raise Not_Appropriate_For_Case;
            end if;
            Update_Pivot (Membership_Test_Expression (Expr));

         when An_In_Type_Membership_Test
           | A_Not_In_Type_Membership_Test
           =>
            if Discrete_Constraining_Lengths (Membership_Test_Subtype_Mark (Expr)) = (1 => Not_Static) then
               raise Not_Appropriate_For_Case;
            end if;
            Update_Pivot (Membership_Test_Expression (Expr));

         when A_Type_Conversion
           | A_Qualified_Expression
           =>
            Check_Condition (Converted_Or_Qualified_Expression (Expr), Pivot);

         when others =>
            raise Not_Appropriate_For_Case;
      end case;
   end Check_Condition;


   -------------------------------
   -- Process_Exception_Handler --
   -------------------------------

   procedure Process_Exception_Handler (Handler : in Asis.Exception_Handler) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Reports, Thick_Queries;

      function Is_Trivial_Handler (H : in Asis.Exception_Handler) return Boolean is
         -- A trivial handler contains only a "raise;" statement
         Stmts : constant Asis.Statement_List := Handler_Statements (H);
      begin
         if Stmts'Length /= 1 then
            return False;
         end if;

         if Statement_Kind (Stmts (Stmts'First)) /= A_Raise_Statement
           or else not Is_Nil (Raised_Exception (Stmts (Stmts'First)))
         then
            return False;
         end if;

         return True;
      end Is_Trivial_Handler;
   begin
      if not Rule_Used (Stmt_Handler) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if not Is_Trivial_Handler (Handler) then
         return;
      end if;

      -- Here, we have a trivial handler.
      -- If the handler is not itself for "others", check that there is no handler for "others",
      -- unless this handler is also trivial
      if Definition_Kind (Exception_Choices (Handler) (1)) /= An_Others_Choice then
         declare
            All_Handlers : constant Asis.Exception_Handler_List := Exception_Handlers (Enclosing_Element (Handler));
         begin
            if Definition_Kind (Exception_Choices (All_Handlers (All_Handlers'Last)) (1)) = An_Others_Choice then
               if not Is_Trivial_Handler (All_Handlers (All_Handlers'Last)) then
                  return;
               end if;
            end if;
         end;
      end if;

      Report (Rule_Id,
              Usage (Stmt_Handler),
              Get_Location (Handler),
              "useless exception handler");
   end Process_Exception_Handler;

   --------------------------
   -- Process_If_Statement --
   --------------------------

   procedure Process_If_Statement (Stmt  : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Reports, Thick_Queries;

      Then_Last_Stmt : Asis.Statement;
      Else_Last_Stmt : Asis.Statement;
      Paths          : constant Asis.Path_List := Statement_Paths (Stmt);
   begin
      -- Stmt_If
      if Rule_Used (Stmt_If) and then Path_Kind (Paths (Paths'Last)) = An_Else_Path then
         if Are_Null_Statements (Sequence_Of_Statements (Paths (Paths'Last))) then
            if Rule_Used (Stmt_If) then
               Report (Rule_Id,
                       Usage (Stmt_If),
                       Get_Location (Paths (Paths'Last)),
                       "empty else path");
            end if;

         elsif Paths'Length = 2 and Rule_Used (Stmt_Nested_Path) then
            -- Since we have an else path here, Paths'Length = 2 means that we have no elsif
            Then_Last_Stmt := Last_Effective_Statement (Sequence_Of_Statements (Paths (Paths'First)));
            Else_Last_Stmt := Last_Effective_Statement (Sequence_Of_Statements (Paths (Paths'Last )));
            if Is_Breaking_Statement (Then_Last_Stmt)
              and then Statement_Kind (Then_Last_Stmt) /= Statement_Kind (Else_Last_Stmt)
            then
               Report (Rule_Id,
                       Usage (Stmt_If),
                       Get_Location (Paths (Paths'Last)),
                       "content of else path can be moved outside ""if"" statement");
            elsif Is_Breaking_Statement (Else_Last_Stmt)
              and then Statement_Kind (Then_Last_Stmt) /= Statement_Kind (Else_Last_Stmt)
            then
               -- Prettier to put message in front of "then" than in front of "if"
               declare
                  Stats : constant Asis.Statement_List := Sequence_Of_Statements (Paths (Paths'First));
               begin
                  Report (Rule_Id,
                          Usage (Stmt_If),
                          Get_Previous_Word_Location (Stats (Stats'First)),
                          "content of then path can be moved outside ""if"" statement");
               end;
            end if;
         end if;
      end if;

      -- Stmt_If_For_Case
      if Rule_Used (Stmt_If_For_Case) then
         declare
            Pivot_Var   : Asis.Expression := Nil_Element;
            Special_Var : Asis.Expression := Nil_Element;
            Last_Elsif  : Positive;
         begin
            if Paths'Length = 1
              or else (Paths'Length = 2 and Path_Kind (Paths (2)) = An_Else_Path)
            then
               -- if .. then .. end if; or if .. then .. else .. end if;
               -- => Not worth a case statement
               return;
            end if;
            if Path_Kind (Paths (Paths'Last)) = An_Else_Path then
               Last_Elsif := Paths'Last - 1;
            else
               Last_Elsif := Paths'Last;
            end if;

            Check_Condition (Condition_Expression (Paths (1)), Pivot => Pivot_Var);

            for I in Positive range 2 .. Last_Elsif loop
               Check_Condition (Condition_Expression (Paths (I)), Pivot => Special_Var);
               if Variables_Proximity (Pivot_Var, Special_Var) /= Same_Variable then
                  return;
               end if;
            end loop;

            Report (Rule_Id,
                    Usage (Stmt_If_For_Case),
                    Get_Location (Stmt),
                    "If statement could be replaced by case statement");

         exception
            when Not_Appropriate_For_Case =>
               null;
         end;
      end if;

      -- Stmt_Dead
      if Rule_Used (Stmt_Dead) then
         for P in Paths'Range loop
            if Path_Kind (Paths (P)) /= An_Else_Path
              and then Static_Expression_Value_Image (Condition_Expression (Paths (P))) = "0"
            then
               Report (Rule_Id,
                       Usage (Stmt_Dead),
                       Get_Location (Paths (P)),
                       "condition is always false");
            end if;
         end loop;
      end if;
   end Process_If_Statement;


   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Stmt : in Asis.Statement) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Statements;
      use Framework.Reports, Thick_Queries, Utilities;
   begin
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Statement_Kind (Stmt) is
         when A_Block_Statement =>
            if Rule_Used (Stmt_Block) then
               if Is_Nil (Statement_Identifier (Stmt))
                 and then Block_Declarative_Items  (Stmt) = Nil_Element_List
                 and then Block_Exception_Handlers (Stmt) = Nil_Element_List
               then
                  Report (Rule_Id,
                          Usage (Stmt_Block),
                          Get_Location (Stmt),
                          "unnecessary block statement");
               end if;
            end if;

         when An_If_Statement =>
            if Rule_Used (Stmt_If)
              or Rule_Used (Stmt_Nested_Path)
              or Rule_Used (Stmt_If_For_Case)
            then
               Process_If_Statement (Stmt);
            end if;

         when A_Null_Statement =>
            if Rule_Used (Stmt_Null) then
               -- If the enclosing statements contain only null statements without labels,
               -- the last one is not considered unnecessary.
               -- A null statement with label(s) is never deemed unnecessary
               if Is_Nil (Label_Names (Stmt)) then
                  declare
                     Stats : constant Asis.Statement_List := Thick_Queries.Statements (Enclosing_Element (Stmt));
                  begin
                     if not Is_Equal (Stmt, Stats (Stats'Last))
                       or else not Are_Null_Statements (Stats, Except_Labelled => True)
                     then
                        Report (Rule_Id,
                                Usage (Stmt_Null),
                                Get_Location (Stmt),
                                "unnecessary null statement");
                     end if;
                  end;
               end if;
            end if;

         when A_For_Loop_Statement =>
            if Rule_Used (Stmt_Dead) then
               if Discrete_Constraining_Lengths (Specification_Subtype_Definition
                                                 (For_Loop_Parameter_Specification
                                                  (Stmt)))(1) = 0
               then
                  Report (Rule_Id,
                          Usage (Stmt_Loop),
                          Get_Location (Stmt),
                          "for loop is never executed");
               end if;
            end if;

         when A_While_Loop_Statement =>
               if Rule_Used (Stmt_Loop) or Rule_Used (Stmt_Dead) then
                  declare
                     Expr   : constant Asis.Expression       := While_Condition (Stmt);
                     E_Kind : constant Asis.Expression_Kinds := Expression_Kind (Expr);
                  begin
                     -- For stmt_loop, signal an explicit "while True"
                     if Rule_Used (Stmt_Loop)
                       and then (E_Kind = An_Enumeration_Literal or E_Kind = A_Selected_Component)
                       and then To_Upper (Full_Name_Image (Expr)) = "STANDARD.TRUE"
                     then
                        Report (Rule_Id,
                                Usage (Stmt_Loop),
                                Get_Location (Stmt),
                                "while loop has True condition");
                     end if;

                     -- For stmt_dead, signal any expression statically false
                     if Rule_Used (Stmt_Dead)
                       and then Static_Expression_Value_Image (Expr) = "0"
                     then
                        Report (Rule_Id,
                                Usage (Stmt_Dead),
                                Get_Location (Stmt),
                                "while loop is never executed");
                     end if;
                  end;
               end if;
         when others =>
            null;
      end case;

      if Rule_Used (Stmt_Dead) and then Is_Breaking_Statement (Stmt) then
         declare
            Enclosing_Sts : constant Statement_List := Thick_Queries.Statements (Enclosing_Element (Stmt));
         begin
            if not Is_Equal (Stmt, Enclosing_Sts (Enclosing_Sts'Last)) then
               Report (Rule_Id,
                       Usage (Stmt_Dead),
                       Get_Location (Stmt),
                       "unreachable code after this statement");
            end if;
         end;
      end if;
   end Process_Statement;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB    => Help'Access,
                                     Add_Use_CB => Add_Use'Access,
                                     Command_CB => Command'Access);
end Rules.Simplifiable_Statements;
