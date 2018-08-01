----------------------------------------------------------------------
--  Rules.Simplifiable_Statement - Package body                     --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005.         --
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

-- AdaControl
with
     Framework.Reports.Fixes,
     Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Simplifiable_Statements is
   use Framework, Framework.Control_Manager;

   type Subrules is (Stmt_Block,  Stmt_Dead, Stmt_Handler,        Stmt_If,          Stmt_If_For_Case,
                     Stmt_If_Not, Stmt_Loop, Stmt_Loop_For_While, Stmt_Nested_Path, Stmt_Null);

   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "STMT_");
   use Subrules_Flags_Utilities;

   type Usage_Flags is array (Subrules) of Boolean;
   Not_Used : constant Usage_Flags := (others => False);
   Rule_Used : Usage_Flags := Not_Used;
   Save_Used : Usage_Flags;
   Usage     : array (Subrules) of Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control Ada statements that can be made simpler");
      User_Message;
      Help_On_Flags ("Parameter(s):");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Utilities;
      Subrule : Subrules;

   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Subrule := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Subrule) then
               Parameter_Error (Rule_Id, "statement already given: " & Image (Subrule, Lower_Case));
            end if;

            Rule_Used (Subrule) := True;
            Usage (Subrule)     := Basic.New_Context (Ctl_Kind, Ctl_Label);
         end loop;
      else
         Rule_Used := (others => True);
         Usage     := (others => Basic.New_Context (Ctl_Kind, Ctl_Label));
      end if;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := Not_Used;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Not_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ---------------------------
   -- Is_Breaking_Statement --
   ---------------------------

   function Is_Breaking_Statement (S : Asis.Statement) return Boolean is
   -- Note: An extended return is always breaking, even when exitable since if it is exited,
   --       the statement after it will not be executed.
   --       TBH: except if the statement is exited by a goto whose target is the statement
   --       after the extended return. But this is really a pathology, and in any case
   --       will result in a false positive, so no need to worry about.
      use Asis, Asis.Elements, Asis.Statements;
      use Thick_Queries, Utilities;
      SP : Asis.Expression;
   begin
      case Statement_Kind (S) is
         when A_Raise_Statement
            | A_Requeue_Statement
            | A_Requeue_Statement_With_Abort
            | A_Return_Statement
            | An_Extended_Return_Statement
            | A_Goto_Statement
              =>
            return True;
         when An_Exit_Statement =>
            return Is_Nil (Exit_Condition (S))
              or else Static_Expression_Value_Image (Exit_Condition (S)) = "1";  -- "1" corresponds to True
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

   -----------------------------
   -- Same_Breaking_Statement --
   -----------------------------

   function Same_Breaking_Statement (Left, Right : Asis.Statement) return Boolean is
      use Asis, Asis.Elements;
      L_Kind : constant Asis.Statement_Kinds := Statement_Kind (Left);
      R_Kind : constant Asis.Statement_Kinds := Statement_Kind (Right);
   begin
      if L_Kind = R_Kind then
         return True;
      end if;

      if (L_Kind = A_Return_Statement           and R_Kind = An_Extended_Return_Statement)
        or else
         (L_Kind = An_Extended_Return_Statement and R_Kind = A_Return_Statement)
      then
         return True;
      end if;

      return False;
   end Same_Breaking_Statement;

   ---------------------
   -- Check_Condition --
   ---------------------

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

   begin  -- Check_Condition
      case Expression_Kind (Expr) is
         when Not_An_Expression =>
            Failure ("Not an expression in traverse", Expr);

         when A_Function_Call =>
            declare
               Func_Name : constant Asis.Expression := Simple_Name (Prefix (Expr));
               Decl      : Asis.Declaration;
            begin
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
                           Failure ("Check_Condition: Wrong operator", Func_Name);

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
                              if Is_Static_Expression (Param) then
                                 -- Left operand is static value, right must be variable
                                 Update_Pivot (Actual_Parameter (Param_List (2)));
                              else
                                 -- Left operand not static, assume it is a variable
                                 Update_Pivot (Param);
                                 if not Is_Static_Expression (Actual_Parameter (Param_List (2))) then
                                    -- Left operand is the good variable, but right is not static
                                    raise Not_Appropriate_For_Case;
                                 end if;
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

         when An_In_Membership_Test
            | A_Not_In_Membership_Test
            =>
            -- Check each membership choice
            declare
               Choices : constant Asis.Element_List := Membership_Test_Choices (Expr);
               Name    : Asis.Expression;
            begin
               for C in Choices'Range loop
                  if Element_Kind (Choices (C)) = An_Expression then
                     Name := Simple_Name (Choices (C));
                     if Expression_Kind (Name) = An_Identifier
                       and then Declaration_Kind (Corresponding_Name_Declaration (Name))
                                in An_Ordinary_Type_Declaration .. A_Subtype_Declaration
                     then
                        -- in subtype_mark
                        if Type_Category (Name) not in Discrete_Types then
                           raise Not_Appropriate_For_Case;
                        end if;
                        if Discrete_Constraining_Lengths (Name) = (1 => Not_Static) then
                           raise Not_Appropriate_For_Case;
                        end if;
                     else
                        -- in value
                        if not Is_Static_Expression (Expr) then
                           raise Not_Appropriate_For_Case;
                        end if;
                     end if;
                  else
                     -- in range
                     if Discrete_Constraining_Lengths (Choices (C)) = (1 => Not_Static) then
                        raise Not_Appropriate_For_Case;
                     end if;
                  end if;
               end loop;
               Update_Pivot (Membership_Test_Expression (Expr));
            end;

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

   begin  -- Process_Exception_Handler
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
      Fixes.Delete (Handler);
   end Process_Exception_Handler;

   --------------------------
   -- Process_If_Statement --
   --------------------------

   procedure Process_If_Statement (Stmt  : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Reports, Thick_Queries;

      Then_Last_Stmt : Asis.Statement;
      Else_Last_Stmt : Asis.Statement;
      Paths          : constant Asis.Path_List := Statement_Paths (Stmt);
      If_Cond        : Asis.Expression;
      Op             : Asis.Operator_Kinds;
   begin
      -- Stmt_If, Stmt_Nested_Path, Stmt_If_Not
      if (Rule_Used (Stmt_If) or Rule_Used (Stmt_Nested_Path) or Rule_Used (Stmt_If_Not))
        and then Path_Kind (Paths (Paths'Last)) = An_Else_Path
      then
         if Rule_Used (Stmt_If) and then Are_Null_Statements (Sequence_Of_Statements (Paths (Paths'Last))) then
            Report (Rule_Id,
                    Usage (Stmt_If),
                    Get_Location (Paths (Paths'Last)),
                    "empty else path");
            Fixes.Delete (Paths (Paths'Last));
         end if;

         if Paths'Length = 2 then
            -- Since we have an else path here, Paths'Length = 2 means that we have no elsif
            if Rule_Used (Stmt_Nested_Path) then
               Then_Last_Stmt := Last_Effective_Statement (Sequence_Of_Statements (Paths (Paths'First)));
               Else_Last_Stmt := Last_Effective_Statement (Sequence_Of_Statements (Paths (Paths'Last )));
               if Is_Breaking_Statement (Then_Last_Stmt)
                 and then not Same_Breaking_Statement (Then_Last_Stmt, Else_Last_Stmt)
               then
                  Report (Rule_Id,
                          Usage (Stmt_Nested_Path),
                          Get_Location (Paths (Paths'Last)),
                          "content of else path can be moved outside ""if"" statement");
               elsif Is_Breaking_Statement (Else_Last_Stmt)
                 and then not Same_Breaking_Statement (Then_Last_Stmt, Else_Last_Stmt)
               then
                  -- Prettier to put message in front of "then" than in front of "if"
                  Report (Rule_Id,
                          Usage (Stmt_Nested_Path),
                          Get_Previous_Word_Location (Sequence_Of_Statements (Paths (Paths'First)), "THEN"),
                          "content of then path can be moved outside ""if"" statement");
               end if;
            end if;

            if Rule_Used (Stmt_If_Not) then
               If_Cond := Condition_Expression (Paths (Paths'First));
               while Expression_Kind (If_Cond) = A_Parenthesized_Expression loop
                  If_Cond := Expression_Parenthesized (If_Cond);
               end loop;
               if Expression_Kind (If_Cond) = A_Function_Call then
                  Op := Operator_Kind (Simple_Name (Prefix (If_Cond)));
                  if Op = A_Not_Operator or Op = A_Not_Equal_Operator then
                     Report (Rule_Id,
                             Usage (Stmt_If_Not),
                             Get_Location (If_Cond),
                             "Negative condition in ""if-else"" statement");
                  end if;
               end if;
            end if;
         end if;
      end if;

      -- Stmt_If_For_Case
      if Rule_Used (Stmt_If_For_Case) then
         declare
            Pivot_Var   : Asis.Expression := Nil_Element;
            Special_Var : Asis.Expression := Nil_Element;
            Last_Elsif  : List_Index;
         begin
            if Paths'Length = 1
              or else (Paths'Length = 2 and Path_Kind (Paths (2)) = An_Else_Path)
            then
               -- if .. then .. end if; or if .. then .. else .. end if;
               -- => Not worth a case statement
               raise Not_Appropriate_For_Case;
            end if;
            if Path_Kind (Paths (Paths'Last)) = An_Else_Path then
               Last_Elsif := Paths'Last - 1;
            else
               Last_Elsif := Paths'Last;
            end if;

            Check_Condition (Condition_Expression (Paths (1)), Pivot => Pivot_Var);

            for I in List_Index range 2 .. Last_Elsif loop
               Check_Condition (Condition_Expression (Paths (I)), Pivot => Special_Var);
               if Variables_Proximity (Pivot_Var, Special_Var) /= Same_Variable then
                  raise Not_Appropriate_For_Case;
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
         -- Special case for "if" path: if followed by "elsif", change it to "if". If followed by "else", replace
         -- by the content of the "else" path. And if none of these, remove the whole if statement
         if Static_Expression_Value_Image (Condition_Expression (Paths (Paths'First))) = "0"  -- "0" => False
         then
            Report (Rule_Id,
                    Usage (Stmt_Dead),
                    Get_Location (Paths (1)),
                    "condition is always false");
            if Paths'Length = 1 then -- no else or elsif
               Fixes.Delete (Stmt);
            else
               case Path_Kind (Paths (2)) is
                  when An_Elsif_Path =>
                     Fixes.Delete (Paths (1));
                     Fixes.Replace (Get_Location (Paths(2)), Length => 3, By => ""); -- 3 characters: "els"
                  when An_Else_Path =>
                     Fixes.Replace (Stmt, Sequence_Of_Statements (Paths (2), Include_Pragmas => True));
                  when others =>
                     Utilities.Failure ("Process_If_Statement: Incorrect path", Paths (2));
               end case;
            end if;
         end if;

         for P in List_Index range Paths'First + 1 .. Paths'Last loop
            if Path_Kind (Paths (P)) /= An_Else_Path
              and then Static_Expression_Value_Image (Condition_Expression (Paths (P))) = "0"  -- "0" => False
            then
               Report (Rule_Id,
                       Usage (Stmt_Dead),
                       Get_Location (Paths (P)),
                       "condition is always false");
               Fixes.Delete (Paths (P));
            end if;
         end loop;
      end if;
   end Process_If_Statement;

   ----------------------------
   -- Process_Case_Statement --
   ----------------------------

   procedure Process_Case_Statement (Stmt  : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Reports;
      use Utilities, Thick_Queries;
   begin
      -- Stmt_Dead
      if Rule_Used (Stmt_Dead) then -- always True (for the moment)
         declare
            Case_Paths   : constant Path_List := Statement_Paths (Stmt);
         begin
            for CP in Case_Paths'Range loop
               declare
                  Path_Elements : constant Element_List := Case_Statement_Alternative_Choices (Case_Paths (CP));
                  Temp          : Extended_Biggest_Natural;
                  Count         : Extended_Biggest_Natural := 0;
               begin
                  Count_Values :
                  for PE in Path_Elements'Range loop
                     if Definition_Kind (Path_Elements (PE)) = A_Discrete_Range then
                        if Discrete_Range_Kind (Path_Elements (PE)) = A_Discrete_Subtype_Indication
                          and then not Is_Nil (Corresponding_Static_Predicates
                                               (Subtype_Simple_Name (Path_Elements (PE))))
                        then
                           -- Subtype with static predicate used for a choice: we don't know (yet) how to evaluate this
                           Uncheckable (Rule_Id,
                                        False_Negative,
                                        Get_Location (Path_Elements (PE)),
                                        "(Dead) Use of subtype with static predicate");
                           Count := 1; -- or whatever /= 0
                           exit Count_Values;
                        end if;
                        Temp := Discrete_Constraining_Lengths (Path_Elements (PE)) (1);
                        if Temp = Not_Static then
                           -- it IS static, but the evaluator cannot evaluate it...
                           -- unless it is of a generic formal type
                           Uncheckable (Rule_Id,
                                        False_Negative,
                                        Get_Location (Path_Elements (PE)),
                                        "(Dead) Could not evaluate bounds of expression");
                           Count := 1; -- or whatever /= 0
                           exit Count_Values;
                        end if;
                        Count := Count + Temp;

                     elsif Definition_Kind (Path_Elements (PE)) = An_Others_Choice then
                        -- Not handled here, equivalent to check Case_Statement (Others_Span, min 1)
                        Count := 1; -- or whatever /= 0
                        exit Count_Values;

                     elsif Element_Kind (Path_Elements (PE)) = An_Expression then
                        Count := 1;
                        exit Count_Values;

                     else
                        Failure ("Unexpected path kind:", Path_Elements (PE));
                     end if;
                  end loop Count_Values;
                  if Count = 0 then
                     Report (Rule_Id,
                             Usage (Stmt_Dead),
                             Get_Location (Case_Paths (CP)),
                             "choices cover no value");
                     Framework.Reports.Fixes.Delete (Case_Paths (Cp));
                  end if;
               end;
            end loop;
         end;
      end if;
   end Process_Case_Statement;

   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Stmt : in Asis.Statement) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Statements;
      use Framework.Reports, Thick_Queries;
   begin
      if Rule_Used = Not_Used then
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
                  declare
                     Content : constant Asis.Statement_List := Thick_Queries.Statements (Stmt, Include_Pragmas => True);
                  begin
                     Fixes.Delete (From => Get_Location (Stmt),
                                   To   => Get_Location (Content (Content'First)));
                     Fixes.Delete (From => Get_End_Location (Content(Content'Last))+1,
                                   To   => Get_End_Location (Stmt) + 1);
                  end;
               end if;
            end if;

         when An_If_Statement =>
            if (Rule_Used and Usage_Flags'(Stmt_If          | Stmt_Nested_Path | Stmt_If_Not |
                                           Stmt_If_For_Case | Stmt_Dead => True,
                                           others                       => False)) /= Not_Used
            then
               Process_If_Statement (Stmt);
            end if;

         when A_Case_Statement =>
            if Rule_Used (Stmt_Dead) then
               Process_Case_Statement (Stmt);
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
                        Fixes.Delete (Stmt);
                     end if;
                  end;
               end if;
            end if;

         when A_Loop_Statement =>
            if Rule_Used (Stmt_Loop_For_While) then
               declare
                  Loop_Stmts : constant Asis.Statement_List := Loop_Statements (Stmt);
               begin
                  if Statement_Kind (Loop_Stmts (Loop_Stmts'First)) = An_Exit_Statement
                    and then Is_Equal (Stmt, Corresponding_Loop_Exited (Loop_Stmts (Loop_Stmts'First)))
                  then
                     Report (Rule_Id,
                             Usage (Stmt_Loop_For_While),
                             Get_Location (Stmt),
                             "simple loop can be changed to ""while""");
                  end if;
               end;
            end if;

         when A_For_Loop_Statement =>
            if Rule_Used (Stmt_Dead) then
               declare
                  Loop_Spec : constant  Asis.Declaration := For_Loop_Parameter_Specification (Stmt);
               begin
                  if Declaration_Kind (Loop_Spec) = A_Loop_Parameter_Specification  -- 2012 Ignore other forms
                    and then Discrete_Constraining_Lengths (Specification_Subtype_Definition (Loop_Spec)) (1) = 0
                  then
                     Report (Rule_Id,
                             Usage (Stmt_Dead),
                             Get_Location (Stmt),
                             "for loop is never executed");
                     Fixes.Delete (Stmt);
                  end if;
               end;
            end if;

         when A_While_Loop_Statement =>
            if Rule_Used (Stmt_Loop) or Rule_Used (Stmt_Dead) then
               declare
                  Expr : constant Asis.Expression := While_Condition (Stmt);
               begin
                  -- For stmt_loop, signal an explicit "while True" (or statically known True)
                  if Rule_Used (Stmt_Loop)
                    and then Discrete_Static_Expression_Value (Expr) = Boolean'Pos (True)
                  then
                     Report (Rule_Id,
                             Usage (Stmt_Loop),
                             Get_Location (Stmt),
                             "while loop has True condition");
                     Fixes.Delete (From => Get_Location (Stmt), To => Get_Next_Word_Location (Expr));
                  end if;

                  -- For stmt_dead, signal any expression statically false
                  if Rule_Used (Stmt_Dead)
                    and then Discrete_Static_Expression_Value (Expr) = Boolean'Pos (False)
                  then
                     Report (Rule_Id,
                             Usage (Stmt_Dead),
                             Get_Location (Stmt),
                             "while loop is never executed");
                     Fixes.Delete (Stmt);
                  end if;
               end;
            end if;
         when others =>
            null;
      end case;

      if Rule_Used (Stmt_Dead) and then Is_Breaking_Statement (Stmt) then
         declare
            Enclosing_Sts : constant Statement_List := Thick_Queries.Statements (Enclosing_Element (Stmt));
            Stmt_Inx      : constant List_Index := Statement_Index (Stmt, Within => Enclosing_Sts);
            Deletion_End  : List_Index;
         begin
            if Stmt_Inx /= Enclosing_Sts'Last
              and then Label_Names (Enclosing_Sts (Stmt_Inx +1)) = Nil_Element_List
            then
               Report (Rule_Id,
                       Usage (Stmt_Dead),
                       Get_Location (Stmt),
                       "unreachable code after this statement");
               Deletion_End := Enclosing_Sts'Last;
               for S in List_Index range Stmt_Inx + 1 .. Enclosing_Sts'Last loop
                  if Label_Names (Enclosing_Sts (S)) /= Nil_Element_List then
                     Deletion_End := S - 1;
                     exit;
                  end if;
               end loop;
               Fixes.Delete (Enclosing_Sts (Stmt_Inx + 1 .. Deletion_End));
            end if;
         end;
      end if;
   end Process_Statement;

begin  -- Rules.Simplifiable_Statements
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Simplifiable_Statements;
