----------------------------------------------------------------------
--  Rules.Simplifiable_Statements - Package body                    --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2021.           --
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

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Exceptions,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Reports.Fixes,
  Framework.Language,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);

package body Rules.Simplifiable_Statements is
   use Framework, Framework.Control_Manager, Framework.Variables, Framework.Variables.Shared_Types;

   type Subrules is (Stmt_All,
                     Stmt_Block,   Stmt_Dead,           Stmt_For_For_Slice, Stmt_For_In_For_For_Of,
                     Stmt_Handler, Stmt_If,             Stmt_If_For_Case,   Stmt_If_Not,
                     Stmt_Loop,    Stmt_Loop_For_While, Stmt_Nested_Path,   Stmt_Null,
                     Stmt_True_If, Stmt_Unnecessary_If, Stmt_While_For_For);
   subtype True_Subrules is Subrules range Subrules'Succ (Stmt_All) .. Subrules'Last;

   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "STMT_");
   use Subrules_Flags_Utilities;

   type Usage_Flags is array (True_Subrules) of Boolean;
   Not_Used    : constant Usage_Flags := (others => False);
   Rule_Used   : Usage_Flags := Not_Used;
   Save_Used   : Usage_Flags;
   Usage       : array (Subrules) of Basic_Rule_Context;
   No_Exit_LFW : Boolean := False;  -- Loop_For_While
   No_Exit_WFF : Boolean := False;  -- While_For_For
   Full_Range  : Boolean := False;

   type While_For_Expression_Kind is (Any, No_Function, Static);
   package Expression_Kind_Type is new Discrete_Type (While_For_Expression_Kind);

   type Dead_Case_Acceptable_Kind is (None, Only_Null, Only_Raise, Null_And_Raise);
   package Acceptable_Kind_Type is new Discrete_Type (Dead_Case_Acceptable_Kind);

   -- Rule variable
   Acceptable_Indexings : aliased Natural_Type.Object          := (Value => 0);
   While_For_Expression : aliased Expression_Kind_Type .Object := (Value => Any);
   Acceptable_Dead_Case : aliased Acceptable_Kind_Type .Object := (Value => None);

   Not_Changeable : exception;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control Ada statements that can be made simpler");
      User_Message;
      Help_On_Flags ("Parameter(s): [no_exit] [full_range]");
      User_Message  ("no_exit can be given with ""all"", ""loop_for_while"", and ""while_for_for""");
      User_Message  ("full_range can be given with ""all"" and ""for_in_for_for_of""");
      User_Message;
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Acceptable_Dead_Case");
      Help_On_Variable (Rule_Id & ".Acceptable_Indexings");
      Help_On_Variable (Rule_Id & ".While_For_Expression");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Utilities;
      Subrule          : Subrules;
      No_Exit_Given    : Boolean;
      Full_Range_Given : Boolean;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            No_Exit_Given    := Get_Modifier ("NO_EXIT");
            Full_Range_Given := Get_Modifier ("FULL_RANGE");
            if not No_Exit_Given then
               -- In case Full_Range is given before No_Exit
               No_Exit_Given := Get_Modifier ("NO_EXIT");
            end if;
            Subrule := Get_Flag_Parameter (Allow_Any => False);

            if Subrule = Stmt_All then
               if Rule_Used /= Not_Used then
                  Parameter_Error (Rule_Id, "some statements already given");
               end if;
               No_Exit_WFF := No_Exit_Given;
               No_Exit_LFW := No_Exit_Given;
               Full_Range  := Full_Range_Given;
               Rule_Used   := (others => True);
               Usage       := (others => Basic.New_Context (Ctl_Kind, Ctl_Label));
            else
               if Subrule = Stmt_Loop_For_While then
                  No_Exit_LFW := No_Exit_Given;
               elsif Subrule = Stmt_While_For_For then
                  No_Exit_WFF := No_Exit_Given;
               elsif No_Exit_Given then
                  Parameter_Error (Rule_Id, "no_exit allowed only with while_for_for");
               end if;

               if Subrule = Stmt_For_In_For_For_Of then
                  Full_Range := Full_Range_Given;
               elsif Full_Range_Given then
                  Parameter_Error (Rule_Id, "full_range allowed only with for_in_for_for_of");
               end if;


               if Rule_Used (Subrule) then
                  if not Basic.Merge_Context (Usage (Subrule), Ctl_Kind, Ctl_Label) then
                     Parameter_Error (Rule_Id, "statement already given: " & Image (Subrule, Lower_Case));
                  end if;
               else
                  Rule_Used (Subrule) := True;
                  Usage (Subrule)     := Basic.New_Context (Ctl_Kind, Ctl_Label);
               end if;
            end if;
         end loop;

      else  -- No parameter
         if Rule_Used /= Not_Used then
            Parameter_Error (Rule_Id, "some statements already given");
         end if;
         No_Exit_LFW := False;
         No_Exit_WFF := False;
         Full_Range  := False;
         Rule_Used   := (others => True);
         Usage       := (others => Basic.New_Context (Ctl_Kind, Ctl_Label));
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
            Rule_Used            := Not_Used;
            No_Exit_LFW          := False;
            No_Exit_WFF          := False;
            Acceptable_Indexings := (Value => 0);
            Acceptable_Dead_Case := (Value => None);
            While_For_Expression := (Value => Any);
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
                           for Param : Asis.Association of Function_Call_Parameters (Expr) loop
                              Check_Condition (Actual_Parameter (Param), Pivot);
                           end loop;

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
               Name : Asis.Expression;
            begin
               for Choice : Asis.Element of Membership_Test_Choices (Expr) loop
                  if Element_Kind (Choice) = An_Expression then
                     Name := Simple_Name (Choice);
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
                        if not Is_Static_Expression (Choice) then
                           raise Not_Appropriate_For_Case;
                        end if;
                     end if;
                  else
                     -- in range
                     if Discrete_Constraining_Lengths (Choice) = (1 => Not_Static) then
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
      use Framework.Locations, Framework.Reports, Thick_Queries;

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
      use Framework.Locations, Framework.Reports, Thick_Queries;

      Then_Last_Stmt : Asis.Statement;
      Else_Last_Stmt : Asis.Statement;
      Paths          : constant Asis.Path_List := Statement_Paths (Stmt);
      If_Cond        : Asis.Expression;
      Op             : Asis.Operator_Kinds;

      procedure Process_If_Static_Paths is
         -- Called with Dead and/or True_If
         use Utilities;

         Conds           : Asis.Expression_List (Paths'Range) := (Condition_Expression (Paths (Paths'First)),
                                                                  others => <>);
         First_Path_Cond : constant Wide_String := Static_Expression_Value_Image (Condition_Expression
                                                                                  (Paths (Paths'First)));
         Already_Fixed   : Boolean := False;
         All_Paths_Dead  : Boolean := False; -- after a statically True if/elsif
      begin
         -- Special case for fixing "if" path: if followed by "elsif", change it to "if". If followed by "else",
         -- replace by the content of the "else" path. And if none of these, remove the whole if statement
         if First_Path_Cond = "0" then  -- "0" => False
            if Rule_Used (Stmt_Dead) then
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
                        Fixes.Replace (Get_Location (Paths (2)), Length => 3, By => ""); -- 3 characters: "els"
                     when An_Else_Path =>
                        Fixes.Replace (Stmt, Sequence_Of_Statements (Paths (2), Include_Pragmas => True));
                     when others =>
                        Utilities.Failure ("Process_If_Statement: Incorrect path", Paths (2));
                  end case;
               end if;
               Already_Fixed := True;
            end if;

            if Rule_Used (Stmt_True_If) and then Paths'Length > 1 and then Path_Kind (Paths (2)) = An_Else_Path then
               Report (Rule_Id,
                       Usage (Stmt_True_If),
                       Get_Location (Paths (2)),
                       "alternative to a statically false path");
            end if;
         elsif First_Path_Cond = "1" then  -- "1" => True
            if Rule_Used (Stmt_True_If) then
               Report (Rule_Id,
                       Usage (Stmt_True_If),
                       Get_Location (Paths (Paths'First)),
                       "condition is always true");
               Fixes.Replace (Stmt, Sequence_Of_Statements (Paths (Paths'First), Include_Pragmas => True));
               Already_Fixed  := True;
            end if;
            All_Paths_Dead := True;
         end if;

         -- If the if statement has been fixed globally (Already_Fixed), don't fix the individual paths,
         -- but give error messages nonetheless
         for Path_Inx in Asis_Natural range Paths'First + 1 .. Paths'Last loop
            if Path_Kind (Paths (Path_Inx)) = An_Else_Path then
               if All_Paths_Dead and Rule_Used (Stmt_Dead) then
                  Report (Rule_Id,
                          Usage (Stmt_Dead),
                          Get_Location (Paths (Path_Inx)),
                          "alternative to a statically True path");
               end if;
               exit;
            end if;

            Conds (Path_Inx) := Condition_Expression (Paths (Path_Inx));
            declare
               Path_Cond : constant Wide_String (1 .. 1) := Choose (Static_Expression_Value_Image
                                                                    (Condition_Expression (Paths (Path_Inx))),
                                                                    " ");  -- can be only "0", "1", or " "
            begin
               if Rule_Used (Stmt_Dead) then
                  if All_Paths_Dead then
                     Report (Rule_Id,
                             Usage (Stmt_Dead),
                             Get_Location (Paths (Path_Inx)),
                             "alternative to a statically True path");
                  elsif Path_Cond = "0" then  -- "0" => False
                     Report (Rule_Id,
                             Usage (Stmt_Dead),
                             Get_Location (Paths (Path_Inx)),
                             "condition is always false");
                     if not Already_Fixed then
                        Fixes.Delete (Paths (Path_Inx));
                     end if;
                  elsif Path_Cond /= "1" then  -- "1" => True
                     -- Check for structurally equivalent paths, makes no sense when the condition is statically
                     -- True or False
                     for Expr : Asis.Expression of Conds (Conds'First .. Path_Inx - 1) loop
                        if Are_Equivalent_Expressions (Conds (Path_Inx), Expr) then
                           Report (Rule_Id,
                                   Usage (Stmt_Dead),
                                   Get_Location (Paths (Path_Inx)),
                                   "condition is equivalent to the one at " & Image (Get_Location (Expr)));
                           if not Already_Fixed then
                              Fixes.Delete (Paths (Path_Inx));
                           end if;
                           exit;  -- No need to check further
                        end if;
                     end loop;
                  end if;
               end if;

               if Rule_Used (Stmt_True_If) then
                  if Path_Cond = "1" then  -- "1" => True
                     Report (Rule_Id,
                             Usage (Stmt_True_If),
                             Get_Location (Paths (Path_Inx)),
                             "condition is always true");
                     if not Already_Fixed then
                        -- Replace this path and all remaining ones with an else path
                        Fixes.Replace (Original   => Paths (Path_Inx .. Paths'Last),
                                       By         => Sequence_Of_Statements (Paths (Path_Inx)),
                                       Add_Before => "else ");
                        All_Paths_Dead := True;
                        Already_Fixed  := True;
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end Process_If_Static_Paths;

      procedure Process_Same_Logical_Assignment is
         use Utilities;
         use Asis.Declarations;

         function Negated_Image (Expr : Asis.Expression) return Wide_String is
         -- Pre: Expr is a boolean expression
         -- Return the image of the negation of expr, i.e.:
         --  - if Expr is a call to "not", remove it
         --  - if Expr is a comparison, replace it by the opposite comparison
         --  - if Expr is an operator call, return "not (Expr)" (to avoid problems with precedence)
         --  - otherwise, return "not Expr"

            subtype Comparison_Operators is Operator_Kinds range An_Equal_Operator .. A_Greater_Than_Or_Equal_Operator;
            Opposite : constant array (Comparison_Operators) of Wide_String (1 .. 2)
              := (An_Equal_Operator                => "/=",
                  A_Not_Equal_Operator             => "= ",
                  A_Less_Than_Operator             => ">=",
                  A_Less_Than_Or_Equal_Operator    => "> ",
                  A_Greater_Than_Operator          => "<=",
                  A_Greater_Than_Or_Equal_Operator => "< ");

            function Format (S : Wide_String) return Wide_String is
            begin
               if S (S'Last) = ' ' then
                  return ' ' & S;
               else
                  return ' ' & S & ' ';
               end if;
            end Format;


            function Stripped_Image (E : Asis.Element) return Wide_String is
               use Asis.Text;
               Result : constant Wide_String :=  Element_Image (E);
            begin
               return Result (A4G_Bugs.Element_Span (E).First_Column .. Result'Last);
            end Stripped_Image;
         begin -- Negated_Image
            case Expression_Kind (Expr) is
               when A_Function_Call =>
                  if Is_Prefix_Call (Expr) then
                     -- might be complicated, qualified, implicit dereference... don't try to make it pretty
                     return "not " & Stripped_Image (Expr);
                  end if;

                  -- Necessarily an operator here (not prefix call)
                  declare
                     Params : constant Asis.Association_List := Function_Call_Parameters (Expr);
                  begin
                     case Operator_Kind (Prefix (Expr)) is
                        when A_Not_Operator =>
                           return Stripped_Image (Strip_Parentheses (Actual_Parameter (Params (1))));
                        when Comparison_Operators =>
                           return Stripped_Image (Actual_Parameter (Params (1)))
                             & Format (Opposite (Operator_Kind (Prefix (Expr))))
                             & Stripped_Image (Actual_Parameter (Params (2)));
                        when others =>
                           return "not (" & Stripped_Image (Expr) & ')';
                     end case;
                  end;
               when An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit =>
                  return "not (" & Stripped_Image (Expr) & ')';
               when others =>
                  return "not " & Stripped_Image (Expr);
            end case;
         end Negated_Image;

         Then_Stmts : constant Asis.Statement_List := Thick_Queries.Statements (Paths (1));
         Else_Stmts : constant Asis.Statement_List := Thick_Queries.Statements (Paths (2));
         Then_Expr  : Asis.Expression;
         Else_Expr  : Asis.Expression;
         Then_Type  : Asis.Declaration;
         Else_Type  : Asis.Declaration;
         Then_Val   : Extended_Biggest_Int;
         Else_Val   : Extended_Biggest_Int;
      begin  -- Process_Same_Logical_Assignment
         -- Only one statement in each path
         if Then_Stmts'Length /= 1 or Else_Stmts'Length /= 1 then
            return;
         end if;

         -- Both are assignments or both are simple return statements, get the expressions
         case Statement_Kind (Then_Stmts (1)) is
            when An_Assignment_Statement =>
               if Statement_Kind (Else_Stmts (1)) /= An_Assignment_Statement then
                  return;
               end if;
               if Variables_Proximity (Assignment_Variable_Name (Then_Stmts (1)),
                                       Assignment_Variable_Name (Else_Stmts (1))) /= Same_Variable
               then
                  return;
               end if;
               Then_Expr := Assignment_Expression (Then_Stmts (1));
               Else_Expr := Assignment_Expression (Else_Stmts (1));
            when A_Return_Statement =>
               if Statement_Kind (Else_Stmts (1)) /= A_Return_Statement then
                  return;
               end if;
               Then_Expr := Return_Expression (Then_Stmts (1));
               if Is_Nil (Then_Expr) then -- return from procedure (the other one is necessarily the same)
                  return;
               end if;
               Else_Expr := Return_Expression (Else_Stmts (1));
            when others =>
               return;
         end case;

         Then_Type := A4G_Bugs.Corresponding_Expression_Type (Then_Expr);
         Else_Type := A4G_Bugs.Corresponding_Expression_Type (Else_Expr);

         -- Both exprs are of type Standard.Boolean
         if    Is_Nil (Then_Type) or Is_Part_Of_Implicit (Then_Type)
            or Is_Nil (Else_Type) or Is_Part_Of_Implicit (Else_Type)
         then
            -- Anonymous type, universal type...
            return;
         end if;
         if        To_Upper (Full_Name_Image (Names (Then_Type) (1))) /= "STANDARD.BOOLEAN"
           or else To_Upper (Full_Name_Image (Names (Else_Type) (1))) /= "STANDARD.BOOLEAN"
         then
            return;
         end if;

         Then_Val := Discrete_Static_Expression_Value (Then_Expr);
         Else_Val := Discrete_Static_Expression_Value (Else_Expr);
         if Then_Val = Not_Static or Else_Val = Not_Static then
            return;
         end if;

         if Then_Val = Else_Val then
            -- Strange (copy/paste error?), certainly worth mentionning
            Report (Rule_Id,
                    Usage (Stmt_Unnecessary_If),
                    Get_Location (Stmt),
                    "Both paths of if statement assign/return the same value");
            -- Better not fix it, the if-expression could have side effects
            return;
         end if;

         if Then_Val = 1 then
            Report (Rule_Id,
                    Usage (Stmt_Unnecessary_If),
                    Get_Location (Stmt),
                    "If statement can be replaced by direct use of logical expression into assignment/return");
            Fixes.Delete (From => Get_Location (Stmt), To => Get_Location (Then_Stmts (1)));
            Fixes.Replace (Then_Expr, Condition_Expression (Paths (1)));
            Fixes.Delete (From => No_Indent (Get_Location (Paths (2)), Stmt),
                          To   => No_Indent (Get_Next_Word_Location (Stmt, Starting => From_Tail), Stmt));

         else
            Report (Rule_Id,
                    Usage (Stmt_Unnecessary_If),
                    Get_Location (Stmt),
                    "If statement can be replaced by inverted use of logical expression into assignment/return");
            Fixes.Delete (From => Get_Location (Stmt), To => Get_Location (Then_Stmts (1)));
            Fixes.Replace (Then_Expr, Negated_Image (Condition_Expression (Paths (1))));
            Fixes.Delete (From => No_Indent (Get_Location (Paths (2)), Stmt),
                          To   => No_Indent (Get_Next_Word_Location (Stmt, Starting => From_Tail), Stmt));
         end if;

      end Process_Same_Logical_Assignment;

   begin   -- Process_If_Statement
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
                  if Op in A_Not_Operator | A_Not_Equal_Operator then
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

            for Path : Asis.Path of Paths (2 .. Last_Elsif) loop
               Check_Condition (Condition_Expression (Path), Pivot => Special_Var);
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
      -- Stmt_True_If
      if Rule_Used (Stmt_Dead) or Rule_Used (Stmt_True_If) then
         Process_If_Static_Paths;
      end if;

      -- Stmt_Unnecessary_If
      -- Assignment of True or False to same variable:
      -- Must be strictly if... then... else... end if;
      if Rule_Used (Stmt_Unnecessary_If) then
         if Paths'Length = 2  and then Path_Kind (Paths (2)) = An_Else_Path then
            Process_Same_Logical_Assignment;
         end if;
      end if;
   end Process_If_Statement;

   ----------------------------
   -- Process_Case_Statement --
   ----------------------------

   procedure Process_Case_Statement (Stmt  : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports;
      use Utilities, Thick_Queries;
      Select_Min : Extended_Biggest_Int;
      Select_Max : Extended_Biggest_Int;
   begin
      -- Stmt_Dead
      if Rule_Used (Stmt_Dead) then -- always True (for the moment)
         Select_Min := Discrete_Static_Expression_Value (Case_Expression (Stmt), Wanted => Minimum);
         Select_Max := Discrete_Static_Expression_Value (Case_Expression (Stmt), Wanted => Maximum);
         for CP : Asis.Path of Statement_Paths (Stmt) loop
            declare
               Bounds          : Extended_Biggest_Int_List (1 .. 2);
               Count           : Extended_Biggest_Natural := 0;
               Spurious_Values : Boolean;
            begin
               Count_Values :
               for PE : Asis.Element of Case_Statement_Alternative_Choices (CP) loop
                  Spurious_Values := False;
                  if Definition_Kind (PE) = An_Others_Choice then
                     -- Not handled here, equivalent to check Case_Statement (Others_Span, min 1)
                     -- Note that it is necessarily the last path
                     Count := 1; -- or whatever /= 0
                     exit Count_Values;
                  end if;

                  if Definition_Kind (PE) = A_Discrete_Range then
                     if Discrete_Range_Kind (PE) = A_Discrete_Subtype_Indication
                       and then not Is_Nil (Corresponding_Static_Predicates
                                            (Subtype_Simple_Name (PE)))
                     then
                        -- Subtype with static predicate used for a choice: we don't know (yet) how to evaluate this
                        Uncheckable (Rule_Id,
                                     False_Negative,
                                     Get_Location (PE),
                                     "(Dead) Use of subtype with static predicate");
                        Count := 1; -- or whatever /= 0
                        exit Count_Values;
                     end if;
                     Bounds := Discrete_Constraining_Values (PE);
                     if Bounds (1) = Not_Static or Bounds (2) = Not_Static then
                        -- it IS static, but the evaluator cannot evaluate it...
                        -- unless it is of a generic formal type
                        Uncheckable (Rule_Id,
                                     False_Negative,
                                     Get_Location (PE),
                                     "(Dead) Could not evaluate bounds of expression");
                        Count := 1; -- or whatever /= 0
                        exit Count_Values;
                     end if;
                  elsif Element_Kind (PE) = An_Expression then   -- It's not a discrete range => it's a real expression
                     Bounds (1) := Discrete_Static_Expression_Value (PE);
                     Bounds (2) := Bounds (1);                   --## rule line off Assignments ## no aggregate possible
                  else
                     Failure ("Unexpected path kind:", PE);
                  end if;

                  if Select_Min /= Not_Static and then Select_Min > Bounds (1) then
                     Bounds (1)      := Select_Min;
                     Spurious_Values := True;
                  end if;
                  if Select_Max /= Not_Static and then Select_Max < Bounds (2) then
                     Bounds (2)      := Select_Max;
                     Spurious_Values := True;
                  end if;
                  if Bounds (2) >= Bounds (1) then -- beware of null ranges
                     Count := Count + (Bounds (2) - Bounds (1)) + 1;  -- Don't remove parentheses, possible overflow
                  end if;

               end loop Count_Values;

               if Count = 0 then
                  case Acceptable_Dead_Case.Value is
                     when None =>
                        null;
                     when Only_Null =>
                        if Are_Null_Statements (Sequence_Of_Statements (CP)) then
                           Count           := 1; -- anything not 0
                           Spurious_Values := False;
                        end if;
                     when Only_Raise =>
                        if Statement_Kind (Sequence_Of_Statements (CP)(1)) = A_Raise_Statement then
                           Count           := 1; -- anything not 0
                           Spurious_Values := False;
                        end if;
                     when Null_And_Raise =>
                        declare
                           Stmts : constant Asis.Statement_List := Sequence_Of_Statements (CP);
                        begin
                           if Statement_Kind (Stmts (1)) = A_Raise_Statement
                             or else Are_Null_Statements (Stmts)
                           then
                              Count           := 1; -- anything not 0
                              Spurious_Values := False;
                           end if;
                        end;
                  end case;
               end if;
               if Count = 0 and Spurious_Values then
                  Report (Rule_Id,
                          Usage (Stmt_Dead),
                          Get_Location (CP),
                          "choice(s) cover no value of the selecting expression of the case statement");
               elsif Spurious_Values then
                  Report (Rule_Id,
                          Usage (Stmt_Dead),
                          Get_Location (CP),
                          "some choice(s) not covering any value of the selecting expression of the case statement");
               elsif Count = 0 then
                  Report (Rule_Id,
                          Usage (Stmt_Dead),
                          Get_Location (CP),
                          "choice(s) cover no value");
                  Framework.Reports.Fixes.Delete (CP);
               end if;
            end;
         end loop;
      end if;
   end Process_Case_Statement;


   -----------------------------
   -- Process_While_Statement --
   -----------------------------

   procedure Process_While_Statement (The_Loop : in Asis.Statement) is
      use Asis.Statements;
      use Framework.Locations, Framework.Reports, Thick_Queries;
      Cond : constant Asis.Expression := While_Condition (The_Loop);

      procedure Check_While_For_For is
      -- for stmt_while_for_for, signal if:
      --   - the condition is a "<", "<=", ">", ">=" comparison of a variable with a value, or a membership operator
      --   - and the variable appears only as "in" parameter of calls, or as the LHS of a single assignment where the
      --     RHS is +1 or -1 on the same variable, or a call to 'Pred or 'Succ, consistent with the condition
      -- We do not require initialisation of the loop variable to be the statement preceding the while loop,
      -- however automatic fix is possible only if this is the case.
         use Asis, Asis.Elements, Asis.Expressions;

         Func : Asis.Expression;
         Var  : Asis.Expression;
         type Direction is (Up, Down, Indeterminate);
         Loop_Dir : Direction;

         procedure Check_Expression (Test_Expr : Asis.Expression) is
            procedure Check_Function_Call (Expr : Asis.Expression) is
            begin
               if Expression_Kind (Expr) = A_Function_Call then
                  case Operator_Kind (Called_Simple_Name (Expr)) is
                     when Not_An_Operator =>
                        -- A true function
                        raise Not_Changeable;
                     when A_Unary_Plus_Operator
                        | A_Unary_Minus_Operator
                        | A_Not_Operator
                        =>
                        -- Unary operators
                        Check_Function_Call (Actual_Parameter (Function_Call_Parameters (Expr) (1)));
                     when others =>
                        -- Binary operators
                        Check_Function_Call (Actual_Parameter (Function_Call_Parameters (Expr) (1)));
                        Check_Function_Call (Actual_Parameter (Function_Call_Parameters (Expr) (2)));
                  end case;
               end if;
            end Check_Function_Call;
         begin  -- Check_Expression
            case While_For_Expression.Value is
               when Any =>
                  return;
               when No_Function =>
                  Check_Function_Call (Test_Expr);
               when Static =>
                  if not Is_Static_Expression (Test_Expr) then
                     raise Not_Changeable;
                  end if;
            end case;
         end Check_Expression;

         procedure Check_Inner_Statements (Stmt : Asis.Element; Assignment_Allowed : in out Boolean) is
         -- Appropriate element_kinds:
         --   - A_Statement
         --   - A_Path
            Inner_Statements : constant Asis.Statement_List := Thick_Queries.Statements (Stmt);
            RHS              : Asis.Expression;
            Called_Func      : Asis.Expression;
         begin
            for S: Asis.Statement of Inner_Statements loop
               case Statement_Kind (S) is
                  when An_Assignment_Statement =>
                     if Variables_Proximity (Assignment_Variable_Name (S), Var) = Same_Variable then
                        if not Assignment_Allowed then
                           raise Not_Changeable;
                        end if;

                        -- Here, we have the index variable on the LHS
                        RHS := Strip_Parentheses (Assignment_Expression (S));
                        if Expression_Kind (RHS) /= A_Function_Call then
                           raise Not_Changeable;
                        end if;

                        Called_Func := Called_Simple_Name (RHS);
                        declare
                           Params : constant Asis.Association_List := Function_Call_Parameters (RHS);
                           Step   : Extended_Biggest_Int;
                        begin
                           case Operator_Kind (Called_Func) is
                              when A_Plus_Operator =>
                                 if Variables_Proximity (Actual_Parameter (Params (1)), Var) = Same_Variable then
                                    Step := Discrete_Static_Expression_Value (Actual_Parameter (Params (2)));
                                    if Step not in 1 | -1 then
                                       raise Not_Changeable;
                                    end if;
                                 elsif Variables_Proximity (Actual_Parameter (Params (2)), Var) = Same_Variable then
                                    Step := Discrete_Static_Expression_Value (Actual_Parameter (Params (1)));
                                    if Step not in 1 | -1 then
                                       raise Not_Changeable;
                                    end if;
                                 else
                                    raise Not_Changeable;
                                 end if;
                              when A_Minus_Operator =>
                                 if Variables_Proximity (Actual_Parameter (Params (1)), Var) /= Same_Variable then
                                    raise Not_Changeable;
                                 end if;
                                 Step := Discrete_Static_Expression_Value (Actual_Parameter (Params (2)));
                                 if Step not in 1 | -1 then
                                    raise Not_Changeable;
                                 end if;
                                 Step := -Step;
                              when Not_An_Operator =>
                                 case Attribute_Kind (Called_Simple_Name (RHS)) is
                                    when A_Pred_Attribute =>
                                       if Variables_Proximity (Actual_Parameter (Params (1)), Var) /= Same_Variable then
                                          raise Not_Changeable;
                                       end if;
                                       Step := -1;
                                    when A_Succ_Attribute =>
                                       if Variables_Proximity (Actual_Parameter (Params (1)), Var) /= Same_Variable then
                                          raise Not_Changeable;
                                       end if;
                                       Step := 1;
                                    when others =>
                                       raise Not_Changeable;
                                 end case;
                              when others =>
                                 raise Not_Changeable;
                           end case;

                           case Loop_Dir is
                              when Indeterminate =>
                                 if Step = 1 then
                                    Loop_Dir := Up;
                                 else
                                    Loop_Dir := Down;
                                 end if;
                              when Up =>
                                 if Step = -1 then
                                    raise Not_Changeable;
                                 end if;
                              when Down =>
                                 if Step = 1 then
                                    raise Not_Changeable;
                                 end if;
                           end case;
                        end;
                        -- Here, we have a good decrement/increment statement, do not allow another one
                        Assignment_Allowed := False;
                     end if;
                  when An_If_Statement    -- Statements with paths
                     | A_Case_Statement
                     | A_Selective_Accept_Statement
                     | A_Timed_Entry_Call_Statement
                     | A_Conditional_Entry_Call_Statement
                     | An_Asynchronous_Select_Statement
                     =>
                     declare
                        Paths   : constant Path_List := Statement_Paths (S);
                        Allowed : Boolean := False;
                     begin
                        for P : Asis.Path of Paths loop
                           Check_Inner_Statements (P, Allowed);
                        end loop;
                     end;
                  when A_Procedure_Call_Statement | An_Entry_Call_Statement =>
                     declare
                        use Asis.Declarations;
                        Profile : constant Asis.Parameter_Specification_List := Called_Profile (S);
                        Actual  : Asis.Expression;
                     begin
                        for P : Asis.Parameter_Specification of Profile loop
                           if Mode_Kind (P) in An_Out_Mode .. An_In_Out_Mode then
                              for Formal_Name : Asis.Defining_Name of Names (P) loop
                                 Actual := Actual_Expression (S, Formal_Name, Return_Default => True);
                                 -- Since it is an [in] out parameter, the actual is a variable
                                 if Variables_Proximity (Actual, Var) = Same_Variable then
                                    raise Not_Changeable;
                                 end if;
                              end loop;
                           end if;
                        end loop;
                     end;

                  when An_Exit_Statement =>
                     if No_Exit_WFF and then Is_Equal (Corresponding_Loop_Exited (S), The_Loop) then
                        raise Not_Changeable;
                     end if;

                  when others =>
                     declare
                        Allowed : Boolean := False;
                     begin
                        Check_Inner_Statements (S, Allowed);
                     end;
               end case;
            end loop;
         end Check_Inner_Statements;

      begin  -- Check_While_For_For
         case Expression_Kind (Cond) is
            when A_Function_Call =>
               Func := Called_Simple_Name (Cond);
               case Operator_Kind (Func) is
                  when A_Less_Than_Operator    | A_Less_Than_Or_Equal_Operator    |
                       A_Greater_Than_Operator | A_Greater_Than_Or_Equal_Operator |
                       A_Not_Equal_Operator
                     =>
                     Var := Actual_Parameter (Function_Call_Parameters (Cond) (1));
                     Check_Expression (Actual_Parameter (Function_Call_Parameters (Cond) (2)));
                     if Expression_Kind (Var) = An_Identifier
                       and then not Is_Nil (Ultimate_Name (Var))
                       and then Declaration_Kind (Corresponding_Name_Declaration
                                                  (Ultimate_Name (Var))) = A_Variable_Declaration
                     then
                        Var := Ultimate_Name (Var);
                        if Operator_Kind (Func) = A_Not_Equal_Operator then
                           Loop_Dir := Indeterminate;
                        elsif Operator_Kind (Func) in A_Less_Than_Operator .. A_Less_Than_Or_Equal_Operator then
                           Loop_Dir := Up;
                        else -- A_Greater_Than_Operator .. A_Greater_Than_Or_Equal_Operator
                          Loop_Dir := Down;
                        end if;
                     else
                        -- Some people may write while 10 > I ...
                        Var := Actual_Parameter (Function_Call_Parameters (Cond) (2));
                        Check_Expression (Actual_Parameter (Function_Call_Parameters (Cond) (1)));
                        if Expression_Kind (Var) = An_Identifier
                          and then not Is_Nil (Ultimate_Name (Var))
                          and then Declaration_Kind (Corresponding_Name_Declaration
                                                     (Ultimate_Name (Var))) = A_Variable_Declaration
                        then   --## rule line off Simplifiable_statements ## Keep symetry with previous case
                           Var := Ultimate_Name (Var);
                           if Operator_Kind (Func) = A_Not_Equal_Operator then
                              Loop_Dir := Indeterminate;
                           elsif Operator_Kind (Func) in A_Less_Than_Operator .. A_Less_Than_Or_Equal_Operator then
                              Loop_Dir := Down;
                           else -- A_Greater_Than_Operator .. A_Greater_Than_Or_Equal_Operator
                              Loop_Dir := Up;
                           end if;
                        else
                           -- No simple variable found
                           return;
                        end if;
                     end if;
                  when others =>  -- Including Not_An_Operator
                     return;
               end case;
            when others =>
               return;
         end case;

         -- Here the condition is a comparison of a variable to an expression
         -- Check that there is only one (and appropriate) assignment to the variable in the direct statements
         -- of the loop, no assignment in compound statements, no use as [in] out or access parameter
         declare
            Allowed : Boolean := True;
         begin
            Check_Inner_Statements (The_Loop, Assignment_Allowed => Allowed);
            if Allowed then -- No assignment in loop
               return;
            end if;
         end;

         case Loop_Dir is
            when Indeterminate =>
               null;
            when Up =>
               Report (Rule_Id,
                       Usage (Stmt_While_For_For),
                       Get_Location (The_Loop),
                       "while loop can be replaced with a direct for loop");
            when Down =>
               Report (Rule_Id,
                       Usage (Stmt_While_For_For),
                       Get_Location (The_Loop),
                       "while loop can be replaced with a reverse for loop");
         end case;
      exception
         when Not_Changeable =>
            null;
      end Check_While_For_For;

   begin  -- Process_While_Statement
          -- For stmt_loop, signal an explicit "while True" (or statically known True)

      if Rule_Used (Stmt_Loop)
        and then Static_Expression_Value_Image (Cond) = "1"  -- = Boolean'Pos (True)
      then
         Report (Rule_Id,
                 Usage (Stmt_Loop),
                 Get_Location (The_Loop),
                 "while loop has True condition");
         Fixes.Delete (From => Get_Location (The_Loop), To => Get_Next_Word_Location (Cond));
      end if;

      -- For stmt_dead, signal any expression statically false
      if Rule_Used (Stmt_Dead)
        and then Static_Expression_Value_Image (Cond) = "0"  -- = Boolean'Pos (False)
      then
         Report (Rule_Id,
                 Usage (Stmt_Dead),
                 Get_Location (The_Loop),
                 "while loop is never executed");
         Fixes.Delete (The_Loop);
      end if;

      if Rule_Used (Stmt_While_For_For) then
         Check_While_For_For;
      end if;
   end Process_While_Statement;


   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Stmt : in Asis.Statement) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports, Thick_Queries;

      procedure Check_For_Of (Loop_Spec : Asis.Declaration) is
         use Asis.Iterator;

         type For_Of_State is
            record
               Indexing_Name  : Asis.Defining_Name;  -- The control variable of the for in stmt
               Indexed_Name   : Asis.Defining_Name;  -- The variable being indexed; if more than one => not changeable
               Indexing_Count : Natural;             -- Count of indexings of Indexed_Name by Indexing_Name
            end record;

         procedure Pre_Operation (Element : in     Asis.Element;
                                  Control : in out Traverse_Control;
                                  State   : in out For_Of_State)
         is
            use Asis.Exceptions, Asis.Expressions;
            Ident_Def : Asis.Defining_Name;
         begin

            if Expression_Kind (Element) /= An_Identifier then
               return;
            end if;
            begin
               Ident_Def := Corresponding_Name_Definition (Element);
            exception
               when ASIS_Inappropriate_Element => -- Attribute name, pragma identifier...
                  return;
            end;
            if Variables_Proximity (Ident_Def, State.Indexing_Name) = Same_Variable then
               -- Since the identifier is of a discrete type, it can appear in a selected name only as the selector,
               -- but it can be qualified (by the loop name, and enclosing structures)
               declare
                  Indexed_Expr : Asis.Expression := Enclosing_Element (Element);
                  Indexed_Var  : Asis.Expression;
               begin
                  while Expression_Kind (Indexed_Expr) = A_Selected_Component loop
                     Indexed_Expr := Enclosing_Element (Indexed_Expr);
                  end loop;

                  if Expression_Kind (Indexed_Expr) /= An_Indexed_Component then
                     -- we must still allow the index to appear in renamings
                     if Declaration_Kind (Indexed_Expr) /= An_Object_Renaming_Declaration then
                       Control := Terminate_Immediately;
                     end if;
                     return;
                  end if;

                  if Index_Expressions (Indexed_Expr)'Length /= 1 then
                     -- Multi-dimensional array: give up
                     Control := Terminate_Immediately;
                     return;
                  end if;

                  Indexed_Var := Prefix (Indexed_Expr);
                  if Expression_Kind (Indexed_Var) in A_Function_Call | An_Explicit_Dereference then
                     Control := Terminate_Immediately;
                     return;
                  end if;

                  if Is_Nil (State.Indexed_Name) then
                     -- If the variable is a component that depends on defaulted discriminants, for..of is not allowed
                     -- (see 5.5.2(6.1/4)).
                     -- We ignore the case of the "known to be constrained" variable (too complicated to check, and a
                     -- false negative is harmless here). Note that Thick_Queries.Known_To_Be_Constrained is too
                     -- simplistic compared to 3.3(23.1 .. 23.13)
                     declare
                        Discrs : constant Asis.Defining_Name_List := Path_Selection_Discriminants (Indexed_Var)
                                                                   & Governing_Discriminants      (Indexed_Var);
                     begin
                        if (for some Discr of Discrs =>
                               not Is_Nil (Initialization_Expression (Enclosing_Element (Discr))))
                        then
                           Control := Terminate_Immediately;
                           return;
                        end if;
                     end;

                     -- If the variable is declared in a block inside the loop, it cannot be replaced
                     -- Indexed_Var can be a component, we must go up to the full variable to check where it is declared
                     declare
                        Good_Var : Asis.Element := Indexed_Var;
                     begin
                        loop
                           case Expression_Kind (Good_Var) is
                              when An_Indexed_Component =>
                                 Good_Var := Prefix (Good_Var);
                              when An_Identifier =>
                                 exit;
                              when others =>
                                 if Is_Expanded_Name (Good_Var) then
                                    Good_Var := Simple_Name (Good_Var);
                                 else
                                    Good_Var := Prefix (Good_Var);  -- can only be a record component
                                 end if;
                           end case;
                        end loop;
                        if Is_Part_Of (Corresponding_Name_Declaration (Good_Var), Inside => Stmt) then
                           Control := Terminate_Immediately;
                           return;
                        end if;
                     end;


                     State.Indexed_Name  := Indexed_Var;
                  elsif Variables_Proximity (State.Indexed_Name, Indexed_Var) /= Same_Variable then
                     Control := Terminate_Immediately;
                     return;
                  end if;
               end;
               -- If we are here, we have a proper indexing
               State.Indexing_Count := State.Indexing_Count + 1;
            end if;
         end Pre_Operation;

         procedure Post_Operation (Element : in     Asis.Element;
                                   Control : in out Traverse_Control;
                                   State   : in out For_Of_State) is null;

         procedure Traverse_For is new Traverse_Element (For_Of_State);

         Control : Traverse_Control := Continue;
         State   : For_Of_State := (Indexing_Name  => Names (Loop_Spec) (1),
                                    Indexed_Name   => Nil_Element,
                                    Indexing_Count => 0);
      begin  -- Check_For_Of
         for S : Asis.Statement of Thick_Queries.Statements (Stmt) loop
            Traverse_For (S, Control, State);
            exit when Control /= Continue;
         end loop;

         -- Control is Terminate_Immediately if not changeable, Continue otherwise
         -- State.Indexed_Name is Nil_Element if the loop index has not been used for indexing
         if Control = Terminate_Immediately
           or Is_Nil (State.Indexed_Name)
           or State.Indexing_Count <= Acceptable_Indexings.Value
         then
            return;
         end if;

         if Full_Range then
            declare
               Loop_Bounds : constant Extended_Biggest_Int_List := Discrete_Constraining_Values
                                                                    (Specification_Subtype_Definition (Loop_Spec));
            begin
               if (for some Bound of Loop_Bounds => Bound = Not_Static)
                 or else Loop_Bounds /= Discrete_Constraining_Values (State.Indexed_Name)
               then
                  return;
               end if;
            end;
         end if;

         Report (Rule_Id,
                 Usage (Stmt_For_In_For_For_Of),
                 Get_Location (Stmt),
                 """for ... in"" loop can be changed to ""for ... of"" loop");
      end Check_For_Of;

      procedure Check_Slice (Loop_Index : Asis.Defining_Name) is
      -- Called when Stmt is a (regular) for loop
      -- Checks if every statement in the body of the loop is an assignment, where the LHS and RHS are
      --    array indexings, with the loop parameter appearing only as Inx +- Constant
         use Asis.Expressions;

         type Indexing_Expr_Kind is (Bad, Good, Good_With_Index);
         function Oper_Expr_Kind (Op : Asis.Expression; Left, Right : Indexing_Expr_Kind) return Indexing_Expr_Kind is
         begin
            if Left = Bad or Right = Bad then
               return Bad;
            elsif Left = Good and Right = Good then
               return Good;
            elsif Operator_Kind (Op) = A_Minus_Operator and then Right = Good_With_Index then
               return Bad;
            elsif Left = Good_With_Index xor Right = Good_With_Index then
               return Good_With_Index;
            else -- Good_With_Index or Good_With_Index
               return Bad;
            end if;
         end Oper_Expr_Kind;

         function Indexing_Kind (Expr : Asis.Expression) return Indexing_Expr_Kind is
         begin
            case Expression_Kind (Expr) is
               when An_Integer_Literal | An_Enumeration_Literal =>
                  return Good;

               when An_Identifier =>
                  if Variables_Proximity (Expr, Loop_Index) = Same_Variable then
                     return Good_With_Index;
                  else
                     return Good;
                  end if;

               when A_Function_Call =>
                  -- we allow only predefined "+" and "-".
                  declare
                     Op_Name : constant Asis.Name := Called_Simple_Name (Expr);
                     Op_Decl : Asis.Declaration ;
                  begin
                     if Expression_Kind (Op_Name) /= An_Operator_Symbol then
                        return Bad;
                     end if;
                     Op_Decl := Corresponding_Name_Declaration (Op_Name);
                     if not Is_Nil (Op_Decl) and then not Is_Predefined_Operator (Op_Decl) then
                         return Bad;
                     end if;

                     case Operator_Kind (Op_Name) is
                        when A_Plus_Operator | A_Minus_Operator =>
                           declare
                              Params : constant Asis.Expression_List := Function_Call_Parameters (Expr);
                           begin
                              return Oper_Expr_Kind (Op_Name,
                                                     Indexing_Kind (Actual_Parameter (Params (1))),
                                                     Indexing_Kind (Actual_Parameter (Params (2))));
                           end;
                        when others =>
                           return Bad;
                     end case;
                  end;

               when A_Parenthesized_Expression =>
                  return Indexing_Kind (Expression_Parenthesized (Expr));

               when  A_Type_Conversion | A_Qualified_Expression =>
                  return Indexing_Kind (Converted_Or_Qualified_Expression (Expr));

               when others =>
                  return Bad;
            end case;
         end Indexing_Kind;

         LHS, RHS     : Asis.Expression;
         Assign_Count : ASIS_Natural := 0;
      begin    -- Check_Slice
         for S : Asis.Statement of  Loop_Statements (Stmt) loop
            case Statement_Kind (S) is
               when An_Assignment_Statement =>
                  LHS := Assignment_Variable_Name (S);
                  RHS := Assignment_Expression    (S);
                  if         Expression_Kind (LHS) /= An_Indexed_Component
                    or else (Expression_Kind (RHS) /= An_Indexed_Component and Indexing_Kind (RHS) /= Good)
                  then
                     return;
                  end if;

                  declare
                     L_Indices : constant Asis.Expression_List := Index_Expressions (LHS);
                  begin
                     if L_Indices'Length /= 1
                       or else Indexing_Kind (L_Indices (L_Indices'First)) /= Good_With_Index
                     then
                        return;
                     end if;
                  end;

                  if Expression_Kind (RHS) = An_Indexed_Component then
                     declare
                        R_Indices : constant Asis.Expression_List := Index_Expressions (RHS);
                     begin
                        if R_Indices'Length /= 1
                          or else Indexing_Kind (R_Indices (R_Indices'First)) /= Good_With_Index
                        then
                           return;
                        end if;
                     end;
                  end if;

                  Assign_Count := Assign_Count + 1;
               when A_Null_Statement =>
                  null;

               when others =>
                  return;
            end case;
         end loop;

         if Assign_Count > 0 then -- We may have a loop with only null statements
            Report (Rule_Id,
                    Usage (Stmt_For_For_Slice),
                    Get_Location (Stmt),
                    "For loop can be replaced by array, array slice, or aggregate assignments");
         end if;
      end Check_Slice;

   begin   -- Process_Statement
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
            if (Rule_Used and Usage_Flags'(Stmt_Dead        | Stmt_If      | Stmt_If_For_Case | Stmt_If_Not |
                                           Stmt_Nested_Path | Stmt_True_If | Stmt_Unnecessary_If
                                                   => True,
                                           others  => False)) /= Not_Used
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
                  procedure Check_Exit (Outer_Stmt : Asis.Statement) is
                  begin
                     case Statement_Kind (Outer_Stmt) is
                        when An_If_Statement    -- Statements with paths
                           | A_Case_Statement
                           | A_Selective_Accept_Statement
                           | A_Timed_Entry_Call_Statement
                           | A_Conditional_Entry_Call_Statement
                           | An_Asynchronous_Select_Statement
                           =>
                           for P : Asis.Path of Statement_Paths (Outer_Stmt) loop
                              for S : Asis.Statement of Thick_Queries.Statements (P) loop
                                 Check_Exit (S);
                              end loop;
                           end loop;
                        when A_Loop_Statement     -- Statements with statements
                           | A_While_Loop_Statement
                           | A_For_Loop_Statement
                           | A_Block_Statement
                           =>
                           for S : Asis.Statement of Thick_Queries.Statements (Outer_Stmt) loop
                              Check_Exit (S);
                           end loop;
                        when An_Exit_Statement =>
                           if Is_Equal (Corresponding_Loop_Exited (Outer_Stmt), Stmt) then
                              raise Not_Changeable;
                           end if;
                        when others =>
                           null;
                     end case;
                  end Check_Exit;

               begin
                  if Statement_Kind (Loop_Stmts (Loop_Stmts'First)) = An_Exit_Statement
                    and then Is_Equal (Stmt, Corresponding_Loop_Exited (Loop_Stmts (Loop_Stmts'First)))
                  then
                     if No_Exit_LFW then
                        -- We know damn well that the first statement is an exit statement...
                        for S : Asis.Statement of Loop_Stmts (Loop_Stmts'First + 1 .. Loop_Stmts'Last) loop
                           Check_Exit (S);    -- Raises Not_Changeable if there is an exit for this loop
                        end loop;
                     end if;
                     Report (Rule_Id,
                             Usage (Stmt_Loop_For_While),
                             Get_Location (Stmt),
                             "simple loop can be changed to ""while""");
                  end if;
               exception
                  when Not_Changeable =>
                     null;
               end;
            end if;

         when A_For_Loop_Statement =>
            declare
               Loop_Spec : constant  Asis.Declaration := For_Loop_Parameter_Specification (Stmt);
            begin
               case Declaration_Kind (Loop_Spec) is
                  when A_Loop_Parameter_Specification =>  -- 2012 Ignore other forms for dead code
                     if Rule_Used (Stmt_Dead)
                       and then Discrete_Constraining_Lengths (Specification_Subtype_Definition (Loop_Spec)) (1) = 0
                     then
                        Report (Rule_Id,
                                Usage (Stmt_Dead),
                                Get_Location (Stmt),
                                "for loop is never executed");
                        Fixes.Delete (Stmt);
                     end if;

                     if Rule_Used (Stmt_For_In_For_For_Of) then
                        Check_For_Of (Loop_Spec);
                     end if;

                     if Rule_Used (Stmt_For_For_Slice) then
                        Check_Slice (Names (Loop_Spec) (1));
                     end if;
                  when others =>
                     null;
               end case;
            end;

         when A_While_Loop_Statement =>
            if Rule_Used (Stmt_Loop) or Rule_Used (Stmt_Dead) or Rule_Used (Stmt_While_For_For) then
               Process_While_Statement (Stmt);
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
   Framework.Variables.Register (Acceptable_Indexings'Access,
                                 Variable_Name => Rule_Id & ".ACCEPTABLE_INDEXINGS");
   Framework.Variables.Register (While_For_Expression'Access,
                                 Variable_Name => Rule_Id & ".WHILE_FOR_EXPRESSION");
   Framework.Variables.Register (Acceptable_Dead_Case'Access,
                                 Variable_Name => Rule_Id & ".ACCEPTABLE_DEAD_CASE");
end Rules.Simplifiable_Statements;
