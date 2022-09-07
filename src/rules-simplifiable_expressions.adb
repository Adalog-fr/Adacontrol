----------------------------------------------------------------------
--  Rules.Simplifiable_Expressions - Package body                   --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
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

-- Asis
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Reports.Fixes,
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Simplifiable_Expressions is
   use Framework, Ada.Strings.Wide_Unbounded;

   -- Algorithm:
   --
   -- Most subrules are straightforward.
   --
   -- Subrule Membership:
   -- -------------------
   -- The goal is to identify comparisons to a same variable that are compatible with a membership test
   -- Such conditions are connected by "or" (for "in") or "and" (for "not in"). We do not accept "and then"
   -- and "or else", because these have a dependence on order of evaluation that would be lost with a membership
   -- test.
   --
   -- When an "or" (resp. "and") is encountered, it is recursively traversed, to check that all included comparisons
   -- are OK for membership: "=" (resp "/="), membership tests, and tests of the form "X>= E1 and X <= E2" (resp.
   -- "X<= E1 or X >= E2"). Note that the logical operator is the opposite of the one between comparisons. We
   -- require "<=" and ">=" (i.e. "<" and ">" are not recognized), because when changed to a range, they would require
   -- adding 1 to make a proper range (but what if an enumerated type? a real type? a string? a redefinition of "+" ?)
   -- The comparisons must involve a same variable (the "pivot" variable). The first comparison initializes the pivot,
   -- other comparisons check that one side is statically the same as the pivot.
   -- As soon as something incompatible is found in the traversal, an exception is raised, therefore canceling the
   -- whole traversal.
   --
   -- An issue is that when a complex expression is changeable to membership, all of its subexpressions are also
   -- changeable to membership, and we want the message to appear only for the topmost expression. Therefore, a global
   -- variable holds the most recent changeable expression encountered. Since the traversal is done in the
   -- pre-operation, subexpression will be found after the top level one: if the subexpression is part of the last
   -- changeable operation, it is ignored.
   --
   -- The values that make up the part of the (future) membership test are accumulated in a global unbounded string,
   -- with appropriate separators. If the traversal is succesful, the variable contains the RHS of the membership
   -- operator for the fix.


   -- All "K_Logical_*" must stay together, and K_Logical must stay last
   type Keywords is (K_Conversion,    K_If_Not,      K_Membership,   K_Parentheses,       K_Range,
                     K_Logical_False, K_Logical_Not, K_Logical_True, K_Logical_Redundant, K_Logical);
   subtype Subrules is Keywords range Keywords'First .. Keywords'Pred (K_Logical);

   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Keywords, "K_");
   use Subrules_Flags_Utilities;

   type Usage_Entry is
      record
         Used  : Boolean := False;
         Label : Unbounded_Wide_String;
      end record;
   type Usages is array (Subrules) of Usage_Entry;

   Ctl_Contexts : array (Control_Kinds) of Usages;
   Rule_Used    : Boolean := False;
   Save_Used    : Boolean;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control occurrence of various forms of expressions that could be made simpler:");
      User_Message  ("  - Conversions of universal values, or to the expression's subtype");
      User_Message  ("  - Negative test in ""if expression""");
      User_Message  ("  - Use of several comparison that can be replaced by membership tests");
      User_Message  ("  - Unnecessary parentheses");
      User_Message  ("  - T'FIRST .. T'LAST that can be replaced by T'RANGE or T.");
      User_Message  ("  - <expression> = (/=) True/False");
      User_Message  ("  - not <comparison>");
      User_Message  ("  - A or/and/or else/and then B, where A and B are the same expression");
      User_Message;
      Help_On_Flags (Header => "Parameter(s):", Footer => "(optional, default=all)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
      Key : Keywords;

      procedure Add_Check (Subrule : Subrules) is
         use Utilities;
      begin
         if Ctl_Contexts (Ctl_Kind)(Subrule).Used then
            Parameter_Error (Rule_Id, "check already given: " & Image (Subrule, Lower_Case));
         else
            Ctl_Contexts (Ctl_Kind)(Subrule) := (Used => True, Label => To_Unbounded_Wide_String (Ctl_Label));
         end if;
      end Add_Check;

   begin  -- Add_Control
      if Parameter_Exists then
         while Parameter_Exists loop
            Key := Get_Flag_Parameter (Allow_Any => False);
            if Key = K_Logical then
               Add_Check (K_Logical_True);
               Add_Check (K_Logical_False);
               Add_Check (K_Logical_Not);
               Add_Check (K_Logical_Redundant);
            else
               Add_Check (Key);
            end if;
         end loop;
      else
         Add_Check (K_Conversion);
         Add_Check (K_If_Not);
         Add_Check (K_Membership);
         Add_Check (K_Parentheses);
         Add_Check (K_Range);
         Add_Check (K_Logical_True);
         Add_Check (K_Logical_False);
         Add_Check (K_Logical_Not);
         Add_Check (K_Logical_Redundant);
      end if;
      Rule_Used  := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            --The following aggregate hangs Gnat, but the explicit loop is OK...
            --Context   := (others => (others => (Used => False, Label => Null_Unbounded_Wide_String)));
            for Usage : Usages of Ctl_Contexts loop
               for U : Usage_Entry of Usage loop
                  U := (Used => False, Label => Null_Unbounded_Wide_String);
               end loop;
            end loop;
            Rule_Used := False;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
     end case;
   end Command;

   ---------------------
   -- Check_Membership --
   ---------------------

   Not_Appropriate_For_Membership : exception;
   Top_Membership_Expr            : Asis.Expression := Asis.Nil_Element;
   Top_Membership_Kind            : Asis.Operator_Kinds;
   Membership_Values              : Unbounded_Wide_String;

   procedure Check_Membership (Expr : Asis.Expression; Pivot : in out Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Utilities, Thick_Queries;

      procedure Update_Pivot (Var : Asis.Expression) is
         Good_Var : constant Asis.Expression := Strip_Parentheses (Var);
      begin
         if Is_Nil (Pivot) then
            Pivot := Good_Var;
         elsif Variables_Proximity (Pivot, Good_Var) /= Same_Variable then
            raise Not_Appropriate_For_Membership;
         end if;
      end Update_Pivot;

      procedure Check_Comparison (Comparison : Asis.Expression; Separator : Wide_String) is
         use Asis.Text;

         Parameters   : constant Association_List := Function_Call_Parameters (Comparison);
         Left_Actual  : constant Asis.Expression  := Strip_Parentheses (Actual_Parameter (Parameters (1)));
         Right_Actual : constant Asis.Expression  := Strip_Parentheses (Actual_Parameter (Parameters (2)));
      begin
         if Is_Static_Object (Left_Actual) then
            Update_Pivot (Left_Actual);
            if Membership_Values /= Null_Unbounded_Wide_String then
               Append (Membership_Values, Separator);
            end if;
            Append (Membership_Values, Trim_All (Element_Image (Right_Actual)));
         else
            Update_Pivot (Right_Actual);
            if Membership_Values /= Null_Unbounded_Wide_String then
               Append (Membership_Values, Separator);
            end if;
            Append (Membership_Values, Trim_All (Element_Image (Left_Actual)));
         end if;
      end Check_Comparison;

      procedure Check_Range is
         Parameters   : constant Association_List := Function_Call_Parameters (Expr);
         Left_Actual  : constant Asis.Expression  := Actual_Parameter (Parameters (1));
         Right_Actual : constant Asis.Expression  := Actual_Parameter (Parameters (2));
      begin
         if   Expression_Kind (Left_Actual)  /= A_Function_Call
           or Expression_Kind (Right_Actual) /= A_Function_Call
         then
            raise Not_Appropriate_For_Membership;
         end if;
         case Operator_Kind (Called_Simple_Name (Left_Actual)) is
            when A_Less_Than_Or_Equal_Operator =>
               if Operator_Kind (Called_Simple_Name (Right_Actual)) /= A_Greater_Than_Or_Equal_Operator then
                  raise Not_Appropriate_For_Membership;
               end if;
               Check_Comparison (Right_Actual, Separator => " | ");
               Check_Comparison (Left_Actual, Separator => " .. ");
            when A_Greater_Than_Or_Equal_Operator =>
               if Operator_Kind (Called_Simple_Name (Right_Actual)) /= A_Less_Than_Or_Equal_Operator then
                  raise Not_Appropriate_For_Membership;
               end if;
               Check_Comparison (Left_Actual, Separator => " | ");
               Check_Comparison (Right_Actual, Separator => " .. ");
            when others =>
               raise Not_Appropriate_For_Membership;
         end case;
      end Check_Range;

   begin  -- Check_Membership
      case Expression_Kind (Expr) is
         when Not_An_Expression =>
            Failure ("Not an expression in Check_Membership", Expr);

         when A_Function_Call =>
            declare
               Func_Name : constant Asis.Expression := Called_Simple_Name (Expr);
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
                        raise Not_Appropriate_For_Membership;
                     end if;

                     if Operator_Kind (Func_Name) = Top_Membership_Kind then
                        for A : Association of Function_Call_Parameters (Expr) loop
                           Check_Membership (Strip_Parentheses (Actual_Parameter (A)), Pivot);
                        end loop;
                        return;
                     end if;

                     case Operator_Kind (Func_Name) is
                        when An_Equal_Operator =>
                           if Top_Membership_Kind /= An_Or_Operator then
                              raise Not_Appropriate_For_Membership;
                           end if;
                           Check_Comparison (Expr, Separator => " | ");

                        when A_Not_Equal_Operator =>
                           if Top_Membership_Kind /= An_And_Operator then
                              raise Not_Appropriate_For_Membership;
                           end if;
                           Check_Comparison (Expr, Separator => " | ");

                        when An_And_Operator =>
                           -- Since it is not the same as Top_Membership_Kind, it is acceptable only in the form of
                           -- I >= X and I <= Y
                           Check_Range;

                        when others =>
                           -- Arithmetic operators
                           -- If we encounter these during normal traversal, it is not as part
                           -- of a comparison
                           raise Not_Appropriate_For_Membership;
                     end case;

                  when An_Identifier   -- A user-defined function
                     | An_Attribute_Reference
                     =>
                     -- If we encounter these during normal traversal, it is not as part
                     -- of a comparison
                     raise Not_Appropriate_For_Membership;

                  when others =>
                     Failure ("Wrong function name", Func_Name);
               end case;
            end;

         when A_Parenthesized_Expression =>
            Check_Membership (Expression_Parenthesized (Expr), Pivot);

         when An_In_Membership_Test =>
            if Top_Membership_Kind /= An_Or_Operator then
               raise Not_Appropriate_For_Membership;
            end if;

            declare
               Lhs : constant Asis.Expression := Membership_Test_Expression (Expr);
            begin
               if not Is_Static_Object (Lhs) then
                  raise Not_Appropriate_For_Membership;
               end if;
               Update_Pivot (Lhs);
               if Membership_Values /= Null_Unbounded_Wide_String then
                  Append (Membership_Values, " | ");
               end if;
               Append (Membership_Values, Trim_All (Element_List_Image (Membership_Test_Choices (Expr))));
            end;

         when A_Not_In_Membership_Test =>
            if Top_Membership_Kind /= An_And_Operator then
               raise Not_Appropriate_For_Membership;
            end if;

            declare
               Lhs : constant Asis.Expression := Membership_Test_Expression (Expr);
            begin
               if not Is_Static_Object (Lhs) then
                  raise Not_Appropriate_For_Membership;
               end if;
               Update_Pivot (Lhs);
               if Membership_Values /= Null_Unbounded_Wide_String then
                  Append (Membership_Values, " | ");
               end if;
               Append (Membership_Values, Trim_All (Element_List_Image (Membership_Test_Choices (Expr))));
            end;

         when A_Type_Conversion
            | A_Qualified_Expression
            =>
            Check_Membership (Converted_Or_Qualified_Expression (Expr), Pivot);

         when others =>
            raise Not_Appropriate_For_Membership;
      end case;
   end Check_Membership;

   ----------------------------------
   -- Check_Operator_For_Redundant --
   ----------------------------------

   procedure Check_Operator_For_Redundant (Expr, Left, Right : Asis.Expression) is
      use Framework.Locations, Framework.Reports, Thick_Queries;

      Reported : Boolean := False;
   begin
      if Are_Equivalent_Expressions (Left, Right) then
         if Ctl_Contexts (Check) (K_Logical_Redundant).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Check) (K_Logical_Redundant).Label),
                    Check,
                    Get_Location (Right),
                    "Expression equivalent to left member of operator");
            Reported := True;
         elsif Ctl_Contexts (Search) (K_Logical_Redundant).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Search) (K_Logical_Redundant).Label),
                    Search,
                    Get_Location (Right),
                    "Expression equivalent to left member of operator");
            Reported := True;
         end if;
         if Reported then
            Fixes.Replace (Expr, Left);
         end if;

         -- Always report count
         if Ctl_Contexts (Count) (K_Logical_Redundant).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Count) (K_Logical_Redundant).Label),
                    Count,
                    Get_Location (Expr),
                    "");
         end if;
      end if;
   end Check_Operator_For_Redundant;


   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Thick_Queries;

      subtype A_Comparison_Operator is Operator_Kinds range An_Equal_Operator .. A_Greater_Than_Or_Equal_Operator;

      type Param_Kind is (Static_True, Static_False, Expr);
      function "+" (Left : Wide_String) return Unbounded_Wide_String renames To_Unbounded_Wide_String;

      -- Message_Table (Operator, Left, Right)
      Message_Table : constant array (Operator_Kinds range An_Equal_Operator .. A_Not_Equal_Operator,
                                      Param_Kind,
                                      Param_Kind) of Unbounded_Wide_String
        := (An_Equal_Operator =>
              (Static_True =>
                 (Static_True  => +"Simplify expression: statically True",                    -- True = True
                  Static_False => +"Simplify expression: statically False",                   -- True = False
                  Expr         => +"Simplify expression 'True = <expr>' to just '<expr>'"),   -- True = <Expr>
               Static_False =>
                 (Static_True  => +"Simplify expression: statically False",                   -- False = True
                  Static_False => +"Simplify expression: statically True",                    -- False = False
                  Expr         => +"Simplify expression 'False = <expr>' to 'not <expr>'"),   -- False = <Expr>
               Expr =>
                 (Static_True  => +"Simplify expression '<expr> = True' to just '<expr>'",    -- <Expr>  = True
                  Static_False => +"Simplify expression '<expr> = False' to 'not <expr>'",    -- <Expr> = False
                  Expr         => +"")),                                                      -- <Expr> = <Expr>
            A_Not_Equal_Operator =>
              (Static_True =>
                 (Static_True  => +"Simplify expression: statically False",                   -- True /= True
                  Static_False => +"Simplify expression: statically True",                    -- True /= False
                  Expr         => + "Simplify expression 'True /= <expr>' to 'not <expr>'"),  -- True /= <Expr>
               Static_False =>
                 (Static_True  => +"Simplify expression: statically True",                    -- False /= True
                  Static_False => +"Simplify expression: statically False",                   -- False /= False
                  Expr         => +"Simplify expression 'False /= <expr>' to just '<expr>'"), -- False /= <Expr>
               Expr =>
                 (Static_True  => +"Simplify expression '<expr> /= True' to 'not <expr>'",    -- <Expr> /= True
                  Static_False => +"Simplify expression '<expr> /= False' to just '<expr>'",  -- <Expr> /= False
                  Expr         => +"")));                                                     -- <Expr> /= <Expr>

      function Get_Kind (Param : Asis.Expression) return Param_Kind is
         use Utilities;
         Good_Param : Asis.Expression := Param;
      begin
         while Expression_Kind (Good_Param) = A_Parenthesized_Expression loop
            Good_Param := Expression_Parenthesized (Good_Param);
         end loop;
         if Expression_Kind (Good_Param) = An_Enumeration_Literal
           and then To_Upper (Full_Name_Image (Good_Param)) = "STANDARD.FALSE"
         then
            return Static_False;
         elsif Expression_Kind (Good_Param) = An_Enumeration_Literal
           and then To_Upper (Full_Name_Image (Good_Param)) = "STANDARD.TRUE"
         then
            return Static_True;
         else
            return Expr;
         end if;
      end Get_Kind;

      procedure Check_Operator_For_Membership (Op : Asis.Operator_Kinds) is
         Pivot_Var   : Asis.Expression := Nil_Element;
         use Asis.Text, Utilities;
      begin
         -- Is this call part of a bigger, simplifiable, expression?
         if Is_Part_Of (Call, Top_Membership_Expr) then
            return;
         end if;

         Top_Membership_Expr := Call;
         Top_Membership_Kind := Op;
         Membership_Values   := Null_Unbounded_Wide_String;
         Check_Membership (Call, Pivot_Var); -- raises Not_Appropriate_For_Membership if unsuccesful

         if Ctl_Contexts (Check) (K_Membership).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Check) (K_Membership).Label),
                    Check,
                    Get_Location (Call),
                    "Multiple tests on " & Trim_All (Element_Image (Pivot_Var))
                    & " can be replaced by "
                    & (if Top_Membership_Kind = An_And_Operator then """not " else """")
                    & "in"" operator");
         elsif Ctl_Contexts (Search) (K_Membership).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Search) (K_Membership).Label),
                    Search,
                    Get_Location (Call),
                    "Multiple tests on " & Trim_All (Element_Image (Pivot_Var))
                    & " can be replaced by "
                    & (if Top_Membership_Kind = An_And_Operator then """not " else """")
                    & "in"" operator");
         end if;
         Fixes.Replace (Top_Membership_Expr,
                        Trim_All (Element_Image (Pivot_Var))
                        & (if Top_Membership_Kind = An_And_Operator then " not" else "") & " in "
                        & To_Wide_String (Membership_Values));

         -- Always report count
         if Ctl_Contexts (Count) (K_Membership).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Count) (K_Membership).Label),
                    Count,
                    Get_Location (Call),
                    "");
         end if;

      exception
         when Not_Appropriate_For_Membership =>
            Top_Membership_Expr := Nil_Element;
      end Check_Operator_For_Membership;

   begin  -- Process_Call
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Op : constant Asis.Operator_Kinds := Operator_Kind (Called_Simple_Name (Call));
      begin
         case Op is
            when An_Equal_Operator .. A_Not_Equal_Operator =>
               declare
                  P : constant Asis.Association_List := Function_Call_Parameters (Call);
                  L : constant Param_Kind := Get_Kind (Actual_Parameter (P (1)));
                  R : constant Param_Kind := Get_Kind (Actual_Parameter (P (2)));
               begin
                  if Message_Table (Op, L, R) /= Null_Unbounded_Wide_String then
                     -- Report the highest priority from Check/Search
                     if Ctl_Contexts (Check) (K_Logical_False).Used
                       and then (L = Static_False or R = Static_False)
                     then
                        Report (Rule_Id,
                                To_Wide_String (Ctl_Contexts (Check) (K_Logical_False).Label),
                                Check,
                                Get_Location (Call),
                                To_Wide_String (Message_Table (Op, L, R)));
                     elsif Ctl_Contexts (Check) (K_Logical_True).Used and then (L = Static_True or R = Static_True) then
                        Report (Rule_Id,
                                To_Wide_String (Ctl_Contexts (Check) (K_Logical_True).Label),
                                Check,
                                Get_Location (Call),
                                To_Wide_String (Message_Table (Op, L, R)));
                     elsif Ctl_Contexts (Search) (K_Logical_False).Used
                       and then (L = Static_False or R = Static_False)
                     then
                        Report (Rule_Id,
                                To_Wide_String (Ctl_Contexts (Search) (K_Logical_False).Label),
                                Search,
                                Get_Location (Call),
                                To_Wide_String (Message_Table (Op, L, R)));
                     elsif Ctl_Contexts (Search) (K_Logical_True).Used
                       and then (L = Static_True or R = Static_True)
                     then
                        Report (Rule_Id,
                                To_Wide_String (Ctl_Contexts (Search) (K_Logical_True).Label),
                                Search,
                                Get_Location (Call),
                                To_Wide_String (Message_Table (Op, L, R)));
                     end if;

                     -- Always report Count
                     if Ctl_Contexts (Count) (K_Logical_False).Used and then (L = Static_False or R = Static_False) then
                        Report (Rule_Id,
                                To_Wide_String (Ctl_Contexts (Count) (K_Logical_False).Label),
                                Count,
                                Get_Location (Call),
                                To_Wide_String (Message_Table (Op, L, R)));
                     elsif Ctl_Contexts (Count) (K_Logical_True).Used and then (L = Static_True or R = Static_True) then
                        Report (Rule_Id,
                                To_Wide_String (Ctl_Contexts (Count) (K_Logical_True).Label),
                                Count,
                                Get_Location (Call),
                                To_Wide_String (Message_Table (Op, L, R)));
                     end if;
                  end if;
               end;

            when A_Not_Operator =>
               declare
                  P : constant Asis.Association_List := Function_Call_Parameters (Call);
                  L : Asis.Expression := Actual_Parameter (P (1));
                  Name : Asis.Expression;
               begin
                  while Expression_Kind (L) = A_Parenthesized_Expression loop
                     L := Expression_Parenthesized (L);
                  end loop;
                  if Expression_Kind (L) = A_Function_Call then
                     Name := Called_Simple_Name (L);
                     if Operator_Kind (Name) in A_Comparison_Operator then
                        if Ctl_Contexts (Check) (K_Logical_Not).Used then
                           Report (Rule_Id,
                                   To_Wide_String (Ctl_Contexts (Check) (K_Logical_Not).Label),
                                   Check,
                                   Get_Location (Call),
                                   """not"" on comparison");
                        elsif Ctl_Contexts (Search) (K_Logical_Not).Used then
                           Report (Rule_Id,
                                   To_Wide_String (Ctl_Contexts (Search) (K_Logical_Not).Label),
                                   Search,
                                   Get_Location (Call),
                                   """not"" on comparison");
                        end if;

                        -- Always report count
                        if Ctl_Contexts (Count) (K_Logical_Not).Used then
                           Report (Rule_Id,
                                   To_Wide_String (Ctl_Contexts (Count) (K_Logical_Not).Label),
                                   Count,
                                   Get_Location (Call),
                                   "");
                        end if;
                     end if;
                  end if;
               end;

            when An_And_Operator | An_Or_Operator => -- Note that "and then" and "or else" are not replaceable
               Check_Operator_For_Membership (Op);
               declare
                  Param_List : constant Asis.Association_List := Function_Call_Parameters (Call);
               begin
                  Check_Operator_For_Redundant (Call,
                                                Actual_Parameter (Param_List (1)),
                                                Actual_Parameter (Param_List (2)));
               end;
            when others =>
               null;
         end case;
      end;
   end Process_Call;

   -------------------
   -- Process_Range --
   -------------------

   procedure Process_Range (Definition : in Asis.Definition) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;

      procedure Do_Reports (Message : Wide_String) is
         use Framework.Locations, Framework.Reports;
      begin
         if Ctl_Contexts (Check)(K_Range).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Check)(K_Range).Label),
                    Check,
                    Get_Location (Definition),
                    Message);
         elsif Ctl_Contexts (Search)(K_Range).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Search)(K_Range).Label),
                    Search,
                    Get_Location (Definition),
                    Message);
         end if;

         if Ctl_Contexts (Count)(K_Range).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Count)(K_Range).Label),
                    Count,
                    Get_Location (Definition),
                    Message);
         end if;
      end Do_Reports;

   begin  -- Process_Range
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Discrete_Range_Kind (Definition) is
         when A_Discrete_Simple_Expression_Range =>
            declare
               LB : constant Expression := Lower_Bound (Definition);
               UB : constant Expression := Upper_Bound (Definition);
            begin
               if Attribute_Kind (LB) /= A_First_Attribute
                 or Attribute_Kind (UB) /= A_Last_Attribute
               then
                  return;
               end if;

               -- Must deal with the following cases when determining 'T':-
               -- 1) T'FIRST
               -- 2) T'BASE'FIRST
               -- 3) T (A)'FIRST
               -- 4) T'FIRST (B)
               -- We must also not fall into the trap of recommending
               -- T'BASE'FIRST .. T'LAST or T'FIRST (X) .. T'LAST (Y) for simplification.
               declare
                  -- First we remove the 'FIRST and 'LAST attributes.
                  LP  : Asis.Expression := Prefix (LB);
                  UP  : Asis.Expression := Prefix (UB);
                  ALB : constant Expression_List := Attribute_Designator_Expressions (LB);
                  AUB : constant Expression_List := Attribute_Designator_Expressions (UB);
               begin
                  -- Both the first and last attributes must have either no attribute designators expressions
                  -- or else have the same value.
                  -- Take the Wide_Value below for the case of the naughty user who wrote something like
                  ---Tab'First (10#1#) .. Tab'Last (1).
                  -- Note that attribute designator expressions can only ever have a length of 0 or 1,
                  --      and are satic integers.
                  if ALB'Length /= AUB'Length
                    or else (ALB'Length = 1  -- Implies AUB'LENGTH = 1
                             and then Discrete_Static_Expression_Value (ALB (1)) /=
                               Discrete_Static_Expression_Value (AUB (1)))
                  then
                     return;
                  end if;

                  -- Remove the 'BASE attribute but only if it is applied to both attributes.
                  if Expression_Kind (LP) = An_Attribute_Reference then
                     if Expression_Kind (UP) /= An_Attribute_Reference  then
                        return;
                     end if;
                     LP := Prefix (LP);
                     UP := Prefix (UP);
                  elsif Expression_Kind (UP) = An_Attribute_Reference then
                     return;
                  end if;

                  -- Remove indexings and selectors for record elements.
                  -- If in doubt, give up.
                  loop
                     case Expression_Kind (LP) is
                        when An_Identifier =>
                           exit;

                        when A_Selected_Component =>
                           case Declaration_Kind (Corresponding_Name_Declaration (Selector (LP))) is
                              when A_Component_Declaration | A_Discriminant_Specification =>
                                 if Expression_Kind (UP) /= A_Selected_Component then
                                    return;
                                 end if;

                                 -- It's a record field, a protected type field...
                                 if not Is_Equal (Corresponding_Name_Declaration (Selector (LP)),
                                                  Corresponding_Name_Declaration (Selector (UP)))
                                 then
                                    return;
                                 end if;
                                 LP := Prefix (LP);
                                 UP := Prefix (UP);
                              when A_Variable_Declaration
                                | A_Constant_Declaration
                                | An_Object_Renaming_Declaration
                                | A_Subtype_Declaration
                                | An_Ordinary_Type_Declaration
                                =>
                                 -- Its a Pack.Var or Pack.T selector
                                 exit;
                              when others =>
                                 Failure ("Wrong selected component",
                                          Corresponding_Name_Declaration (Selector (LP)));
                           end case;

                        when An_Indexed_Component =>
                           if Expression_Kind (UP) /= An_Indexed_Component then
                              return;
                           end if;

                           -- Check that the indexing expressions are statically the same.
                           -- We currently recognize as identical indexing expressions that are:
                           --   - Integer litterals
                           --   - Enumeration litterals
                           --   - Identical constants and loop control parameters
                           declare
                              L_Indexers : constant Asis.Expression_List := Index_Expressions (LP);
                              U_Indexers : constant Asis.Expression_List := Index_Expressions (UP);
                           begin
                              if L_Indexers'Length /= U_Indexers'Length then
                                 return;
                              end if;
                              for I in L_Indexers'Range loop
                                 if Expression_Kind (L_Indexers (I)) /= Expression_Kind (U_Indexers (I)) then
                                    return;
                                 end if;

                                 case Expression_Kind (L_Indexers (I)) is
                                    when An_Integer_Literal =>
                                       if ASIS_Integer'Wide_Value (Value_Image (L_Indexers (I)))
                                         /= ASIS_Integer'Wide_Value (Value_Image (U_Indexers (I)))
                                       then
                                          return;
                                       end if;
                                    when An_Enumeration_Literal =>
                                       if To_Upper (Value_Image (L_Indexers (I)))
                                         /= To_Upper (Value_Image (U_Indexers (I)))
                                       then
                                          return;
                                       end if;
                                    when An_Identifier =>
                                       case Declaration_Kind (Corresponding_Name_Declaration (L_Indexers (I)))
                                       is
                                          when A_Constant_Declaration
                                            | A_Deferred_Constant_Declaration
                                            | A_Loop_Parameter_Specification
                                            =>
                                             if not Is_Equal (Corresponding_Name_Definition (L_Indexers (I)),
                                                              Corresponding_Name_Definition (U_Indexers (I)))
                                             then
                                                return;
                                             end if;
                                          when others =>
                                             return;
                                       end case;
                                    when others =>
                                       return;
                                 end case;
                              end loop;
                           end;

                           -- Here, both indexings are the same
                           LP := Prefix (LP);
                           UP := Prefix (UP);

                        when A_Function_Call | An_Explicit_Dereference =>
                           -- Certainly not static
                           return;

                        when others =>
                          Failure ("Unexpected expression kind", LP);
                     end case;

                  end loop;

                  -- If we still have a selected name, the prefixes are packages
                  -- => Get rid of them
                  -- To be honnest: maybe not for UP, but then it will fail later
                  if Expression_Kind (LP) = A_Selected_Component then
                     LP := Selector (LP);
                  end if;
                  if Expression_Kind (UP) = A_Selected_Component then
                     UP := Selector (UP);
                  end if;

                  -- Here we have a "clean" name for lower/upper prefix
                  -- Check the full expanded names of both bounds.
                  if Full_Name_Image (LP) = Full_Name_Image (UP) then
                     case Declaration_Kind (Corresponding_Name_Declaration (LP)) is
                        when A_Subtype_Declaration
                          | An_Ordinary_Type_Declaration
                          | A_Formal_Type_Declaration
                          =>
                           Do_Reports ("(T)'First .. (T)'Last replaceable with (sub)type(T)");
                        when A_Variable_Declaration
                          | A_Constant_Declaration
                          | An_Object_Renaming_Declaration
                          | A_Deferred_Constant_Declaration
                          | A_Formal_Object_Declaration
                          | A_Parameter_Specification
                          | A_Component_Declaration
                          =>
                           Do_Reports ("(T)'First .. (T)'Last replaceable with (T)'Range");
                        when others =>
                           Failure ("Unexpected Element_Kind 1: " &
                                    Declaration_Kinds'Wide_Image (Declaration_Kind
                                                                  (Corresponding_Name_Declaration (LP))));
                     end case;
                  end if;
               end;
            end;

         when A_Discrete_Range_Attribute_Reference =>
            -- We are interested only in the case where the prefix is a (sub)type
            declare
               P    : Asis.Expression := Prefix (Range_Attribute (Definition));
               Decl : Asis.Declaration;
               Def  : Asis.Definition;
            begin
               case Expression_Kind (P) is
                  when An_Identifier =>
                     null;
                  when A_Selected_Component =>
                     -- Could be Pack.T
                     P := Selector (P);
                  when others =>
                     -- Prefix cannot denote a (sub)type
                     return;
               end case;

               -- Get rid of subtypes, incomplete views...
               Decl := Corresponding_Name_Declaration (P);
               if Declaration_Kind (Decl) = A_Subtype_Declaration then
                  Decl := A4G_Bugs.Corresponding_First_Subtype (Decl);
               end if;
               Decl := Corresponding_Full_Type_Declaration (Decl);

               case Declaration_Kind (Decl) is
                  when An_Ordinary_Type_Declaration
                    | A_Formal_Type_Declaration
                    =>
                     -- Get rid of derived types, including formal derived type
                     -- We can of course have a type derived from a formal derived type,
                     -- and conversely. To any depth.
                     Def := Type_Declaration_View (Decl);
                     loop
                        if Type_Kind (Def) in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition then
                           Def := Type_Declaration_View (Corresponding_Root_Type (Def));
                        elsif Formal_Type_Kind (Def) = A_Formal_Derived_Type_Definition then
                           Def := Type_Declaration_View (A4G_Bugs.Corresponding_First_Subtype
                                                         (Corresponding_Name_Declaration
                                                          (Subtype_Simple_Name (Def))));
                        else
                           exit;
                        end if;
                     end loop;

                     case Type_Kind (Def) is
                        when An_Enumeration_Type_Definition
                          | A_Signed_Integer_Type_Definition
                          | A_Modular_Type_Definition
                          | A_Floating_Point_Definition
                          | An_Ordinary_Fixed_Point_Definition
                          | A_Decimal_Fixed_Point_Definition
                          =>
                           Do_Reports ("(T)'RANGE replaceable with (sub)type(T)");
                        when An_Unconstrained_Array_Definition
                          | A_Constrained_Array_Definition
                          =>
                           null;
                        when Not_A_Type_Definition =>
                           -- Can  be a formal type here
                           case Formal_Type_Kind (Def) is
                              when A_Formal_Discrete_Type_Definition
                                | A_Formal_Signed_Integer_Type_Definition
                                | A_Formal_Modular_Type_Definition
                                | A_Formal_Floating_Point_Definition
                                | A_Formal_Ordinary_Fixed_Point_Definition
                                | A_Formal_Decimal_Fixed_Point_Definition
                                =>
                                 Do_Reports ("(T)'RANGE replaceable with (sub)type(T)");
                              when A_Formal_Unconstrained_Array_Definition
                                | A_Formal_Constrained_Array_Definition
                                =>
                                 null;
                              when others =>
                                 Failure ("Unexpected formal type kind: "
                                          & Formal_Type_Kinds'Wide_Image(Formal_Type_Kind (Def)), P);
                           end case;

                        when others =>
                           Failure ("Unexpected type kind: " & Type_Kinds'Wide_Image(Type_Kind (Def)), P);
                     end case;

                  when A_Variable_Declaration
                     | A_Constant_Declaration
                     | A_Return_Variable_Specification
                     | A_Return_Constant_Specification
                     | A_Deferred_Constant_Declaration
                     | A_Formal_Object_Declaration
                     | A_Parameter_Specification
                     | A_Component_Declaration
                     | An_Object_Renaming_Declaration
                     | An_Element_Iterator_Specification
                     =>
                     null;

                  when others =>
                     Failure ("Unexpected Element_Kind 2: " &
                              Declaration_Kinds'Wide_Image (Declaration_Kind (Decl)));
               end case;
            end;

         when A_Discrete_Subtype_Indication =>
            -- Nothing simplifiable here
            null;

         when Not_A_Discrete_Range =>
            Failure ("Not a discrete range");
      end case;
   end Process_Range;

   ---------------------------
   -- Process_Parenthesized --
   ---------------------------

   procedure Process_Parenthesized (Expr : in Asis.Expression) is
      use Asis, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;

      procedure Do_Report is
         use Framework.Locations, Framework.Reports;
         Message   : constant Wide_String := "Unnecessary parentheses in expression";
         Start_Loc : constant Location := Get_Location (Expr);
         End_Loc   : constant Location := Get_End_Location (Expr);
         Reported  : Boolean := False;
      begin
         if Ctl_Contexts (Check)(K_Parentheses).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Check)(K_Parentheses).Label),
                    Check,
                    Start_Loc,
                    Message);
            Reported := True;
         elsif Ctl_Contexts (Search)(K_Parentheses).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Search)(K_Parentheses).Label),
                    Search,
                    Start_Loc,
                    Message);
            Reported := True;
         end if;

         if Reported then
            Fixes.Delete (Start_Loc, Start_Loc + 1);
            Fixes.Delete (End_Loc,   End_Loc   + 1);
         end if;

         if Ctl_Contexts (Count) (K_Parentheses).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Count)(K_Parentheses).Label),
                    Count,
                    Start_Loc,
                    Message);
         end if;
      end Do_Report;

      type Priority_Level is (Logical, Relational, Adding, Multiplying, Highest, Primary);
      Priority : constant array (Asis.Operator_Kinds) of Priority_Level
        := (Not_An_Operator                                              => Primary,
            An_And_Operator          .. An_Xor_Operator                  => Logical,
            An_Equal_Operator        .. A_Greater_Than_Or_Equal_Operator => Relational,
            A_Plus_Operator          .. A_Unary_Minus_Operator           => Adding,
            A_Multiply_Operator      .. A_Rem_Operator                   => Multiplying,
            An_Exponentiate_Operator .. A_Not_Operator                   => Highest);
      subtype A_Structured_Expression is Expression_Kinds range A_Case_Expression .. A_For_Some_Quantified_Expression;
      subtype A_Simple_Expression     is Priority_Level range Adding   .. Priority_Level'Last;

      Enclosing : Asis.Element;
      Enclosed  : Asis.Element;
   begin  -- Process_Parenthesized
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Enclosing := Enclosing_Element (Expr);
      Enclosed  := Expression_Parenthesized (Expr);

      case Expression_Kind (Enclosed) is
         when A_Structured_Expression =>                  -- Things that generally require parentheses
            -- Parentheses are not required if Expr is part of a positional association and there is
            -- only one association in the association list (which rules out aggregates associations)
            if Element_Kind (Enclosing) = An_Association then
               case Association_Kind (Enclosing) is
                  when Not_An_Association =>
                     Failure (Rule_Id & ": Not an association");
                  when A_Pragma_Argument_Association =>
                     -- Minimal value, give up.
                     null;
                  when A_Discriminant_Association =>
                     if Is_Nil (Discriminant_Selector_Names (Enclosing))
                       and then Discriminant_Associations (Enclosing_Element (Enclosing))'Length = 1
                     then
                        Do_Report;
                     end if;
                  when A_Record_Component_Association
                     | An_Array_Component_Association
                     =>
                     null;
                  when A_Parameter_Association =>
                     if Is_Nil (Formal_Parameter (Enclosing))
                       and then Actual_Parameters (Enclosing_Element (Enclosing))'Length = 1 -- One positional parameter
                     then
                        if Expression_Kind (Enclosing_Element (Enclosing)) /= A_Function_Call
                          or else Is_Prefix_Call (Enclosing_Element (Enclosing))   -- not (if C then A else B)
                        then
                           Do_Report;
                        end if;
                     end if;
                  when A_Generic_Association =>
                     if Is_Nil (Formal_Parameter (Enclosing))
                       and then Actual_Parameters (Enclosing_Element (Enclosing))'Length = 1
                     then
                        Do_Report;
                     end if;
               end case;
            end if;

         when A_Parenthesized_Expression =>
            Do_Report;

         when others =>                         -- Normal expressions
            -- Get up to the real enclosing expression
            loop
               case Element_Kind (Enclosing) is
                  when An_Association =>
                     Enclosing := Enclosing_Element (Enclosing);
                  when An_Expression =>
                     exit when Expression_Kind (Enclosing) /= A_Parenthesized_Expression;
                     Enclosing := Enclosing_Element (Enclosing);
                  when others =>
                     exit;
               end case;
            end loop;

            case Expression_Kind (Enclosing) is
               when A_Function_Call =>
                  if Is_Prefix_Call (Enclosing) then
                     Do_Report;
                  else
                     case Expression_Kind (Enclosed) is
                        when A_Function_Call =>
                           if Is_Prefix_Call (Enclosed)
                              or else  Priority (Operator_Kind (Prefix (Enclosing)))
                                     < Priority (Operator_Kind (Prefix (Enclosed)))
                           then
                              Do_Report;
                           end if;
                        when An_And_Then_Short_Circuit
                           | An_Or_Else_Short_Circuit
                           | A_Structured_Expression
                           =>
                           null;
                        when An_In_Membership_Test
                           | A_Not_In_Membership_Test
                           =>
                           if Priority (Operator_Kind (Prefix (Enclosing))) < Relational then
                              Do_Report;
                           end if;
                        when others =>
                           Do_Report;
                     end case;
                  end if;

               when An_And_Then_Short_Circuit
                  | An_Or_Else_Short_Circuit
                  =>
                  case Expression_Kind (Enclosed) is
                     when An_And_Then_Short_Circuit .. An_Or_Else_Short_Circuit =>
                        if Expression_Kind (Enclosing) = Expression_Kind (Enclosed) then
                           Do_Report;
                        end if;
                     when A_Function_Call =>
                        if Is_Prefix_Call (Enclosed) or else Priority (Operator_Kind (Prefix (Enclosed))) > Logical then
                           Do_Report;
                        end if;
                     when others =>
                        Do_Report;
                  end case;

               when An_In_Membership_Test
                  | A_Not_In_Membership_Test
                  =>
                  -- The parenthesized expression can only be on the LHS of the membership test
                  case Expression_Kind (Enclosed) is
                     when An_In_Membership_Test
                        | A_Not_In_Membership_Test
                        =>
                        null;
                     when A_Function_Call =>
                        if Is_Prefix_Call (Enclosed)
                          or else Priority (Operator_Kind (Prefix (Enclosed))) > Relational
                        then
                           Do_Report;
                        end if;
                     when others =>
                        Do_Report;
                  end case;

               when others =>  -- Including Not_An_Expression
                  if Declaration_Kind (Enclosing) /= An_Expression_Function_Declaration
                  -- Special case: the parentheses around the expression of an expression function
                  -- Always required

                    and then (Element_Kind (Enclosing) /= A_Definition
                              or else Expression_Kind (Enclosed) /= A_Function_Call
                              or else Priority (Operator_Kind (Prefix (Enclosed))) in A_Simple_Expression)
                  -- Expressions appearing in definitions must be simple expressions.
                  -- Those that are not must be parenthesized
                  then
                     Do_Report;
                  end if;

            end case;
      end case;

   end Process_Parenthesized;

   ------------------------
   -- Process_Conversion --
   ------------------------

   procedure Process_Conversion (Expr : in Asis.Expression) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;

      Source : Asis.Declaration;
      Target : Asis.Element;

      procedure Do_Report is
         use Framework.Locations, Framework.Reports;
         Inner_Expr : Asis.Expression;
         Encl_Expr  : Asis.Expression;
      begin
         if Ctl_Contexts (Check) (K_Conversion).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Check) (K_Conversion).Label),
                    Check,
                    Get_Location (Expr),
                    "unnecessary conversion");
         elsif Ctl_Contexts (Search) (K_Conversion).Used  then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Search) (K_Conversion).Label),
                    Search,
                    Get_Location (Expr),
                    "unnecessary conversion");
         end if;

         -- Always report Count
         if Ctl_Contexts (Count) (K_Conversion).Used then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Contexts (Count) (K_Conversion).Label),
                    Count,
                    Get_Location (Expr),
                    "");
         end if;

         -- Normally, the fix removes the whole conversion.
         -- However, if the inner expression is an infixed operator and the conversion appears as an
         -- operand of an infixed operator, we mus keep the parentheses, in order not to mess up
         -- associativity in cases like "integer (I+3) * 2".
         Inner_Expr := Converted_Or_Qualified_Expression (Expr);
         Encl_Expr  := Enclosing_Element (Expr);
         if Element_Kind (Encl_Expr) = An_Association then
            Encl_Expr := Enclosing_Element (Encl_Expr);
            if         Expression_Kind (Encl_Expr)  = A_Function_Call and then not Is_Prefix_Call (Encl_Expr)
              and then Expression_Kind (Inner_Expr) = A_Function_Call and then not Is_Prefix_Call (Inner_Expr)
            then
               Fixes.Delete (Converted_Or_Qualified_Subtype_Mark (Expr));
            else
               Fixes.Replace (Expr, By => Inner_Expr);
            end if;
         else
            Fixes.Replace (Expr, By => Inner_Expr);
         end if;
      end Do_Report;
   begin   -- Process_Conversion
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Source := A4G_Bugs.Corresponding_Expression_Type (Converted_Or_Qualified_Expression (Expr));
      if Is_Nil (Source) then
         -- The expression is of an anonymous or dynamic type
         -- certainly not suspicious
         return;
      end if;
      Source := Corresponding_Full_Type_Declaration (Source);

      Target := Converted_Or_Qualified_Subtype_Mark (Expr);
      case Expression_Kind (Target) is
         when A_Selected_Component =>
            Target := Selector (Target);
         when An_Attribute_Reference =>
            case Attribute_Kind (Target) is
               when A_Base_Attribute =>
                  -- Never necessary
                  Do_Report;
                  return;
               when A_Class_Attribute =>
                  -- Since a class wide type has no declaration, it is hard
                  -- to check whether the source is class-wide
                  -- Just consider it is OK
                  return;
               when others =>
                  Failure ("Unexpected type attribute", Target);
            end case;
         when others =>
            null;
      end case;
      Target := Corresponding_Name_Declaration (Target);

      if Is_Equal (Source, Target)
        or else Is_Equal (A4G_Bugs.Corresponding_First_Subtype (Source), Target)
      then
         Do_Report;
         return;
      end if;

      -- Explicit conversion is never required for universal expressions (a literal or a named number,
      -- or a static expression of those) of the same category, except for a Universal_Fixed that results
      -- from a multiply or divide of fixed point numbers
      if Type_Kind (Type_Declaration_View (Source)) = A_Root_Type_Definition then
         -- This is how it should happen
         case Root_Type_Kind (Type_Declaration_View (Source)) is
            when A_Root_Integer_Definition | A_Universal_Integer_Definition =>
               if Type_Category (Target) in A_Signed_Integer_Type .. A_Modular_Type then
                  Do_Report;
               end if;
               return;
            when A_Root_Real_Definition | A_Universal_Real_Definition =>
               if Type_Category (Target) in A_Fixed_Point_Type .. A_Floating_Point_Type then
                  Do_Report;
               end if;
               return;
            when A_Universal_Fixed_Definition =>
               -- This happens only for the result of fixed-point multiply/divide, conversion always allowed
               return;
            when Not_A_Root_Type_Definition =>
               Failure ("Not_A_Root_Type_Definition", Source);
         end case;
      elsif Is_Nil (Enclosing_Element (Source)) then
         -- In the case of GNAT (sometimes?), the type returned by A4G is not a correct root type,
         -- but a declaration for Universal_Integer (or similar) that appears "out of the blue":
         -- it is not included in anything else, and that's how we recognize it.
         declare
            Universal_Name : constant Wide_String := To_Upper (Defining_Name_Image (Names (Source) (1)));
         begin
            if Universal_Name = "UNIVERSAL_INTEGER" then
               if Type_Category (Target) in A_Signed_Integer_Type .. A_Modular_Type then
                  Do_Report;
               end if;
               return;
            elsif Universal_Name = "UNIVERSAL_REAL" then
               if Type_Category (Target) in A_Fixed_Point_Type .. A_Floating_Point_Type then
                  Do_Report;
               end if;
               return;
            elsif Universal_Name = "UNIVERSAL_FIXED" then
               -- This happens only for the result of fixed-point multiply/divide, conversion always allowed
               return;
            else
               Failure ("Unexpected universal name: " & Universal_Name);
            end if;
         end;
      end if;

   end Process_Conversion;

   ---------------------------
   -- Process_If_Expression --
   ---------------------------

   procedure Process_If_Expression (Expr : in Asis.Expression) is
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
         use Framework.Locations, Framework.Reports, Thick_Queries;

         Paths   : constant Asis.Path_List := Expression_Paths (Expr);
         If_Cond : Asis.Expression;
         Op      : Asis.Operator_Kinds;
      begin
         if Paths'Length /= 2 then
            return;
         end if;

         If_Cond := Condition_Expression (Paths (Paths'First));
         while Expression_Kind (If_Cond) = A_Parenthesized_Expression loop
            If_Cond := Expression_Parenthesized (If_Cond);
         end loop;
         if Expression_Kind (If_Cond) = A_Function_Call then
            Op := Operator_Kind (Simple_Name (Prefix (If_Cond)));
            if Op in A_Not_Operator | A_Not_Equal_Operator then
               if Ctl_Contexts (Check) (K_If_Not).Used then
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Contexts (Check) (K_If_Not).Label),
                          Check,
                          Get_Location (Expr),
                          "Negative condition in ""if-else"" expression");
               elsif Ctl_Contexts (Search) (K_If_Not).Used  then
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Contexts (Search) (K_If_Not).Label),
                          Search,
                          Get_Location (Expr),
                          "Negative condition in ""if-else"" expression");
               end if;

               -- Always report Count
               if Ctl_Contexts (Count) (K_If_Not).Used then
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Contexts (Count) (K_If_Not).Label),
                          Count,
                          Get_Location (Expr),
                          "");
               end if;
            end if;
         end if;

      end;
   end Process_If_Expression;

   ---------------------------
   -- Process_Short_Circuit --
   ---------------------------

   procedure Process_Short_Circuit (Expr : in Asis.Expression) is
      use Asis.Expressions;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_Operator_For_Redundant (Expr,
                                    Short_Circuit_Operation_Left_Expression  (Expr),
                                    Short_Circuit_Operation_Right_Expression (Expr));
   end Process_Short_Circuit;

begin  -- Rules.Simplifiable_expressions
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Simplifiable_Expressions;
