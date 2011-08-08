----------------------------------------------------------------------
--  Rules.If_For_Case - Package body                                --
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

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Asis
with
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

package body Rules.If_For_Case is
   use Framework, Utilities;

   -- Algorithm:
   -- The heart of the algorithm is the procedure Check.
   -- It checks that an expression is made only of comparisons of a single variable (the pivot)
   -- with static expressions.
   -- On input, the pivot can be Nil_Element, meaning that the pivot is unknown. If a pivot is found,
   -- it is returned on output. If the pivot is not Nil_Element on input, then any pivot found must be
   -- the same as the one provided.
   --
   -- If the expression has not the required form, the exception Not_Appropriate_For_Case is raised.
   -- This avoids analysing further expressions when we know that the expression is not good.


   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Rule_Type  : Rule_Types;
   Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): none");
      User_Message ("Control if statements that could be replaced by case statements");
   end Help;


   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if Rule_Used then
         Parameter_Error (Rule_Id, "this rule can be specified only once");
      end if;

      if  Parameter_Exists then
         Parameter_Error (Rule_Id, "no parameter allowed");
      end if;

      Rule_Type  := Rule_Use_Type;
      Rule_Label := To_Unbounded_Wide_String (Label);
      Rule_Used  := True;
   end Add_Use;


   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := False;
            Rule_Label := Null_Unbounded_Wide_String;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -----------
   -- Check --
   -----------

   Not_Appropriate_For_Case : exception;

   procedure Check (Expr : Asis.Expression; Pivot : in out Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions, Thick_Queries;

      procedure Update_Pivot (Var : Asis.Expression) is
         Good_Var : Asis.Expression := Var;
      begin
         while Expression_Kind (Good_Var) = A_Parenthesized_Expression loop
            Good_Var := Expression_Parenthesized (Good_Var);
         end loop;

         if Is_Nil (Pivot) then
            if Expression_Type_Kind (Good_Var) in An_Enumeration_Type_Definition .. A_Modular_Type_Definition then
               Pivot := Good_Var;
            else
               -- Not a discrete type
               raise Not_Appropriate_For_Case;
            end if;
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
                           Check (Actual_Parameter (Function_Call_Parameters (Expr) (1)), Pivot);

                        when An_And_Operator
                          | An_Or_Operator
                          | An_Xor_Operator
                          =>
                           declare
                              Params : constant Asis.Association_List := Function_Call_Parameters (Expr);
                           begin
                              for I in Params'Range loop
                                 Check (Actual_Parameter (Params (I)), Pivot);
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
            Check (Short_Circuit_Operation_Left_Expression (Expr), Pivot);
            Check (Short_Circuit_Operation_Right_Expression (Expr), Pivot);

         when A_Parenthesized_Expression =>
            Check (Expression_Parenthesized (Expr), Pivot);

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
            Check (Converted_Or_Qualified_Expression (Expr), Pivot);

         when others =>
            raise Not_Appropriate_For_Case;
      end case;
   end Check;


   --------------------------
   -- Process_If_Statement --
   --------------------------

   procedure Process_If_Statement (If_Stmt: Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements;
      use Ada.Strings.Wide_Unbounded, Framework.Reports, Thick_Queries;

   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Paths : constant Asis.Path_List := Statement_Paths (If_Stmt);
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

         Check (Condition_Expression (Paths (1)), Pivot => Pivot_Var);

         for I in Positive range 2 .. Last_Elsif loop
            Check (Condition_Expression (Paths (I)), Pivot => Special_Var);
            if Variables_Proximity (Pivot_Var, Special_Var) /= Same_Variable then
               return;
            end if;
         end loop;

         Report (Rule_Id,
                 To_Wide_String (Rule_Label),
                 Rule_Type,
                 Get_Location (If_Stmt),
                 "If statement could be replaced by case statement");

      end;
   exception
      when Not_Appropriate_For_Case =>
         null;
   end Process_If_Statement;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.If_For_Case;
