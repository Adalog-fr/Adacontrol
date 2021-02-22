----------------------------------------------------------------------
--  Rules.Max_Expression_Items - Package body                       --
--                                                                  --
--  This software is (c) Adalog 2004-2021.                          --
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
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Max_Expression_Items is
   use Asis;
   use Framework;

   -- Algorithm:
   --
   -- When an expression is encountered, a recursive function is invoked through its subexpressions to evaluate its
   -- complexity (a full ASIS traverse would be overkill).
   -- This implies that we must first decide whether the expression is a root expression (one that has to be counted),
   -- or a subexpression. This is done by checking the enclosing element.
   --
   -- The only issue is deciding what constitutes a root expression, and what is a subexpression (and if the latter,
   -- what is its own complexity). This is done by assigning a counting_mode to each main class of subexpression, see
   -- details in the description of type Counting_Mode


   type Counting_Kind is (Fixed, Summ, Max, Failure);
   pragma Assert (Failure = Counting_Kind'Last, Message => "Failure must stay last");
   -- How the complexity of expressions is counted
   -- Fixed:   Complexity is a fixed value, given by Value parameter (see Counting_Mode)
   -- Summ:    Complexity is the summ of the complexities of subexpressions
   -- Max:     Complexity is the maximum of the complexities of subexpressions
   -- Failure: Expr is special and is not expected to be encountered during traversal

   subtype Dependent_Kind is Counting_Kind range Summ  .. Max;

   type Counting_Mode is
      record
         Subexpr_Kind : Counting_Kind;  -- What to do when expr is part of another evaluation
         Root_Kind    : Counting_Kind;  -- What to do to evaluate expr's own complexity
         Is_Root      : Boolean;
         Value        : ASIS_Natural;
      end record;

   -- How to compute complexity of any expression. Elements with no subexpressions must be Fixed, Fixed
   Expr_Mode : constant array (Asis.Expression_Kinds) of Counting_Mode :=
   --                                                         Subexpr_Kind  Root_Kind   Is_Root  Value
                 (Not_An_Expression                       => (Failure,      Failure,    others => <>),

                  A_Box_Expression                        => (Fixed,        Fixed,      False,   1),

                  An_Integer_Literal                      => (Fixed,        Fixed,      False,   1),
                  A_Real_Literal                          => (Fixed,        Fixed,      False,   1),
                  A_String_Literal                        => (Fixed,        Fixed,      False,   1),
                  An_Identifier                           => (Fixed,        Fixed,      False,   1),
                  An_Operator_Symbol                      => (Fixed,        Fixed,      False,   1),
                  A_Character_Literal                     => (Fixed,        Fixed,      False,   1),
                  An_Enumeration_Literal                  => (Fixed,        Fixed,      False,   1),
                  An_Explicit_Dereference                 => (Fixed,        Fixed,      False,   1),
                  -- P.all.Comp and P.comp should be counted same
                  A_Function_Call                         => (Fixed,        Max,        True,    1),
                  -- Except operators, see variable Operator_Mode

                  An_Indexed_Component                    => (Fixed,        Fixed,      True,    1),
                  A_Slice                                 => (Fixed,        Fixed,      True,    1),
                  A_Selected_Component                    => (Fixed,        Fixed,      False,   1),
                  An_Attribute_Reference                  => (Fixed,        Fixed,      True,    1),

                  A_Record_Aggregate                      => (Fixed,        Fixed,      True,    1),
                  An_Extension_Aggregate                  => (Fixed,        Fixed,      True,    1),
                  A_Positional_Array_Aggregate            => (Fixed,        Fixed,      True,    1),
                  A_Named_Array_Aggregate                 => (Fixed,        Fixed,      True,    1),

                  An_And_Then_Short_Circuit               => (Summ,         Summ,       False,   0),
                  An_Or_Else_Short_Circuit                => (Summ,         Summ,       True,    0),

                  An_In_Membership_Test                   => (Fixed,        Summ,       False,   1),
                  A_Not_In_Membership_Test                => (Fixed,        Summ,       False,   1),

                  A_Null_Literal                          => (Fixed,        Fixed,      False,   1),
                  A_Parenthesized_Expression              => (Summ,         Summ,       False,   0),
                  -- 0 to not penalize (<expr>) over <expr>

                  A_Raise_Expression                      => (Fixed,        Fixed,      True,    1),

                  A_Type_Conversion                       => (Summ,         Summ,       False,   1),
                  A_Qualified_Expression                  => (Summ,         Summ,       False,   1),

                  An_Allocation_From_Subtype              => (Fixed,        Fixed,      False,   1),
                  An_Allocation_From_Qualified_Expression => (Fixed,        Fixed,      True,    1),
                  A_Case_Expression                       => (Fixed,        Max,        True,    1),
                  An_If_Expression                        => (Fixed,        Max,        True,    1),
                  A_For_All_Quantified_Expression         => (Fixed,        Max,        True,    1),
                  A_For_Some_Quantified_Expression        => (Fixed,        Max,        True,    1),
                  others                                  => (Fixed,        Fixed,      False,   1) -- A_Target_Name
                 );
   Operator_Mode : constant Counting_Mode                 := (Summ,         Summ,       False,   0);

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Ctl_Labels : array (Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Maximum    : array (Control_Kinds) of Asis.ASIS_Natural := (others => Asis.ASIS_Natural'Last);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control that the number of items in an expression");
      User_Message ("does not exceed the indicated maximum");
      User_Message;
      User_Message ("Parameter: <Max allowed items>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;
   begin
      if Maximum (Ctl_Kind) /= Asis.ASIS_Natural'Last then
         Parameter_Error (Rule_Id, "rule already specified");
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "Maximum value required");
      end if;

      begin
         Maximum (Ctl_Kind) := Get_Integer_Parameter (Min => 2, Max => Asis.ASIS_Natural'Last - 1);
      exception
         when Constraint_Error =>
            Parameter_Error (Rule_Id, "maximum value negative or too big");
      end;

      Ctl_Labels (Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);

      Rule_Used := True;
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
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   ---------------
   -- Good_Mode --
   ---------------

   function Good_Mode (Expr : Asis.Expression) return Counting_Mode is
   -- We must differentiate operators from other functions, which is not done by the language
   -- This function returns the mode of Expr, special-casing operators
      use Asis.Elements, Asis.Expressions;
      use Thick_Queries;
   begin
      if Expression_Kind (Expr) = A_Function_Call
        and then Expression_Kind (Simple_Name (Prefix (Expr))) = An_Operator_Symbol
      then
         return Operator_Mode;
      else
         return Expr_Mode (Expression_Kind (Expr));
      end if;
   end Good_Mode;

   ---------------------
   -- Eval_Complexity --
   ---------------------

   function Eval_Complexity (Expr : Asis.Expression; As_Root : Boolean) return Asis.ASIS_Natural is
      use Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Utilities;

      Offset : ASIS_Natural;
      Result : ASIS_Natural;
      Mode   : constant Counting_Mode := Good_Mode (Expr);
      Good_Kind : Counting_Kind;
   begin
      if As_Root then
         if Mode.Root_Kind = Fixed then
            return Mode.Value;
         end if;
         Good_Kind := Mode.Root_Kind;
      else
         if Mode.Subexpr_Kind = Fixed then
            return Mode.Value;
         end if;
         Good_Kind := Mode.Subexpr_Kind;
      end if;
      Offset := Mode.Value;

      -- Compute complexity of subexpressions
      -- The following case statement must explicitely mention every expression_kind whose Subexpr_Kind or Root_Kind
      -- is not Fixed in table Expr_Mode
      case Expression_Kind (Expr) is
         when A_Parenthesized_Expression =>
            Result := Eval_Complexity (Expression_Parenthesized (Expr), As_Root => False);

         when A_Qualified_Expression | A_Type_Conversion =>
            Result := Eval_Complexity (Converted_Or_Qualified_Expression (Expr), As_Root => False);

         when An_Indexed_Component =>
            Result := 0;
            for I : Asis.Expression of Index_Expressions (Expr) loop
               case Dependent_Kind (Good_Kind) is
                  when Summ =>
                     Result := Result + Eval_Complexity (I, As_Root => False);
                  when Max =>
                     Result := Asis_Natural'Max (Result, Eval_Complexity (I, As_Root => False));
               end case;
            end loop;

         when An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit =>
            case Dependent_Kind (Good_Kind) is
               when Summ =>
                  Result := Eval_Complexity (Short_Circuit_Operation_Left_Expression (Expr),  As_Root => False)
                          + Eval_Complexity (Short_Circuit_Operation_Right_Expression (Expr), As_Root => False);
               when Max =>
                  Result := ASIS_Natural'Max (Eval_Complexity (Short_Circuit_Operation_Left_Expression (Expr),
                                                               As_Root => False),
                                              Eval_Complexity (Short_Circuit_Operation_Right_Expression (Expr),
                                                               As_Root => False));
            end case;

         when A_Function_Call =>
            Result := 0;
            for P : Asis.Association of Function_Call_Parameters (Expr) loop
               case Dependent_Kind (Good_Kind) is
                  when Summ =>
                     Result := Result + Eval_Complexity (Actual_Parameter (P), As_Root => False);
                  when Max =>
                     Result := Asis_Natural'Max (Result, Eval_Complexity (Actual_Parameter (P), As_Root => False));
               end case;
            end loop;

         when An_If_Expression | A_Case_Expression =>
            Result := 0;
            for P : Path of Expression_Paths (Expr) loop
               case Dependent_Kind (Good_Kind) is
                  when Summ =>
                     Result := Result + Eval_Complexity (Dependent_Expression (P), As_Root => False);
                  when Max =>
                     Result := Asis_Natural'Max (Result, Eval_Complexity (Dependent_Expression (P),
                                                                          As_Root => False));
               end case;
            end loop;

         when An_In_Membership_Test | A_Not_In_Membership_Test =>
            Result := Eval_Complexity (Membership_Test_Expression (Expr), As_Root => False);
            for C : Asis.Element of Membership_Test_Choices (Expr) loop
               if Element_Kind (C) = An_Expression then
                  case Dependent_Kind (Good_Kind) is
                     when Summ =>
                        Result := Result + Eval_Complexity (C, As_Root => False);
                     when Max =>
                        Result := Asis_Natural'Max (Result, Eval_Complexity (C, As_Root => False));
                  end case;
               else  -- A constraint
                  case Constraint_Kind (C) is
                     when A_Range_Attribute_Reference =>
                        case Dependent_Kind (Good_Kind) is
                           when Summ =>
                              Result := Result + Eval_Complexity (Range_Attribute (C), As_Root => False);
                           when Max =>
                              Result := Asis_Natural'Max (Result, Eval_Complexity (Range_Attribute (C),
                                                          As_Root => False));
                        end case;
                     when A_Simple_Expression_Range =>
                        case Dependent_Kind (Good_Kind) is
                           when Summ =>
                              Result := Result + Eval_Complexity (Lower_Bound (C), As_Root => False);
                              Result := Result + Eval_Complexity (Upper_Bound (C), As_Root => False);
                           when Max =>
                              Result := Asis_Natural'Max (Result, Eval_Complexity (Lower_Bound (C),
                                                          As_Root => False));
                              Result := Asis_Natural'Max (Result, Eval_Complexity (Upper_Bound (C),
                                                          As_Root => False));
                        end case;
                     when others =>
                        Failure ("Eval_Complexity: unexpected constraint", C);
                  end case;
               end if;
            end loop;

         when A_For_All_Quantified_Expression | A_For_Some_Quantified_Expression =>
            -- TBSL What to do with the iterator?
            case Dependent_Kind (Good_Kind) is
               when Summ =>
                  Result := Eval_Complexity (Predicate (Expr), As_Root => False);
               when Max =>
                  Result := Eval_Complexity (Predicate (Expr), As_Root => False);
            end case;

         when others =>
            Failure ("Bad composite expression: " & Expression_Kinds'Wide_Image (Expression_Kind (Expr)));
      end case;

      return Offset + Result;
   end Eval_Complexity;


   ------------------------
   -- Process_Expression --
   ------------------------

   procedure Process_Expression (Expr : Asis.Expression)is
      use Ada.Strings.Wide_Unbounded;
      use Asis.Elements;
      use Framework.Locations, Framework.Reports, Utilities;

      Encl            : Asis.Element;
      Expr_Complexity : ASIS_Natural;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Is this an element of a root expression, or a subexpression (which should be ignored)?
      Encl := Enclosing_Element (Expr);
      if Association_Kind (Encl) = A_Parameter_Association then
         Encl := Enclosing_Element (Encl);
      end if;
      if Element_Kind (Encl) = An_Expression  and then not Good_Mode (Encl).Is_Root then
         return;
      end if;

      Expr_Complexity := Eval_Complexity (Expr, As_Root => True);

      if Expr_Complexity > Maximum (Check) then
         Report (Rule_Id, To_Wide_String (Ctl_Labels (Check)), Check, Get_Location (Expr),
                 "Expression is too complex (" & Integer_Img (Expr_Complexity) & ')');
      elsif Expr_Complexity > Maximum (Search) then
         Report (Rule_Id, To_Wide_String (Ctl_Labels (Search)), Search, Get_Location (Expr),
                 "Expression is too complex (" & Integer_Img (Expr_Complexity) & ')');
      end if;

      if Expr_Complexity > Maximum (Count) then
         Report (Rule_Id, To_Wide_String (Ctl_Labels (Count)), Count, Get_Location (Expr), "");
      end if;

   end Process_Expression;


begin  -- Rules.Max_Expression_Items
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Max_Expression_Items;
