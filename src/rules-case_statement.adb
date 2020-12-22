----------------------------------------------------------------------
--  Rules.Case_Statement - Package body                             --
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
  Framework.Language.Shared_Keys;
pragma Elaborate (Framework.Language);

package body Rules.Case_Statement is

   use Asis, Framework, Framework.Language.Shared_Keys, Thick_Queries;

   type Subrules is (Others_Span, Paths, Range_Span, Values, Values_If_Others);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Usage is array (Subrules, Discrete_Categories) of Control_Kinds_Set;
   Rule_Used : Usage := (others => (others => (others => False)));
   Save_Used : Usage;

   Labels : array (Subrules, Discrete_Categories, Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   Bounds : array (Subrules, Discrete_Categories, Control_Kinds) of Framework.Language.Shared_Keys.Bounds_Values
     := (others => (others => (others => (0, 0))));

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: "& Rule_Id);
      User_Message ("Controls various sizes related to the case statement");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags (Header => "Parameter(1)   : [<category>] ");
      User_Message ("Parameter(2..3): <bound> <value>");
      User_Message ("                (at least one parameter required)");
      Help_On_Categories (Expected => Discrete_Set);
      Help_On_Bounds (Header => "<bound>: ");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Categories_Utilities, Subrules_Flag_Utilities;

      Subrule_Name   : Subrules;
      Subrule_Cat    : Categories;
      Subrule_Bounds : Bounds_Values;

      procedure Add_Cat  (Cat : Categories) is
         use Ada.Strings.Wide_Unbounded;
      begin
         if Rule_Used (Subrule_Name, Cat) (Ctl_Kind) then
            Parameter_Error (Rule_Id, "rule already specified for " & Control_Kinds'Wide_Image (Ctl_Kind));
         end if;

         Bounds    (Subrule_Name, Cat, Ctl_Kind)  := Subrule_Bounds;
         Labels    (Subrule_Name, Cat, Ctl_Kind)  := To_Unbounded_Wide_String (Ctl_Label);
         Rule_Used (Subrule_Name, Cat) (Ctl_Kind) := True;
      end Add_Cat;

   begin  -- Add_Control
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "parameters required");
      end if;

      Subrule_Cat  := Get_Modifier (Required => False, Expected => Discrete_Set, Default => Cat_Any);
      Subrule_Name := Get_Flag_Parameter (Allow_Any => False);
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "two or three parameters required");
      end if;

      Subrule_Bounds := Get_Bounds_Parameters (Rule_Id);
      if Subrule_Cat = Cat_Any then
         for C in Discrete_Categories loop
            Add_Cat (C);
         end loop;
      else
          Add_Cat (Subrule_Cat);
      end if;
    end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : in Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => (others => (others => False)));
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => (others => (others => False)));
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ------------------
   -- Check_Report --
   ------------------

   procedure Check_Report (Subrule_Name : Subrules;
                           Min_Value    : Extended_Biggest_Natural;
                           Max_Value    : Extended_Biggest_Natural;
                           Message      : Wide_String;
                           Elem         : Asis.Element;
                           Elem_Cat     : Categories)
   is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Locations, Framework.Reports;
   begin
      if Min_Value /= Not_Static then
         if Rule_Used (Subrule_Name, Elem_Cat) (Check)
           and then Min_Value < Bounds (Subrule_Name, Elem_Cat, Check).Min
         then
            Report (Rule_Id,
                    To_Wide_String (Labels (Subrule_Name, Elem_Cat, Check)),
                    Check,
                    Get_Location (Elem),
                    "too few " & Message
                    & " (" & Biggest_Int_Img (Min_Value)
                    & " < " & Biggest_Int_Img (Bounds (Subrule_Name, Elem_Cat, Check).Min) & ')');
         elsif Rule_Used (Subrule_Name, Elem_Cat) (Search)
           and then Min_Value < Bounds (Subrule_Name, Elem_Cat, Search).Min
         then
            Report (Rule_Id,
                    To_Wide_String (Labels (Subrule_Name, Elem_Cat, Search)),
                    Search,
                    Get_Location (Elem),
                    "too few " & Message
                    & " (" & Biggest_Int_Img (Min_Value)
                    & " < " & Biggest_Int_Img (Bounds (Subrule_Name, Elem_Cat, Search).Min) & ')');
         end if;

         if Rule_Used (Subrule_Name, Elem_Cat) (Count)
           and then Min_Value < Bounds (Subrule_Name, Elem_Cat, Count).Min
         then
            Report (Rule_Id,
                    To_Wide_String (Labels (Subrule_Name, Elem_Cat, Count)),
                    Count,
                    Get_Location (Elem),
                    "");
         end if;
      end if;

      if Max_Value /= Not_Static then
         if Rule_Used (Subrule_Name, Elem_Cat) (Check)
           and then Max_Value > Bounds (Subrule_Name, Elem_Cat, Check).Max
         then
            Report (Rule_Id,
                    To_Wide_String (Labels (Subrule_Name, Elem_Cat, Check)),
                    Check,
                    Get_Location (Elem),
                    "too many " & Message
                    & " (" & Biggest_Int_Img (Max_Value)
                    & " > " & Biggest_Int_Img (Bounds (Subrule_Name, Elem_Cat, Check).Max) & ')');
         elsif Rule_Used (Subrule_Name, Elem_Cat) (Search)
           and then Max_Value > Bounds (Subrule_Name, Elem_Cat, Search).Max
         then
            Report (Rule_Id,
                    To_Wide_String (Labels (Subrule_Name, Elem_Cat, Search)),
                    Search,
                    Get_Location (Elem),
                    "too many " & Message
                    & " (" & Biggest_Int_Img (Max_Value)
                    & " > " & Biggest_Int_Img (Bounds (Subrule_Name, Elem_Cat, Search).Max) & ')');
         end if;

         if Rule_Used (Subrule_Name, Elem_Cat) (Count)
           and then Max_Value > Bounds (Subrule_Name, Elem_Cat, Count).Max
         then
            Report (Rule_Id,
                    To_Wide_String (Labels (Subrule_Name, Elem_Cat, Count)),
                    Count,
                    Get_Location (Elem),
                    "");
         end if;
      end if;
   end Check_Report;

   ----------------------------
   -- Process_Case_Statement --
   ----------------------------

   procedure Process_Case_Statement (Statement : in Asis.Statement) is
   -- Case completeness is based on RM-staticness, better do the same in this procedure.
   -- The object tracker is so clever that otherwise, it could result in strange things like negative number
   -- of values covered by "when others"...
      use Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports;

      Non_Evaluable : exception;
      Case_Cat      : Categories;

      -- Compute the number of cases covered by all case alternatives
      -- (including discrete ranges) excluding the "when others" alternative
      function Count_Non_Others_Choices (Case_Paths : in Path_List) return Biggest_Natural is
         use Utilities;
         Count : Biggest_Natural := 0;
         Temp  : Extended_Biggest_Natural;
      begin
         -- We know that the last path is for "when others":
         for P : Asis.Path of Case_Paths (Case_Paths'First .. Case_Paths'Last - 1) loop
            for PE : Asis.Element of  Case_Statement_Alternative_Choices (P) loop
               if Definition_Kind (PE) = A_Discrete_Range then
                  if Discrete_Range_Kind (PE) = A_Discrete_Subtype_Indication
                    and then not Is_Nil (Corresponding_Static_Predicates (Subtype_Simple_Name (PE)))
                  then
                     -- A subtype with static predicate used for a choice: we don't know (yet) how to evaluate this
                     Uncheckable (Rule_Id,
                                  False_Negative,
                                  Get_Location (PE),
                                  "(others_span) Use of subtype with static predicate");
                     raise Non_Evaluable;
                  end if;
                  Temp := Discrete_Constraining_Lengths (PE, RM_Static => True) (1);
                  if Temp = Not_Static then
                     -- it IS static, but the evaluator cannot evaluate it...
                     -- unless it is of a generic formal type
                     Uncheckable (Rule_Id,
                                  False_Negative,
                                  Get_Location (PE),
                                  "(others_span) Could not evaluate bounds of expression");
                     raise Non_Evaluable;
                  end if;
                  Count := Count + Temp;

               elsif Element_Kind (PE) = An_Expression then
                  Count := Count + 1;

               else
                  Failure ("Unexpected path kind:", PE);
               end if;
            end loop;
         end loop;

         return Count;
      end Count_Non_Others_Choices;

      function Case_Expression_Length (Case_Expr : Asis.Expression;
                                       Wanted    : Expression_Info) return Extended_Biggest_Natural
      is
      -- Number of values covered by the case expression, taking into account the type when not static
      -- The object evaluator does this better, but here we are RM_Static
         use Asis.Declarations, Asis.Expressions;

         Result : Extended_Biggest_Natural;
         Checked_Subtype : Asis.Declaration := A4G_Bugs.Corresponding_Expression_Type (Case_Expr);
      begin
         loop
            if Declaration_Kind (Checked_Subtype) = A_Formal_Type_Declaration then
               return Not_Static;
            end if;
            Result := Discrete_Constraining_Lengths (Checked_Subtype, Wanted => Wanted, RM_Static => True) (1);
            exit when Result /= Not_Static or else Declaration_Kind (Checked_Subtype) in A_Type_Declaration;
            Checked_Subtype := Corresponding_Name_Declaration (Subtype_Simple_Name
                                                               (Type_Declaration_View
                                                                  (Checked_Subtype)));
         end loop;
         return Result;
      end Case_Expression_Length;

      procedure Process_Min_Others_Range (Case_Expr : Asis.Expression) is
         Case_Paths   : constant Path_List := Statement_Paths (Statement);
         Min_Subtype_Span : Extended_Biggest_Int;
         Max_Subtype_Span : Extended_Biggest_Int;
         Non_Others_Count : Biggest_Natural;

      begin
         -- Don't waste time if there is no "when others" choice (must be last)
         if Definition_Kind (Case_Statement_Alternative_Choices
                             (Case_Paths (Case_Paths'Last))(1)) /= An_Others_Choice
         then
            return;
         end if;

         if not Is_Nil (Corresponding_Static_Predicates (Case_Expr)) then
            Uncheckable (Rule_Id,
                         False_Negative,
                         Get_Location (Case_Expr),
                         "(others_span) Expression is of a subtype with static predicate");
            return;
         end if;

         Min_Subtype_Span := Case_Expression_Length (Case_Expr, Minimum);
         Max_Subtype_Span := Case_Expression_Length (Case_Expr, Maximum);
         Non_Others_Count := Count_Non_Others_Choices (Case_Paths);

         Check_Report (Others_Span,
                       Min_Value => Extended_Biggest_Int'Max (Max_Subtype_Span - Non_Others_Count, 0),
                       Max_Value => Extended_Biggest_Int'Max (Min_Subtype_Span - Non_Others_Count, 0),
                       Message   => "values covered by ""others"" in case statement",
                       Elem      => Case_Paths (Case_Paths'Last),
                       Elem_Cat  => Case_Cat);

      exception
         when Non_Evaluable =>
            return;
      end Process_Min_Others_Range;

      --
      -- max_values is the number of values covered by the subtype
      -- of the case selector
      --
      procedure Process_Max_Values (Case_Expr : Asis.Expression) is
         Case_Paths       : constant Path_List := Statement_Paths (Statement);
         Min_Subtype_Span : Extended_Biggest_Int;
         Max_Subtype_Span : Extended_Biggest_Int;
         Has_Others       : constant Boolean := Definition_Kind (Case_Statement_Alternative_Choices
                                                                 (Case_Paths (Case_Paths'Last)) (1)) = An_Others_Choice;

      begin
         if not Is_Nil (Corresponding_Static_Predicates (Case_Expr)) then
            Uncheckable (Rule_Id,
                         False_Negative,
                         Get_Location (Case_Expression (Statement)),
                         "(values) Expression is of a subtype with static predicate");
            return;
         end if;

         Min_Subtype_Span := Case_Expression_Length (Case_Expr, Wanted => Minimum);
         Max_Subtype_Span := Case_Expression_Length (Case_Expr, Wanted => Maximum);
         Check_Report (Values,
                       Min_Value => Max_Subtype_Span,
                       Max_Value => Min_Subtype_Span,
                       Message   => "values for subtype of selector in case statement",
                       Elem      => Statement,
                       Elem_Cat  => Case_Cat);

         if Has_Others then
            Check_Report (Values_If_Others,
                          Min_Value => Max_Subtype_Span,
                          Max_Value => Min_Subtype_Span,
                          Message   => "values for subtype of selector in case statement with ""others""",
                          Elem      => Statement,
                          Elem_Cat  => Case_Cat);
         end if;

      exception
         when Non_Evaluable =>
            return;
      end Process_Max_Values;

      procedure Process_Min_Paths is
         Path_Number : constant Extended_Biggest_Int := Statement_Paths (Statement)'Length;
      begin
         Check_Report (Paths,
                       Min_Value => Path_Number,
                       Max_Value => Path_Number,
                       Message   => "paths in case statement",
                       Elem      => Statement,
                       Elem_Cat  => Case_Cat);
      end Process_Min_Paths;

      Case_Expr : Asis.Expression;
   begin  -- Process_Case_Statement
      if Rule_Used = (Subrules => (Discrete_Categories => (Control_Kinds => False))) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Case_Expr := Case_Expression (Statement);
      Case_Cat  := Matching_Category (Case_Expr,
                                      From_Cats          => Discrete_Set,
                                      Follow_Derived     => True,
                                      Privacy            => Follow_User_Private,
                                      Separate_Extension => False);

      if   Rule_Used (Values, Case_Cat)           /= (Control_Kinds => False)
        or Rule_Used (Values_If_Others, Case_Cat) /= (Control_Kinds => False)
      then
         Process_Max_Values (Case_Expr);
      end if;

      if Rule_Used (Paths, Case_Cat) /= (Control_Kinds => False) then
         Process_Min_Paths;
      end if;

      if Rule_Used (Others_Span, Case_Cat) /= (Control_Kinds => False) then
         Process_Min_Others_Range (Case_Expr);
      end if;
   end Process_Case_Statement;

   ------------------
   -- Process_Path --
   ------------------

   procedure Process_Path (Path : Asis.Path) is
      use Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports, Utilities;

      Min_Values : Extended_Biggest_Natural;
      Max_Values : Extended_Biggest_Natural;
      Case_Cat   : constant Categories := Matching_Category (Case_Expression (Enclosing_Element (Path)),
                                                           From_Cats          => Discrete_Set,
                                                           Follow_Derived     => True,
                                                           Privacy            => Follow_User_Private,
                                                           Separate_Extension => False);
   begin
      if Rule_Used (Range_Span, Case_Cat) = (Control_Kinds => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      for Choice : Asis.Element of Case_Statement_Alternative_Choices (Path) loop
         case Definition_Kind (Choice) is
            when Not_A_Definition -- An_Expression
               | An_Others_Choice
                 =>
               null;
            when A_Discrete_Range =>
               if Discrete_Range_Kind (Choice) /= A_Discrete_Subtype_Indication
                 or else Is_Nil (Corresponding_Static_Predicates (Subtype_Simple_Name (Choice)))
               then
                  -- Normal case
                  Min_Values := Discrete_Constraining_Lengths (Choice, Wanted => Minimum, RM_Static => True) (1);
                  Max_Values := Discrete_Constraining_Lengths (Choice, Wanted => Maximum, RM_Static => True) (1);

                  Check_Report (Range_Span,
                                Min_Value => Min_Values,
                                Max_Value => Max_Values,
                                Message   => "values in choice range",
                                Elem      => Choice,
                                Elem_Cat  => Case_Cat);
               else
                  Uncheckable (Rule_Id,
                               False_Negative,
                               Get_Location (Choice),
                               "(range_span) Range is of a subtype with static predicate");
               end if;

            when others =>
               Failure ("Wrong definition in case path");
         end case;
      end loop;
   end Process_Path;

begin  -- Rules.Case_Statement
   Rules_Manager.Register (Rule_Id,
                           Rules_Manager.Semantic,
                           Help_CB        => Help'Access,
                           Add_Control_CB => Add_Control'Access,
                           Command_CB     => Command'Access);
end Rules.Case_Statement;
