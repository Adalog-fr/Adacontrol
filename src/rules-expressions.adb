----------------------------------------------------------------------
--  Rules.Expressions - Package body                                --
--                                                                  --
--  This software  is (c) SAGEM DS and  Adalog  2004-2005.  The Ada --
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
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language,
  Framework.Language.Shared_Keys;
pragma Elaborate (Framework.Language);

package body Rules.Expressions is
   use Framework, Framework.Control_Manager, Framework.Language.Shared_Keys;

   type Subrules is (E_And,                  E_And_Array,              E_And_Binary,
                     E_And_Boolean,          E_And_Then,               E_Array_Aggregate,
                     E_Array_Partial_Others, E_Array_Non_Static_Range, E_Array_Others,
                     E_Array_Range,

                     E_Case, E_Complex_Parameter,

                     E_Downward_Conversion,

                     E_Explicit_Dereference, E_Extendable_Aggregate, E_Extension_Aggregate,

                     E_Fixed_Multiplying_Op, E_For_All, E_For_Some,

                     E_If,                      E_If_Elsif, E_If_No_Else,
                     E_Implicit_Dereference,    E_In,       E_Inconsistent_Attribute_Dimension,
                     E_Inherited_Function_Call,

                     E_Mixed_Operators,

                     E_Not_In,

                     E_Or,         E_Or_Array, E_Or_Binary,
                     E_Or_Boolean, E_Or_Else,

                     E_Parameter_View_Conversion, E_Prefixed_Operator,

                     E_Real_Equality, E_Record_Aggregate, E_Record_Partial_Others,
                     E_Record_Others,

                     E_Static_Membership, E_Slice,

                     E_Type_Conversion,

                     E_Unconverted_Fixed_Multiplying_Op, E_Underived_Conversion, E_Universal_Range,
                     E_Unqualified_Aggregate,            E_Upward_Conversion,

                     E_Xor,         E_Xor_Array, E_Xor_Binary,
                     E_Xor_Boolean);

   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "E_");
   use Subrules_Flags_Utilities;

   Key_Inherited_Function_Call   : constant Entity_Specification := Value (Image (E_Inherited_Function_Call));
   Key_Parameter_View_Conversion : constant Entity_Specification := Value (Image (E_Parameter_View_Conversion));
   Key_Prefixed_Operator         : constant Entity_Specification := Value (Image (E_Prefixed_Operator));

   type Usage_Flags is array (Subrules) of Boolean;
   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;
   Usage     : Context_Store;

   type Categories_Context (Nb_Categories : Asis.ASIS_Natural) is new Basic_Rule_Context with
      record
         Cats : Categories_Utilities.Modifier_List (1 .. Nb_Categories);
      end record;

   package Categories_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Usage);

   Expected_Categories : constant Categories_Utilities.Modifier_Set := (Cat_Extension => False, others => True);


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Framework.Language.Shared_Keys.Categories_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of Ada expressions");
      User_Message;
      Help_On_Flags (Header => "Parameter (s):");
      User_Message ("For all *_conversion subrules:");
      User_Message ("    [[<source_category>] <target_category>] <subrule>");
      User_Message ("For subrules inherited_function_call and prefixed_operator:");
      User_Message ("    [<result_category>] <subrule>");
      Help_On_Modifiers (Header => "Categories:", Expected => Expected_Categories);
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Categories_Utilities, Utilities;
      Subrule : Subrules;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "At least one parameter required");
      end if;

      while Parameter_Exists loop
         declare
            Cat_List : constant Modifier_List := Get_Modifier_List (Expected_Categories);
         begin
            for C in Cat_List'Range loop
               if Cat_List (C) = Cat_New then
                  Parameter_Error (Rule_Id, "Category ""new"" not allowed here");
               end if;
            end loop;

            Subrule := Get_Flag_Parameter (Allow_Any => False);

            case Subrule is
               when E_Prefixed_Operator | E_Inherited_Function_Call =>
                  if Cat_List'Length > 1 then
                     Parameter_Error (Rule_Id, "At most one category allowed");
                  end if;
                  Associate (Usage,
                             Value (Image (Subrule)),
                             Categories_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                                 Nb_Categories => Cat_List'Length,
                                                 Cats          => Cat_List),
                             Additive => True);

               when E_Type_Conversion
                  | E_Underived_Conversion
                  | E_Downward_Conversion
                  | E_Upward_Conversion
                  | E_Parameter_View_Conversion
                    =>
                  case Cat_List'Length is
                     when 0 | 2 =>
                        Associate (Usage,
                                   Value (Image (Subrule)),
                                   Categories_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                                       Nb_Categories => Cat_List'Length,
                                                       Cats          => Cat_List),
                                   Additive => True);
                     when 1 =>
                        Associate (Usage,
                                   Value (Image (Subrule)),
                                   Categories_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                                       Nb_Categories => 2,
                                                       Cats          => Cat_Any & Cat_List),
                                   Additive => True);
                     when others =>
                        Parameter_Error (Rule_Id, "At most two categories allowed");
                  end case;
               when others =>
                  if Rule_Used (Subrule) then
                     Parameter_Error (Rule_Id, "Subrule already given: " & Image (Subrule, Lower_Case));
                  end if;

                  if Cat_List /= Empty_List then
                     Parameter_Error (Rule_Id, "No categories allowed for this subrule");
                  end if;
                  Associate (Usage, Value (Image (Subrule)), Basic.New_Context (Ctl_Kind, Ctl_Label));
            end case;

            Rule_Used (Subrule) := True;
         exception
            when Already_In_Store =>
               Parameter_Error (Rule_Id, "Subrule already given with the same categories: "
                                         & Image (Subrule, Lower_Case));
         end;
      end loop;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => False);
            Clear (Usage);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ---------------
   -- Do_Report --
   ---------------

   -- This one for simple rules with only one context
   procedure Do_Report (Expr : Subrules; Loc : Location) is
      use Framework.Reports, Utilities;
   begin
      if not Rule_Used (Expr) then
         return;
      end if;

      Report (Rule_Id,
              Association (Usage, Image (Expr)),
              Loc,
              "use of expression """ & Image (Expr, Lower_Case) & '"');
   end Do_Report;

   -- This one for rules with several contexts
   -- Avoids calling Association, which would reset the iterator while we are iterating!
   -- Pre-Condition: Rule_Used (subrule) = True
   procedure Do_Report (Expr : Subrules;
                        Cont : Basic_Rule_Context'Class;
                        Loc  : Location;
                        Cats : Categories_Utilities.Modifier_List := Categories_Utilities.Empty_List)
   is
      use Framework.Reports, Categories_Utilities, Utilities;
   begin
      Report (Rule_Id,
              Cont,
              Loc,
              "use of expression """ & Image (Cats) & Image (Expr, Lower_Case) & '"');
   end Do_Report;


   ------------------------
   -- Do_Category_Report --
   ------------------------

   procedure Do_Category_Report (Expr_List : Asis.Expression_List;
                                 Cont      : Categories_Context;
                                 Sr        : Subrules;
                                 Loc       : Location)
   is
   begin
      for E in Cont.Cats'Range loop
         if not Matches (Expr_List (E), Cont.Cats (E), Follow_Derived => True) then
            return;
         end if;
      end loop;
      Do_Report (Sr, Cont, Loc, Cont.Cats);
   end Do_Category_Report;


   --------------------------
   -- Do_Conversion_Report --
   --------------------------

   procedure Do_Conversion_Report (Source_Expr : Asis.Expression;
                                   Target_Expr : Asis.Expression;
                                   Subrule     : Subrules)
   is
      Key  : constant Entity_Specification := Value (Image (Subrule));
      Iter : Context_Iterator := Categories_Iterator.Create;
   begin
      Reset (Iter, Key);
      while not Is_Exhausted (Iter) loop
         Do_Category_Report ((Source_Expr, Target_Expr),
                             Categories_Context (Value (Iter)),
                             Subrule,
                             Get_Location (Target_Expr));
         Next (Iter);
      end loop;
   end Do_Conversion_Report;


   ------------------------
   -- Do_Operator_Report --
   ------------------------

   procedure Do_Operator_Report (Call : Asis.Expression; Normal_Op, Array_Op, Binary_Op, Logical_Op : Subrules) is
      use Asis.Declarations, Asis.Expressions;
      use Thick_Queries, Utilities;

      Loc : constant Location := Get_Location (Prefix (Call));
   begin
      Do_Report (Normal_Op, Loc);

      if To_Upper (Full_Name_Image (Names (A4G_Bugs.Corresponding_Expression_Type (Call)) (1)))
        = "STANDARD.BOOLEAN"
      then
         Do_Report (Logical_Op, Loc);
      end if;

      case Type_Category (Call, Follow_Derived => True) is
         when A_Modular_Type =>
            Do_Report (Binary_Op, Loc);
         when An_Array_Type =>
            Do_Report (Array_Op, Loc);
         when others =>
            null;
      end case;
   end Do_Operator_Report;


   ---------------------
   -- Do_Mixed_Report --
   ---------------------

   procedure Do_Mixed_Report (Call : Asis.Expression; Parameter : Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Framework.Reports;
   begin
      case Expression_Kind (Parameter) is
         when A_Function_Call =>
            if not Is_Prefix_Call (Parameter)
              and then (Expression_Kind (Call) /= A_Function_Call
                        or else Operator_Kind (Prefix (Parameter)) /= Operator_Kind (Prefix (Call)))
            then
               Report (Rule_Id,
                       Control_Manager.Association (Usage, Image (E_Mixed_Operators)),
                       Get_Location (Prefix (Parameter)),
                       "Unparenthesized mixed operators in expression");
            end if;
         when An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit =>
            if Expression_Kind (Call) /= Expression_Kind (Parameter) then
               Report (Rule_Id,
                       Control_Manager.Association (Usage, Image (E_Mixed_Operators)),
                       Get_Location (Short_Circuit_Operation_Left_Expression (Parameter)),
                       "Unparenthesized mixed operators in expression");
            end if;
         when An_In_Membership_Test | A_Not_In_Membership_Test =>
            if Expression_Kind (Call) /= Expression_Kind (Parameter) then
               Report (Rule_Id,
                       Control_Manager.Association (Usage, Image (E_Mixed_Operators)),
                       Get_Location (Membership_Test_Expression (Parameter)),
                       "Unparenthesized mixed operators in expression");
            end if;
         when others =>
            null;
      end case;
   end Do_Mixed_Report;


   ---------------------------
   -- Process_Function_Call --
   ---------------------------

   procedure Process_Function_Call (Call : in Asis.Expression) is
   -- Handles subrules: Prefixed_Operator, Mixed_Operators, Real_Equality, Fixed_Multiplying_Op,
   --                   Unconverted_Fixed_Multiplying_Op, And, Or, Xor
      use Asis, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Thick_Queries;
      Called : Asis.Expression  := Called_Simple_Name (Call);
      Iter   : Context_Iterator := Categories_Iterator.Create;
   begin
      if Is_Nil (Called) then
         -- Implicit or explicit dereference
         -- It could be argued that this is an uncheckable, but in this case no rule
         -- is really concerned, and it would create a lot of unnecessary uncheckable messages
            return;
      end if;

      -- Rules that don't follow renamings
      if Expression_Kind (Called) = An_Operator_Symbol then
         -- Prefixed_Operator
         if Rule_Used (E_Prefixed_Operator) and then Is_Prefix_Call (Call) then
            Reset (Iter, Key_Prefixed_Operator);
            while not Is_Exhausted (Iter) loop
               Do_Category_Report ((1 => Call),
                                   Categories_Context (Value (Iter)),
                                   E_Prefixed_Operator,
                                   Get_Location (Call));
               Next (Iter);
            end loop;
         end if;

         -- Mixed_Operators
         if Rule_Used (E_Mixed_Operators) and then not Is_Prefix_Call (Call) then
            declare
               Params : constant Asis.Association_List := Function_Call_Parameters (Call);
               Called_Kind : constant Asis.Operator_Kinds := Operator_Kind (Called);
            begin
               if Called_Kind /= Not_An_Operator and Params'Length = 2 then -- Only binary operators
                  for P in Params'Range loop
                     Do_Mixed_Report (Call, Actual_Parameter (Params (P)));
                  end loop;
               end if;
            end;
         end if;
      end if;

      -- Rules that follow renamings
      -- Real_Equality, and, or, xor, Inherited_Function_Call
      Called := Ultimate_Name (Called);

      if Rule_Used (E_Inherited_Function_Call)
        and then Expression_Kind (Called) /= An_Attribute_Reference
        and then Is_Part_Of_Inherited (Corresponding_Name_Definition (Called))
      then
         Reset (Iter, Key_Inherited_Function_Call);
         while not Is_Exhausted (Iter) loop
            Do_Category_Report ((1 => Call),
                                Categories_Context (Value (Iter)),
                                E_Inherited_Function_Call,
                                Get_Location (Call));
            Next (Iter);
         end loop;
      end if;

      case Operator_Kind (Called) is
         when An_Equal_Operator
            | A_Not_Equal_Operator
              =>
            if not Rule_Used (E_Real_Equality) then
               return;
            end if;

            if Corresponding_Call_Description (Call).Kind /= A_Predefined_Entity_Call then
               -- Not the predefined equality
               return;
            end if;

            -- Now check the context in which the operator is used and report
            -- errors according to the following rules

            -- 1) if    the left  parameter is not universal, print the message
            --    according to it
            --
            -- 2) elsif the right parameter is not universal, print the message
            --    according to it
            --
            -- 3) else we must be in a context like: if 0.0 = 1.0 then ....

            declare
               Parsed_First_Parameter : Boolean := False;
               F                      : constant Asis.Association_List := Function_Call_Parameters (Call);
            begin
               Parameter_Loop :
               for I in F'Range loop
                  declare
                     P : constant Asis.Expression := Actual_Parameter (F (I));
                     T : constant Asis.Definition := Ultimate_Expression_Type (P);
                  begin
                     case Type_Kind (T) is
                        when A_Root_Type_Definition =>
                           case Root_Type_Kind (T) is
                              when A_Root_Real_Definition => -- 3.4.1(8)
                                 Report
                                   (Rule_Id,
                                    Control_Manager.Association (Usage, Image (E_Real_Equality)),
                                    Get_Location (Prefix (Call)),
                                    "equality or inequality with Root Real");
                              when A_Universal_Real_Definition => -- 3.4.1(6)
                                 if Parsed_First_Parameter then
                                    Report
                                      (Rule_Id,
                                       Control_Manager.Association (Usage, Image (E_Real_Equality)),
                                       Get_Location (Prefix (Call)),
                                       "equality or inequality with two Universal Real constants");
                                 else
                                    Parsed_First_Parameter := True;
                                 end if;
                              when others =>
                                 null;
                           end case;
                        when A_Floating_Point_Definition => -- 3.5.7(2)
                           Report
                             (Rule_Id,
                              Control_Manager.Association (Usage, Image (E_Real_Equality)),
                              Get_Location (Prefix (Call)),
                              "equality or inequality with Floating Point");
                           exit Parameter_Loop;
                        when An_Ordinary_Fixed_Point_Definition => -- 3.5.9(3)
                           Report
                             (Rule_Id,
                              Control_Manager.Association (Usage, Image (E_Real_Equality)),
                              Get_Location (Prefix (Call)),
                              "equality or inequality with Ordinary Fixed Point");
                           exit Parameter_Loop;
                        when A_Decimal_Fixed_Point_Definition => -- 3.5.9(4)
                           Report
                             (Rule_Id,
                              Control_Manager.Association (Usage, Image (E_Real_Equality)),
                              Get_Location (Prefix (Call)),
                              "equality or inequality with Decimal Fixed Point");
                           exit Parameter_Loop;
                        when others =>
                           null;
                     end case;
                  end;
               end loop Parameter_Loop;
            end;

         when A_Multiply_Operator
            | A_Divide_Operator
              =>
            if not Rule_Used (E_Fixed_Multiplying_Op) and not Rule_Used (E_Unconverted_Fixed_Multiplying_Op) then
               return;
            end if;

            if Corresponding_Call_Description (Call).Kind /= A_Predefined_Entity_Call then
               -- Not the predefined operator
               return;
            end if;

            -- Since we deal only with predefined operators, the only possibilities are
            -- Fx * Fx, Fx / Fx, Fx * Integer, Integer * Fx, Fx / Integer (plus universals)
            -- => we control when at least one operand is of an explicit fixed point type
            declare
               F : constant Asis.Association_List := Function_Call_Parameters (Call);
               Fixed_Parameters_Count : Natural := 0; -- Yes we are lazy to use Integer here...
            begin
               for I in F'Range loop
                  declare
                     P : constant Asis.Expression := Actual_Parameter (F (I));
                     T : constant Asis.Definition := Ultimate_Expression_Type (P);
                  begin
                     case Type_Kind (T) is
                        when A_Root_Type_Definition =>
                           null;
                        when An_Ordinary_Fixed_Point_Definition
                           | A_Decimal_Fixed_Point_Definition
                             =>
                           Fixed_Parameters_Count := Fixed_Parameters_Count + 1;
                        when A_Signed_Integer_Type_Definition
                           | A_Modular_Type_Definition
                             =>
                           null;
                        when others =>
                           exit;  -- No need to check further
                     end case;
                  end;
               end loop;
               if Fixed_Parameters_Count > 0 then
                  Report (Rule_Id,
                          Control_Manager.Association (Usage, Image (E_Fixed_Multiplying_Op)),
                          Get_Location (Prefix (Call)),
                          "fixed point multiplying operator");
                  if Fixed_Parameters_Count = 2      -- Otherwise, it's "*" or "/" with Integer
                    and then Expression_Kind (Enclosing_Element (Call)) /= A_Type_Conversion
                  then
                     Report (Rule_Id,
                             Control_Manager.Association (Usage, Image (E_Unconverted_Fixed_Multiplying_Op)),
                             Get_Location (Prefix (Call)),
                             "unconverted fixed point multiplying operator");
                  end if;
               end if;
            end;

         when An_And_Operator =>
            Do_Operator_Report (Call, E_And, E_And_Array, E_And_Binary, E_And_Boolean);

         when An_Or_Operator =>
            Do_Operator_Report (Call, E_Or, E_Or_Array, E_Or_Binary, E_Or_Boolean);

         when An_Xor_Operator =>
            Do_Operator_Report (Call, E_Xor, E_Xor_Array, E_Xor_Binary, E_Xor_Boolean);

         when others =>
            -- Including Not_An_Operator
            null;
      end case;
   end Process_Function_Call;

   ---------------------------------
   -- Process_Attribute_Dimension --
   ----------------------------------

   procedure Process_Attribute_Dimension (Attr : Asis.Expression) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;

      Name          : Asis.Expression  := Prefix (Attr);
      Def           : Asis.Definition  := Ultimate_Expression_Type (Name);
      Has_Dimension : constant Boolean := not Is_Nil (Attribute_Designator_Expressions (Attr));
      Multidimensional_Array : Boolean;

   begin
      if Is_Nil (Def) then
         -- The prefix may be a type name, not a real expression
         case Expression_Kind (Name) is
            when A_Selected_Component =>
               Name := Selector (Name);
            when An_Attribute_Reference =>
               -- Can only be a 'Base attribute, but then it is not an array type
               return;
            when others =>
               null;
         end case;
         Def := Type_Declaration_View (Ultimate_Type_Declaration
                                       (Corresponding_Name_Declaration (Name)));
      end if;

      case Definition_Kind (Def) is
         when A_Type_Definition =>
            case Type_Kind (Def) is
               when An_Unconstrained_Array_Definition =>
                  Multidimensional_Array := Index_Subtype_Definitions (Def)'Length /= 1;
               when A_Constrained_Array_Definition =>
                  Multidimensional_Array := Discrete_Subtype_Definitions (Def)'Length /= 1;
               when others =>
                  -- 'First of integer type... not array type
                  return;
            end case;
         when A_Formal_Type_Definition =>
            case Formal_Type_Kind (Def) is
               when A_Formal_Unconstrained_Array_Definition =>
                  Multidimensional_Array := Index_Subtype_Definitions (Def)'Length /= 1;
               when A_Formal_Constrained_Array_Definition =>
                  Multidimensional_Array := Discrete_Subtype_Definitions (Def)'Length /= 1;
               when others =>
                  -- 'First of integer type... not array type
                  return;
            end case;
         when others =>
            Failure ("Process_Attribute_Dimension: wrong array definition kind "
                     & Definition_Kinds'Wide_Image (Definition_Kind (Def)),
                    Def);
      end case;

      if Has_Dimension xor Multidimensional_Array then
         Do_Report (E_Inconsistent_Attribute_Dimension, Get_Location (Attr));
      end if;
   end Process_Attribute_Dimension;

   ------------------------
   -- Process_Expression --
   ------------------------

   procedure Process_Expression (Expression : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
   begin
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Expression_Kind (Expression) is
         when An_Attribute_Reference =>
            case Attribute_Kind (Expression) is
               when A_First_Attribute
                  | A_Last_Attribute
                  | A_Length_Attribute
                  | A_Range_Attribute
                    =>
                  if Rule_Used (E_Inconsistent_Attribute_Dimension) then
                     Process_Attribute_Dimension (Expression);
                  end if;

                  if Rule_Used (E_Implicit_Dereference) and then Is_Access_Expression (Prefix (Expression)) then
                     Do_Report (E_Implicit_Dereference, Get_Location (Prefix (Expression)));
                  end if;

               when A_Callable_Attribute
                  | A_Component_Size_Attribute
                  | A_Constrained_Attribute
                  | An_Identity_Attribute
                  | A_Storage_Size_Attribute
                  | A_Tag_Attribute
                  | A_Terminated_Attribute
                  | A_Valid_Attribute
                    =>
                  if Rule_Used (E_Implicit_Dereference) and then Is_Access_Expression (Prefix (Expression)) then
                     Do_Report (E_Implicit_Dereference, Get_Location (Prefix (Expression)));
                  end if;

               when others =>
                  null;
            end case;

         when An_Explicit_Dereference =>
            -- Location below computed to point a the "all"
            Do_Report (E_Explicit_Dereference, Get_Next_Word_Location (Prefix (Expression)));

         when A_Function_Call =>
            Process_Function_Call (Expression);

         when An_In_Membership_Test =>
            Do_Report (E_In, Get_Next_Word_Location (Membership_Test_Expression (Expression)));

            if Rule_Used (E_Static_Membership) then
               declare
                  Choices : constant Asis.Element_List := Membership_Test_Choices (Expression);
               begin
                  if Choices'Length = 1  -- give up on multiple choices from Ada 2012
                    and then Are_Matching_Subtypes (Membership_Test_Expression (Expression), Choices (1))
                  then
                     Do_Report (E_Static_Membership, Get_Location (Choices (1)));
                  end if;
               end;
            end if;

            if Rule_Used (E_Mixed_Operators) then
               Do_Mixed_Report (Expression, Membership_Test_Expression (Expression));
            end if;

         when A_Not_In_Membership_Test =>
            Do_Report (E_Not_In, Get_Next_Word_Location (Membership_Test_Expression (Expression)));

            if Rule_Used (E_Static_Membership) then
               declare
                  Choices : constant Asis.Element_List := Membership_Test_Choices (Expression);
               begin
                  if Choices'Length = 1  -- give up on multiple choices from Ada 2012
                    and then Are_Matching_Subtypes (Membership_Test_Expression (Expression), Choices (1))
                  then
                     Do_Report (E_Static_Membership, Get_Location (Choices (1)));
                  end if;
               end;
            end if;

            if Rule_Used (E_Mixed_Operators) then
               Do_Mixed_Report (Expression, Membership_Test_Expression (Expression));
            end if;

         when An_Indexed_Component
            | A_Selected_Component
              =>
            if Rule_Used (E_Implicit_Dereference) and then Is_Access_Expression (Prefix (Expression)) then
               Do_Report (E_Implicit_Dereference, Get_Location (Prefix (Expression)));
            end if;

         when A_Slice =>
            Do_Report (E_Slice, Get_Location (Slice_Range (Expression)));

            if Rule_Used (E_Implicit_Dereference) and then Is_Access_Expression (Prefix (Expression)) then
               Do_Report (E_Implicit_Dereference, Get_Location (Prefix (Expression)));
            end if;

         when An_And_Then_Short_Circuit =>
            Do_Report (E_And_Then, Get_Next_Word_Location (Short_Circuit_Operation_Left_Expression(Expression)));

            if Rule_Used (E_Mixed_Operators) then
               Do_Mixed_Report (Expression, Short_Circuit_Operation_Left_Expression  (Expression));
               Do_Mixed_Report (Expression, Short_Circuit_Operation_Right_Expression (Expression));
            end if;

         when An_Or_Else_Short_Circuit =>
            Do_Report (E_Or_Else, Get_Next_Word_Location (Short_Circuit_Operation_Left_Expression(Expression)));

            if Rule_Used (E_Mixed_Operators) then
               Do_Mixed_Report (Expression, Short_Circuit_Operation_Left_Expression  (Expression));
               Do_Mixed_Report (Expression, Short_Circuit_Operation_Right_Expression (Expression));
            end if;

         when A_Named_Array_Aggregate
            | A_Positional_Array_Aggregate
              =>
            Do_Report (E_Array_Aggregate, Get_Location (Expression));

            declare
               Assocs  : constant Asis.Association_List := Array_Component_Associations (Expression);
            begin
               for A in Assocs'Range loop
                  declare
                     Choices : constant Asis.Expression_List  := Array_Component_Choices (Assocs (A));
                  begin
                     for C in Choices'Range loop
                        if Definition_Kind (Choices (C)) = A_Discrete_Range then
                           Do_Report (E_Array_Range, Get_Location (Choices (C)));
                           if Rule_Used (E_Array_Non_Static_Range) then
                              declare
                                 Bounds : constant Extended_Biggest_Int_List
                                        := Discrete_Constraining_Values (Choices (C));
                              begin
                                 if Bounds (1) = Not_Static or Bounds (2) = Not_Static then
                                    Do_Report (E_Array_Non_Static_Range, Get_Location (Choices (C)));
                                 end if;
                              end;
                           end if;
                        end if;
                     end loop;
                  end;
               end loop;

               Check_Others :
               declare
                  Choices : constant Asis.Expression_List  := Array_Component_Choices (Assocs (Assocs'Last));
               begin
                  if not Is_Nil (Choices)
                    and then Definition_Kind (Choices (Choices'First)) = An_Others_Choice
                  then
                     Do_Report (E_Array_Others, Get_Location (Choices (Choices'First)));
                     if Assocs'Length > 1 then
                        -- Note that others must appear alone as a choice, therefore we cannot
                        -- be fooled by multiple choices
                        Do_Report (E_Array_Partial_Others, Get_Location (Choices (Choices'First)));
                     end if;
                  end if;
               end Check_Others;
            end;

            if Expression_Kind (Enclosing_Element (Expression)) /= A_Qualified_Expression then
               Do_Report (E_Unqualified_Aggregate, Get_Location (Expression));
            end if;

         when A_Record_Aggregate =>
            Do_Report (E_Record_Aggregate, Get_Location (Expression));

            declare
               Assocs  : constant Asis.Association_List := Record_Component_Associations (Expression);
            begin
               -- check for (null record)
               if not Is_Nil (Assocs) then
                  declare
                     Choices : constant Asis.Expression_List  := Record_Component_Choices (Assocs (Assocs'Last));
                  begin
                     if not Is_Nil (Choices)
                       and then Definition_Kind (Choices (Choices'First)) = An_Others_Choice
                     then
                        Do_Report (E_Record_Others, Get_Location (Choices (Choices'First)));
                        if Assocs'Length > 1 then
                           -- Note that others must appear alone as a choice, therefore we cannot
                           -- be fooled by multiple choices
                           Do_Report (E_Record_Partial_Others, Get_Location (Choices (Choices'First)));
                        end if;
                     end if;
                  end;
               end if;
            end;

            if Expression_Kind (Enclosing_Element (Expression)) /= A_Qualified_Expression then
               Do_Report (E_Unqualified_Aggregate, Get_Location (Expression));
            end if;

            if Type_Category (A4G_Bugs.Corresponding_Expression_Type (Expression), Separate_Extension => True)
                 = An_Extended_Tagged_Type
            then
               Do_Report (E_Extendable_Aggregate, Get_Location (Expression));
            end if;

         when An_Extension_Aggregate =>
            Do_Report (E_Record_Aggregate,    Get_Location (Expression));
            Do_Report (E_Extension_Aggregate, Get_Location (Expression));

            declare
               Assocs  : constant Asis.Association_List := Record_Component_Associations (Expression);
            begin
               -- check for (null record)
               if not Is_Nil (Assocs) then
                  declare
                     Choices : constant Asis.Expression_List  := Record_Component_Choices (Assocs (Assocs'Last));
                  begin
                     if not Is_Nil (Choices)
                       and then Definition_Kind (Choices (Choices'First)) = An_Others_Choice
                     then
                        Do_Report (E_Record_Others, Get_Location (Choices (Choices'First)));
                        if Assocs'Length > 1 then
                           -- Note that others must appear alone as a choice, therefore we cannot
                           -- be fooled by multiple choices
                           Do_Report (E_Record_Partial_Others, Get_Location (Choices (Choices'First)));
                        end if;
                     end if;
                  end;
               end if;
            end;

            if Expression_Kind (Enclosing_Element (Expression)) /= A_Qualified_Expression then
               Do_Report (E_Unqualified_Aggregate, Get_Location (Expression));
            end if;

            declare
               Ancestor_Decl : Asis.Declaration := Simple_Name (Extension_Aggregate_Expression (Expression));
            begin
               if Expression_Kind (Ancestor_Decl) = An_Identifier then
                  Ancestor_Decl := Corresponding_Name_Declaration (Ancestor_Decl);
               end if;
               if Declaration_Kind (Ancestor_Decl) not in An_Ordinary_Type_Declaration .. A_Subtype_Declaration then
                  Ancestor_Decl := A4G_Bugs.Corresponding_Expression_Type (Extension_Aggregate_Expression (Expression));
               end if;

               if Derivation_Depth (A4G_Bugs.Corresponding_Expression_Type (Expression))
                 /= 1 +  Derivation_Depth (Ancestor_Decl)
               then
                  Do_Report (E_Extendable_Aggregate, Get_Location (Expression));
               end if;
            end;

         when A_Type_Conversion =>
            -- E_Parameter_View_Conversion is handled by Process_Call
            if Rule_Used (E_Type_Conversion) then
               Do_Conversion_Report (Converted_Or_Qualified_Expression (Expression),
                                     Expression,
                                     E_Type_Conversion);
            end if;

            if Rule_Used (E_Underived_Conversion) then
               declare
                  Source_Type : constant Asis.Declaration := A4G_Bugs.Corresponding_Expression_Type
                                                              (Converted_Or_Qualified_Expression (Expression));
                  Target_Type : constant Asis.Declaration := A4G_Bugs.Corresponding_Expression_Type (Expression);
               begin
                  if        Is_Nil (Source_Type)   -- Anonymous types can't be derived from
                    or else Is_Nil (Target_Type)
                    or else not Is_Equal (Ultimate_Type_Declaration (Source_Type),
                                          Ultimate_Type_Declaration (Target_Type))
                  then
                     Do_Conversion_Report (Converted_Or_Qualified_Expression (Expression),
                                           Expression,
                                           E_Underived_Conversion);
                  end if;
               end;
            end if;

            if Rule_Used (E_Upward_Conversion) then
               declare
                  Source_Descr : constant Derivation_Descriptor := Corresponding_Derivation_Description
                                                                    (A4G_Bugs.Corresponding_Expression_Type
                                                                     (Converted_Or_Qualified_Expression
                                                                      (Expression)));
                  Target_Descr : constant Derivation_Descriptor := Corresponding_Derivation_Description
                                                                    (A4G_Bugs.Corresponding_Expression_Type
                                                                     (Expression));
               begin
                  if         not Is_Nil (Source_Descr.Ultimate_Type)
                    and then not Is_Nil (Target_Descr.Ultimate_Type)
                    and then Is_Equal (Source_Descr.Ultimate_Type, Target_Descr.Ultimate_Type)
                    and then Source_Descr.Derivation_Depth > Target_Descr.Derivation_Depth
                  then
                     Do_Conversion_Report (Converted_Or_Qualified_Expression (Expression),
                                           Expression,
                                           E_Upward_Conversion);
                  end if;
               end;
            end if;

            if Rule_Used (E_Downward_Conversion) then
               declare
                  Source_Descr : constant Derivation_Descriptor := Corresponding_Derivation_Description
                                                                    (A4G_Bugs.Corresponding_Expression_Type
                                                                     (Converted_Or_Qualified_Expression
                                                                      (Expression)));
                  Target_Descr : constant Derivation_Descriptor := Corresponding_Derivation_Description
                                                                    (A4G_Bugs.Corresponding_Expression_Type
                                                                     (Expression));
               begin
                  if         not Is_Nil (Source_Descr.Ultimate_Type)
                    and then not Is_Nil (Target_Descr.Ultimate_Type)
                    and then Is_Equal (Source_Descr.Ultimate_Type, Target_Descr.Ultimate_Type)
                    and then Source_Descr.Derivation_Depth < Target_Descr.Derivation_Depth
                  then
                     Do_Conversion_Report (Converted_Or_Qualified_Expression (Expression),
                                           Expression,
                                           E_Downward_Conversion);
                  end if;
               end;
            end if;

         when A_Case_Expression =>
            Do_Report (E_Case, Get_Location (Expression));

         when An_If_Expression =>
            Do_Report (E_If, Get_Location (Expression));
            declare
               Paths : constant Asis.Path_List := Expression_Paths (Expression);
            begin
               if Path_Kind (Paths (Paths'Last)) /= An_Else_Expression_Path then
                  Do_Report (E_If_No_Else, Get_Location (Expression));
               end if;
               if Paths'Length >= 2 and then Path_Kind (Paths (2)) = An_Elsif_Expression_Path then
                  Do_Report (E_If_Elsif, Get_Location (Paths (2)));
               end if;
            end;

         when A_For_All_Quantified_Expression =>
            Do_Report (E_For_All, Get_Location (Expression));

         when A_For_Some_Quantified_Expression =>
            Do_Report (E_For_Some, Get_Location (Expression));

         when others =>
            null;
      end case;
   end Process_Expression;


   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Element) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries;
      type Callable_Kind is (Regular, Operator, Attribute);
      Called      : Asis.Element;
      Called_Kind : Callable_Kind;
      Iter        : Context_Iterator := Categories_Iterator.Create;
   begin
      if not (   Rule_Used (E_Complex_Parameter)
              or Rule_Used (E_Parameter_View_Conversion)
              or Rule_Used (E_Implicit_Dereference))
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Called := Called_Simple_Name (Call);

      if Rule_Used (E_Implicit_Dereference)
        and then Is_Nil (Called) -- Access to subprogram, cf. Called_Simple_Name
      then
         -- Make sure it is an /implicit/ dereference
         if Expression_Kind (Call) = A_Function_Call then
             if Expression_Kind (Prefix (Call)) /= An_Explicit_Dereference then
               Do_Report (E_Implicit_Dereference, Get_Location (Simple_Name (Prefix (Call))));
            end if;
         else
            -- Must be a procedure or entry call
            if Expression_Kind (Called_Name (Call)) /= An_Explicit_Dereference then
               Do_Report (E_Implicit_Dereference, Get_Location (Simple_Name (Called_Name (Call))));
            end if;
         end if;
         if not (   Rule_Used (E_Complex_Parameter)
                 or Rule_Used (E_Parameter_View_Conversion))
         then
            return;
         end if;
      end if;

      case Expression_Kind (Called) is
         when An_Operator_Symbol =>
            -- The complex_parameter subrule does not apply to operators, otherwise no expression
            -- more complicated than a single operation would be allowed.
            if not Rule_Used (E_Parameter_View_Conversion) then
               return;
            end if;
            Called_Kind := Operator;
         when An_Attribute_Reference =>
            Called_Kind := Attribute;
            if Attribute_Kind (Called) = A_Read_Attribute then
               -- This is the only attribute SP with an out parameter, in the second position
               declare
                  Parameters : constant Asis.Association_List := Actual_Parameters (Call, Normalized => False);
                  Expression : constant Asis.Expression := Actual_Parameter (Parameters (2));
               begin
                  if Rule_Used (E_Parameter_View_Conversion)
                    and then Expression_Kind (Expression) = A_Type_Conversion
                  then
                     Reset (Iter, Key_Parameter_View_Conversion);
                     while not Is_Exhausted (Iter) loop
                        Do_Category_Report ((Converted_Or_Qualified_Expression (Expression), Expression),
                                            Categories_Context (Value (Iter)),
                                            E_Parameter_View_Conversion,
                                            Get_Location (Expression));
                        Next (Iter);
                     end loop;
                  end if;
               end;
            end if;
         when others =>
            Called_Kind := Regular;
      end case;

      declare
         use Framework.Reports;
         Parameters : constant Asis.Association_List := Actual_Parameters (Call);
         -- Normalized form does not work for attributes, since they have no formal parameters.
         -- The only case of an attribute subprogram with an (in) out parameter is 'Read
         Expression : Asis.Expression;
         Formal     : Asis.Expression;
      begin
         if Rule_Used (E_Complex_Parameter) then
            for P in Parameters'Range loop
               Expression := Actual_Parameter (Parameters (P));
               if Called_Kind /= Operator
                 and then Expression_Kind (Expression) = A_Function_Call
               then
                  Do_Report (E_Complex_Parameter, Get_Location (Expression));
               end if;
            end loop;
         end if;

         if Rule_Used (E_Parameter_View_Conversion)
           and then Expression_Kind (Call) /= A_Function_Call  -- Functions have no [in] out parameters...
         then
            for P in Parameters'Range loop
               Expression := Actual_Parameter (Parameters (P));
               if Called_Kind /= Attribute
                 and then Expression_Kind (Expression) = A_Type_Conversion
               then
                  Formal := Formal_Name (Call, P);
                  if Is_Nil (Formal) then
                     Uncheckable (Rule_Id,
                                  False_Negative,
                                  Get_Location (Expression),
                                  "type conversion used in dispatching call");
                  elsif Mode_Kind (Enclosing_Element (Formal)) in An_Out_Mode .. An_In_Out_Mode then
                     Reset (Iter, Key_Parameter_View_Conversion);
                     while not Is_Exhausted (Iter) loop
                        Do_Category_Report ((Converted_Or_Qualified_Expression (Expression), Expression),
                                            Categories_Context (Value (Iter)),
                                            E_Parameter_View_Conversion,
                                            Get_Location (Expression));
                        Next (Iter);
                     end loop;
                  end if;
               end if;
            end loop;
         end if;
      end;
   end Process_Call;

   -------------------
   -- Process_Range --
   -------------------

   procedure Process_Range (Def : in Asis.Definition) is
      use Asis, Asis.Definitions, Asis.Elements;

      function Root_Kind (Expr : Asis.Expression) return Root_Type_Kinds is
         use Asis.Declarations;
      begin
         return Root_Type_Kind (Type_Declaration_View (A4G_Bugs.Corresponding_Expression_Type (Expr)));
      end Root_Kind;

   begin  -- Process_Range
      if not Rule_Used (E_Universal_Range) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Discrete_Range_Kind (Def) /= A_Discrete_Simple_Expression_Range then
         return;
      end if;

      if         Root_Kind (Lower_Bound (Def)) = A_Universal_Integer_Definition
        and then Root_Kind (Upper_Bound (Def)) = A_Universal_Integer_Definition
      then
         Do_Report (E_Universal_Range, Get_Location (Def));
      end if;
   end Process_Range;

begin  -- Rules.Expressions
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);

end Rules.Expressions;
