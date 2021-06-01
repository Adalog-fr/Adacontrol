----------------------------------------------------------------------
--  Rules.Expressions - Package body                                --
--                                                                  --
--  This software is (c) SAGEM DS and Adalog 2004-2021.             --
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
  Framework.Reports.Fixes,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);
pragma Elaborate (Framework.Language.Shared_Keys);

package body Rules.Expressions is
   use Framework, Framework.Control_Manager, Framework.Language.Shared_Keys,
       Framework.Variables, Framework.Variables.Shared_Types;

   type Subrules is (E_And,                              E_And_Array,              E_And_Binary,
                     E_And_Boolean,                      E_And_Then,               E_Array_Aggregate,
                     E_Array_Named_Others,               E_Array_Non_Static_Range, E_Array_Partial_Others,
                     E_Array_Positional_Others,          E_Array_Others,           E_Array_Range,

                     E_Case,                             E_Complex_Parameter,

                     E_Dispatching_Function_Call,        E_Dynamic_Function_Call,  E_Downward_Conversion,

                     E_Explicit_Dereference,             E_Extendable_Aggregate,   E_Extension_Aggregate,

                     E_Fixed_Multiplying_Op,             E_For_All,                E_For_Some,
                     E_Function_Call,

                     E_If,                               E_If_Elsif,               E_If_No_Else,
                     E_Implicit_Dereference,             E_In,                     E_Inconsistent_Attribute_Dimension,
                     E_Inherited_Function_Call,

                     E_Mixed_Operators,

                     E_Not,                              E_Not_In,

                     E_Or,                               E_Or_Array,               E_Or_Binary,
                     E_Or_Boolean,                       E_Or_Else,

                     E_Parameter_View_Conversion,        E_Prefixed_Operator,

                     E_Real_Equality,                    E_Record_Aggregate,       E_Record_Partial_Others,
                     E_Record_Others,                    E_Redispatching_Function_Call,

                     E_Static_Membership,                E_Slice,

                     E_Target_Name,                      E_Trivial_Target_Name,    E_Type_Conversion,

                     E_Unconverted_Fixed_Multiplying_Op, E_Underived_Conversion,   E_Universal_Range,
                     E_Unqualified_Aggregate,            E_Upward_Conversion,

                     E_Xor,                              E_Xor_Array,              E_Xor_Binary,
                     E_Xor_Boolean);

   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "E_");
   use Subrules_Flags_Utilities;

   type Usage_Flags is array (Subrules) of Boolean;
   No_Rule_Used : constant Usage_Flags := (others => False);
   Rule_Used    : Usage_Flags := No_Rule_Used;
   Save_Used    : Usage_Flags;
   Usage        : Context_Store;

   type Categories_Context (Nb_Categories : Asis.ASIS_Natural) is new Basic_Rule_Context with
      record
         Cats : Categories_Utilities.Modifier_List (1 .. Nb_Categories);
      end record;

   package Categories_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Usage);

   Expected_Categories : constant Categories_Utilities.Modifier_Set := Basic_Set + Cat_Any + Cat_Private;

   -- Rule variables
   Called_Info : aliased Extra_Infos_Type.Object := (Value => None);


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of Ada expressions");
      User_Message;
      Help_On_Flags (Header => "Parameter(s):");
      User_Message;
      User_Message ("For all *_conversion subrules:");
      User_Message ("    [[<source_category>] <target_category>] <subrule>");
      User_Message;
      User_Message ("For all *_function_call and prefixed_operator subrules:");
      User_Message ("    [<result_category>] <subrule>");
      Help_On_Categories (Expected => Expected_Categories);
      User_Message;
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Called_Info");
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
            Subrule := Get_Flag_Parameter (Allow_Any => False);

            case Subrule is
               when E_Dispatching_Function_Call
                  | E_Dynamic_Function_Call
                  | E_Function_Call
                  | E_Inherited_Function_Call
                  | E_Prefixed_Operator
                  | E_Redispatching_Function_Call
                  =>
                  if Cat_List'Length = 1 then
                     if not Expected_Categories (Cat_List (1)) then
                        Parameter_Error (Rule_Id, "Category not allowed: " & Image (Cat_List (1)));
                     end if;
                  elsif Cat_List'Length > 1 then
                     Parameter_Error (Rule_Id, "At most one category allowed for subrule " & Image (Subrule));
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
                        for C : Categories of Cat_List loop
                           if not Expected_Categories (C) then
                              Parameter_Error (Rule_Id, "Category not allowed: " & Image (C));
                           end if;
                        end loop;
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
                        Parameter_Error (Rule_Id, "At most two categories allowed for subrule " & Image (Subrule));
                  end case;
               when others =>
                  if Rule_Used (Subrule) then
                     Parameter_Error (Rule_Id, "Subrule already given: " & Image (Subrule, Lower_Case));
                  end if;

                  if Cat_List /= Empty_List then
                     Parameter_Error (Rule_Id, "No categories allowed for subrule " & Image (Subrule));
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
            Called_Info := (Value => None);
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
   procedure Do_Report (Expr : Subrules; Loc : Locations.Location; Extra_Info : Wide_String := "") is
      use Framework.Reports, Utilities;
   begin
      if not Rule_Used (Expr) then
         return;
      end if;

      if Extra_Info = "" then
         Report (Rule_Id,
                 Association (Usage, Image (Expr)),
                 Loc,
                 "use of expression """ & Image (Expr, Lower_Case) & '"');
      else
         Report (Rule_Id,
                 Association (Usage, Image (Expr)),
                 Loc,
                 "use of expression """ & Image (Expr, Lower_Case) & """, " & Extra_Info);
      end if;
   end Do_Report;

   ------------------------
   -- Do_Category_Report --
   ------------------------

   -- For reports where a list of expressions (possibly 1) has to match a list of categories given as
   -- contexts to the subrule
   procedure Do_Category_Report (Subrule   : Subrules;
                                 Expr_List : Asis.Expression_List;
                                 Loc       : Locations.Location;
                                 Extra     : Wide_String := "")
   is
      use Framework.Reports, Categories_Utilities, Utilities;

      Key  : constant Entity_Specification := Value (Image (Subrule));
      Iter : Context_Iterator := Categories_Iterator.Create;
   begin
      Reset (Iter, Key);
      while not Is_Exhausted (Iter) loop
         declare
            Cont        : constant Categories_Context := Categories_Context (Value (Iter));
            All_Matches : Boolean := True;
         begin
            for E in Cont.Cats'Range loop
               if not Matches (Expr_List (E),
                               Cont.Cats (E),
                               Follow_Derived     => True,
                               Privacy            => Thick_Queries.Stop_At_Private,
                               Separate_Extension => False)
               then
                  All_Matches := False;
                  exit;
               end if;
            end loop;
            if All_Matches then
               if Extra = "" then
                  Report (Rule_Id,
                          Cont,
                          Loc,
                          "use of expression """ & Image (Cont.Cats) & Image (Subrule, Lower_Case) & '"');
               else
                  Report (Rule_Id,
                          Cont,
                          Loc,
                          "use of expression """ & Image (Cont.Cats) & Image (Subrule, Lower_Case) & """, " & Extra);
               end if;
            end if;
         end;
         Next (Iter);
      end loop;
   end Do_Category_Report;


   ------------------------
   -- Do_Operator_Report --
   ------------------------

   procedure Do_Operator_Report (Call : Asis.Expression; Normal_Op, Array_Op, Binary_Op, Logical_Op : Subrules) is
      use Asis.Declarations, Asis.Expressions;
      use Framework.Locations, Thick_Queries, Utilities;

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

   -- Fixes:
   -- We delay the insertion of parentheses, since in mixed expressions several parentheses may need
   -- to be inserted at the same place. This would create conflicts when fixing, but making only one fix
   -- would result in code that does not compile any more. Hence, we merge all inserts up to the level
   -- where the operator is not within another operator call.
   Mixed_Fixes : Reports.Fixes.Incremental_Fix;

   procedure Do_Mixed_Report (Call : Asis.Expression; Parameter : Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports;
      use all type Reports.Fixes.Insert_Place;
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
               Fixes.Insert (Mixed_Fixes, "(", Before, Parameter);
               Fixes.Insert (Mixed_Fixes, ")", After,  Parameter);
            end if;
         when An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit =>
            if Expression_Kind (Call) /= Expression_Kind (Parameter) then
               Report (Rule_Id,
                       Control_Manager.Association (Usage, Image (E_Mixed_Operators)),
                       Get_Location (Short_Circuit_Operation_Left_Expression (Parameter)),
                       "Unparenthesized mixed operators in expression");
               Fixes.Insert (Mixed_Fixes, "(", Before, Parameter);
               Fixes.Insert (Mixed_Fixes, ")", After,  Parameter);
            end if;
         when An_In_Membership_Test | A_Not_In_Membership_Test =>
            if Expression_Kind (Call) /= Expression_Kind (Parameter) then
               Report (Rule_Id,
                       Control_Manager.Association (Usage, Image (E_Mixed_Operators)),
                       Get_Location (Membership_Test_Expression (Parameter)),
                       "Unparenthesized mixed operators in expression");
               Fixes.Insert (Mixed_Fixes, "(", Before, Parameter);
               Fixes.Insert (Mixed_Fixes, ")", After,  Parameter);
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
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Locations, Framework.Reports, Thick_Queries;
      Called : Asis.Expression  := Called_Simple_Name (Call);

      function Extra_Info (Name : Asis.Name) return Wide_String is
         use Utilities;
      begin
         if Is_Nil (Name) then
            if Called_Info.Value = None then
               return "";
            else
               return "Dynamic call";
            end if;
         end if;

         case Called_Info.Value is
            when None =>
               return "";
            when Compact =>
               return Adjust_Image (Full_Name_Image (Name, With_Profile => False));
            when Detailed =>
               return Adjust_Image (Full_Name_Image (Name, With_Profile => True));
            when Root_Detailed =>
               return Adjust_Image (Full_Name_Image (Ultimate_Name (Name), With_Profile => True));
         end case;
      end Extra_Info;

   begin  -- Process_Function_Call

      -- Rules for all calls, even dynamic ones
      if   Rule_Used (E_Redispatching_Function_Call)
        or Rule_Used (E_Dispatching_Function_Call)
        or Rule_Used (E_Dynamic_Function_Call)
        or Rule_Used (E_Function_Call)
      then
         if Expression_Kind (Called) /= An_Operator_Symbol
           or else not Is_Nil (Corresponding_Name_Declaration (Called))
         then
            -- Predefined operators purposedly excluded

            if Is_Nil (Called) then
               Do_Category_Report (E_Function_Call,         (1 => Call), Get_Location (Call), Extra_Info (Called));
               Do_Category_Report (E_Dynamic_Function_Call, (1 => Call), Get_Location (Call));
            else
               Do_Category_Report (E_Function_Call, (1 => Call), Get_Location (Called), Extra_Info (Called));
            end if;

            if Is_Dispatching_Call (Call) then
               Do_Category_Report (E_Dispatching_Function_Call,
                                   (1 => Call),
                                   Get_Location (Called),
                                   Extra_Info (Called));

               if Rule_Used (E_Redispatching_Function_Call) then
                  declare
                     Name : Asis.Defining_Name := Enclosing_Program_Unit (Call);
                  begin
                     while not Is_Nil (Name) loop
                        if Is_Dispatching_Operation (Corresponding_Declaration (Enclosing_Element (Name))) then
                           Do_Category_Report (E_Redispatching_Function_Call,
                                               (1 => Call),
                                               Get_Location (Called),
                                               Extra_Info (Called));
                        end if;
                        Name := Enclosing_Program_Unit (Name);
                     end loop;
                  end;
               end if;
            end if;
         end if;
      end if;

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
            Do_Category_Report (E_Prefixed_Operator,
                                  (1 => Call),
                                  Get_Location (Call));
         end if;

         -- Mixed_Operators
         if Rule_Used (E_Mixed_Operators) and then not Is_Prefix_Call (Call) then
            declare
               Params : constant Asis.Association_List := Function_Call_Parameters (Call);
            begin
               if Operator_Kind (Called) /= Not_An_Operator then
                  if Params'Length = 2 then -- Only binary operators...
                     for P : Asis.Association of Params loop
                        Do_Mixed_Report (Call, Actual_Parameter (P));
                     end loop;
                  elsif Operator_Kind (Called) = A_Unary_Minus_Operator
                       and then Expression_Kind (Actual_Parameter (Params (1))) = A_Function_Call
                       and then Operator_Kind (Prefix (Actual_Parameter (Params (1)))) = A_Mod_Operator
                  then
                     -- ... except for the case of -A mod B (which means -(A mod B), /= (-A) mod B)
                     Do_Mixed_Report (Call, Actual_Parameter (Params (1)));
                  end if;
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
         Do_Category_Report (E_Inherited_Function_Call,
                               (1 => Call),
                                Get_Location (Call));
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
            begin
               Parameter_Loop :
               for Param : Asis.Association of Function_Call_Parameters (Call) loop
                  declare
                     P : constant Asis.Expression := Actual_Parameter (Param);
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
               Fixed_Parameters_Count : Natural := 0; -- Yes we are lazy to use Integer here...
            begin
               for Param : Asis.Association of Function_Call_Parameters (Call) loop
                  declare
                     P : constant Asis.Expression := Actual_Parameter (Param);
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

         when A_Not_Operator =>
            Do_Report (E_Not, Get_Location (Call));

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
      use Framework.Locations, Thick_Queries, Utilities;

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
      use Framework.Locations, Thick_Queries;

      subtype An_Array_Aggregate is Expression_Kinds range A_Positional_Array_Aggregate .. A_Named_Array_Aggregate;
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

            -- Processing internals of aggregates is quite costly in Asis-for-Gnat, do it only if necessary
            if (Rule_Used and Usage_Flags'(E_Array_Range        | E_Array_Non_Static_Range  | E_Array_Others |
                                           E_Array_Named_Others | E_Array_Positional_Others | E_Array_Partial_Others
                                                  => True,
                                           others => False))
              /= No_Rule_Used
            then
               declare
                  Assocs  : constant Asis.Association_List := Array_Component_Associations (Expression);
               begin
                  for Assoc : Asis.Association of Assocs loop
                     for Choice : Asis.Expression of Array_Component_Choices (Assoc) loop
                        if Definition_Kind (Choice) = A_Discrete_Range then
                           Do_Report (E_Array_Range, Get_Location (Choice));
                           if Rule_Used (E_Array_Non_Static_Range) then
                              declare
                                 Bounds : constant Asis.Element_List := Discrete_Constraining_Bounds (Choice);
                              begin
                                 if not Is_Static_Expression (Bounds (1)) or not Is_Static_Expression (Bounds (2)) then
                                    Do_Report (E_Array_Non_Static_Range, Get_Location (Choice));
                                 end if;
                              end;
                           end if;
                        end if;
                     end loop;
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
                           case An_Array_Aggregate (Expression_Kind (Expression)) is
                              when A_Named_Array_Aggregate =>
                                 Do_Report (E_Array_Named_Others, Get_Location (Choices (Choices'First)));
                              when A_Positional_Array_Aggregate =>
                                 Do_Report (E_Array_Positional_Others, Get_Location (Choices (Choices'First)));
                           end case;
                           Do_Report (E_Array_Partial_Others, Get_Location (Choices (Choices'First)));
                        end if;
                     end if;
                  end Check_Others;
               end;
            end if;

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
               Do_Category_Report (E_Type_Conversion,
                                     (Converted_Or_Qualified_Expression (Expression), Expression),
                                     Get_Location (Expression));
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
                     Do_Category_Report (E_Underived_Conversion,
                                           (Converted_Or_Qualified_Expression (Expression), Expression),
                                           Get_Location (Expression));
                  end if;
               end;
            end if;

            if Rule_Used (E_Upward_Conversion) or Rule_Used (E_Downward_Conversion) then
               declare
                  -- In the following declarations, we obtain the declaration of the type of the expression by
                  -- taking the enclosing element of the type definition rather than
                  -- by calling Corresponding_Expression_Type because the latter would return Nil_Element in the case
                  -- of a class-wide type. Note that we use our own Corresponding_Expression_Type_Definition,
                  -- this is not guaranteed to work with the ASIS one.

                  Source_Expr     : constant Asis.Expression := Converted_Or_Qualified_Expression   (Expression);
                  Source_Type_Def : constant Asis.Definition := Thick_Queries.Corresponding_Expression_Type_Definition
                                                                 (Source_Expr);
                  Target_Expr     : constant Asis.Expression := Converted_Or_Qualified_Subtype_Mark (Expression);
                  Source_Descr    : Derivation_Descriptor;
                  Target_Descr    : Derivation_Descriptor;
               begin
                  -- we must protect against predefined "*" and "/" on fixed points, since the result has no
                  -- well defined type.
                  -- This should be the only case where Source_Type_Def is Nil.
                  -- Not applicable either if the source is of an anonymous type (Definition not in a (sub)type
                  if not Is_Nil (Source_Type_Def)
                    and then Declaration_Kind (Enclosing_Element (Source_Type_Def))
                             in A_Type_Declaration | A_Subtype_Declaration
                  then
                     Source_Descr := Corresponding_Derivation_Description (Enclosing_Element (Source_Type_Def));
                     Target_Descr := Corresponding_Derivation_Description (Corresponding_Name_Declaration
                                                                           (Simple_Name
                                                                            (Strip_Attributes
                                                                             (Target_Expr))));
                     if         not Is_Nil (Source_Descr.Ultimate_Type)
                       and then not Is_Nil (Target_Descr.Ultimate_Type)
                       and then Is_Equal (Source_Descr.Ultimate_Type, Target_Descr.Ultimate_Type)
                     then
                        if Rule_Used (E_Upward_Conversion)
                          and then Source_Descr.Derivation_Depth > Target_Descr.Derivation_Depth
                        then
                           Do_Category_Report (E_Upward_Conversion,
                                               (Converted_Or_Qualified_Expression (Expression), Expression),
                                               Get_Location (Expression));
                        end if;
                        if Rule_Used (E_Downward_Conversion)
                          and then Source_Descr.Derivation_Depth < Target_Descr.Derivation_Depth
                        then
                           Do_Category_Report (E_Downward_Conversion,
                                               (Converted_Or_Qualified_Expression (Expression), Expression),
                                               Get_Location (Expression));
                        end if;
                     end if;
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

         when others =>   -- Including A_Target_Name if supported
            if Expression_Kinds'Wide_Image (Expression_Kind (Expression)) = "A_TARGET_NAME" then
               Do_Report (E_Target_Name, Get_Location (Expression));
               if Rule_Used (E_Trivial_Target_Name) then
                  if Expression_Kind (Corresponding_Target (Expression)) = An_Identifier then
                     Do_Report (E_Trivial_Target_Name, Get_Location (Expression));
                  end if;
               end if;
            end if;
      end case;
   end Process_Expression;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Element) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Locations, Thick_Queries;
      type Callable_Kind is (Regular, Operator, Attribute);
      Called      : Asis.Element;
      Called_Kind : Callable_Kind;
   begin
      if not (   Rule_Used (E_Complex_Parameter)
              or Rule_Used (E_Parameter_View_Conversion)
              or Rule_Used (E_Implicit_Dereference))
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Called := Called_Simple_Name (Call);

      if Rule_Used (E_Implicit_Dereference) then
         if Is_Nil (Called) then -- Access to subprogram, cf. Called_Simple_Name
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
         end if;

         if Is_Prefix_Notation (Call) then
            -- The first parameter may be an implicit dereference (there must be at least one parameter!)
            declare
               First_Param : constant Asis.Expression := Actual_Parameter (Actual_Parameters (Call) (1));
            begin
               if Is_Access_Expression (First_Param) and Expression_Kind (First_Param) /= An_Explicit_Dereference then
                  Do_Report (E_Implicit_Dereference, Get_Location (First_Param));
               end if;
            end;
         end if;

         if not (Rule_Used (E_Complex_Parameter) or Rule_Used (E_Parameter_View_Conversion)) then
            return;
         end if;
      end if;

      case Expression_Kind (Called) is
         when An_Operator_Symbol =>
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
                     Do_Category_Report (E_Parameter_View_Conversion,
                                         (Converted_Or_Qualified_Expression (Expression), Expression),
                                         Get_Location (Expression));
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
         -- The complex_parameter subrule does not apply to operators, otherwise no expression
         -- more complicated than a single operation would be allowed.
         if Rule_Used (E_Complex_Parameter) and Called_Kind /= Operator then
            for P : Asis.Association of Parameters loop
               Expression := Actual_Parameter (P);
               if Called_Kind /= Operator
                 and then Expression_Kind (Expression) = A_Function_Call
               then
                  Do_Report (E_Complex_Parameter, Get_Location (Expression));
               end if;
            end loop;
         end if;

         if Rule_Used (E_Parameter_View_Conversion) then
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
                     Do_Category_Report (E_Parameter_View_Conversion,
                                         (Converted_Or_Qualified_Expression (Expression), Expression),
                                         Get_Location (Expression));
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
      use Framework.Locations;

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

   -----------------------
   -- Post_Process_Call --
   -----------------------

   procedure Post_Process_Call (Call : in Asis.Element) is
      use Asis, Asis.Elements;
   begin
      if not Rule_Used (E_Mixed_Operators) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Element_Kind (Enclosing_Element (Call)) is
         when An_Expression | An_Association =>
            null;
         when others =>
            Reports.Fixes.Flush (Mixed_Fixes);
      end case;
   end Post_Process_Call;

begin  -- Rules.Expressions
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);

   Framework.Variables.Register (Called_Info'Access,
                                 Variable_Name => Rule_Id & ".CALLED_INFO");
end Rules.Expressions;
