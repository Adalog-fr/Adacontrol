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
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Expressions is
   use Framework;

   type Subrules is (E_And,                              E_And_Then,        E_Array_Aggregate,
                     E_Array_Partial_Others,             E_Array_Others,    E_Complex_Parameter,
                     E_Inconsistent_Attribute_Dimension, E_Mixed_Operators, E_Or,
                     E_Or_Else,                          E_Real_Equality,   E_Record_Aggregate,
                     E_Record_Partial_Others,            E_Record_Others,   E_Slice,
                     E_Unqualified_Aggregate,            E_Xor);

   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "E_");
   use Subrules_Flags_Utilities;

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
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags (Header => "Parameter (s):");
      User_Message ("Control occurrences of Ada expressions");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
      Subrule : Subrules;

   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "At least one parameter required");
      end if;

      while Parameter_Exists loop
         Subrule := Get_Flag_Parameter (Allow_Any => False);
         if Rule_Used (Subrule) then
            Parameter_Error (Rule_Id, "Expression already given: " & Image (Subrule));
         end if;

         Rule_Used (Subrule) := True;
         Usage (Subrule)     := Basic.New_Context (Ctl_Kind, Ctl_Label);
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

   procedure Do_Report (Expr : Subrules; Loc : Location) is
      use Framework.Reports;
   begin
      if not Rule_Used (Expr) then
         return;
      end if;

      Report (Rule_Id,
              Usage (Expr),
              Loc,
              "use of expression """ & Image (Expr) & '"');
   end Do_Report;


   ---------------------------
   -- Process_Function_Call --
   ---------------------------

   procedure Process_Function_Call (Call : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Thick_Queries;
      Called : Asis.Expression := Prefix (Call);
   begin
      if Expression_Kind (Called) = A_Selected_Component then
         Called := Selector (Called);
      end if;

      if Expression_Kind (Called) /= An_Operator_Symbol then
         return;
      end if;

      if Rule_Used (E_Mixed_Operators)
        and then not Is_Prefix_Call (Call)
        and then Operator_Kind (Called) not in A_Unary_Plus_Operator .. A_Unary_Minus_Operator
      then
         declare
            Params : constant Asis.Association_List := Function_Call_Parameters (Call);
            Expr   : Asis.Expression;
         begin
            for P in Params'Range loop
               Expr := Actual_Parameter (Params (P));
               if Expression_Kind (Expr) = A_Function_Call
                 and then not Is_Prefix_Call (Expr)
                 and then Operator_Kind (Prefix (Expr)) /= Operator_Kind (Called)
               then
                  Report (Rule_Id,
                          Usage (E_Mixed_Operators),
                          Get_Location (Prefix (Expr)),
                          "Unparenthesized mixed operators in expression");
               end if;
            end loop;
         end;
      end if;

      case Operator_Kind (Called) is
         when An_Equal_Operator
           | A_Not_Equal_Operator
              =>
            if not Rule_Used (E_Real_Equality) then
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
               F : constant Asis.Association_List := Function_Call_Parameters (Call);
            begin
            Parameter_Loop:
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
                                  Usage (E_Real_Equality),
                                  Get_Location (Prefix (Call)),
                                  "equality or inequality with Root Real !!!");
                              when A_Universal_Real_Definition => -- 3.4.1(6)
                                 if Parsed_First_Parameter then
                                    Report
                                    (Rule_Id,
                                     Usage (E_Real_Equality),
                                     Get_Location (Prefix (Call)),
                                     "equality or inequality with two Universal Real constants !!!");
                                 else
                                    Parsed_First_Parameter := True;
                                 end if;
                              when others =>
                                 null;
                           end case;
                        when A_Floating_Point_Definition => -- 3.5.7(2)
                           Report
                           (Rule_Id,
                            Usage (E_Real_Equality),
                            Get_Location (Prefix (Call)),
                            "equality or inequality with Floating Point");
                            exit Parameter_Loop;
                        when An_Ordinary_Fixed_Point_Definition => -- 3.5.9(3)
                           Report
                           (Rule_Id,
                            Usage (E_Real_Equality),
                            Get_Location (Prefix (Call)),
                            "equality or inequality with Ordinary Fixed Point");
                            exit Parameter_Loop;
                         when A_Decimal_Fixed_Point_Definition => -- 3.5.9(4)
                           Report
                           (Rule_Id,
                            Usage (E_Real_Equality),
                            Get_Location (Prefix (Call)),
                            "equality or inequality with Decimal Fixed Point");
                            exit Parameter_Loop;
                       when others =>
                           null;
                     end case;
                  end;
               end loop Parameter_Loop;
            end;

         when An_And_Operator =>
            Do_Report (E_And, Get_Location (Prefix (Call)));

         when An_Or_Operator =>
            Do_Report (E_Or, Get_Location (Prefix (Call)));

         when An_Xor_Operator =>
            Do_Report (E_Xor, Get_Location (Prefix (Call)));

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
         Def := Corresponding_Name_Declaration (Name);
         if Is_Nil (Def) then
            -- ASIS bug [G419-004]: Corresponding_Name_Declaration returns Nil_Element if Name is
            -- part of a pragma Priority. Not a problem in practice, since this would make a false
            -- negative only in the case of:
            --   pragma Priority (Array_Type'First(1));
            -- which would be really strange programming !
            A4G_Bugs.Trace_Bug ("Rules.Expressions: Nil name declaration");
            return;
         end if;
         Def := Type_Declaration_View (Ultimate_Type_Declaration (Def));
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
            Failure ("wrong array definition kind");
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
   begin
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Expression_Kind (Expression) is
         when An_Attribute_Reference =>
            case A4G_Bugs.Attribute_Kind (Expression) is
               when A_First_Attribute
                  | A_Last_Attribute
                  | A_Length_Attribute
                  | A_Range_Attribute
                    =>
                  if Rule_Used (E_Inconsistent_Attribute_Dimension) then
                     Process_Attribute_Dimension (Expression);
                  end if;
               when others =>
                  null;
            end case;

         when A_Function_Call =>
            Process_Function_Call (Expression);

         when A_Slice =>
            Do_Report (E_Slice, Get_Location (Slice_Range (Expression)));

         when An_And_Then_Short_Circuit =>
            Do_Report (E_And_Then, Get_Next_Word_Location (Short_Circuit_Operation_Left_Expression(Expression)));

         when An_Or_Else_Short_Circuit =>
            Do_Report (E_Or_Else, Get_Next_Word_Location (Short_Circuit_Operation_Left_Expression(Expression)));

         when A_Named_Array_Aggregate
            | A_Positional_Array_Aggregate
              =>
            Do_Report (E_Array_Aggregate, Get_Location (Expression));

            declare
               Assocs  : constant Asis.Association_List := Array_Component_Associations (Expression);
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
            end;

            if Expression_Kind (Enclosing_Element (Expression)) /= A_Qualified_Expression then
               Do_Report (E_Unqualified_Aggregate, Get_Location (Expression));
            end if;

         when A_Record_Aggregate
            | An_Extension_Aggregate
              =>
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

         when others =>
            null;
      end case;
   end Process_Expression;


   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Element) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
   begin
      if not Rule_Used (E_Complex_Parameter) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Expression_Kind (Called_Simple_Name (Call)) = An_Operator_Symbol then
         -- This rule does not apply to operators, otherwise no expression more complicated
         -- than a single operation would be allowed.
         return;
      end if;

      declare
         Parameters : constant Asis.Association_List := Actual_Parameters (Call);
      begin
         for P in Parameters'Range loop
            if Expression_Kind (Actual_Parameter (Parameters (P))) = A_Function_Call then
               Do_Report (E_Complex_Parameter, Get_Location (Actual_Parameter (Parameters (P))));
            end if;
         end loop;
      end;
   end Process_Call;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Expressions;
