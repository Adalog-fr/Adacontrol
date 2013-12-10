----------------------------------------------------------------------
--  Rules.Positional_Associations - Package body                    --
--                                                                  --
--  This software  is (c) Adalog  2004-2013. The Ada  Controller is --
--  free software;  you can redistribute it and/or  modify it under --
--  terms of  the GNU  General Public License  as published  by the --
--  Free Software Foundation; either version 2, or (at your option) --
--  any later version.   This unit is distributed in  the hope that --
--  it will be  useful, but WITHOUT ANY WARRANTY;  without even the --
--  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
--  PURPOSE.  See the GNU  General Public License for more details. --
--  You  should have  received a  copy  of the  GNU General  Public --
--  License distributed  with this  program; see file  COPYING.  If --
--  not, write to  the Free Software Foundation, 59  Temple Place - --
--  Suite 330, Boston, MA 02111-1307, USA.                          --
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
  Asis.Elements,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Positional_Associations is
   use Framework, Framework.Control_Manager, Utilities;
   use type Thick_Queries.Biggest_Int;

   type Subrules is (Sr_All, Sr_All_Positional, Sr_Same_Type);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Subrules,
                                                                             Prefix => "Sr_" );

   type Association_Names is (Na_Pragma,       Na_Call,            Na_Instantiation,
                              Na_Discriminant, Na_Array_Aggregate, Na_Record_Aggregate);
   -- Na_Pragma must stay first. When adding a new value, take
   -- also care of the following subtype:
   subtype Exceptionable_Association_Names is Association_Names range Na_Pragma .. Na_Instantiation;

   package Association_Flag_Utilities is new Framework.Language.Flag_Utilities
     (Flags  => Association_Names,
      Prefix => "Na_" );

   type Association_Context is new Basic_Rule_Context with
      record
         Allowed_Number  : Asis.ASIS_Natural;
         Except_Operator : Boolean;
      end record;

   Contexts : array (Subrules, Association_Names, Control_Kinds) of Association_Context;

   Positional_Exceptions : array (Subrules, Exceptionable_Association_Names, Control_Kinds) of Context_Store;
   -- A Context_Store of Null_Context to flag entities that need not obey the rule

   type Association_Flags is array (Association_Names) of Control_Kinds_Set;
   type Usage_Flags is array (Subrules) of Association_Flags;
   Nothing_Used : constant Usage_Flags := (others => (others => Empty_Control_Kinds_Set));

   Rule_Used : Usage_Flags := Nothing_Used;
   Save_Used : Usage_Flags;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control use of positional_association:");
      Subrules_Flag_Utilities.Help_On_Flags
        (Header =>  "   Parameter (1):");
      User_Message ("   Parameter (2): <value>");
      Association_Flag_Utilities.Help_On_Flags
        (Header =>  "   Parameter (3): [not_operator]",
         Footer =>  "(default = all)");
      User_Message ("   Parameter (4..): <entities>");
      User_Message ("                    (entities not required to follow the rule)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control(Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Asis;
      use Framework.Language;
      use Subrules_Flag_Utilities, Association_Flag_Utilities;

      Subrule   : Subrules;
      Assoc     : Association_Names;
      Max       : ASIS_Integer;
      Except_Op : Boolean := False;
   begin
      if Parameter_Exists then
         Subrule := Get_Flag_Parameter (Allow_Any => False);
         case Subrule is
            when Sr_All | Sr_All_Positional =>
               Max := Get_Integer_Parameter (Min => 0);
            when Sr_Same_Type =>
               Max := Get_Integer_Parameter (Min => 1);
         end case;

         if Parameter_Exists then
            Except_Op := Get_Modifier ("NOT_OPERATOR");
            Assoc     := Get_Flag_Parameter (Allow_Any => False);

            if Rule_Used (Subrule) (Assoc) (Ctl_Kind) then
               Parameter_Error (Rule_Id, "This combination already given");
            end if;

            if Except_Op and Assoc /= Na_Call then
               Parameter_Error (Rule_Id, "Not_Operator can be specified only with ""call""");
            end if;

            if Assoc = Na_Pragma and Subrule = Sr_Same_Type then
               Parameter_Error (Rule_Id, "Same_Type not allowed for pragmas");
            end if;

            Contexts (Subrule, Assoc, Ctl_Kind) := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Max, Except_Op);

            if Parameter_Exists and Assoc not in Exceptionable_Association_Names then
               Parameter_Error (Rule_Id, "No exceptions allowed for " & Image (Assoc));
            end if;
            while Parameter_Exists loop
               Associate (Positional_Exceptions (Subrule, Assoc, Ctl_Kind), Get_Entity_Parameter, Null_Context);
            end loop;

            Rule_Used (Subrule) (Assoc) (Ctl_Kind):= True;

         else
            for A in Association_Names loop
               if Rule_Used (Subrule) (A) (Ctl_Kind) then
                  Parameter_Error (Rule_Id, "This combination already given");
               end if;
               Rule_Used (Subrule) (A) (Ctl_Kind) := True;
               Contexts  (Subrule,  A,  Ctl_Kind) := (Basic.New_Context (Ctl_Kind, Ctl_Label)
                                                      with Allowed_Number => Max, Except_Operator => False);
            end loop;
         end if;

      else
         -- No parameter => positional_associations (all, 0)
         for A in Association_Names loop
            if Rule_Used (Sr_All) (A) (Ctl_Kind) then
               Parameter_Error (Rule_Id, "This combination already given");
            end if;
            Rule_Used (Sr_All) (A) (Ctl_Kind) := True;
            Contexts  (Sr_All,  A,  Ctl_Kind) := (Basic.New_Context (Ctl_Kind, Ctl_Label)
                                                  with Allowed_Number => 0, Except_Operator => False);
         end loop;
      end if;

   exception
      when Already_In_Store =>
         Parameter_Error (Rule_Id, "Entity already provided: " & Image (Subrule, Lower_Case));
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            for R in Subrules loop
               for A in Association_Names loop
                  for C in Control_Kinds loop
                     Clear (Contexts (R, A, C));
                  end loop;
               end loop;
            end loop;
            for R in Subrules loop
               for A in Exceptionable_Association_Names loop
                  for C in Control_Kinds loop
                     Clear (Positional_Exceptions (R, A, C));
                  end loop;
               end loop;
            end loop;
            Rule_Used := Nothing_Used;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Nothing_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   --------------------------
   -- Process__Association --
   --------------------------

   procedure Process_Association (Association : in Asis.Association) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Expressions, Asis.Elements, Asis.Statements;
      use Thick_Queries;

      procedure Check_Association (Na               : Association_Names;
                                   Ident            : Asis.Element;
                                   Association_Expr : Asis.Expression;
                                   All_Associations : Association_List;
                                   Is_Operator      : Boolean := False)
      is

         function Positional_Count (Expr : Asis.Expression; Subrule : Subrules) return Asis.ASIS_Natural is
            Expr_Type : constant Asis.Definition := Thick_Queries.Corresponding_Expression_Type_Definition (Expr);
            Count     : Asis.ASIS_Natural := 0;

            function Is_Same_Type (Left, Right : Asis.Definition) return Boolean is
            -- Checks is Left and Right are the same, or if one of them is universal and
            -- appropriate for the other one
            begin
               -- Easy case first:
               if Is_Equal (Left, Right) then
                  return True;
               end if;

               case Root_Type_Kind (Left) is
                  when Not_A_Root_Type_Definition =>
                     null; -- (let's go to the test the other way round)
                  when A_Root_Integer_Definition | A_Root_Real_Definition =>
                     Failure ("Is_Same_type: Root_Definition");   -- not sure how we could get these.
                  when A_Universal_Integer_Definition =>
                     return Type_Category (Right) in Integer_Types;
                  when A_Universal_Real_Definition =>
                     return Type_Category (Right) = A_Floating_Point_Type;
                  when A_Universal_Fixed_Definition =>
                     return Type_Category (Right) = A_Fixed_Point_Type;
               end case;

               case Root_Type_Kind (Right) is
                  when Not_A_Root_Type_Definition =>
                     return False;
                  when A_Root_Integer_Definition | A_Root_Real_Definition =>
                     Failure ("Is_Same_type: Root_Definition");   -- not sure how we could get these.
                  when A_Universal_Integer_Definition =>
                     return Type_Category (Left) in Integer_Types;
                  when A_Universal_Real_Definition =>
                     return Type_Category (Left) = A_Floating_Point_Type;
                  when A_Universal_Fixed_Definition =>
                     return Type_Category (Left) = A_Fixed_Point_Type;
               end case;
            end Is_Same_Type;

         begin   -- Positional_Count
            if Is_Nil (Expr_Type) then
               return 0;
            end if;

            if Subrule = Sr_All then
               Count := All_Associations'Length;
            else
               -- Of course, Expr is part of the association list, therefore it will match itself
               -- and be counted for 1, as it should be.
               for A in All_Associations'Range loop
                  -- No more positional associations once we find a named one:
                  exit when Association_Choices (All_Associations (A)) /= Nil_Element_List;
                  case Subrule is
                     when Sr_All =>
                        Failure ("Positional_Count: Sr_All");
                     when Sr_All_Positional =>
                        Count := Count + 1;
                     when Sr_Same_Type =>
                        if Association_Kind (Association) = A_Pragma_Argument_Association then
                           -- no need to go further, Same_Type not applicable to pragmas
                           return 0;
                        elsif Is_Same_Type (Expr_Type,
                                            Thick_Queries.Corresponding_Expression_Type_Definition
                                              (Association_Value (All_Associations (A))))
                        then
                           Count := Count + 1;
                        elsif Expression_Kind (Strip_Parentheses (Expr)) = A_Character_Literal
                          and then Is_Character_Subtype (Thick_Queries.Corresponding_Expression_Type_Definition
                                                         (Association_Value (All_Associations (A))))
                        then
                           Count := Count + 1;
                        end if;
                  end case;
               end loop;
            end if;
            return Count;
         end Positional_Count;

         procedure Check_Report (Subrule : Subrules; Kind : Control_Kinds; Reported : out Boolean) is
            use Association_Flag_Utilities, Framework.Reports;
         begin
            Reported := False;

            if not Rule_Used (Subrule) (Na) (Kind) then
               return;
            end if;

            if Na in Exceptionable_Association_Names
               and then Matching_Context (Positional_Exceptions (Subrule, Na, Kind), Ident, Extend_To => All_Extensions)
                       /= No_Matching_Context
            then
               return;
            end if;

            declare
               Ctx : constant Association_Context := Contexts (Subrule, Na, Kind);
            begin
               if not (Is_Operator and Ctx.Except_Operator)
                 and then Positional_Count (Association_Expr, Subrule) > Ctx.Allowed_Number
               then
                  case Subrule is
                     when Sr_All =>
                        Report (Rule_Id,
                                Ctx,
                                Get_Location (Association),
                                "positional association used in " & Image (Na, Lower_Case)
                                & Choose (Ctx.Allowed_Number = 0,
                                  "",
                                  " with more than " & ASIS_Integer_Img (Ctx.Allowed_Number)
                                  & " association(s)"));
                     when Sr_All_Positional =>
                        Report (Rule_Id,
                                Ctx,
                                Get_Location (Association),
                                "positional association used in " & Image (Na, Lower_Case)
                                & Choose (Ctx.Allowed_Number = 0,
                                  "",
                                  " with more than " & ASIS_Integer_Img (Ctx.Allowed_Number)
                                  & " positional association(s)"));
                     when Sr_Same_Type =>
                        Report (Rule_Id,
                                Ctx,
                                Get_Location (Association),
                                "positional association used in " & Image (Na, Lower_Case)
                                & " with more than " & ASIS_Integer_Img (Ctx.Allowed_Number)
                                & " element(s) of the same type");
                  end case;
                  Reported := True;
               end if;
            end;
         end Check_Report;

         Reported : Boolean;
      begin   -- Check_Association
         for Sr in Subrules loop
            Check_Report (Sr, Check, Reported);
            if not Reported then
               Check_Report (Sr, Search, Reported);
            end if;
            Check_Report (Sr, Count, Reported);
         end loop;
      end Check_Association;

      Encl   : Asis.Element;
      Called : Asis.Element;
   begin -- Process_Association
      if Rule_Used = Nothing_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if not Is_Nil (Association_Choices (Association)) then
         -- Named association
         return;
      end if;

      Encl := Enclosing_Element (Association);
      case Association_Kind (Association) is
         when Not_An_Association =>
            Failure ("Not an association", Association);
         when A_Discriminant_Association =>
            Check_Association (Na_Discriminant,
                               Nil_Element,
                               Discriminant_Expression (Association),
                               Discriminant_Associations (Encl));
         when A_Record_Component_Association =>
            Check_Association (Na_Record_Aggregate,
                               Nil_Element,
                               Component_Expression (Association),
                               Record_Component_Associations (Encl));
         when An_Array_Component_Association =>
            Check_Association (Na_Array_Aggregate,
                               Nil_Element,
                               Component_Expression (Association),
                               Array_Component_Associations (Encl));
         when A_Pragma_Argument_Association =>
            Check_Association (Na_Pragma,
                               Encl,
                               Actual_Parameter (Association),
                               Pragma_Argument_Associations (Encl));
         when A_Parameter_Association =>
            -- Do not check infix (operators) function calls or attribute functions and procedures
            if Expression_Kind (Encl) = A_Function_Call then
               if Is_Prefix_Call (Encl) and then Expression_Kind (Prefix (Encl)) /= An_Attribute_Reference then
                  Check_Association (Na_Call,
                                     Called_Simple_Name (Encl),
                                     Actual_Parameter (Association),
                                     Function_Call_Parameters (Encl),
                                     Operator_Kind (Simple_Name (Prefix (Encl))) /= Not_An_Operator);
               end if;
            elsif Statement_Kind (Encl) = A_Procedure_Call_Statement then
               Called := Called_Simple_Name (Encl);
               if Expression_Kind (Called) /= An_Attribute_Reference then
                  Check_Association (Na_Call,
                                     Called,
                                     Actual_Parameter (Association),
                                     Call_Statement_Parameters (Encl));
               end if;
            else
               -- Entries, cannot be attributes...
               Called := Called_Simple_Name (Encl);
               if Expression_Kind (Called) = An_Indexed_Component then
                  -- Member of a family
                  Called := Prefix (Called);
               end if;
               Check_Association (Na_Call,
                                  Called,
                                  Actual_Parameter (Association),
                                  Call_Statement_Parameters (Encl));
            end if;
         when A_Generic_Association =>
               Check_Association (Na_Instantiation,
                                  Generic_Unit_Name (Encl),
                                  Actual_Parameter (Association),
                                  Generic_Actual_Part (Encl));
      end case;
   end Process_Association;

begin  -- Rules.Positional_Associations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic_Textual,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Positional_Associations;
