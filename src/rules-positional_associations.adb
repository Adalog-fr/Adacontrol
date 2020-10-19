----------------------------------------------------------------------
--  Rules.Positional_Associations - Package body                    --
--                                                                  --
--  This software is (c) Adalog 2004-2013.                          --
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
  Framework.Reports.Fixes,
  Framework.Language,
  Framework.Variables,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);

-----------------------------------
-- Rules.Positional_Associations --
-----------------------------------

package body Rules.Positional_Associations is
   use Framework, Framework.Control_Manager, Framework.Variables, Framework.Variables.Shared_Types, Utilities;

   type Subrules is (Sr_All, Sr_All_Positional, Sr_Same_Type, Sr_Declared);
   -- Sr_Declared must stay last
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Subrules,
                                                                             Prefix => "Sr_" );

   type Association_Names is (Na_Call,                       Na_Instantiation, Na_Pragma,
                              Na_Enumeration_Representation, Na_Discriminant,  Na_Array_Aggregate,
                              Na_Record_Aggregate);
   -- When adding a new value, consider the right place given the following subtypes:
   subtype Exceptionable_Association_Names is Association_Names range Na_Call .. Na_Enumeration_Representation;
   subtype Declared_Association_Names      is Association_Names range Na_Call   .. Na_Instantiation;

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

   -- Rule variables
   Count_Prefix_Operand : aliased Switch_Type.Object := (Value => On);

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control use of positional_association:");
      Subrules_Flag_Utilities.Help_On_Flags (Header =>  "Parameter (1):");
      User_Message ("Parameter (2): <value>");
      Association_Flag_Utilities.Help_On_Flags
        (Header =>  "Parameter (3): [not_operator]",
         Footer =>  "(default = all)");
      User_Message ("Parameter (4..): <entities>");
      User_Message ("                (entities not required to follow the rule)");
      User_Message;
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Count_Prefix_Operand");
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
            when Sr_All | Sr_All_Positional | Sr_Declared =>
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

            if Subrule = Sr_Declared and Assoc not in Declared_Association_Names then
               Parameter_Error (Rule_Id, "Declared not allowed for " & Image (Assoc));
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
            if Subrule = Sr_Declared then
               for A in Declared_Association_Names loop
                  if Rule_Used (Subrule) (A) (Ctl_Kind) then
                     Parameter_Error (Rule_Id, "This combination already given");
                  end if;
                  Rule_Used (Subrule) (A) (Ctl_Kind) := True;
                  Contexts  (Subrule,  A,  Ctl_Kind) := (Basic.New_Context (Ctl_Kind, Ctl_Label)
                                                         with Allowed_Number => Max, Except_Operator => False);
               end loop;
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
            for Cont : Association_Context of Contexts loop
               Clear (Cont);
            end loop;
            for Store : Context_Store of Positional_Exceptions loop
               Clear (Store);
            end loop;
            Rule_Used            := Nothing_Used;
            Count_Prefix_Operand := (Value => On);
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
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries;

      procedure Check_Association (Na                  : Association_Names;
                                   Ident               : Asis.Element;
                                   Association_Expr    : Asis.Expression;
                                   Actual_Associations : Association_List;   -- Associations as written
                                   All_Associations    : Association_List;   -- Normalized association list
                                   Is_Operator         : Boolean := False;
                                   Is_Prefixed         : Boolean := False)
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

            case Subrule is
               when Sr_All =>
                  Count := Actual_Associations'Length;
               when Sr_Declared =>
                  Count := All_Associations'Length;
               when others =>
                  -- Of course, Expr is part of the association list, therefore it will match itself
                  -- and be counted for 1, as it should be.
                  for Ass_List : Asis.Association of Actual_Associations loop
                     -- No more positional associations once we find a named one:
                     exit when Association_Choices (Ass_List) /= Nil_Element_List;
                     case Subrule is
                        when Sr_All | Sr_Declared =>
                           Failure ("Positional_Count: " & Subrules'Wide_Image (Subrule));
                        when Sr_All_Positional =>
                           Count := Count + 1;
                        when Sr_Same_Type =>
                           if Association_Kind (Association) = A_Pragma_Argument_Association then
                              -- no need to go further, Same_Type not applicable to pragmas
                              return 0;
                           elsif Is_Same_Type (Expr_Type,
                                               Thick_Queries.Corresponding_Expression_Type_Definition
                                                 (Association_Value (Ass_List)))
                           then
                              Count := Count + 1;
                           elsif Expression_Kind (Strip_Parentheses (Expr)) = A_Character_Literal
                             and then Is_Character_Subtype (Thick_Queries.Corresponding_Expression_Type_Definition
                                                            (Association_Value (Ass_List)))
                           then
                              Count := Count + 1;
                           end if;
                     end case;
                  end loop;
            end case;

            if Count_Prefix_Operand.Value = Off and then Is_Prefixed then
               Count := Count - 1;
            end if;

            return Count;
         end Positional_Count;

         procedure Check_Report (Subrule : Subrules; Kind : Control_Kinds; Reported : out Boolean) is
            use Association_Flag_Utilities, Framework.Locations, Framework.Reports;
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
               Ctx  : constant Association_Context := Contexts (Subrule, Na, Kind);
               Name : Asis.Defining_Name;
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
                     when Sr_Declared =>
                        Report (Rule_Id,
                                Ctx,
                                Get_Location (Association),
                                "positional association used in " & Image (Na, Lower_Case)
                                & Choose (Ctx.Allowed_Number = 0,
                                  "",
                                  " with more than " & ASIS_Integer_Img (Ctx.Allowed_Number)
                                  & " declared component(s)/parameters(s)"));
                  end case;
                  if Kind /= Count and Na in Na_Call | Na_Instantiation then
                     Name := Formal_Name (Association);
                     if not Is_Nil (Name) then
                        -- Name is nil for predefined operators used in prefixed calls
                        Fixes.Insert (Text => Defining_Name_Image (Name) & " => ",
                                      From => Get_Location (Association));
                     end if;
                  end if;
                  Reported := True;
               end if;
            end;
         end Check_Report;

         Reported : Boolean;
      begin   -- Check_Association
         if Is_Prefixed and then Is_Equal (Enclosing_Element (Association_Expr), Actual_Associations (1)) then
            -- We have prefixed notation, and this is the first parameter (i.e. the one used as prefix)
            -- => ignore
            return;
         end if;

         for Sr in Subrules range Subrules'First .. Subrules'Pred (Sr_Declared) loop
            Check_Report (Sr, Check, Reported);
            if not Reported then
               Check_Report (Sr, Search, Reported);
            end if;
            Check_Report (Sr, Count, Reported);
         end loop;

         if Na in Declared_Association_Names then
            Check_Report (Sr_Declared, Check, Reported);
            if not Reported then
               Check_Report (Sr_Declared, Search, Reported);
            end if;
            Check_Report (Sr_Declared, Count, Reported);
         end if;
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

      Encl := Strip_Parentheses (Enclosing_Element (Association));
      case Association_Kind (Association) is
         when Not_An_Association =>
            Failure ("Not an association", Association);
         when A_Discriminant_Association =>
            Check_Association (Na_Discriminant,
                               Nil_Element,
                               Discriminant_Expression (Association),
                               Discriminant_Associations (Encl),
                               Nil_Element_List);
         when A_Record_Component_Association =>
            Check_Association (Na_Record_Aggregate,
                               Nil_Element,
                               Component_Expression (Association),
                               Record_Component_Associations (Encl, Normalized => False),
                               Nil_Element_List);
         when An_Array_Component_Association =>
            if Representation_Clause_Kind (Enclosing_Element (Encl)) = An_Enumeration_Representation_Clause then
               Check_Association (Na_Enumeration_Representation,
                                  Nil_Element,
                                  Component_Expression (Association),
                                  Array_Component_Associations (Encl),
                                  Nil_Element_List);
            else
               Check_Association (Na_Array_Aggregate,
                                  Nil_Element,
                                  Component_Expression (Association),
                                  Array_Component_Associations (Encl),
                                  Nil_Element_List);
            end if;
         when A_Pragma_Argument_Association =>
            Check_Association (Na_Pragma,
                               Encl,
                               Actual_Parameter (Association),
                               Pragma_Argument_Associations (Encl),
                               Nil_Element_List);
         when A_Parameter_Association =>
            -- Do not check infix (operators) function calls or attribute functions and procedures
            if Expression_Kind (Encl) = A_Function_Call then
               if Is_Prefix_Call (Encl) and then Expression_Kind (Prefix (Encl)) /= An_Attribute_Reference then
                  Check_Association (Na_Call,
                                     Called_Simple_Name (Encl),
                                     Actual_Parameter (Association),
                                     Function_Call_Parameters (Encl, Normalized => False),
                                     Function_Call_Parameters (Encl, Normalized => True),
                                     Is_Operator => Operator_Kind (Simple_Name (Prefix (Encl))) /= Not_An_Operator,
                                     Is_Prefixed => Is_Prefix_Notation (Encl));
               end if;
            elsif Statement_Kind (Encl) = A_Procedure_Call_Statement then
               Called := Called_Simple_Name (Encl);
               if Expression_Kind (Called) /= An_Attribute_Reference then
                  Check_Association (Na_Call,
                                     Called,
                                     Actual_Parameter (Association),
                                     Call_Statement_Parameters (Encl, Normalized => False),
                                     Call_Statement_Parameters (Encl, Normalized => True),
                                     Is_Prefixed => Is_Prefix_Notation (Encl));
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
                                  Call_Statement_Parameters (Encl, Normalized => False),
                                  Call_Statement_Parameters (Encl, Normalized => True));
            end if;
         when A_Generic_Association =>
               Check_Association (Na_Instantiation,
                                  Generic_Unit_Name (Encl),
                                  Actual_Parameter (Association),
                                  Generic_Actual_Part (Encl, Normalized => False),
                                  Generic_Actual_Part (Encl, Normalized => True));
      end case;
   end Process_Association;

begin  -- Rules.Positional_Associations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic_Textual,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
   Framework.Variables.Register (Count_Prefix_Operand'Access,
                                 Variable_Name => Rule_Id & ".COUNT_PREFIX_OPERAND");
end Rules.Positional_Associations;
