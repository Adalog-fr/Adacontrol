----------------------------------------------------------------------
--  Rules.Non_Static - Package body                                 --
--                                                                  --
--  This software is (c) Adalog 2004-2005.                          --
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

-- Asis
with
  Asis.Definitions,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Variables,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);

package body Rules.Non_Static is
   use Framework, Framework.Control_Manager, Framework.Variables.Shared_Types;

   type Subrules is (K_Variable_Initialization, K_Constant_Initialization,
                     K_Index_Constraint,        K_Discriminant_Constraint,
                     K_Instantiation,           K_Index_Check);

   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "K_");

   type Usage_Flags is array (Subrules) of Boolean;

   Rule_Used  : Usage_Flags := (others => False);
   Save_Used  : Usage_Flags;
   Usage      : array (Subrules) of Basic_Rule_Context;

   -- Rule variables
   RM_Static : aliased Switch_Type.Object := (Value => On);


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
      use Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control that indicated contexts use only static expressions");
      User_Message;
      Help_On_Flags (Header => "Parameter(s):", Footer => "(optional, default = all)");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
      use Subrules_Flag_Utilities;

      procedure Add_One (Subrule : Subrules) is
         use Utilities;
      begin
         if Rule_Used (Subrule) then
            if not Basic.Merge_Context (Usage (Subrule), Ctl_Kind, Ctl_Label) then
               Parameter_Error (Rule_Id, "parameter already specified: " & Image (Subrule, Lower_Case));
            end if;
         else
            Rule_Used (Subrule) := True;
            Usage (Subrule)     := Basic.New_Context (Ctl_Kind, Ctl_Label);
         end if;
      end Add_One;

   begin  -- Add_Control
      if Parameter_Exists then
         loop
            Add_One (Get_Flag_Parameter (Allow_Any => False));
            exit when not Parameter_Exists;
         end loop;
      else
         for K in Subrules loop
            Add_One (K);
         end loop;
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
            Rule_Used  := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   ------------------------
   -- Check_Static_Index --
   ------------------------

   procedure Check_Static_Index (Constraint_List : Asis.Element_List) is
      use Framework.Locations, Framework.Reports, Thick_Queries;
   begin
      for Const : Asis.Element of Constraint_List loop
         if Discrete_Constraining_Lengths (Const, RM_Static => RM_Static.Value = On)(1) = Not_Static then
            Report (Rule_Id,
                    Usage (K_Index_Constraint),
                    Get_Location (Const),
                    "array index non-statically constrained");
         end if;
      end loop;
   end Check_Static_Index;


   ------------------------------------------
   -- Process_Constrained_Array_Definition --
   ------------------------------------------

   procedure Process_Constrained_Array_Definition (Elem : Asis.Type_Definition) is
      use Asis.Definitions;
   begin
      if not Rule_Used (K_Index_Constraint) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_Static_Index (Discrete_Subtype_Definitions (Elem));
   end Process_Constrained_Array_Definition;


   ------------------------------
   -- Process_Index_Constraint --
   ------------------------------

   procedure Process_Index_Constraint (Elem : Asis.Discrete_Range) is
      use Asis.Definitions;
   begin
      if not Rule_Used (K_Index_Constraint) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_Static_Index (Discrete_Ranges (Elem));
   end Process_Index_Constraint;


   -------------------------------------
   -- Process_Discriminant_Constraint --
   -------------------------------------

   procedure Process_Discriminant_Constraint (Elem : Asis.Constraint) is
      use Asis.Definitions, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Thick_Queries;
   begin
      if not Rule_Used (K_Discriminant_Constraint) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      for Const : Asis.Discriminant_Association of Discriminant_Associations (Elem) loop
         if not Is_Static_Expression (Discriminant_Expression (Const), RM_Static => RM_Static.Value = On) then
            Report (Rule_Id,
                    Usage (K_Discriminant_Constraint),
                    Get_Location (Const),
                    "discriminant non-statically constrained");
         end if;
      end loop;
   end Process_Discriminant_Constraint;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Elem : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;

      procedure Do_Report (Assoc : Asis.Association) is
         use Framework.Locations, Framework.Reports;
      begin
         Report (Rule_Id,
                 Usage (K_Instantiation),
                 Get_Location (Elem),
                 "Actual for "
                 & Defining_Name_Image (Formal_Parameter (Assoc))
                 & " in instantiation is not static"
                 & Choose (Is_Defaulted_Association (Assoc), " (default value)", ""));
      end Do_Report;

   begin  -- Process_Instantiation
      if not Rule_Used (K_Instantiation) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Param_Decl : Asis.Declaration;
      begin
         for A : Asis.Association of Generic_Actual_Part (Elem, Normalized => True) loop
            Param_Decl := Enclosing_Element (Formal_Parameter (A));
            case Declaration_Kind (Param_Decl) is
               when A_Formal_Object_Declaration =>
                  case Mode_Kind (Param_Decl) is
                     when An_In_Mode | A_Default_In_Mode =>
                        if not Is_Static_Expression (Actual_Parameter (A), RM_Static => RM_Static.Value = On) then
                           Do_Report (A);
                        end if;
                     when An_In_Out_Mode =>
                        if not Is_Static_Object (Actual_Parameter (A)) then
                           Do_Report (A);
                        end if;
                     when others =>
                        Failure ("Bad formal object mode");
                  end case;
               when A_Formal_Type_Declaration
                  | A_Formal_Incomplete_Type_Declaration
                  | A_Formal_Package_Declaration
                  | A_Formal_Package_Declaration_With_Box
                    =>
                  -- These are always static (and cannot be renamed!)
                  null;
               when A_Formal_Procedure_Declaration
                  | A_Formal_Function_Declaration
                 =>
                  case Expression_Kind (Actual_Parameter (A)) is
                     when An_Attribute_Reference =>
                        -- Protects from calling Ultimate_Name (in when others) with an attribute reference
                        null;
                     when A_Function_Call =>
                        -- A4G still confused when the actual is an attribute
                        -- (Actually, it is the case above)
                        A4G_Bugs.Trace_Bug ("Non_Static.Process_Instantiation: "
                                            & "Function_Call instead of Attribute_Reference");
                     when An_Explicit_Dereference =>
                        Do_Report (A);
                     when others =>
                        if Is_Nil (Ultimate_Name (Actual_Parameter (A))) then
                           Do_Report (A);
                        end if;
                  end case;
               when others =>
                  Failure ("Not a formal parameter declaration", Param_Decl);
            end case;
         end loop;
      end;
   end Process_Instantiation;

   --------------------------------
   -- Process_Object_Declaration --
   --------------------------------

   procedure Process_Object_Declaration (Decl : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Thick_Queries, Utilities;

      subtype Structured_Types is Asis.Type_Kinds
         range An_Unconstrained_Array_Definition .. A_Tagged_Record_Type_Definition;

      Expr : Asis.Expression;
      Temp : Asis.Element;
   begin
      if not Rule_Used (K_Variable_Initialization)
        and not Rule_Used (K_Constant_Initialization)
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Expr := Initialization_Expression (Decl);
      if Is_Nil (Expr) then
         return;
      end if;

      if Expression_Kind (Expr) = A_Null_Literal then
         -- Always static
         return;
      end if;

      -- Structured types not (yet) controlled
      Temp := Object_Declaration_View (Decl);
      if Definition_Kind (Temp) /= A_Subtype_Indication then
         -- Anonymous array, task, protected
         return;
      end if;
      Temp := Subtype_Simple_Name (Temp);
      if Expression_Kind (Temp) = An_Attribute_Reference then
         case Attribute_Kind (Temp) is
            when A_Base_Attribute =>
               Temp := Prefix (Temp);
            when A_Class_Attribute =>
               return;
            when others =>
               Failure ("Bad type attribute");
         end case;
      end if;
      if Type_Kind (Type_Declaration_View
                    (Ultimate_Type_Declaration
                     (Corresponding_Name_Declaration
                      (Temp)))) in Structured_Types
      then
         return;
      end if;

      if Rule_Used (K_Variable_Initialization)
        and then Declaration_Kind (Decl) = A_Variable_Declaration
        and then not Is_Static_Expression (Expr, RM_Static => RM_Static.Value = On)
      then
         Report (Rule_Id,
                 Usage (K_Variable_Initialization),
                 Get_Location (Expr),
                 "non-static initialization expression");
      end if;

      if Rule_Used (K_Constant_Initialization)
        and then Declaration_Kind (Decl) = A_Constant_Declaration
        and then not Is_Static_Expression (Expr, RM_Static => RM_Static.Value = On)
      then
         Report (Rule_Id,
                 Usage (K_Constant_Initialization),
                 Get_Location (Expr),
                 "non-static initialization expression");
      end if;
   end Process_Object_Declaration;

   ------------------------------
   -- Process_Index_Expression --
   ------------------------------

   procedure Process_Index_Expression (Expr : Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Thick_Queries;
   begin
      if not Rule_Used (K_Index_Check) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Bounds   : constant Extended_Biggest_Int_List := Discrete_Constraining_Values (Prefix (Expr),
                                                                                        Follow_Access => True);
         -- NB: Follow_Access must be true for the case where the prefix is an implicit dereference
         Inx_List : constant Asis.Expression_List := Index_Expressions (Expr);
         Inx      : Extended_Biggest_Int;
      begin
         -- Note: Bounds'Length = 2 * Inx_List'Length
         for I in Inx_List'Range loop
            if Not_Static in Bounds (2 * I - 1) | Bounds (2 * I) then
               -- non static bounds
               Report (Rule_Id,
                       Usage (K_Index_Check),
                       Get_Location (Expr),
                       "indexing of non statically constrained array");
            else
               -- static bounds
               Inx := Discrete_Static_Expression_Value (Inx_List (I), RM_Static => RM_Static.Value = On);
               if Inx = Not_Static then
                  -- static bounds, non static index
                  if Expression_Kind (Simple_Name (Inx_List (I))) = An_Identifier then
                     declare
                        Inx_Bounds : constant Extended_Biggest_Int_List := Discrete_Constraining_Values (Inx_List (I));
                     begin
                        if Inx_Bounds (1) = Not_Static or Inx_Bounds (2) = Not_Static then
                           Report (Rule_Id,
                                   Usage (K_Index_Check),
                                   Get_Location (Inx_List (I)),
                                   "indexing by non statically checkable expression");
                        elsif Inx_Bounds (1) < Bounds (2 * I - 1) or Inx_Bounds (2) > Bounds (2 * I) then
                           Report (Rule_Id,
                                   Usage (K_Index_Check),
                                   Get_Location (Inx_List (I)),
                                   "index expression may fail index_check");
                        end if;
                     end;
                  else
                     Report (Rule_Id,
                             Usage (K_Index_Check),
                             Get_Location (Inx_List (I)),
                             "indexing by non statically checkable expression");
                  end if;
               else
                  -- static bounds, static index
                  if Inx < Bounds (2 * I - 1) or Inx > Bounds (2 * I) then
                     Report (Rule_Id,
                             Usage (K_Index_Check),
                             Get_Location (Expr),
                             "indexing out of bounds");
                  end if;
               end if;
            end if;
         end loop;
      end;
   end Process_Index_Expression;

begin  -- Rules.Non_Static
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
   Framework.Variables.Register (RM_Static'Access,
                                 Rule_Id & ".RM_STATIC");
end Rules.Non_Static;
