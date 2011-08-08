----------------------------------------------------------------------
--  Rules.Non_Static - Package body                                 --
--                                                                  --
--  This software  is (c) Adalog  2004-2005. The Ada  Controller is --
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

-- Adactl
with
  Framework,
  Framework.Reports,
  Framework.Rules_Manager,
  Framework.Language;


pragma Elaborate (Framework.Language);

package body Rules.Non_Static is
   use Framework;

   type Available_Keyword is (K_Index_Constraint, K_Discriminant_Constraint, K_Instantiation);

   package Keyword_Flag_Utilities is new Framework.Language.Flag_Utilities (Available_Keyword, "K_");

   type Usage_Flags is array (Available_Keyword) of Boolean;

   Rule_Used  : Usage_Flags := (others => False);
   Save_Used  : Usage_Flags;
   Usage      : array (Available_Keyword) of Basic_Rule_Context;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
      use Keyword_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags (Header => "Parameter(s):", Footer => "(optional, default = all)");
      User_Message ("Control that index and discriminant constraints use only static expressions");
   end Help;


   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Framework.Language;
      use Keyword_Flag_Utilities;

      procedure Add_One (Key : Available_Keyword) is
      begin
         if Rule_Used (Key) then
            Parameter_Error (Rule_Id & ": parameter already specified: " & Image (Key));
         end if;

         Rule_Used (Key) := True;
         Usage (Key)     := Basic.New_Context (Rule_Use_Type, Label);
      end Add_One;

   begin
      if Parameter_Exists then
         loop
            Add_One (Get_Flag_Parameter (Allow_Any => False));
            exit when not Parameter_Exists;
         end loop;
      else
         for K in Available_Keyword loop
            Add_One (K);
         end loop;
      end if;
   end Add_Use;


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
      use Framework.Reports, Thick_Queries;
   begin
      for I in Constraint_List'Range loop
         if Discrete_Constraining_Lengths (Constraint_List (I))(1) = Not_Static then
            Report (Rule_Id,
                    Usage (K_Index_Constraint),
                    Get_Location (Constraint_List (I)),
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


   ----------------------------
   -- Process_Discrete_Range --
   ----------------------------

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
      use Framework.Reports, Thick_Queries;
   begin
      if not Rule_Used (K_Discriminant_Constraint) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Constraint_List : constant Asis.Discriminant_Association_List := Discriminant_Associations (Elem);
      begin
         for I in Constraint_List'Range loop
            if Static_Expression_Value_Image (Discriminant_Expression (Constraint_List (I))) = "" then
               Report (Rule_Id,
                       Usage (K_Discriminant_Constraint),
                       Get_Location (Constraint_List (I)),
                       "discriminant non-statically constrained");
            end if;
         end loop;
      end;
   end Process_Discriminant_Constraint;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Elem : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;

      procedure Do_Report (Assoc : Asis.Association) is
         use Framework.Reports;
      begin
         Report (Rule_Id,
                 Usage (K_Instantiation),
                 Get_Location (Elem),
                 "Actual for "
                 & Defining_Name_Image (Formal_Parameter (Assoc))
                 & " in instantiation is not static"
                 & Choose (Is_Defaulted_Association (Assoc), " (default value)", ""));
      end Do_Report;

      function Is_Static_Object (Obj : Asis.Expression) return Boolean is
      begin
         case Expression_Kind (Obj) is
            when A_Selected_Component =>
               return Is_Static_Object (Selector (Obj)) and then Is_Static_Object (Prefix (Obj));
            when An_Indexed_Component =>
               if Is_Static_Object (Prefix (Obj)) then
                  declare
                     Indexes : constant Asis.Expression_List := Index_Expressions (Obj);
                  begin
                     for I in Indexes'Range loop
                        if Static_Expression_Value_Image (Indexes (I)) = "" then
                           return False;
                        end if;
                     end loop;
                     return True;
                  end;
               else
                  return False;
               end if;
            when An_Explicit_Dereference =>
               return False;
            when others =>
               return not Is_Nil (Ultimate_Name (Obj));
         end case;
      end Is_Static_Object;

   begin
      if not Rule_Used (K_Instantiation) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Actuals    : constant Asis.Association_List := Generic_Actual_Part (Elem, Normalized => True);
         Param_Decl : Asis.Declaration;
      begin
         for A in Actuals'Range loop
            Param_Decl := Enclosing_Element (Formal_Parameter (Actuals (A)));
            case Declaration_Kind (Param_Decl) is
               when A_Formal_Object_Declaration =>
                  case Mode_Kind (Param_Decl) is
                     when An_In_Mode | A_Default_In_Mode =>
                        if Static_Expression_Value_Image (Actual_Parameter (Actuals (A))) = "" then
                           Do_Report (Actuals (A));
                        end if;
                     when An_In_Out_Mode =>
                        if Is_Static_Object (Actual_Parameter (Actuals (A))) then
                           Do_Report (Actuals (A));
                        end if;
                     when others =>
                        Failure ("Bad formal object mode");
                  end case;
               when A_Formal_Type_Declaration
                  | A_Formal_Package_Declaration
                  | A_Formal_Package_Declaration_With_Box
                    =>
                  -- These are always static (and cannot be renamed!)
                  null;
               when A_Formal_Procedure_Declaration
                  | A_Formal_Function_Declaration
                 =>
                  case Expression_Kind (Actual_Parameter (Actuals (A))) is
                     when An_Attribute_Reference =>
                        -- Protects from calling Ultimate_Name (in when others) with an attribute reference
                        null;
                     when A_Function_Call =>
                        -- A4G still confused when the actual is an attribute
                        -- (Actually, it is the case above)
                        A4G_Bugs.Trace_Bug ("Non_Static: Function_Call in place of Attribute_Reference");
                     when An_Explicit_Dereference =>
                        Do_Report (Actuals (A));
                     when others =>
                        if Is_Nil (Ultimate_Name (Actual_Parameter (Actuals (A)))) then
                           Do_Report (Actuals (A));
                        end if;
                  end case;
               when others =>
                  Failure ("Not a formal parameter declaration");
            end case;
         end loop;
      end;
   end Process_Instantiation;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.Non_Static;
