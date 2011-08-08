----------------------------------------------------------------------
--  Rules.Return_Type - Package body                                --
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
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Return_Type is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --   First, in case the function declaration is a body or a body stub, we
   --   check if the specification is explicitely given.
   --   If yes, the specification will be checked, so we return.
   --   If not, we proceed as with other declarations.
   --
   --   We then check the kind of the result profile.
   --   When the result profile is an attribute reference, we can return as
   --   it never represents an array, and can never be unconstrained or
   --   non-statically constrained.
   --   When matching a selected component, we retrieve the final identifier
   --   and proceed as when matching a simple identifier.
   --
   --   We can then unwind the type of the result profile to check if it is
   --   of a type that can be problematic for some systems. Checks depend on
   --   the types the user specified when calling the rule.
   --   Types that can be checked are class-wide, discriminated, indexed,
   --   protected and task types.
   --

   type Subrules is (K_Class_Wide,        K_Protected,           K_Task,
                     K_Constrained_Array, K_Unconstrained_Array, K_Unconstrained_Discriminated);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "K_");

   type Usage_Flags is array (Subrules) of Boolean;

   Rule_Used  : Usage_Flags := (others => False);
   Save_Used  : Usage_Flags;
   Usage      : array (Subrules) of Basic_Rule_Context;

   -- A global to store location of instantiation
   Instantiation_Location : Location := Null_Location;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Subrules_Flag_Utilities.Help_On_Flags ("Parameter(s): ");
      User_Message ("Control various forms of the type returned by functions");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
      use Subrules_Flag_Utilities;

      procedure Add_One (Key : in Subrules) is
      begin
         if Rule_Used (Key) then
            Parameter_Error (Rule_Id, "rule can be specified only once for each parameter.");
         end if;

         Rule_Used (Key) := True;
         Usage (Key)     := Basic.New_Context (Ctl_Kind, Ctl_Label);
      end Add_One;

   begin  -- Add_Control
      -- each existing parameter must be added for rule checking
      if Parameter_Exists then
         loop
            Add_One (Get_Flag_Parameter (Allow_Any => False));
            exit when not Parameter_Exists;
         end loop;
      else -- no parameter means all return kinds are checked
         for K in Subrules loop
            Add_One (K);
         end loop;
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
            Rule_Used := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   ---------------------------
   -- Process_Instantiation --
   ---------------------------
   procedure Process_Instantiation (Decl : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;

      --------------
      -- Traverse --
      --------------
      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Asis.Traverse_Control;
                               State   : in out Null_State);

      procedure Traverse is new Asis.Iterator.Traverse_Element (Null_State, Pre_Procedure, Null_State_Procedure);
      --
      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Asis.Traverse_Control;
                               State   : in out Null_State)
      is
      begin
         case Element_Kind (Element) is
            when A_Declaration =>
               case Declaration_Kind (Element) is
                  --
                  when A_Function_Body_Declaration =>
                     Rules.Return_Type. Process_Function_Declaration (Element);
                  --
                  when A_Function_Declaration
                    | A_Function_Renaming_Declaration
                    | A_Function_Body_Stub
                    | A_Formal_Function_Declaration
                    | A_Generic_Function_Declaration
                    =>
                     Rules.Return_Type. Process_Function_Declaration (Element);
                     Control := Abandon_Children;
                  --
                  when A_Generic_Instantiation =>
                     declare
                        Instantiated_Body : constant Asis.Declaration := Corresponding_Body (Element);
                     begin
                        Traverse (Corresponding_Declaration (Element), Control, State);

                        if not Is_Nil (Instantiated_Body) then
                           Traverse (Instantiated_Body, Control, State);
                        end if;
                     end;
                  --
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
      end Pre_Procedure;

      The_State   : Null_State;
      The_Control : Asis.Traverse_Control := Continue;
   begin   -- Process_Instantiation
      if Rule_Used = Usage_Flags'(others => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Instantiation_Location := Get_Location (Decl);
      declare
         Instantiated_Body : constant Asis.Declaration := Corresponding_Body (Decl);
      begin
         Traverse (Corresponding_Declaration (Decl), The_Control, The_State);

         if not Is_Nil (Instantiated_Body) then
            Traverse (Instantiated_Body, The_Control, The_State);
         end if;
      end;
      Instantiation_Location := Null_Location;
   end Process_Instantiation;


   ----------------------------------
   -- Process_Function_Declaration --
   ----------------------------------

   procedure Process_Function_Declaration (Decl : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;
      Result_Expression       : Asis.Expression;
      Result_Type_Declaration : Asis.Declaration;

      procedure Do_Report (Usage_Kind : in Subrules; Error_Message : in Wide_String) is
         use Framework.Reports;
      begin
         if Rule_Used (Usage_Kind) then
            if Instantiation_Location = Null_Location then
               Report (Rule_Id,
                       Usage (Usage_Kind),
                       Get_Location (Result_Expression),
                       Error_Message);
            else
               Report (Rule_Id,
                       Usage (Usage_Kind),
                       Instantiation_Location,
                       Error_Message & " from instantiation");
            end if;
         end if;
      end Do_Report;

      procedure Check_Discriminants is
         Result_Type_Discriminant_Part : constant Asis.Definition := Discriminant_Part (Result_Type_Declaration);
      begin
         if Is_Nil (Result_Type_Discriminant_Part) then
            return;
         end if;
         case Definition_Kind (Result_Type_Discriminant_Part) is
            when An_Unknown_Discriminant_Part =>
               -- Here, private types must have been eliminated
               -- We must have a formal type declaration
               null;
            when A_Known_Discriminant_Part =>
               Do_Report (K_Unconstrained_Discriminated, "function returns unconstrained discriminated type");
            when others =>
               Failure ("unexpected definition: not a discriminant");
         end case;
      end Check_Discriminants;

      procedure Check_Tasks_Protected is
      begin
         if Contains_Type_Declaration_Kind (Result_Type_Declaration, A_Task_Type_Declaration) then
            Do_Report (K_Task, "function returns task type");
         end if;
         if Contains_Type_Declaration_Kind (Result_Type_Declaration, A_Protected_Type_Declaration) then
            Do_Report (K_Protected, "function returns protected type");
         end if;
      end Check_Tasks_Protected;

      procedure Check_Class (Good_Expr : Asis.Expression) is
         Good_Res : Asis.Expression;
      begin
         Do_Report (K_Class_Wide, "function returns class-wide type");
         Good_Res := Simple_Name (Prefix (Good_Expr));

         Result_Type_Declaration := A4G_Bugs.Corresponding_Name_Declaration (Good_Res);
         Check_Tasks_Protected;
         Check_Discriminants;
      end Check_Class;

   begin   -- Process_Function_Declaration
      if Rule_Used = Usage_Flags'(others => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Decl) is
         when A_Function_Body_Declaration
           | A_Function_Body_Stub
           =>
            -- return when the specification has already been declared
            -- as the specification exists, there is no need to check the body (stub)
            if not Is_Nil (Corresponding_Declaration (Decl)) then
               return;
            end if;
         when others =>
            null;
      end case;

      -- Retrieve the returned type from the function declaration
      Result_Expression := Simple_Name (Result_Profile (Decl));
      case Expression_Kind (Result_Expression) is
         when An_Identifier =>
            null;
         when An_Attribute_Reference =>
            case A4G_Bugs.Attribute_Kind (Result_Expression) is
               when A_Base_Attribute =>
                  -- when matching A_Base_Attribute, we need to retrieve the Selector
                  Result_Expression := Simple_Name (Prefix (Result_Expression));
               when A_Class_Attribute =>
                  Check_Class (Result_Expression);
                  return;
               when others =>
                  Failure ("unexpected return type: attribute", Result_Expression);
            end case;
         when others =>
            Failure ("unexpected return type: others", Result_Expression);
      end case;

      -- Here we have a good ol' identifier
      Result_Type_Declaration := A4G_Bugs.Corresponding_Name_Declaration (Result_Expression);

      loop
         -- Here we have a type declaration
         case Declaration_Kind (Result_Type_Declaration) is
            when A_Subtype_Declaration =>
               declare
                  -- Retrieve the type definition for the expression
                  Result_Type_Definition : constant Asis.Definition := Type_Declaration_View (Result_Type_Declaration);
                  Constraint             : constant Asis.Constraint := Subtype_Constraint (Result_Type_Definition);
               begin
                  if not Is_Nil (Constraint) then
                     -- OK, return type is constrained
                     -- Can be an array subtype
                     if Constraint_Kind (Constraint) = An_Index_Constraint then
                        Do_Report (K_Constrained_Array, "function returns constrained array type");
                     end if;

                     -- But it can also be a task or protected type (with discriminants)...
                     Check_Tasks_Protected;

                     -- No discriminants to check...
                     return;
                  end if;

                  declare
                     -- needed to keep the right location of Result_Expression when reporting
                     Result_Type_Expression : constant Asis.Expression := Subtype_Simple_Name (Result_Type_Definition);
                  begin
                     if Expression_Kind (Result_Type_Expression) = An_Attribute_Reference then
                        case A4G_Bugs.Attribute_Kind (Result_Type_Expression) is
                           when A_Base_Attribute =>
                              -- when matching A_Base_Attribute, we need to retrieve the Selector
                              Result_Type_Declaration := A4G_Bugs.Corresponding_Name_Declaration (Prefix
                                                                                         (Result_Type_Expression));
                           when A_Class_Attribute =>
                              Check_Class (Result_Type_Expression);
                              return;
                           when others =>
                              Failure ("unexpected subtype : attribute");
                        end case;
                     else
                        Result_Type_Declaration := A4G_Bugs.Corresponding_Last_Subtype (Result_Type_Declaration);
                     end if;
                  end;
               end;


            when A_Task_Type_Declaration =>
               Do_Report (K_Task, "function returns task type");
               Check_Discriminants;
               return;

            when A_Protected_Type_Declaration =>
               Do_Report (K_Protected, "function returns protected type");
               Check_Discriminants;
               return;

            when An_Incomplete_Type_Declaration
              | A_Private_Type_Declaration
              | A_Private_Extension_Declaration
              =>
               -- Retrieve full type declaration
               Result_Type_Declaration := Corresponding_Type_Declaration (Result_Type_Declaration);

            when An_Ordinary_Type_Declaration =>
               declare
                  -- Retrieve the type definition for the expression
                  Result_Type_Definition : constant Asis.Definition
                    := Type_Declaration_View (Result_Type_Declaration);
               begin
                  case Definition_Kind (Result_Type_Definition) is
                     when A_Type_Definition =>
                        case Type_Kind (Result_Type_Definition) is
                           when A_Derived_Type_Definition
                             | A_Derived_Record_Extension_Definition
                             =>
                              declare
                                 Parent_Type : constant Asis.Subtype_Indication :=
                                   Parent_Subtype_Indication (Result_Type_Definition);
                                 Parent_Name : Asis.Expression;
                              begin
                                 if not Is_Nil (Subtype_Constraint (Parent_Type)) then
                                    -- OK, return type is constrained
                                    -- But it can still be a task or protected type (with discriminants)...
                                    Check_Tasks_Protected;

                                    -- No more discriminants to check...
                                    return;
                                 end if;
                                 -- in any other case, we need to check the parent subtype
                                 Parent_Name := Subtype_Simple_Name (Parent_Type);
                                 if Expression_Kind (Parent_Name) = An_Attribute_Reference then
                                    Parent_Name := Simple_Name (Prefix (Parent_Name));
                                 end if;
                                 Result_Type_Declaration := A4G_Bugs.Corresponding_Name_Declaration (Parent_Name);
                              end;

                           when A_Constrained_Array_Definition =>
                              Do_Report (K_Constrained_Array, "function returns constrained array type");
                              Check_Tasks_Protected;

                              -- Cannot have discriminants
                              return;

                           when An_Unconstrained_Array_Definition =>
                              Do_Report (K_Unconstrained_Array, "function returns unconstrained array type");
                              Check_Tasks_Protected;

                              -- Cannot have discriminants
                              return;

                           when An_Enumeration_Type_Definition
                             | A_Signed_Integer_Type_Definition
                             | A_Modular_Type_Definition
                             | A_Floating_Point_Definition
                             | An_Ordinary_Fixed_Point_Definition
                             | A_Decimal_Fixed_Point_Definition
                             | An_Access_Type_Definition
                             =>
                              -- Cannot have discriminants
                              return;

                           when A_Record_Type_Definition
                             | A_Tagged_Record_Type_Definition
                             =>
                              Check_Tasks_Protected;
                              Check_Discriminants;
                              return;

                           when Not_A_Type_Definition
                             | A_Root_Type_Definition
                             =>
                              Failure ("unexpected type definition");

                           when others =>
                              -- Unused, for Ada 2005 compatibility
                              return;
                        end case;

                     when others =>
                        return;
                  end case;
               end;

            when A_Formal_Type_Declaration =>
               declare
                  -- Retrieve the type definition for the expression
                  Result_Type_Definition : constant Asis.Definition := Type_Declaration_View (Result_Type_Declaration);
               begin
                  case Definition_Kind (Result_Type_Definition) is
                     when A_Formal_Type_Definition =>
                        case Formal_Type_Kind (Result_Type_Definition) is
                           when A_Formal_Derived_Type_Definition =>
                              declare
                                 Name : Asis.Expression := Subtype_Simple_Name (Result_Type_Definition);
                              begin
                                 if Expression_Kind (Name) = An_Attribute_Reference then
                                    -- It can only be T'Base which has the same constraints as T
                                    Name := Prefix (Name);
                                 end if;
                                 -- in any other case, we need to check the parent subtype
                                 Result_Type_Declaration :=
                                   A4G_Bugs.Corresponding_Name_Declaration (Name);
                              end;


                           when A_Formal_Private_Type_Definition
                             | A_Formal_Tagged_Private_Type_Definition
                             =>
                              Check_Discriminants;
                              return;

                              -- when matching A_Formal_Unconstrained_Array_Definition, report an error
                           when A_Formal_Unconstrained_Array_Definition =>
                              Do_Report (K_Unconstrained_Array, "function returns unconstrained formal array type");
                              Check_Tasks_Protected;

                              -- Cannot have discriminants
                              return;

                           when A_Formal_Discrete_Type_Definition
                             | A_Formal_Signed_Integer_Type_Definition
                             | A_Formal_Modular_Type_Definition
                             | A_Formal_Floating_Point_Definition
                             | A_Formal_Ordinary_Fixed_Point_Definition
                             | A_Formal_Decimal_Fixed_Point_Definition
                             | A_Formal_Access_Type_Definition
                             =>
                              -- Cannot have discriminants
                              return;

                           when A_Formal_Constrained_Array_Definition =>
                              Check_Tasks_Protected;
                              -- Cannot have discriminants
                              return;

                           when Not_A_Formal_Type_Definition =>
                              Failure ("unexpected formal type definition");

                           when others =>
                              -- Unused, for Ada 2005 compatibility
                              return;
                        end case;

                     when others =>
                        Failure ("Not a formal type for function result", Result_Type_Declaration);
                  end case;
               end;

            when others =>
               Failure ("Not a type for function result", Result_Type_Declaration);
         end case;
      end loop;

      -- Here, the type may have discriminants
  end Process_Function_Declaration;

begin  -- Rules.Return_Type
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Return_Type;
