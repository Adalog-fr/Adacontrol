----------------------------------------------------------------------
--  Rules.Declarations - Package body                               --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
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

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;
pragma Elaborate (Framework.Language);
package body Rules.Declarations is
   use Framework;

   type Declaration_Names is (D_Access_Protected_Type,        D_Access_Subprogram_Type,  D_Access_Task_Type,
                              D_Access_Type,                  D_Aliased,                 D_Array,
                              D_Array_Type,                   D_Child_Unit,              D_Constant,
                              D_Constrained_Array_Type,       D_Decimal_Fixed_Type,      D_Defaulted_Discriminant,
                              D_Defaulted_Generic_Parameter,  D_Defaulted_Parameter,     D_Derived_Type,
                              D_Discriminant,                 D_Enumeration_Type,        D_Entry,
                              D_Exception,                    D_Extension,               D_Fixed_Type,
                              D_Float_Type,                   D_Formal_Function,         D_Formal_Package,
                              D_Formal_Procedure,             D_Generic,                 D_Handlers,
                              D_In_Out_Generic_Parameter,     D_In_Out_Parameter,        D_Initialized_Record_Field,
                              D_Initialized_Protected_Field,  D_Integer_Type,            D_Limited_Private_Type,
                              D_Modular_Type,                 D_Multiple_Names,          D_Named_Number,
                              D_Nested_Package,               D_Nested_Generic_Function, D_Nested_Generic_Package,
                              D_Nested_Generic_Procedure,     D_Nested_Function_Instantiation,
                              D_Nested_Package_Instantiation, D_Nested_Procedure_Instantiation,
                              D_Non_Identical_Renaming,
                              D_Non_Limited_Private_Type,     D_Not_Operator_Renaming,   D_Null_Extension,
                              D_Null_Ordinary_Record_Type,    D_Null_Tagged_Type,        D_Operator,
                              D_Ordinary_Fixed_Type,          D_Ordinary_Record_Type,    D_Out_Parameter,
                              D_Package_Statements,           D_Private_Extension,       D_Protected,
                              D_Protected_Entry,              D_Protected_Type,          D_Record_Type,
                              D_Renaming,                     D_Separate,                D_Signed_Type,
                              D_Single_Array,                 D_Single_Protected ,       D_Single_Task,
                              D_Subtype,                      D_Tagged_Type,             D_Task,
                              D_Task_Entry,                   D_Task_Type,               D_Type,
                              D_Unconstrained_Array_Type,     D_Uninitialized_Record_Field,
                              D_Uninitialized_Protected_Field);
   type Declaration_Names_List is array (Positive range <>) of Declaration_Names;

   package Usage_Flags_Utilities is new Framework.Language.Flag_Utilities (Declaration_Names, "D_");
   use Usage_Flags_Utilities;

   type Usage_Flags is array (Declaration_Names) of Boolean;
   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;
   Usage     : array (Declaration_Names) of Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags (Header => "Parameter(s):");
      User_Message ("Control occurrences of Ada declarations");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language;
      Decl : Declaration_Names;

   begin
      if not Parameter_Exists then
         Parameter_Error ("At least one parameter required for rule " & Rule_Id);
      end if;

      while Parameter_Exists loop
         Decl := Get_Flag_Parameter (Allow_Any => False);
         if Rule_Used (Decl) then
            Parameter_Error ("Declaration already given for rule " & Rule_Id
                             & ": " & Image (Decl));
         end if;

         Rule_Used (Decl) := True;
         Usage (Decl)     := Basic.New_Context (Rule_Type, Label);
      end loop;
   end Add_Use;

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

   procedure Do_Report (Decl : Declaration_Names; Loc : Location) is
      use Framework.Reports;
   begin
      if not Rule_Used (Decl) then
         return;
      end if;

      Report (Rule_Id,
              Usage (Decl),
              Loc,
              "use of declaration """ & Image (Decl) & '"');
   end Do_Report;


   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Decl_List : Declaration_Names_List; Loc : Location) is
      -- When more than one declaration name is applicable, list given from
      -- less specific to most specific
      -- Report the most specific for each of Check, Search and Count
      use Framework.Reports;
      Found      : array (Rule_Types) of Boolean := (others => False);
      This_Usage : Basic_Rule_Context;
   begin
      for Decl in reverse Decl_List'Range loop
         if Rule_Used (Decl_List (Decl)) then
            This_Usage := Usage (Decl_List (Decl));
            if not Found (Basic.Rule_Type (This_Usage)) then
               Report (Rule_Id,
                       This_Usage,
                       Loc,
                       "use of declaration """ & Image (Decl_List (Decl)) & '"');
               Found (Basic.Rule_Type (This_Usage)) := True;
            end if;
         end if;
      end loop;
   end Do_Report;


   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Element : in Asis.Declaration) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Declarations, Asis.Definitions;
      use Framework.Scope_Manager, Thick_Queries, Utilities;

      Accessed_Type  : Asis.Element;
      Renamed_Entity : Asis.Name;

      procedure Check_Discriminant (Discr : Asis.Definition) is
      begin
         if Is_Nil (Discr) then
            return;
         end if;

         if Is_Nil (Initialization_Expression (Discriminants (Discr)(1))) then
            Do_Report (D_Discriminant, Get_Location (Discr));
         else
            Do_Report ((D_Discriminant, D_Defaulted_Discriminant), Get_Location (Discr));
         end if;
      end Check_Discriminant;

      function Is_Null_Record (Def : Asis.Definition) return Boolean is
      begin
         case Definition_Kind (Def) is
            when A_Null_Record_Definition =>
               return True;
            when A_Record_Definition =>
               declare
                  Components : constant Asis.Record_Component_List := Record_Components (Def);
               begin
                  for I in Components'Range loop
                     if Definition_Kind (Components (I)) /= A_Null_Component then
                        -- This includes the case of variant parts
                        return False;
                     end if;
                  end loop;
               end;
               return True;
            when others =>
               return False;
         end case;
      end Is_Null_Record;

   begin
      if Rule_Used = (Declaration_Names => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Names(Element)'Length > 1 then
         Do_Report (D_Multiple_Names, Get_Location (Names (Element)(2)));
      end if;

      case Declaration_Kind (Element) is
         when An_Ordinary_Type_Declaration =>
            case Type_Kind (Type_Declaration_View (Element)) is
               when Not_A_Type_Definition =>
                  Failure ("Not a type definition");

               when A_Root_Type_Definition =>
                  Failure ("A root type definition");

               when An_Access_Type_Definition =>
                  -- Check first if we have something more specific than "access":
                  case Access_Type_Kind (Type_Declaration_View (Element)) is
                     when Access_To_Subprogram_Definition =>
                        Do_Report ((D_Type, D_Access_Type, D_Access_Subprogram_Type), Get_Location (Element));
                     when others =>
                        Accessed_Type := Subtype_Simple_Name (Asis.Definitions.Access_To_Object_Definition
                                                              (Type_Declaration_View (Element)));
                        if Expression_Kind (Accessed_Type) = An_Attribute_Reference then
                           -- Must be 'Base or 'Class, the prefix is as good for our purpose
                           Accessed_Type := Prefix (Accessed_Type);
                        end if;

                        -- Get rid of selected components
                        if Expression_Kind (Accessed_Type) = A_Selected_Component then
                           Accessed_Type := Selector (Accessed_Type);
                        end if;

                        Accessed_Type := Ultimate_Type_Declaration (Corresponding_Name_Declaration
                                                                      (Accessed_Type));
                        case Declaration_Kind (Accessed_Type) is
                           when A_Task_Type_Declaration =>
                              Do_Report ((D_Type, D_Access_Type, D_Access_Task_Type), Get_Location (Element));
                           when A_Protected_Type_Declaration =>
                              Do_Report ((D_Type, D_Access_Type, D_Access_Protected_Type), Get_Location (Element));
                           when others =>
                              Do_Report ((D_Type, D_Access_Type), Get_Location (Element));
                        end case;
                  end case;

               when A_Derived_Record_Extension_Definition =>
                  if Is_Null_Record (Asis.Definitions.Record_Definition (Type_Declaration_View (Element))) then
                     Do_Report ((D_Type, D_Record_Type, D_Tagged_Type, D_Extension, D_Null_Extension),
                                Get_Location (Element));
                  else
                     Do_Report ((D_Type, D_Record_Type, D_Tagged_Type, D_Extension), Get_Location (Element));
                  end if;

               when A_Derived_Type_Definition =>
                  Do_Report ((D_Type, D_Derived_Type), Get_Location (Element));

               when An_Enumeration_Type_Definition =>
                  Do_Report ((D_Type, D_Enumeration_Type), Get_Location (Element));

               when A_Signed_Integer_Type_Definition =>
                  Do_Report ((D_Type, D_Integer_Type, D_Signed_Type), Get_Location (Element));

               when A_Modular_Type_Definition =>
                  Do_Report ((D_Type, D_Integer_Type, D_Modular_Type), Get_Location (Element));

               when A_Floating_Point_Definition =>
                  Do_Report ((D_Type, D_Float_Type), Get_Location (Element));

               when An_Ordinary_Fixed_Point_Definition =>
                  Do_Report ((D_Type, D_Fixed_Type, D_Ordinary_Fixed_Type), Get_Location (Element));

               when A_Decimal_Fixed_Point_Definition =>
                  Do_Report ((D_Type, D_Fixed_Type, D_Decimal_Fixed_Type), Get_Location (Element));

               when A_Constrained_Array_Definition =>
                  Do_Report ((D_Type, D_Array, D_Array_Type, D_Constrained_Array_Type),
                             Get_Location (Element));

               when An_Unconstrained_Array_Definition =>
                  Do_Report ((D_Type, D_Array, D_Array_Type, D_Unconstrained_Array_Type),
                             Get_Location (Element));

               when A_Record_Type_Definition =>
                  if Is_Null_Record (Asis.Definitions.Record_Definition (Type_Declaration_View (Element))) then
                     Do_Report ((D_Type, D_Record_Type, D_Ordinary_Record_Type, D_Null_Ordinary_Record_Type),
                                Get_Location (Element));
                  else
                     Do_Report ((D_Type, D_Record_Type, D_Ordinary_Record_Type), Get_Location (Element));
                  end if;

               when A_Tagged_Record_Type_Definition =>
                  if Is_Null_Record (Asis.Definitions.Record_Definition (Type_Declaration_View (Element))) then
                     Do_Report ((D_Type, D_Record_Type, D_Tagged_Type, D_Null_Tagged_Type), Get_Location (Element));
                  else
                     Do_Report ((D_Type, D_Record_Type, D_Tagged_Type), Get_Location (Element));
                  end if;

               when others =>
                  -- An_Interface_Type_Definition for Ada2005
                  null;
            end case;

            Check_Discriminant (Discriminant_Part (Element));

         when A_Private_Type_Declaration =>
            if Trait_Kind (Element) = A_Limited_Private_Trait then
               Do_Report (D_Limited_Private_Type, Get_Location (Element));
            else
               Do_Report (D_Non_Limited_Private_Type, Get_Location (Element));
            end if;

         when A_Private_Extension_Declaration =>
            Do_Report (D_Private_Extension, Get_Location (Element));

         when A_Subtype_Declaration =>
            Do_Report (D_Subtype, Get_Location (Element));

         when A_Number_Declaration =>
            Do_Report (D_Named_Number, Get_Location (Element));

         when A_Variable_Declaration =>
            case Trait_Kind (Element) is
               when An_Aliased_Trait =>
                  Do_Report (D_Aliased, Get_Location (Element));
               when others =>
                  null;
            end case;

            if Definition_Kind (Object_Declaration_View (Element)) = A_Type_Definition then
               -- This happens only for anonymous arrays
               Do_Report ((D_Array, D_Single_Array), Get_Location (Element));
            end if;

         when A_Constant_Declaration =>
            case Trait_Kind (Element) is
               when An_Aliased_Trait =>
                  Do_Report (D_Aliased, Get_Location (Element));
               when others =>
                  null;
            end case;

            Do_Report (D_Constant, Get_Location (Element));

            if Definition_Kind (Object_Declaration_View (Element)) = A_Type_Definition then
               -- This happens only for anonymous arrays
               Do_Report ((D_Array, D_Single_Array), Get_Location (Element));
            end if;

         when A_Component_Declaration =>
            if Definition_Kind (Enclosing_Element (Element)) = A_Protected_Definition then
               if Is_Nil (Initialization_Expression (Element)) then
                  Do_Report (D_Uninitialized_Protected_Field, Get_Location (Element));
               else
                  Do_Report (D_Initialized_Protected_Field, Get_Location (Element));
               end if;
            else
               if Is_Nil (Initialization_Expression (Element)) then
                  Do_Report (D_Uninitialized_Record_Field, Get_Location (Element));
               else
                  Do_Report (D_Initialized_Record_Field, Get_Location (Element));
               end if;
            end if;

         when A_Parameter_Specification =>
            if not Is_Nil (Initialization_Expression (Element)) then
               Do_Report (D_Defaulted_Parameter, Get_Location (Element));
            end if;

            case Mode_Kind (Element) is
               when An_Out_Mode =>
                  Do_Report (D_Out_Parameter, Get_Location (Element));
               when An_In_Out_Mode =>
                  Do_Report (D_In_Out_Parameter, Get_Location (Element));
               when others =>
                  null;
            end case;

         when A_Formal_Object_Declaration =>
            if not Is_Nil (Initialization_Expression (Element)) then
               Do_Report (D_Defaulted_Generic_Parameter, Get_Location (Element));
            end if;

            if Mode_Kind (Element) = An_In_Out_Mode then
               Do_Report (D_In_Out_Generic_Parameter, Get_Location (Element));
            end if;

         when A_Package_Declaration =>
            if Current_Depth /= 1 then
               Do_Report (D_Nested_Package, Get_Location (Element));
            end if;

         when A_Package_Body_Declaration =>
            if Body_Statements (Element) /= Nil_Element_List then
               Do_Report (D_Package_Statements, Get_Previous_Word_Location (Body_Statements (Element)(1)));
            end if;
            if Body_Exception_Handlers (Element) /= Nil_Element_List then
               Do_Report (D_Handlers, Get_Previous_Word_Location (Body_Exception_Handlers (Element) (1)));
            end if;

         when A_Function_Declaration =>
            if Defining_Name_Kind (Names (Element)(1)) = A_Defining_Operator_Symbol then
               Do_Report (D_Operator, Get_Location (Element));
            end if;

         when A_Procedure_Body_Declaration
           | A_Function_Body_Declaration
           | A_Task_Body_Declaration
           | An_Entry_Body_Declaration
           =>
            if Body_Exception_Handlers (Element) /= Nil_Element_List then
               Do_Report (D_Handlers, Get_Previous_Word_Location (Body_Exception_Handlers (Element) (1)));
            end if;

            if Defining_Name_Kind (Names (Element)(1)) = A_Defining_Operator_Symbol
              and then Is_Nil (Corresponding_Declaration (Element))
            then
               -- If there is an explicit spec, we give the message on the spec
               Do_Report (D_Operator, Get_Location (Element));
            end if;

         when A_Task_Type_Declaration =>
            Do_Report ((D_Type, D_Task, D_Task_Type), Get_Location (Element));
            Check_Discriminant (Discriminant_Part (Element));

         when A_Single_Task_Declaration =>
            Do_Report ((D_Task, D_Single_Task), Get_Location (Element));

         when A_Protected_Type_Declaration =>
            Do_Report ((D_Type, D_Protected, D_Protected_Type), Get_Location (Element));
            Check_Discriminant (Discriminant_Part (Element));

         when A_Single_Protected_Declaration =>
            Do_Report ((D_Protected, D_Single_Protected), Get_Location (Element));

         when An_Entry_Declaration =>
            case Definition_Kind (Enclosing_Element (Element)) is
               when A_Task_Definition =>
                  Do_Report ((D_Entry, D_Task_Entry), Get_Location (Element));
               when A_Protected_Definition =>
                  Do_Report ((D_Entry, D_Protected_Entry), Get_Location (Element));
               when others =>
                  Failure ("Entry not in protected or task");
            end case;

         when An_Exception_Declaration =>
            Do_Report (D_Exception, Get_Location (Element));

         when A_Generic_Function_Declaration =>
            if Current_Depth /= 1 then
               Do_Report ((D_Generic, D_Nested_Generic_Function), Get_Location (Element));
            else
               Do_Report (D_Generic, Get_Location (Element));
            end if;

         when A_Generic_Package_Declaration =>
            if Current_Depth /= 1 then
               Do_Report ((D_Generic, D_Nested_Generic_Package), Get_Location (Element));
            else
               Do_Report (D_Generic, Get_Location (Element));
            end if;

         when A_Generic_Procedure_Declaration =>
            if Current_Depth /= 1 then
               Do_Report ((D_Generic, D_Nested_Generic_Procedure), Get_Location (Element));
            else
               Do_Report (D_Generic, Get_Location (Element));
            end if;

         when A_Function_Instantiation =>
            if Current_Depth /= 1 then
               Do_Report (D_Nested_Function_Instantiation, Get_Location (Element));
            end if;

         when A_Package_Instantiation =>
            if Current_Depth /= 1 then
               Do_Report (D_Nested_Package_Instantiation, Get_Location (Element));
            end if;

         when A_Procedure_Instantiation =>
            if Current_Depth /= 1 then
               Do_Report (D_Nested_Procedure_Instantiation, Get_Location (Element));
            end if;

         when A_Body_Stub =>
            Do_Report (D_Separate, Get_Location (Element));

         when A_Function_Renaming_Declaration
           | A_Generic_Function_Renaming_Declaration
           =>
            if Rule_Used (D_Not_Operator_Renaming) or Rule_Used (D_Non_Identical_Renaming) then
               Renamed_Entity := A4G_Bugs.Renamed_Entity (Element);
               if  Expression_Kind (Renamed_Entity) = A_Selected_Component then
                  Renamed_Entity := Selector (Renamed_Entity);
               end if;
               if Rule_Used (D_Not_Operator_Renaming)
                 and then Expression_Kind (Renamed_Entity) /= An_Operator_Symbol
               then
                  Do_Report (D_Not_Operator_Renaming, Get_Location (Element));
               end if;
               if Rule_Used (D_Non_Identical_Renaming) then
                  Renamed_Entity := A4G_Bugs.Renamed_Entity (Element);
                  loop
                     case Expression_Kind (Renamed_Entity) is
                        when An_Explicit_Dereference
                          | An_Attribute_Reference
                          | A_Character_Literal
                          =>
                           -- Always triggered
                           Do_Report (D_Non_Identical_Renaming, Get_Location (Element));
                           exit;
                        when A_Selected_Component =>
                           Renamed_Entity := Selector (Renamed_Entity);
                        when An_Identifier
                          | An_Operator_Symbol
                          | An_Enumeration_Literal
                          =>
                           if   To_Upper (Defining_Name_Image (Names (Element)(1)))
                             /= To_Upper (Name_Image (Renamed_Entity))
                           then
                              Do_Report (D_Non_Identical_Renaming, Get_Location (Element));
                           end if;
                           exit;
                        when others =>
                           Failure ("Not a function name in function renaming");
                     end case;
                  end loop;
               end if;
            end if;
            Do_Report (D_Renaming, Get_Location (Element));

         when An_Object_Renaming_Declaration
           | An_Exception_Renaming_Declaration
           | A_Package_Renaming_Declaration
           | A_Procedure_Renaming_Declaration
           | A_Generic_Package_Renaming_Declaration
           | A_Generic_Procedure_Renaming_Declaration
           =>
            if Rule_Used (D_Not_Operator_Renaming) then
               Do_Report (D_Not_Operator_Renaming, Get_Location (Element));
            end if;
            if Rule_Used (D_Non_Identical_Renaming) then
               Renamed_Entity := A4G_Bugs.Renamed_Entity (Element);
               loop
                  case Expression_Kind (Renamed_Entity) is
                     when An_Explicit_Dereference
                        | An_Indexed_Component
                        | A_Slice
                        | An_Attribute_Reference
                        | A_Function_Call
                        | A_Character_Literal
                          =>
                        -- Always triggered
                        Do_Report (D_Non_Identical_Renaming, Get_Location (Element));
                        exit;
                     when A_Selected_Component =>
                        Renamed_Entity := Selector (Renamed_Entity);
                     when A_Type_Conversion =>
                        Renamed_Entity := Converted_Or_Qualified_Expression (Renamed_Entity);
                     when An_Identifier | An_Operator_Symbol | An_Enumeration_Literal =>
                        if To_Upper (Defining_Name_Image (Names (Element)(1))) /= To_Upper (Name_Image (Renamed_Entity))
                        then
                           Do_Report (D_Non_Identical_Renaming, Get_Location (Element));
                        end if;
                        exit;
                     when others =>
                        Failure ("Not a name in renaming");
                  end case;
               end loop;
            end if;
            Do_Report (D_Renaming, Get_Location (Element));

         when A_Formal_Function_Declaration =>
            Do_Report (D_Formal_Function, Get_Location (Element));

         when A_Formal_Package_Declaration | A_Formal_Package_Declaration_With_Box =>
            Do_Report (D_Formal_Package, Get_Location (Element));

         when A_Formal_Procedure_Declaration =>
            Do_Report (D_Formal_Procedure, Get_Location (Element));

         when others =>
            null;
      end case;
   end Process_Declaration;


   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Element : in Asis.Declaration) is
      use Asis, Asis.Elements, Asis.Statements, Utilities;
   begin
      if Rule_Used = (Declaration_Names => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Statement_Kind (Element) is
         when An_Accept_Statement =>
            if Accept_Body_Exception_Handlers (Element) /= Nil_Element_List then
               Do_Report (D_Handlers, Get_Previous_Word_Location (Accept_Body_Exception_Handlers (Element) (1)));
            end if;
         when A_Block_Statement =>
            if Block_Exception_Handlers (Element) /= Nil_Element_List then
               Do_Report (D_Handlers, Get_Previous_Word_Location (Block_Exception_Handlers (Element) (1)));
            end if;
         when others =>
            Failure ("Bad statement", Element);
      end case;
   end Process_Statement;


   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Unit : in Asis.Compilation_Unit) is
      use Asis, Asis.Declarations, Asis.Elements;
   begin
      if not Rule_Used (D_Child_Unit) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Defining_Name_Kind (Names (Unit_Declaration (Unit))(1)) = A_Defining_Expanded_Name then
         Do_Report (D_Child_Unit, Get_Location (Unit_Declaration (Unit)));
      end if;
   end Process_Unit;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.Declarations;
