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
  Framework.Language.Shared_Keys,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Declarations is
   use Framework;

   type Subrules is
     (D_Abstract_Function,               D_Abstract_Procedure,              D_Abstract_Type,
      D_Access_All_Type,                 D_Access_Constant_Type,            D_Access_Protected_Type,
      D_Access_Subprogram_Type,          D_Access_Task_Type,                D_Access_Type,
      D_Aliased,                         D_Array,                           D_Array_Type,
      D_Binary_Modular_Type,             D_Character_Literal,               D_Child_Unit,
      D_Constant,                        D_Constrained_Array_Type,          D_Constrained_Array_Variable,
      D_Controlled_Type,                 D_Decimal_Fixed_Type,              D_Defaulted_Discriminant,
      D_Defaulted_Generic_Parameter,     D_Defaulted_Parameter,             D_Deferred_Constant,
      D_Derived_Type,                    D_Discriminant,                    D_Enumeration_Type,
      D_Entry,                           D_Exception,                       D_Extension,
      D_Fixed_Type,                      D_Float_Type,                      D_Formal_Function,
      D_Formal_Package,                  D_Formal_Procedure,                D_Formal_Type,
      D_Function,                        D_Function_Instantiation,          D_Generic,
      D_Generic_Function,                D_Generic_Package,                 D_Generic_Procedure,
      D_Handlers,                        D_Incomplete_Type,                 D_In_Out_Generic_Parameter,
      D_In_Out_Parameter,                D_Initialized_Record_Field,        D_Initialized_Protected_Field,
      D_Instantiation,                   D_Integer_Type,                    D_Limited_Private_Type,
      D_Modular_Type,                    D_Multiple_Names,                  D_Multiple_Protected_Entries,
      D_Named_Number,                    D_Non_Binary_Modular_Type,         D_Non_Identical_Renaming,
      D_Non_Identical_Operator_Renaming, D_Non_Limited_Private_Type,        D_Not_Operator_Renaming,
      D_Null_Extension,                  D_Null_Ordinary_Record_Type,       D_Null_Procedure,
      D_Null_Tagged_Type,                D_Operator,                        D_Operator_Renaming,
      D_Ordinary_Fixed_Type,             D_Ordinary_Record_Type,            D_Out_Parameter,
      D_Package,                         D_Package_Instantiation,           D_Package_Statements,
      D_Predefined_Operator,             D_Private_Extension,               D_Procedure,
      D_Procedure_Instantiation,         D_Protected,                       D_Protected_Entry,
      D_Protected_Type,                  D_Record_Type,                     D_Renaming,
      D_Separate,                        D_Signed_Type,                     D_Single_Array,
      D_Single_Protected ,               D_Single_Task,                     D_Subtype,
      D_Tagged_Type,                     D_Task,                            D_Task_Entry,
      D_Task_Type,                       D_Type,                            D_Unconstrained_Array_Type,
      D_Unconstrained_Array_Variable,    D_Unconstrained_Subtype,           D_Uninitialized_Protected_Field,
      D_Uninitialized_Record_Field,      D_Uninitialized_Variable,          D_Variable,
      D_Variant_Part);
   type Subrules_List is array (Positive range <>) of Subrules;

   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "D_");

   type Declaration_Context is new Basic_Rule_Context with
      record
         Locations : Framework.Language.Shared_Keys.Places_Set;
      end record;

   type Usage_Flags is array (Subrules) of Boolean;
   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;
   Usage     : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Framework.Language.Shared_Keys;
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): {<location>} <decl_kind>");
      Scope_Places_Utilities.Help_On_Modifiers (Header => "<location>:");
      Subrules_Flag_Utilities.Help_On_Flags (Header => "<decl_kind>:");
      User_Message ("Control occurrences of Ada declarations");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind  : in Control_Kinds) is
      use Framework.Language, Framework.Language.Shared_Keys;
      use Subrules_Flag_Utilities;
      Subrule : Subrules;
      Loc     : Places_Set;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      while Parameter_Exists loop
         Loc  := Get_Places_Set_Modifiers;
         Subrule := Get_Flag_Parameter (Allow_Any => False);

         Rule_Used (Subrule) := True;
         Associate (Usage,
                    Value (Subrules'Wide_Image (Subrule)),
                    Declaration_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Loc),
                    Additive => True);
      end loop;
   exception
      when Already_In_Store =>
         Parameter_Error (Rule_Id, "parameters already specified");
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

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Usage);
   end Prepare;

   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Decl : Subrules; Loc : Location) is
      use Framework.Reports, Framework.Language.Shared_Keys;
      use Scope_Places_Utilities, Subrules_Flag_Utilities;

      First_Context : constant Root_Context'Class
        := Framework.Association (Usage, Value (Subrules'Wide_Image (Decl)));
   begin
      if First_Context = No_Matching_Context then
         return;
      end if;

      if Is_Applicable (Declaration_Context (First_Context).Locations)  then
         Report (Rule_Id,
                 First_Context,
                 Loc,
                 "use of declaration """
                 & Image (Declaration_Context (First_Context).Locations, Default => Everywhere)
                 & Image (Decl)
                 & '"');
      end if;

      loop
         declare
            Next_Context : constant Root_Context'Class := Next_Matching_Context (Usage);
         begin
            exit when Next_Context = No_Matching_Context;

            if Is_Applicable (Declaration_Context (Next_Context).Locations) then
               Report (Rule_Id,
                       Next_Context,
                       Loc,
                       "use of declaration """
                       & Image (Declaration_Context (Next_Context).Locations, Default => Everywhere)
                       & Image (Decl)
                       & '"');
            end if;
         end;
      end loop;
   end Do_Report;


   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Decl_List : Subrules_List; Loc : Location) is
      -- When more than one declaration name is applicable, list given from
      -- less specific to most specific
   begin
      for Decl in reverse Decl_List'Range loop
         Do_Report (Decl_List (Decl), Loc);
      end loop;
   end Do_Report;


   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Element : in Asis.Declaration) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Declarations, Asis.Definitions;
      use Thick_Queries, Utilities;

      Accessed_Type  : Asis.Element;
      Renamed_Entity : Asis.Name;
      Enclosing      : Asis.Element;

      procedure Check_Abstract is
      begin
         case Declaration_Kind (Element) is
            when A_Function_Declaration =>
               if Trait_Kind (Element) in An_Abstract_Trait .. An_Abstract_Limited_Private_Trait then
                  Do_Report (D_Abstract_Function, Get_Location (Element));
               end if;
            when A_Procedure_Declaration =>
               if Trait_Kind (Element) in An_Abstract_Trait .. An_Abstract_Limited_Private_Trait then
                  Do_Report (D_Abstract_Procedure, Get_Location (Element));
               end if;
            when An_Ordinary_Type_Declaration
               | A_Private_Type_Declaration
               | A_Private_Extension_Declaration
                 =>
               if Trait_Kind (Type_Declaration_View (Element))
                  in An_Abstract_Trait .. An_Abstract_Limited_Private_Trait
               then
                  Do_Report (D_Abstract_Type, Get_Location (Element));
               end if;
            when others =>
               Failure ("Abstract not type or subprogram");
         end case;
      end Check_Abstract;

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

      procedure Check_Multiple_Entries (Def : Asis.Definition) is
         Decls : constant Asis.Declaration_List := Visible_Part_Items (Def)
                                                 & Private_Part_Items (Def);
         First_Seen : Boolean := False;
      begin
         for I in Decls'Range loop
            if Declaration_Kind (Decls (I)) = An_Entry_Declaration then
               if First_Seen then
                  Do_Report (D_Multiple_Protected_Entries, Get_Location (Decls (I)));
               else
                  First_Seen := True;
               end if;
            end if;
         end loop;
      end Check_Multiple_Entries;

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

      function Is_Predefined_Operator (Decl : Asis.Declaration) return Boolean is
         -- Expected declaration kind:
         --    A_Function_Declaration
         --    A_Function_Body_Declaration
         -- (of operator)
         -- Returns True if the operator is identical to a predefined one.

         -- Convenience subtypes (for binary operators only)
         subtype Logical_Operators     is Operator_Kinds range An_And_Operator     .. An_Xor_Operator;
         subtype Equality_Operators    is Operator_Kinds range An_Equal_Operator   .. A_Not_Equal_Operator;
         subtype Relational_Operators  is Operator_Kinds range An_Equal_Operator   .. A_Greater_Than_Or_Equal_Operator;
         subtype Adding_Operators      is Operator_Kinds range A_Plus_Operator     .. A_Minus_Operator;
         subtype Multiplying_Operators is Operator_Kinds range A_Multiply_Operator .. A_Rem_Operator;

         subtype Discrete_Type_Kinds is Type_Kinds range An_Enumeration_Type_Definition .. A_Modular_Type_Definition;
         subtype Fixed_Type_Kinds    is Type_Kinds
                 range An_Ordinary_Fixed_Point_Definition .. A_Decimal_Fixed_Point_Definition;

         Profile : constant Profile_Descriptor := Types_Profile (Decl);
         Temp    : Asis.Element;
         Name    : constant Asis.Defining_Name := Names (Decl) (1);
         Kind    : constant Operator_Kinds := Operator_Kind (Name);

         Operation_Ultimate_Type : constant Asis.Definition
           := Type_Declaration_View (Ultimate_Type_Declaration (Enclosing_Element (Profile.Formals (1).Name)));

         function Array_Dimensions (Arr_Def : Asis.Definition) return Asis.List_Index is
            -- How many dimensions in provided array declaration ?
         begin
            if Type_Kind (Arr_Def) = A_Constrained_Array_Definition then
               return Discrete_Subtype_Definitions (Arr_Def)'Length;
            else
               -- unconstrained array
               return Index_Subtype_Definitions (Arr_Def)'Length;
            end if;
         end Array_Dimensions;

         function Is_Type (N : Asis.Defining_Name; Value : Wide_String; Or_Derived : Boolean := False) return Boolean is
            -- True if the ultimate type of N is Value
           D : Asis.Declaration := Enclosing_Element (N);
         begin
            if Or_Derived then
               D := Ultimate_Type_Declaration (D);
            end if;
            return To_Upper (Full_Name_Image (Names (D) (1))) = Value;
         end Is_Type;

      begin   -- Is_Predefined_Operator
         if Profile.Formals_Length = 1 then
            -- Unary operators

            -- Eliminate weird cases (not homogenous, access, class...) that cannot be predefined
            -- We purposedly ignore the 'Base attribute
            if not Is_Equal (Profile.Formals (1).Name, Profile.Result_Type.Name)
              or Profile.Formals (1).Is_Access
              or Profile.Formals (1).Attribute = Class
              or Profile.Result_Type.Attribute = Class
            then
               return False;
            end if;

            case Type_Kind (Operation_Ultimate_Type) is
               when A_Signed_Integer_Type_Definition
                  | A_Floating_Point_Definition
                  | An_Ordinary_Fixed_Point_Definition
                  | A_Decimal_Fixed_Point_Definition
                    =>
                  -- All unary operators except "not" are predefined
                  return Kind /= A_Not_Operator;
               when A_Modular_Type_Definition =>
                  -- All unary operators are predefined
                  return True;
               when An_Enumeration_Type_Definition =>
                  -- Only Boolean has predefined operators
                  return Is_Type (Profile.Formals (1).Name, "STANDARD.BOOLEAN", Or_Derived => True)
                    and then Kind = A_Not_Operator;
               when A_Constrained_Array_Definition
                  | An_Unconstrained_Array_Definition
                    =>
                  if Array_Dimensions (Operation_Ultimate_Type) /= 1 then
                     return False;
                  end if;
                  -- Temp <- True component type name definition
                  Temp := Corresponding_Name_Definition (Subtype_Simple_Name
                                                         (Component_Subtype_Indication
                                                          (Array_Component_Definition
                                                           (Operation_Ultimate_Type))));
                  -- Boolean array?
                  if Is_Type (Temp, "STANDARD.BOOLEAN", Or_Derived => True) then
                     return Kind = A_Not_Operator;
                  end if;
                  return False;
               when others =>
                  return False;
            end case;
         end if;

         -- Binary operators

         -- Special case: "**" on floating point types
         if Kind = An_Exponentiate_Operator then
            return Is_Equal (Profile.Formals (1).Name, Profile.Result_Type.Name)
              and then not Profile.Formals (1).Is_Access
              and then not Profile.Formals (2).Is_Access
              and then Type_Kind (Operation_Ultimate_Type) = A_Floating_Point_Definition
              and then Is_Type (Profile.Formals (2).Name, "STANDARD.INTEGER");
         end if;

         -- Special case: "*" and "/" on fixed point types
         if Kind in A_Multiply_Operator .. A_Divide_Operator then
            return Is_Equal (Profile.Formals (1).Name, Profile.Result_Type.Name)
              and then not Profile.Formals (1).Is_Access
              and then not Profile.Formals (2).Is_Access
              and then Type_Kind (Operation_Ultimate_Type) in Fixed_Type_Kinds
              and then Is_Type (Profile.Formals (2).Name, "STANDARD.INTEGER");
         end if;

         -- Eliminate weird cases (not homogenous, access, class...) that cannot be predefined
         -- We purposedly ignore the 'Base attribute
         if Kind in Relational_Operators then
            if not Is_Equal (Profile.Formals (1).Name, Profile.Formals (2).Name)
              or not Is_Type (Profile.Result_Type.Name, "STANDARD.BOOLEAN", Or_Derived => True)
              or Profile.Formals (1).Is_Access
              or Profile.Formals (2).Is_Access
              or Profile.Formals (1).Attribute = Class
              or Profile.Formals (2).Attribute = Class
              or Profile.Result_Type.Attribute = Class
            then
               return False;
            end if;
         else
            if not Is_Equal (Profile.Formals (1).Name, Profile.Formals (2).Name)
              or not Is_Equal (Profile.Formals (1).Name, Profile.Result_Type.Name)
              or Profile.Formals (1).Is_Access
              or Profile.Formals (2).Is_Access
              or Profile.Formals (1).Attribute = Class
              or Profile.Formals (2).Attribute = Class
              or Profile.Result_Type.Attribute = Class
            then
               return False;
            end if;
         end if;

         -- Special case: "=" and "/=" of limited types
         if Kind in Equality_Operators and then Is_Limited (Profile.Formals (1).Name) then
            return False;
         end if;

         case Type_Kind (Operation_Ultimate_Type) is
            when A_Signed_Integer_Type_Definition
               | A_Floating_Point_Definition
               | An_Ordinary_Fixed_Point_Definition
               | A_Decimal_Fixed_Point_Definition
                 =>
               return Kind in Adding_Operators
                 or Kind in Multiplying_Operators
                 or Kind in Relational_Operators;
            when A_Modular_Type_Definition =>
               return Kind in Adding_Operators
                 or Kind in Multiplying_Operators
                 or Kind in Logical_Operators
                 or Kind in Relational_Operators;
            when A_Constrained_Array_Definition
                 | An_Unconstrained_Array_Definition
                 =>
               if Array_Dimensions (Operation_Ultimate_Type) /= 1 then
                  return Kind in Equality_Operators;
               end if;

               -- Temp <- True component type name definition
               Temp := Corresponding_Name_Definition (Subtype_Simple_Name
                                                      (Component_Subtype_Indication
                                                       (Array_Component_Definition
                                                        (Operation_Ultimate_Type))));
               -- Boolean array?
               if Is_Type (Temp, "STANDARD.BOOLEAN", Or_Derived => True) then
                  return Kind in Logical_Operators
                    or Kind in Relational_Operators
                    or Kind = A_Concatenate_Operator;
               end if;

               -- Discrete array ?
               if Type_Kind (Enclosing_Element (Temp)) in Discrete_Type_Kinds then
                  return Kind in Relational_Operators
                    or Kind = A_Concatenate_Operator;
               end if;

               return Kind in Equality_Operators
                 or Kind = A_Concatenate_Operator;
            when others =>
               return Kind in Equality_Operators;
         end case;
      end Is_Predefined_Operator;

      function Is_Controlled (The_Subtype : Asis.Declaration) return Boolean is
         Ultimate_Ancestor : constant Wide_String := To_Upper (Full_Name_Image
                                                               (Names (Ultimate_Type_Declaration (The_Subtype))(1)));
      begin
         return Ultimate_Ancestor = "ADA.FINALIZATION.CONTROLLED"
           or else Ultimate_Ancestor = "ADA.FINALIZATION.LIMITED_CONTROLLED";
      end Is_Controlled;


   begin   -- Process_Declaration
      if Rule_Used = (Subrules => False) then
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
                           Accessed_Type := Simple_Name (Prefix (Accessed_Type));
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

                  -- Check for "all" or "constant" (separate message)
                  case Access_Type_Kind (Type_Declaration_View (Element)) is
                     when Not_An_Access_Type_Definition =>
                        Failure ("Access type is not_an_access");
                     when A_Pool_Specific_Access_To_Variable
                        | Access_To_Subprogram_Definition
                          =>
                        null;
                     when An_Access_To_Variable =>
                        Do_Report (D_Access_All_Type, Get_Location (Element));
                     when An_Access_To_Constant =>
                        Do_Report (D_Access_Constant_Type, Get_Location (Element));
                  end case;

               when A_Derived_Record_Extension_Definition =>
                  declare
                     Decls : Subrules_List (1 .. 4 + 2);
                     Last  : Positive;
                  begin
                     Decls (1 .. 4) := (D_Type, D_Record_Type, D_Tagged_Type, D_Extension);
                     Last := 4;
                     if Is_Null_Record (Asis.Definitions.Record_Definition (Type_Declaration_View (Element))) then
                        Last := Last + 1;
                        Decls (Last) := D_Null_Extension;
                     end if;
                     if Is_Controlled (Element) then
                        Last := Last + 1;
                        Decls (Last) := D_Controlled_Type;
                     end if;
                     Do_Report (Decls (1..Last), Get_Location (Element));
                  end;
                  Check_Abstract;

               when A_Derived_Type_Definition =>
                  Do_Report ((D_Type, D_Derived_Type), Get_Location (Element));

               when An_Enumeration_Type_Definition =>
                  Do_Report ((D_Type, D_Enumeration_Type), Get_Location (Element));
                  if Rule_Used (D_Character_Literal) then
                     declare
                        Literals : constant Asis.Declaration_List
                          := Enumeration_Literal_Declarations (Type_Declaration_View (Element));
                     begin
                        for I in Literals'Range loop
                           if Defining_Name_Kind (Names (Literals (I)) (1)) = A_Defining_Character_Literal then
                              Do_Report (D_Character_Literal, Get_Location (Literals (I)));
                           end if;
                        end loop;
                     end;
                  end if;

               when A_Signed_Integer_Type_Definition =>
                  Do_Report ((D_Type, D_Integer_Type, D_Signed_Type), Get_Location (Element));

               when A_Modular_Type_Definition =>
                  if Rule_Used (D_Binary_Modular_Type) or Rule_Used (D_Non_Binary_Modular_Type) then
                     declare
                        use Framework.Reports;
                        Expr    : constant Asis.Expression
                                  := Mod_Static_Expression (Type_Declaration_View (Element));
                        Mod_Val : constant Extended_Biggest_Natural := Discrete_Static_Expression_Value (Expr);
                        Val     : Biggest_Natural;
                     begin
                        if Mod_Val = Not_Static then
                           Uncheckable (Rule_Id,
                                        False_Negative,
                                        Get_Location (Expr),
                                        "unable to evaluate mod expression");
                        else
                           Val := Mod_Val;
                           while Val rem 2 = 0 loop
                              Val := Val / 2;
                           end loop;
                           if Val = 1 then
                              -- Power of 2
                              Do_Report ((D_Type, D_Integer_Type, D_Modular_Type, D_Binary_Modular_Type),
                                         Get_Location (Element));
                           else
                              Do_Report ((D_Type, D_Integer_Type, D_Modular_Type, D_Non_Binary_Modular_Type),
                                         Get_Location (Element));
                           end if;
                        end if;
                     end;
                  else
                     Do_Report ((D_Type, D_Integer_Type, D_Modular_Type), Get_Location (Element));
                  end if;

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
                  Check_Abstract;

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
            Check_Abstract;

         when A_Private_Extension_Declaration =>
            Do_Report (D_Private_Extension, Get_Location (Element));
            Check_Abstract;

         when An_Incomplete_Type_Declaration =>
            Do_Report (D_Incomplete_Type, Get_Location (Element));

         when A_Subtype_Declaration =>
            Do_Report (D_Subtype, Get_Location (Element));
            if Is_Nil (Subtype_Constraint (Type_Declaration_View (Element))) then
               Do_Report (D_Unconstrained_Subtype, Get_Location (Element));
            end if;

         when A_Number_Declaration =>
            Do_Report (D_Named_Number, Get_Location (Element));

         when A_Variable_Declaration =>
            if Is_Nil (Initialization_Expression (Element)) then
               Do_Report (D_Uninitialized_Variable, Get_Location (Element));
            end if;

            case Trait_Kind (Element) is
               when An_Aliased_Trait =>
                  Do_Report (D_Aliased, Get_Location (Element));
               when others =>
                  null;
            end case;

            Do_Report (D_Variable, Get_Location (Element));

            declare
               Def       : constant Asis.Definition := Object_Declaration_View (Element);
               Type_Name : Asis.Expression;
            begin
               if Definition_Kind (Def) = A_Type_Definition then
               -- This happens only for anonymous arrays
                  if Type_Kind (Def) = An_Unconstrained_Array_Definition then
                     Do_Report ((D_Array, D_Single_Array, D_Unconstrained_Array_Variable), Get_Location (Element));
                  else
                     Do_Report ((D_Array, D_Single_Array, D_Constrained_Array_Variable), Get_Location (Element));
                  end if;
               else
                  Type_Name := Subtype_Simple_Name (Def);
                  if Expression_Kind (Type_Name) /= An_Attribute_Reference then
                     -- 'Base is only for scalar types
                     -- 'Class is only for tagged types
                     -- None applies to arrays
                     case Type_Kind
                          (Type_Declaration_View
                             (Ultimate_Type_Declaration
                                (Corresponding_Name_Declaration (Type_Name))))
                     is
                        when An_Unconstrained_Array_Definition =>
                           Do_Report (D_Unconstrained_Array_Variable, Get_Location (Element));
                        when A_Constrained_Array_Definition =>
                           Do_Report (D_Constrained_Array_Variable, Get_Location (Element));
                        when others =>
                           null;
                     end case;
                  end if;
               end if;
            end;

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

         when A_Deferred_Constant_Declaration =>
            Do_Report (D_Deferred_Constant, Get_Location (Element));

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
            -- Do not print message if the parameter is for a procedure or function body
            -- with an explicit specification
            Enclosing := Enclosing_Element (Element);
            if Declaration_Kind (Enclosing) not in A_Procedure_Body_Declaration .. A_Function_Body_Declaration
              or else Is_Nil (Corresponding_Declaration (Enclosing))
            then
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
            end if;

         when A_Formal_Object_Declaration =>
            if not Is_Nil (Initialization_Expression (Element)) then
               Do_Report (D_Defaulted_Generic_Parameter, Get_Location (Element));
            end if;

            if Mode_Kind (Element) = An_In_Out_Mode then
               Do_Report (D_In_Out_Generic_Parameter, Get_Location (Element));
            end if;

         when A_Package_Declaration =>
            Do_Report (D_Package, Get_Location (Element));

         when A_Package_Body_Declaration =>
            if Body_Statements (Element) /= Nil_Element_List then
               Do_Report (D_Package_Statements, Get_Previous_Word_Location (Body_Statements (Element)(1)));
            end if;
            if Body_Exception_Handlers (Element) /= Nil_Element_List then
               Do_Report (D_Handlers, Get_Previous_Word_Location (Body_Exception_Handlers (Element) (1)));
            end if;

         when A_Procedure_Declaration =>
            Do_Report (D_Procedure, Get_Location (Element));
            Check_Abstract;

         when A_Procedure_Body_Declaration =>
            if Is_Nil (Corresponding_Declaration (Element)) then
               -- If there is no explicit spec, process as a spec.
               Do_Report (D_Procedure, Get_Location (Element));
            end if;

            if Are_Null_Statements (Body_Statements (Element)) then
               Do_Report (D_Null_Procedure, Get_Location (Body_Statements (Element)(1)));
            end if;

            if Body_Exception_Handlers (Element) /= Nil_Element_List then
               Do_Report (D_Handlers, Get_Previous_Word_Location (Body_Exception_Handlers (Element) (1)));
            end if;

         when A_Function_Declaration =>
            if Defining_Name_Kind (Names (Element)(1)) = A_Defining_Operator_Symbol then
               Do_Report (D_Operator, Get_Location (Element));
               if Is_Predefined_Operator (Element) then
                  Do_Report (D_Predefined_Operator, Get_Location (Element));
               end if;
            end if;
            Do_Report (D_Function, Get_Location (Element));
            Check_Abstract;

         when A_Function_Body_Declaration =>
            if Is_Nil (Corresponding_Declaration (Element)) then
               -- If there is no explicit spec, process as a spec.
               if Defining_Name_Kind (Names (Element) (1)) = A_Defining_Operator_Symbol then
                  Do_Report (D_Operator, Get_Location (Element));
                  if Is_Predefined_Operator (Element) then
                     Do_Report (D_Predefined_Operator, Get_Location (Element));
                  end if;
               end if;
               Do_Report (D_Function, Get_Location (Element));
            end if;

            if Body_Exception_Handlers (Element) /= Nil_Element_List then
               Do_Report (D_Handlers, Get_Previous_Word_Location (Body_Exception_Handlers (Element) (1)));
            end if;

         when A_Task_Body_Declaration
           | An_Entry_Body_Declaration
           =>
            if Body_Exception_Handlers (Element) /= Nil_Element_List then
               Do_Report (D_Handlers, Get_Previous_Word_Location (Body_Exception_Handlers (Element) (1)));
            end if;

         when A_Task_Type_Declaration =>
            Do_Report ((D_Type, D_Task, D_Task_Type), Get_Location (Element));
            Check_Discriminant (Discriminant_Part (Element));

         when A_Single_Task_Declaration =>
            Do_Report ((D_Task, D_Single_Task), Get_Location (Element));

         when A_Protected_Type_Declaration =>
            Do_Report ((D_Type, D_Protected, D_Protected_Type), Get_Location (Element));
            Check_Discriminant (Discriminant_Part (Element));
            if Rule_Used (D_Multiple_Protected_Entries) then
               Check_Multiple_Entries (Type_Declaration_View (Element));
            end if;

         when A_Single_Protected_Declaration =>
            Do_Report ((D_Protected, D_Single_Protected), Get_Location (Element));
            if Rule_Used (D_Multiple_Protected_Entries) then
               Check_Multiple_Entries (Object_Declaration_View (Element));
            end if;

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
            Do_Report ((D_Generic, D_Generic_Function), Get_Location (Element));

         when A_Generic_Package_Declaration =>
            Do_Report ((D_Generic, D_Generic_Package), Get_Location (Element));

         when A_Generic_Procedure_Declaration =>
            Do_Report ((D_Generic, D_Generic_Procedure), Get_Location (Element));

         when A_Function_Instantiation =>
            Do_Report ((D_Instantiation, D_Function_Instantiation), Get_Location (Element));

         when A_Package_Instantiation =>
            Do_Report ((D_Instantiation, D_Package_Instantiation), Get_Location (Element));

         when A_Procedure_Instantiation =>
            Do_Report ((D_Instantiation, D_Procedure_Instantiation), Get_Location (Element));

         when A_Body_Stub =>
            Do_Report (D_Separate, Get_Location (Element));

         when A_Function_Renaming_Declaration
           | A_Generic_Function_Renaming_Declaration
              =>
            Do_Report (D_Renaming, Get_Location (Element));

            if   Rule_Used (D_Not_Operator_Renaming)
              or Rule_Used (D_Non_Identical_Renaming)
              or Rule_Used (D_Operator_Renaming)
              or Rule_Used (D_Non_Identical_Operator_Renaming)
            then
               Renamed_Entity := A4G_Bugs.Renamed_Entity (Element);
               if  Expression_Kind (Renamed_Entity) = A_Selected_Component then
                  Renamed_Entity := Selector (Renamed_Entity);
               end if;

               case Expression_Kind (Renamed_Entity) is
                  when An_Explicit_Dereference
                     | An_Attribute_Reference
                     | A_Character_Literal
                       =>
                     Do_Report (D_Not_Operator_Renaming, Get_Location (Element));
                     -- Cannot be identical name
                     Do_Report (D_Non_Identical_Renaming, Get_Location (Element));
                  when An_Operator_Symbol =>
                     Do_Report (D_Operator_Renaming, Get_Location (Element));
                     if   To_Upper (Defining_Name_Image (Names (Element) (1)))
                       /= To_Upper (A4G_Bugs.Name_Image (Renamed_Entity))
                     then
                        Do_Report ((D_Non_Identical_Renaming, D_Non_Identical_Operator_Renaming),
                                   Get_Location (Element));
                     end if;
                  when An_Identifier
                     | An_Enumeration_Literal
                       =>
                     Do_Report (D_Not_Operator_Renaming, Get_Location (Element));
                     if   To_Upper (Defining_Name_Image (Names (Element) (1)))
                       /= To_Upper (A4G_Bugs.Name_Image (Renamed_Entity))
                     then
                        Do_Report (D_Non_Identical_Renaming, Get_Location (Element));
                     end if;
                  when others =>
                     Failure ("Not a function name in function renaming");
               end case;
            end if;

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
                        if   To_Upper (Defining_Name_Image (Names (Element) (1)))
                          /= To_Upper (A4G_Bugs.Name_Image (Renamed_Entity))
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

         when A_Formal_Type_Declaration =>
            Do_Report (D_Formal_Type, Get_Location (Element));

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
      if Rule_Used = (Subrules => False) then
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

   ---------------------
   -- Process_Variant --
   ---------------------

   procedure Process_Variant (Variant : in Asis.Definition) is
   begin
      if not Rule_Used (D_Variant_Part) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Do_Report (D_Variant_Part, Get_Location (Variant));
   end Process_Variant;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Declarations;
