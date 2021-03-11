----------------------------------------------------------------------
--  Rules.Declarations - Package body                               --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2021.           --
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

-- Ada
with
  Ada.Containers.Indefinite_Holders;

-- ASIS
with
  Asis.Clauses,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  String_Matching,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Variables,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);

package body Rules.Declarations is
   use Framework, Framework.Control_Manager, Framework.Variables.Shared_Types;

   type Subrules is
     (D_Any_Declaration,
      D_Abstract_Function,                 D_Abstract_Operator,                 D_Abstract_Procedure,
      D_Abstract_Type,                     D_Access_All_Type,                   D_Access_Constant_Type,
      D_Access_Constrained_Array_Type,     D_Access_Def_Discriminated_Type,     D_Access_Formal_Type,
      D_Access_Language_Type,              D_Access_Nondef_Discriminated_Type,  D_Access_Protected_Type,
      D_Access_Subprogram_Type,            D_Access_Task_Type,                  D_Access_Type,
      D_Access_Unconstrained_Array_Type,   D_Access_Unknown_Discriminated_Type, D_Aliased_Array_Component,
      D_Aliased_Constant,                  D_Aliased_Protected_Component,       D_Aliased_Record_Component,
      D_Aliased_Variable,                  D_Anonymous_Access_Component,        D_Anonymous_Access_Constant,
      D_Anonymous_Access_Discriminant,     D_Anonymous_Access_Parameter,        D_Anonymous_Access_Variable,
      D_Anonymous_Subtype_Allocator,       D_Anonymous_Subtype_Case,            D_Anonymous_Subtype_Declaration,
      D_Anonymous_Subtype_For,             D_Anonymous_Subtype_Indexing,        D_Array,
      D_Array_Type,

      D_Binary_Modular_Type,               D_Box_Defaulted_Formal_Function,     D_Box_Defaulted_Formal_Procedure,

      D_Character_Literal,                 D_Child_Unit,                        D_Class_Wide_Constant,
      D_Class_Wide_Variable,               D_Constant,                          D_Constrained_Array_Constant,
      D_Constrained_Array_Type,            D_Constrained_Array_Variable,        D_Controlled_Type,
      D_Constructor,

      D_Decimal_Fixed_Type,                D_Defaulted_Discriminant,            D_Defaulted_Generic_Parameter,
      D_Defaulted_Parameter,               D_Deferred_Constant,                 D_Derived_Type,
      D_Discriminant,

      D_Empty_Private_Part,                D_Empty_Visible_Part,                D_Enumeration_Type,
      D_Entry,                             D_Equality_Operator,                 D_Exception,
      D_Expression_Function,               D_Extension,

      D_Fixed_Type,                        D_Float_Type,                        D_Formal_Function,
      D_Formal_Package,                    D_Formal_Procedure,                  D_Formal_Type,
      D_Function,                          D_Function_Instantiation,

      D_Generic,                           D_Generic_Function,                  D_Generic_Package,
      D_Generic_Procedure,

      D_Handlers,

      D_Incomplete_Type,                   D_In_Out_Generic_Parameter,          D_In_Out_Parameter,
      D_Initialized_Protected_Component,   D_Initialized_Record_Component,      D_Initialized_Variable,
      D_Instantiation,                     D_Integer_Type,                      D_Interface_Type,

      D_Limited_Private_Type,

      D_Modular_Type,                      D_Multiple_Names,                    D_Multiple_Protected_Entries,

      D_Name_Defaulted_Formal_Function,    D_Name_Defaulted_Formal_Procedure,   D_Named_Number,
      D_No_Spec_Function,                  D_No_Spec_Procedure,                 D_Non_Binary_Modular_Type,
      D_Non_Joint_CE_NE_Handler,           D_Non_Limited_Private_Type,          D_Non_Ravenscar_Task,
      D_Null_Defaulted_Formal_Procedure,   D_Null_Extension,                    D_Null_Ordinary_Record_Type,
      D_Null_Procedure,                    D_Null_Procedure_Body,               D_Null_Procedure_Declaration,
      D_Null_Tagged_Type,

      D_Operator,                          D_Ordinary_Fixed_Type,               D_Ordinary_Fixed_Type_No_Small,
      D_Ordinary_Fixed_Type_With_Small,    D_Ordinary_Record_Type,              D_Ordinary_Record_Variable,
      D_Out_Parameter,

      D_Package,                           D_Package_Instantiation,             D_Package_Statements,
      D_Predefined_Operator,               D_Private_Extension,                 D_Procedure,
      D_Procedure_Instantiation,           D_Protected,                         D_Protected_Discriminant,
      D_Protected_Entry,                   D_Protected_Type,                    D_Protected_Variable,

      D_Record_Type,                       D_Relay_Function,                    D_Relay_Package,
      D_Relay_Procedure,

      D_Scalar_Variable,                   D_Self_Calling_Function,             D_Self_Calling_Procedure,
      D_Separate,                          D_Signed_Type,                       D_Single_Array,
      D_Single_Protected,                  D_Single_Task,                       D_Subtype,

      D_Tagged_Incomplete_Type,            D_Tagged_Private_Type,               D_Tagged_Type,
      D_Tagged_Variable,                   D_Task,                              D_Task_Discriminant,
      D_Task_Entry,                        D_Task_Type,                         D_Task_Variable,
      D_Type,

      D_Unconstrained_Array_Constant,      D_Unconstrained_Array_Type,          D_Unconstrained_Array_Variable,
      D_Unconstrained_Subtype,             D_Uninitialized_Protected_Component, D_Uninitialized_Record_Component,
      D_Uninitialized_Variable,            D_Unknown_Discriminant,

      D_Variable,                          D_Variant_Part);
   subtype All_Anonymous_Access is Subrules range D_Anonymous_Access_Component .. D_Anonymous_Access_Variable;
   type Subrules_List is array (Positive range <>) of Subrules;

   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "D_");

   package Pattern_Holder is new Ada.Containers.Indefinite_Holders (String_Matching.Compiled_Pattern,
                                                                    String_Matching."=");
   type Declaration_Context is new Basic_Rule_Context with
      record
         Locations : Framework.Language.Shared_Keys.Places_Set;
         Ignored   : Pattern_Holder.Holder;
      end record;

   type Usage_Flags is array (Subrules) of Boolean;
   No_Rule_Used : constant Usage_Flags := (others => False);
   Rule_Used : Usage_Flags := No_Rule_Used;
   Save_Used : Usage_Flags;
   Usage     : Context_Store;
   package Usage_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Usage);

   -- Rule variables
   Limited_Initialization : aliased Switch_Type.Object := (Value => Off);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Framework.Language.Shared_Keys, Framework.Variables;
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of Ada declarations");
      User_Message;
      User_Message ("Parameter(s): {<location>} [ignore ""<pattern>""] <decl>");
      Help_On_Scope_Places (Header => "<location>:");
      Subrules_Flag_Utilities.Help_On_Flags (Header => "<decl>:");
      User_Message;
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Limited_Initialization");

   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind  : in Control_Kinds) is
      use Framework.Language, Framework.Language.Shared_Keys, String_Matching;
      use Pattern_Holder, Subrules_Flag_Utilities;
      Subrule : Subrules;
      Loc     : Places_Set;
      Pat     : Holder;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      while Parameter_Exists loop
         Loc  := Get_Places_Set_Modifiers (Rule_Id);

         if Get_Modifier ("IGNORE") then
            if Is_String_Parameter then
               declare
                  Pat_String : constant Wide_String := Get_String_Modifier;
               begin
                  Pat := To_Holder (Compile (Pat_String, Ignore_Case => True));
               exception
                  when Pattern_Error =>
                     Parameter_Error (Rule_Id, "Incorrect pattern: " & Pat_String);
               end;
            else
               Parameter_Error (Rule_Id, "Pattern string expected");
            end if;
         end if;

         Subrule := Get_Flag_Parameter (Allow_Any => False);

         Rule_Used (Subrule) := True;
         Associate (Usage,
                    Value (Subrules'Wide_Image (Subrule)),
                    Declaration_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Loc, Pat),
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
            Rule_Used := No_Rule_Used;
            Clear (Usage);
            Limited_Initialization := (Value => Off);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule_Used;
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

   procedure Do_Report (Decl : Subrules; Loc : Locations.Location; Name : Asis.Name := Asis.Nil_Element) is
      use Framework.Reports, Framework.Language.Shared_Keys;
      use Subrules_Flag_Utilities, Utilities;

      Iter : Context_Iterator := Usage_Iterator.Create;

      function Is_Applicable (Cont : Declaration_Context) return Boolean is
         use Asis.Elements;
         use Pattern_Holder, String_Matching, Thick_Queries;
      begin
         if not Is_Applicable (Cont.Locations) then
            return False;
         end if;

         if Is_Nil (Name) then
            -- Things that don't have a name
            return True;
         end if;

         if Cont.Ignored = Empty_Holder then
            return True;
         end if;

         return not Match (Full_Name_Image (Name), Element (Cont.Ignored));
      end Is_Applicable;

   begin  -- Do_Report
      Reset (Iter, Value (Subrules'Wide_Image (Decl)));
      while not Is_Exhausted (Iter) loop
         if Is_Applicable (Declaration_Context (Value (Iter))) then
            Report (Rule_Id,
                    Value (Iter),
                    Loc,
                    "use of declaration """
                    & Image (Declaration_Context (Value (Iter)).Locations, Default => Everywhere)
                    & Image (Decl, Lower_Case)
                    & '"');
         end if;
         Next (Iter);
      end loop;
   end Do_Report;


   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Decl : Subrules; Elem : Asis.Element) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Framework.Locations;
   begin
      -- For an object declaration, report individually for each name
      -- in the object declaration (otherwise, count f. e. would be wrong)
      case  Declaration_Kind (Elem) is
         when A_Variable_Declaration       .. A_Deferred_Constant_Declaration
            | A_Discriminant_Specification .. A_Component_Declaration
            =>
            for N : Asis.Name of Names (Elem) loop
               Do_Report (Decl, Get_Location (N), N);
            end loop;
         when Not_A_Declaration =>
            -- block statements passed for the location...
            Do_Report (Decl, Get_Location (Elem), Nil_Element);
         when others =>
            Do_Report (Decl, Get_Location (Elem), Names (Elem)(1));
      end case;
   end Do_Report;


   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Decl_List : Subrules_List; Elem : Asis.Element) is
      -- When more than one declaration name is applicable, list given from
      -- less specific to most specific
   begin
      for Sr : Subrules of reverse Decl_List loop
         Do_Report (Sr, Elem);
      end loop;
   end Do_Report;


   --------------------
   -- Check_Handlers --
   --------------------

   procedure Check_Handlers (Handlers : Asis.Exception_Handler_List) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Locations, Thick_Queries, Utilities;

      CE_Found : Boolean;
      NE_Found : Boolean;
   begin
      if Handlers /= Nil_Element_List then
         Do_Report (D_Handlers, Get_Previous_Word_Location (Handlers, "EXCEPTION"));
      end if;

      if Rule_Used (D_Non_Joint_CE_NE_Handler) then
         for H : Asis.Exception_Handler of Handlers loop
            declare
               Choices : constant Asis.Element_List := Exception_Choices (H);
            begin
               if Definition_Kind (Choices (1)) /= An_Others_Choice then
                  CE_Found := False;
                  NE_Found := False;
                  for C : Asis.Element of Choices loop
                     declare
                        Name : constant Wide_String := To_Upper (Full_Name_Image (C));
                     begin
                        if Name = "STANDARD.CONSTRAINT_ERROR" then
                           CE_Found := True;
                        elsif Name = "STANDARD.NUMERIC_ERROR" then
                           NE_Found := True;
                        end if;
                     end;
                  end loop;
                  if CE_Found xor NE_Found then
                     Do_Report (D_Non_Joint_CE_NE_Handler, H);
                  end if;
               end if;
            end;
         end loop;
      end if;
   end Check_Handlers;

   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Element : in Asis.Element) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Declarations, Asis.Definitions;
      use Framework.Locations, Thick_Queries, Utilities;

      procedure Check_Abstract is
      begin
         case Declaration_Kind (Element) is
            when A_Function_Declaration =>
               if Trait_Kind (Element) in An_Abstract_Trait .. An_Abstract_Limited_Private_Trait then
                  if Defining_Name_Kind (Names (Element) (1)) = A_Defining_Operator_Symbol then
                     Do_Report (D_Abstract_Operator, Get_Location (Element));
                  end if;
                  Do_Report (D_Abstract_Function, Element);
               end if;
            when An_Expression_Function_Declaration =>
               null;   -- Cannot be abstract
            when A_Procedure_Declaration =>
               if Trait_Kind (Element) in An_Abstract_Trait .. An_Abstract_Limited_Private_Trait then
                  Do_Report (D_Abstract_Procedure, Element);
               end if;
            when An_Ordinary_Type_Declaration
               | A_Private_Type_Declaration
               | A_Private_Extension_Declaration
                 =>
               if Trait_Kind (Type_Declaration_View (Element))
                  in An_Abstract_Trait .. An_Abstract_Limited_Private_Trait
               then
                  Do_Report (D_Abstract_Type, Element);
               end if;
            when others =>
               Failure ("Abstract not type or subprogram", Element);
         end case;
      end Check_Abstract;

      procedure Check_Constructor (Decl : in Asis.Declaration) is
         Result_Type : constant Profile_Entry := Types_Profile (Decl).Result_Type;
         Formals     : constant Profile_Table := Types_Profile (Decl).Formals;
         Result_Category : constant Type_Categories := Type_Category (Elem    => Result_Type.General_Name.Name,
                                                                      Privacy => Follow_User_Private);
      begin
         if Result_Type.General_Name.Attribute = Class or Result_Category /= A_Tagged_Type
         then
            return;
         end if;

         for Formal : Profile_Entry of Formals loop
            if Is_Equal (Formal.General_Name.Name, Result_Type.General_Name.Name) then
               return;
            end if;
         end loop;

         if Is_Primitive_Of (Enclosing_Element(Result_Type.General_Name.Name), Decl) then
            Do_Report (D_Constructor, Decl);
         end if;
      end Check_Constructor;

      procedure Check_Discriminant (Decl : Asis.Declaration; Extra_Check : Subrules := D_Any_Declaration) is
         Discr  : constant Asis.Definition         := Discriminant_Part (Decl);
         D_Kind : constant Discriminant_Part_Kinds := Discriminant_Part_Kind (Discr);
      begin
         if D_Kind = No_Discriminant_Part then
            return;
         end if;

         if Extra_Check /= D_Any_Declaration then
            Do_Report (Extra_Check, Discr);
         end if;

         case D_Kind is
            when No_Discriminant_Part =>
               Failure ("Check_Discriminant", Decl);
            when An_Unknown_Discriminant_Part =>
               Do_Report (D_Unknown_Discriminant, Discr);
            when A_Defaulted_Discriminant_Part =>
               Do_Report (D_Defaulted_Discriminant, Discr);
            when A_Nondefaulted_Discriminant_Part =>
               null;
         end case;

         Do_Report (D_Discriminant, Discr);
      end Check_Discriminant;

      procedure Check_Access_Discriminated (Decl : Asis.Declaration; Accessed : Asis.Declaration) is
         D_Kind : constant Discriminant_Part_Kinds := Discriminant_Part_Kind (Accessed);
      begin
         case D_Kind is
            when No_Discriminant_Part =>
               return;
            when An_Unknown_Discriminant_Part =>
               Do_Report (D_Access_Unknown_Discriminated_Type, Decl);
            when  A_Defaulted_Discriminant_Part =>
               Do_Report (D_Access_Def_Discriminated_Type, Decl);
            when  A_Nondefaulted_Discriminant_Part =>
               Do_Report (D_Access_Nondef_Discriminated_Type, Decl);
         end case;
      end Check_Access_Discriminated;

      procedure Check_Multiple_Entries (Def : Asis.Definition) is
         First_Seen : Boolean := False;
      begin
         for Decl : Asis.Declaration of Asis.Declaration_List'(Visible_Part_Items (Def) & Private_Part_Items (Def)) loop
            if Declaration_Kind (Decl) = An_Entry_Declaration then
               if First_Seen then
                  Do_Report (D_Multiple_Protected_Entries, Decl);
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
               for Compo : Asis.Record_Component of Record_Components (Def) loop
                  if Definition_Kind (Compo) /= A_Null_Component then
                     -- This includes the case of variant parts
                     return False;
                  end if;
               end loop;
               return True;
            when others =>
               return False;
         end case;
      end Is_Null_Record;

   begin   -- Process_Declaration
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (D_Any_Declaration) then
         Do_Report (D_Any_Declaration, Element);
         if Rule_Used = Usage_Flags'(D_Any_Declaration => True, others => False) then
            -- no need to continue if Any_Declaration is the only one used
            return;
         end if;
      end if;

      if Names (Element)'Length > 1 then
         Do_Report (D_Multiple_Names, Names (Element)(2));
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
                        Do_Report ((D_Type, D_Access_Type, D_Access_Subprogram_Type), Element);
                     when others =>
                        declare
                           Accessed_Type : Asis.Declaration := Subtype_Simple_Name
                                                                (Asis.Definitions.Access_To_Object_Definition
                                                                 (Type_Declaration_View (Element)));
                           Accessed_Ancestor_Descr : Derivation_Descriptor;
                        begin
                           if Expression_Kind (Accessed_Type) = An_Attribute_Reference then
                              -- Must be 'Base or 'Class, the prefix is as good for our purpose
                              Accessed_Type := Simple_Name (Prefix (Accessed_Type));
                           end if;

                           Accessed_Ancestor_Descr := Corresponding_Derivation_Description
                                                       (Corresponding_Name_Declaration (Accessed_Type));
                           case Declaration_Kind (Accessed_Ancestor_Descr.Ultimate_Type) is
                              when A_Task_Type_Declaration =>
                                 Check_Access_Discriminated (Element, Accessed_Ancestor_Descr.Ultimate_Type);
                                 Do_Report ((D_Type, D_Access_Type, D_Access_Task_Type), Element);
                              when A_Protected_Type_Declaration =>
                                 Check_Access_Discriminated (Element, Accessed_Ancestor_Descr.Ultimate_Type);
                                 Do_Report ((D_Type, D_Access_Type, D_Access_Protected_Type), Element);
                              when An_Ordinary_Type_Declaration =>
                                 case Type_Kind (Type_Declaration_View (Accessed_Ancestor_Descr.Ultimate_Type)) is
                                    when A_Constrained_Array_Definition =>
                                       Do_Report ((D_Type, D_Access_Type, D_Access_Constrained_Array_Type),
                                                  Element);
                                    when An_Unconstrained_Array_Definition =>
                                       -- It might have been constrained on the way up
                                       if Is_Nil (Accessed_Ancestor_Descr.First_Constraint) then
                                          Do_Report ((D_Type, D_Access_Type, D_Access_Unconstrained_Array_Type),
                                                     Element);
                                       else
                                          Do_Report ((D_Type, D_Access_Type, D_Access_Constrained_Array_Type),
                                                     Element);
                                       end if;
                                    when others =>
                                       Check_Access_Discriminated (Element, Accessed_Ancestor_Descr.Ultimate_Type);
                                       Do_Report ((D_Type, D_Access_Type), Element);
                                 end case;
                              when A_Formal_Type_Declaration | A_Formal_Incomplete_Type_Declaration =>
                                 Check_Access_Discriminated (Element, Accessed_Ancestor_Descr.Ultimate_Type);
                                 Do_Report ((D_Type, D_Access_Type, D_Access_Formal_Type), Element);
                              when A_Private_Type_Declaration | A_Private_Extension_Declaration =>
                                 Check_Access_Discriminated (Element, Accessed_Ancestor_Descr.Ultimate_Type);
                                 Do_Report ((D_Type, D_Access_Type, D_Access_Language_Type), Element);
                              when others =>
                                 Failure ("Declarations: unexpected accessed type", Accessed_Type);
                           end case;
                        end;
                  end case;

                  -- Check for "all" or "constant"
                  case Access_Type_Kind (Type_Declaration_View (Element)) is
                     when Not_An_Access_Type_Definition =>
                        Failure ("Access type is not_an_access");
                     when A_Pool_Specific_Access_To_Variable
                        | Access_To_Subprogram_Definition
                          =>
                        null;
                     when An_Access_To_Variable =>
                        Do_Report (D_Access_All_Type, Element);
                     when An_Access_To_Constant =>
                        Do_Report (D_Access_Constant_Type, Element);
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
                     Do_Report (Decls (1..Last), Element);
                  end;
                  Check_Abstract;

               when A_Derived_Type_Definition =>
                  Do_Report ((D_Type, D_Derived_Type), Element);

               when An_Enumeration_Type_Definition =>
                  Do_Report ((D_Type, D_Enumeration_Type), Element);
                  if Rule_Used (D_Character_Literal) then
                     for Lit : Asis.Declaration of Enumeration_Literal_Declarations (Type_Declaration_View (Element))
                     loop
                        if Defining_Name_Kind (Names (Lit) (1)) = A_Defining_Character_Literal then
                           Do_Report (D_Character_Literal, Lit);
                        end if;
                     end loop;
                  end if;

               when A_Signed_Integer_Type_Definition =>
                  Do_Report ((D_Type, D_Integer_Type, D_Signed_Type), Element);

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
                                         Element);
                           else
                              Do_Report ((D_Type, D_Integer_Type, D_Modular_Type, D_Non_Binary_Modular_Type),
                                         Element);
                           end if;
                        end if;
                     end;
                  else
                     Do_Report ((D_Type, D_Integer_Type, D_Modular_Type), Element);
                  end if;

               when A_Floating_Point_Definition =>
                  Do_Report ((D_Type, D_Float_Type), Element);

               when An_Ordinary_Fixed_Point_Definition =>
                  if Rule_Used (D_Ordinary_Fixed_Type_With_Small) or Rule_Used (D_Ordinary_Fixed_Type_No_Small) then
                     declare
                        use Asis.Clauses;
                        Small_Found : Boolean := False;
                     begin
                        for Rep : Asis.Representation_Clause of Corresponding_Representation_Clauses (Element) loop
                           if Representation_Clause_Kind (Rep) = An_Attribute_Definition_Clause
                             and then Attribute_Kind (Representation_Clause_Name (Rep)) = A_Small_Attribute
                           then
                              Small_Found := True;
                              exit;
                           end if;
                        end loop;
                        if Small_Found then
                           Do_Report ((D_Type, D_Fixed_Type, D_Ordinary_Fixed_Type, D_Ordinary_Fixed_Type_With_Small),
                                      Element);
                        else
                           Do_Report ((D_Type, D_Fixed_Type, D_Ordinary_Fixed_Type, D_Ordinary_Fixed_Type_No_Small),
                                      Element);
                        end if;
                     end;
                  else
                     Do_Report ((D_Type, D_Fixed_Type, D_Ordinary_Fixed_Type), Element);
                  end if;

               when A_Decimal_Fixed_Point_Definition =>
                  Do_Report ((D_Type, D_Fixed_Type, D_Decimal_Fixed_Type), Element);

               when A_Constrained_Array_Definition =>
                  Do_Report ((D_Type, D_Array, D_Array_Type, D_Constrained_Array_Type),
                             Element);

                  case Trait_Kind (Array_Component_Definition (Type_Declaration_View (Element))) is
                     when An_Aliased_Trait =>
                        Do_Report (D_Aliased_Array_Component, Element);
                     when others =>
                        null;
                  end case;

               when An_Unconstrained_Array_Definition =>
                  Do_Report ((D_Type, D_Array, D_Array_Type, D_Unconstrained_Array_Type),
                             Element);

                  case Trait_Kind (Array_Component_Definition (Type_Declaration_View (Element))) is
                     when An_Aliased_Trait =>
                        Do_Report (D_Aliased_Array_Component, Element);
                     when others =>
                        null;
                  end case;

               when A_Record_Type_Definition =>
                  if Is_Null_Record (Asis.Definitions.Record_Definition (Type_Declaration_View (Element))) then
                     Do_Report ((D_Type, D_Record_Type, D_Ordinary_Record_Type, D_Null_Ordinary_Record_Type),
                                Element);
                  else
                     Do_Report ((D_Type, D_Record_Type, D_Ordinary_Record_Type), Element);
                  end if;

               when A_Tagged_Record_Type_Definition =>
                  if Is_Null_Record (Asis.Definitions.Record_Definition (Type_Declaration_View (Element))) then
                     Do_Report ((D_Type, D_Record_Type, D_Tagged_Type, D_Null_Tagged_Type), Element);
                  else
                     Do_Report ((D_Type, D_Record_Type, D_Tagged_Type), Element);
                  end if;
                  Check_Abstract;

               when An_Interface_Type_Definition =>
                  Do_Report ((D_Type, D_Interface_Type), Element);
            end case;

            Check_Discriminant (Element);

         when A_Private_Type_Declaration =>
            if Definition_Kind (Type_Declaration_View (Element)) = A_Tagged_Private_Type_Definition then
               Do_Report ((D_Type, D_Tagged_Private_Type), Element);
            end if;
            case Trait_Kind (Element) is
               when Not_A_Trait
                  | An_Ordinary_Trait
                  | An_Aliased_Trait
                  | An_Access_Definition_Trait  -- Obsolescent
                  | A_Reverse_Trait
                  | A_Limited_Trait
                  | An_Abstract_Trait
                  | An_Abstract_Limited_Trait
                  =>
                  Failure ("Bad trait in A_Private_Type_Declaration", Element);
               when A_Limited_Private_Trait
                  | An_Abstract_Limited_Private_Trait
                  =>
                  Do_Report (D_Limited_Private_Type, Element);
               when A_Private_Trait
                  | An_Abstract_Private_Trait
                  =>
                  Do_Report (D_Non_Limited_Private_Type, Element);
               when A_Null_Exclusion_Trait => -- 2005
                  null;
            end case;
            Check_Abstract;
            Check_Discriminant (Element);

         when A_Private_Extension_Declaration =>
            Do_Report (D_Private_Extension, Element);
            Check_Abstract;
            Check_Discriminant (Element);

         when An_Incomplete_Type_Declaration =>
            Do_Report (D_Incomplete_Type, Element);
            Check_Discriminant (Element);

         when A_Tagged_Incomplete_Type_Declaration =>
            Do_Report (D_Tagged_Incomplete_Type, Element);
            Check_Discriminant (Element);

         when A_Subtype_Declaration =>
            Do_Report (D_Subtype, Element);
            if Is_Nil (Subtype_Constraint (Type_Declaration_View (Element))) then
               Do_Report (D_Unconstrained_Subtype, Element);
            end if;

         when A_Number_Declaration =>
            Do_Report (D_Named_Number, Element);

         when A_Variable_Declaration =>
            Do_Report (D_Variable, Element);

            case Trait_Kind (Element) is
               when An_Aliased_Trait =>
                  Do_Report (D_Aliased_Variable, Element);
               when others =>
                  null;
            end case;

            declare
               Def           : Asis.Definition := Object_Declaration_View (Element);
               Type_Name     : Asis.Expression;
               Is_Class_Wide : Boolean := False;
            begin
               if Definition_Kind (Def) = A_Type_Definition then
                  -- This happens only for anonymous arrays
                  case Type_Kind (Def) is
                     when An_Unconstrained_Array_Definition =>
                        Do_Report (D_Single_Array, Element);
                     when A_Constrained_Array_Definition =>
                        Do_Report (D_Single_Array, Element);
                     when others =>
                        -- not an array
                        Failure ("type def not an array");
                  end case;

                  case Trait_Kind (Array_Component_Definition (Def)) is
                     when An_Aliased_Trait =>
                        Do_Report (D_Aliased_Array_Component, Element);
                     when others =>
                        null;
                  end case;

               elsif Is_Class_Wide_Subtype (Def) then
                  Is_Class_Wide := True;
                  Do_Report ((D_Tagged_Variable, D_Class_Wide_Variable), Element);
               end if;

               -- Find if the type of the variable is one which is controlled
               loop
                  case Definition_Kind (Def) is
                     when Not_A_Definition =>
                        Failure ("Not_A_Definition in variable type analysis", Def);
                     when  A_Type_Definition =>
                        case Type_Kind (Def) is
                           when An_Enumeration_Type_Definition
                              | A_Signed_Integer_Type_Definition
                              | A_Modular_Type_Definition
                              | A_Floating_Point_Definition
                              | An_Ordinary_Fixed_Point_Definition
                              | A_Decimal_Fixed_Point_Definition
                              =>
                              Do_Report (D_Scalar_Variable, Element);
                              exit;
                           when An_Unconstrained_Array_Definition =>
                              Do_Report ((D_Array, D_Unconstrained_Array_Variable), Element);
                              exit;
                           when A_Constrained_Array_Definition =>
                              Do_Report ((D_Array, D_Constrained_Array_Variable), Element);
                              exit;
                           when A_Derived_Type_Definition =>
                              Def := Parent_Subtype_Indication (Def);
                              -- don't exit!
                           when A_Record_Type_Definition =>
                              Do_Report (D_Ordinary_Record_Variable, Element);
                              exit;
                           when A_Tagged_Record_Type_Definition
                              | A_Derived_Record_Extension_Definition =>
                              Do_Report (D_Tagged_Variable, Element);
                              exit;
                           when others =>
                              -- not (yet) controlled
                              exit;
                        end case;
                     when A_Protected_Definition =>
                        Do_Report (D_Protected_Variable, Element);
                        exit;
                     when A_Task_Definition =>
                        Do_Report (D_Task_Variable, Element);
                        if not Is_Profile_Applied (Element, "RAVENSCAR") then
                           Do_Report (D_Non_Ravenscar_Task, Element);
                        end if;
                        exit;
                     when A_Subtype_Indication =>
                        case Constraint_Kind (Subtype_Constraint (Def)) is
                           when An_Index_Constraint =>
                              Do_Report ((D_Array, D_Constrained_Array_Variable), Element);
                              exit;
                           when A_Discriminant_Constraint
                              | Not_A_Constraint
                                =>
                              -- no constraint, search parent subtype
                              -- discriminant_constraint: not an array, but can still be a task or protected
                              Type_Name := Subtype_Simple_Name (Def);
                              if Expression_Kind (Type_Name) = An_Attribute_Reference then
                                 -- 'Base is only for scalar types
                                 -- 'Class is only for tagged types
                                 -- None applies to arrays, tasks, or protected
                                 exit;
                              end if;
                              Def := Type_Declaration_View (Corresponding_Name_Declaration (Type_Name));
                              if Is_Nil (Def) then
                                 -- Type_Declaration_View says:
                                 -- Returns a Nil_Element for a task_type_declaration that has no explicit
                                 --  task_definition.
                                 Do_Report (D_Task_Variable, Element);
                                 if not Is_Profile_Applied (Element, "RAVENSCAR") then
                                    Do_Report (D_Non_Ravenscar_Task, Element);
                                 end if;
                                 exit;
                              end if;
                           when A_Range_Attribute_Reference
                              | A_Simple_Expression_Range
                              | A_Digits_Constraint
                              | A_Delta_Constraint
                                =>
                              Do_Report (D_Scalar_Variable, Element);
                              exit;
                        end case;
                     when others =>
                        exit;
                  end case;
               end loop;

               if Is_Nil (Initialization_Expression (Element)) then
                  if not Is_Limited (Element)
                    or else (Limited_Initialization.Value = On
                             and then Type_Category (Element, Follow_Derived => True) not in Synchronized_Types)
                  then
                     Do_Report (D_Uninitialized_Variable, Element);
                  end if;
               else
                  if not Is_Class_Wide then -- Class-wide variables must be initialized
                     Do_Report (D_Initialized_Variable, Element);
                  end if;
               end if;
            end;

         when A_Constant_Declaration =>
            Do_Report (D_Constant, Element);

            case Trait_Kind (Element) is
               when An_Aliased_Trait =>
                  Do_Report (D_Aliased_Constant, Element);
               when others =>
                  null;
            end case;

            declare
               Def : Asis.Definition := Object_Declaration_View (Element);
               Type_Name : Asis.Expression;
            begin
               if Definition_Kind (Def) = A_Type_Definition then
                  -- This happens only for anonymous arrays
                  Do_Report ((D_Array, D_Single_Array), Element);
               elsif Is_Class_Wide_Subtype (Def) then
                  Do_Report (D_Class_Wide_Constant, Element);
               end if;

               -- Find if the type refers to a constrained array or an unconstrained array
               -- This is copied from the sequence for variables, but is slightly simpler since there are no
               -- tasks or protected constants
               loop
                  case Definition_Kind (Def) is
                     when Not_A_Definition =>
                        Failure ("Not_A_Definition in variable type analysis", Def);
                     when  A_Type_Definition =>
                        case Type_Kind (Def) is
                           when An_Unconstrained_Array_Definition =>
                              Do_Report ((D_Array, D_Unconstrained_Array_Constant), Element);
                              exit;
                           when A_Constrained_Array_Definition =>
                              Do_Report ((D_Array, D_Constrained_Array_Constant), Element);
                              exit;
                           when A_Derived_Type_Definition =>
                              Def := Parent_Subtype_Indication (Def);
                           when others =>
                              -- not an array
                              exit;
                        end case;
                     when A_Subtype_Indication =>
                        case Constraint_Kind (Subtype_Constraint (Def)) is
                           when An_Index_Constraint =>
                              Do_Report ((D_Array, D_Constrained_Array_Constant), Element);
                              exit;
                           when Not_A_Constraint =>
                              -- no constraint, search parent subtype
                              Type_Name := Subtype_Simple_Name (Def);
                              if Expression_Kind (Type_Name) = An_Attribute_Reference then
                                 -- 'Base is only for scalar types
                                 -- 'Class is only for tagged types
                                 -- None applies to arrays
                                 exit;
                              end if;
                              Def := Type_Declaration_View (Corresponding_Name_Declaration (Type_Name));
                           when A_Discriminant_Constraint
                              | A_Range_Attribute_Reference
                              | A_Simple_Expression_Range
                              | A_Digits_Constraint
                              | A_Delta_Constraint
                                =>
                              -- not an array
                              exit;
                        end case;
                     when others =>
                        exit;
                  end case;
               end loop;
            end;

         when A_Deferred_Constant_Declaration =>
            Do_Report ((D_Deferred_Constant, D_Constant), Element);

         when A_Component_Declaration =>
            if Definition_Kind (Enclosing_Element (Element)) = A_Protected_Definition then
               if Is_Nil (Initialization_Expression (Element)) and then not Is_Limited (Element) then
                  Do_Report (D_Uninitialized_Protected_Component, Element);
               else
                  Do_Report (D_Initialized_Protected_Component, Element);
               end if;

               case Trait_Kind (Object_Declaration_View (Element)) is
                  when An_Aliased_Trait =>
                     Do_Report (D_Aliased_Protected_Component, Element);
                  when others =>
                     null;
               end case;

            else
               if Is_Nil (Initialization_Expression (Element)) and then not Is_Limited (Element) then
                  Do_Report (D_Uninitialized_Record_Component, Element);
               else
                  Do_Report (D_Initialized_Record_Component, Element);
               end if;

               case Trait_Kind (Object_Declaration_View (Element)) is
                  when An_Aliased_Trait =>
                     Do_Report (D_Aliased_Record_Component, Element);
                  when others =>
                     null;
               end case;
            end if;

         when A_Parameter_Specification =>
            -- Do not print message if the parameter is for a procedure or function body
            -- with an explicit specification
            declare
               Enclosing : constant Asis.Element := Enclosing_Element (Element);
            begin
               if Declaration_Kind (Enclosing) not in A_Procedure_Body_Declaration .. A_Function_Body_Declaration
                 or else Is_Nil (Corresponding_Declaration (Enclosing))
               then
                  if not Is_Nil (Initialization_Expression (Element)) then
                     Do_Report (D_Defaulted_Parameter, Element);
                  end if;

                  case Mode_Kind (Element) is
                     when An_Out_Mode =>
                        Do_Report (D_Out_Parameter, Element);
                     when An_In_Out_Mode =>
                        Do_Report (D_In_Out_Parameter, Element);
                     when others =>
                        null;
                  end case;
               end if;
            end;

         when A_Formal_Object_Declaration =>
            if not Is_Nil (Initialization_Expression (Element)) then
               Do_Report (D_Defaulted_Generic_Parameter, Element);
            end if;

            if Mode_Kind (Element) = An_In_Out_Mode then
               Do_Report (D_In_Out_Generic_Parameter, Element);
            end if;

         when A_Package_Declaration =>
            Do_Report (D_Package, Element);

            declare
               Visible_Part : constant Element_List := Visible_Part_Declarative_Items (Element);
            begin
               if Visible_Part = Nil_Element_List then
                  Do_Report (D_Empty_Visible_Part, Element);
               elsif Visible_Part'Length = 1
                 and then   (   Declaration_Kind (Visible_Part (1)) = A_Package_Declaration
                             or Declaration_Kind (Visible_Part (1)) = A_Package_Instantiation
                             or Declaration_Kind (Visible_Part (1)) = A_Package_Renaming_Declaration)
               then
                  Do_Report (D_Relay_Package, Element);
               end if;
            end;

            if Asis.Declarations.Is_Private_Present (Element)
              and then Private_Part_Declarative_Items (Element) = Nil_Element_List
            then
               Do_Report (D_Empty_Private_Part, Get_Previous_Word_Location (Element, "END", Starting => From_Tail));
            end if;

         when A_Package_Body_Declaration =>
            if Body_Statements (Element) /= Nil_Element_List then
               Do_Report (D_Package_Statements, Get_Previous_Word_Location (Body_Statements (Element), "BEGIN"));
            end if;
            Check_Handlers (Body_Exception_Handlers (Element));

         when A_Procedure_Declaration =>
            Do_Report (D_Procedure, Element);
            Check_Abstract;

         when A_Null_Procedure_Declaration =>
            Do_Report ((D_Procedure, D_Null_Procedure, D_Null_Procedure_Declaration), Element);
            -- This one can't be abstact

         when A_Procedure_Body_Declaration =>
            if Is_Nil (Corresponding_Declaration (Element)) then
               -- If there is no explicit spec, process as a spec.
               Do_Report (D_Procedure, Element);
               if not Is_Subunit (Element) then   -- No_Spec_Procedure checked on stub
                  Do_Report (D_No_Spec_Procedure, Element);
               end if;
            end if;

            declare
               Stmts  : constant Asis.Statement_List := Body_Statements (Element);
               Called : Asis.Expression;
            begin
               if Are_Null_Statements (Stmts) then
                  Do_Report ((D_Null_Procedure, D_Null_Procedure_Body), Body_Statements (Element) (1));
               end if;

               if Stmts'Length = 1
                 and then Statement_Kind (Stmts (1)) = A_Procedure_Call_Statement
               then
                  Called := Called_Simple_Name (Stmts (1));
                  if not Is_Nil (Called)
                    and then Full_Name_Image (Called,            With_Profile => True)
                           = Full_Name_Image (Names(Element)(1), With_Profile => True)
                  then
                     Do_Report (D_Self_Calling_Procedure, Element);
                  else
                     Do_Report (D_Relay_Procedure, Element);
                  end if;
               end if;
            end;

            Check_Handlers (Body_Exception_Handlers (Element));

         when A_Function_Declaration =>
            if Defining_Name_Kind (Names (Element)(1)) = A_Defining_Operator_Symbol then
               Do_Report (D_Operator, Element);
               if Is_Predefined_Operator (Element) then
                  Do_Report (D_Predefined_Operator, Element);
               end if;
               if Operator_Kind (Names (Element) (1)) in Equality_Operators then
                  Do_Report (D_Equality_Operator, Element);
               end if;
            end if;
            Do_Report (D_Function, Element);
            Check_Abstract;
            if Rule_Used (D_Constructor) then
               Check_Constructor (Element);
            end if;

         when An_Expression_Function_Declaration =>   -- Ada 2012
            if Defining_Name_Kind (Names (Element)(1)) = A_Defining_Operator_Symbol then
               Do_Report (D_Operator, Element);
               if Is_Predefined_Operator (Element) then
                  Do_Report (D_Predefined_Operator, Element);
               end if;
               if Operator_Kind (Names (Element) (1)) in Equality_Operators then
                  Do_Report (D_Equality_Operator, Element);
               end if;
            end if;
            Do_Report (D_Expression_Function, Element);
            -- Cannot be abstract
            if Rule_Used (D_Constructor) then
               Check_Constructor (Element);
            end if;

         when A_Function_Body_Declaration =>
            if Is_Nil (Corresponding_Declaration (Element)) then
               -- If there is no explicit spec, process as a spec.
               if Defining_Name_Kind (Names (Element) (1)) = A_Defining_Operator_Symbol then
                  Do_Report (D_Operator, Element);
                  if Is_Predefined_Operator (Element) then
                     Do_Report (D_Predefined_Operator, Element);
                  end if;
                  if Operator_Kind (Names (Element) (1)) in Equality_Operators then
                     Do_Report (D_Equality_Operator, Element);
                  end if;
               end if;
               Do_Report (D_Function, Element);
               if not Is_Subunit (Element) then   -- No_Spec_Function checked on stub
                  Do_Report (D_No_Spec_Function, Element);
               end if;
            end if;

            declare
               use Asis.Statements;
               Stmts : constant Asis.Statement_List := Body_Statements (Element);
               Expr  : Asis.Expression;
            begin
               if Stmts'Length = 1
                 and then Statement_Kind (Stmts (1)) = A_Return_Statement
               then
                  Expr := Return_Expression (Stmts (1));
                  if Expression_Kind (Expr) = A_Function_Call then
                     Expr := Called_Simple_Name (Expr);
                     if not Is_Nil (Expr)
                       and then Full_Name_Image (Expr,              With_Profile => True)
                              = Full_Name_Image (Names(Element)(1), With_Profile => True)
                     then
                        Do_Report (D_Self_Calling_Function, Element);
                     else
                        Do_Report (D_Relay_Function, Element);
                     end if;
                  end if;
               end if;
            end;

            Check_Handlers (Body_Exception_Handlers (Element));

         when A_Task_Body_Declaration
           | An_Entry_Body_Declaration
           =>
            Check_Handlers (Body_Exception_Handlers (Element));

         when A_Task_Type_Declaration =>
            Do_Report ((D_Type, D_Task, D_Task_Type), Element);
            if not Is_Profile_Applied (Element, "RAVENSCAR") then
               Do_Report (D_Non_Ravenscar_Task, Element);
            end if;
            Check_Discriminant (Element, Extra_Check => D_Task_Discriminant);

         when A_Single_Task_Declaration =>
            Do_Report ((D_Task, D_Task_Variable, D_Single_Task), Element);
            if not Is_Profile_Applied (Element, "RAVENSCAR") then
               Do_Report (D_Non_Ravenscar_Task, Element);
            end if;

         when A_Protected_Type_Declaration =>
            Do_Report ((D_Type, D_Protected, D_Protected_Type), Element);
            Check_Discriminant (Element, Extra_Check => D_Protected_Discriminant);
            if Rule_Used (D_Multiple_Protected_Entries) then
               Check_Multiple_Entries (Type_Declaration_View (Element));
            end if;

         when A_Single_Protected_Declaration =>
            Do_Report ((D_Protected, D_Protected_Variable, D_Single_Protected), Element);
            if Rule_Used (D_Multiple_Protected_Entries) then
               Check_Multiple_Entries (Object_Declaration_View (Element));
            end if;

         when An_Entry_Declaration =>
            case Definition_Kind (Enclosing_Element (Element)) is
               when A_Task_Definition =>
                  Do_Report ((D_Entry, D_Task_Entry), Element);
               when A_Protected_Definition =>
                  Do_Report ((D_Entry, D_Protected_Entry), Element);
               when others =>
                  Failure ("Entry not in protected or task");
            end case;

         when An_Exception_Declaration =>
            Do_Report (D_Exception, Element);

         when A_Generic_Function_Declaration =>
            Do_Report ((D_Generic, D_Generic_Function), Element);
            -- We can't define an operation on a primitive type by an instanciation, see 13.14(5), 13.14(16)

         when A_Generic_Package_Declaration =>
            Do_Report ((D_Generic, D_Generic_Package), Element);

            if Visible_Part_Declarative_Items (Element) = Nil_Element_List then
               if Generic_Formal_Part (Element) = Nil_Element_List then
                  Do_Report (D_Empty_Visible_Part, Get_Next_Word_Location (Element, "PACKAGE", Starting => From_Head));
               else
                  Do_Report (D_Empty_Visible_Part, Get_Next_Word_Location (Generic_Formal_Part (Element), "PACKAGE"));
               end if;
            end if;

            if Asis.Declarations.Is_Private_Present (Element)
              and then Private_Part_Declarative_Items (Element) = Nil_Element_List
            then
               Do_Report (D_Empty_Private_Part, Get_Previous_Word_Location (Element, "END", Starting => From_Tail));
            end if;

         when A_Generic_Procedure_Declaration =>
            Do_Report ((D_Generic, D_Generic_Procedure), Element);

         when A_Function_Instantiation =>
            Do_Report ((D_Instantiation, D_Function_Instantiation), Element);
            if Rule_Used (D_Constructor) then
               Check_Constructor (Corresponding_Declaration (Element));
            end if;

         when A_Package_Instantiation =>
            Do_Report ((D_Instantiation, D_Package_Instantiation), Element);

         when A_Procedure_Instantiation =>
            Do_Report ((D_Instantiation, D_Procedure_Instantiation), Element);

         when A_Function_Body_Stub =>
            Do_Report (D_Separate, Element);
            if Is_Nil (Corresponding_Declaration (Element)) then
               Do_Report (D_No_Spec_Function, Element);
            end if;

         when A_Procedure_Body_Stub =>
            Do_Report (D_Separate, Element);
            if Is_Nil (Corresponding_Declaration (Element)) then
               Do_Report (D_No_Spec_Procedure, Element);
            end if;

         when A_Package_Body_Stub
            | A_Task_Body_Stub
            | A_Protected_Body_Stub
            =>
            Do_Report (D_Separate, Element);

         when A_Function_Renaming_Declaration =>
            if Rule_Used (D_Constructor) then
               Check_Constructor (Element);
            end if;

         when A_Formal_Function_Declaration =>
            Do_Report (D_Formal_Function, Element);

            case Default_Kind (Element) is
               when Not_A_Default =>
                  Failure ("Declarations: Not_A_Default (1)");
               when A_Name_Default =>
                  Do_Report (D_Name_Defaulted_Formal_Function, Element);
               when A_Box_Default =>
                  Do_Report (D_Box_Defaulted_Formal_Function, Element);
               when A_Null_Default =>
                  Failure ("Declarations: A_Null_Default");
               when A_Nil_Default =>
                  null;
            end case;

         when A_Formal_Package_Declaration | A_Formal_Package_Declaration_With_Box =>
            Do_Report (D_Formal_Package, Element);

         when A_Formal_Procedure_Declaration =>
            Do_Report (D_Formal_Procedure, Element);

            case Default_Kind (Element) is
               when Not_A_Default =>
                  Failure ("Declarations: Not_A_Default (1)");
               when A_Name_Default =>
                  Do_Report (D_Name_Defaulted_Formal_Procedure, Element);
               when A_Box_Default =>
                  Do_Report (D_Box_Defaulted_Formal_Procedure, Element);
               when A_Null_Default =>
                  Do_Report (D_Null_Defaulted_Formal_Procedure, Element);
               when A_Nil_Default =>
                  null;
            end case;

         when A_Formal_Type_Declaration =>
            Do_Report (D_Formal_Type, Element);

         when others =>
            null;
      end case;
   end Process_Declaration;


   ------------------------
   -- Process_Definition --
   ------------------------

   procedure Process_Definition (Element : in Asis.Definition) is
      use Asis, Asis.Definitions, Asis.Elements;
      use Utilities;

      procedure Anonymous_Subtype_Report is
         Ctxt : Asis.Element := Enclosing_Element (Element);
      begin
         if Declaration_Kind (Ctxt) = A_Subtype_Declaration then
            return;
         end if;
         loop
            case Element_Kind (Ctxt) is
               when A_Declaration =>
                  case Declaration_Kind (Ctxt) is
                     when A_Loop_Parameter_Specification =>
                        Do_Report (D_Anonymous_Subtype_For, Element);
                        exit;
                     when others =>
                        Do_Report (D_Anonymous_Subtype_Declaration, Element);
                        exit;
                  end case;
               when An_Expression =>
                  case Expression_Kind (Ctxt) is
                     when An_Allocation_From_Subtype =>
                        Do_Report (D_Anonymous_Subtype_Allocator, Element);
                        exit;
                     when A_Slice | A_Named_Array_Aggregate =>
                        Do_Report (D_Anonymous_Subtype_Indexing, Element);
                        exit;
                     when others =>
                        Ctxt := Enclosing_Element (Ctxt);
                  end case;
               when A_Path =>
                  case Path_Kind (Ctxt) is
                     when A_Case_Path | A_Case_Expression_Path =>
                        Do_Report (D_Anonymous_Subtype_Case, Element);
                        exit;
                     when others =>
                        Failure ("Anonymous_subtype_report: bad path", Ctxt);
                  end case;
               when others =>
                  Ctxt := Enclosing_Element (Ctxt);
            end case;
         end loop;
      end Anonymous_Subtype_Report;

   begin  -- Process_Definition
      if (Rule_Used
          and Usage_Flags'(D_Variant_Part |
                           D_Anonymous_Subtype_Case | D_Anonymous_Subtype_Declaration |
                           D_Anonymous_Subtype_For  | D_Anonymous_Subtype_Indexing => True,
                           others                                                  => False))
          = No_Rule_Used
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Definition_Kind (Element) is
         when A_Variant_Part =>
            Do_Report (D_Variant_Part, Element);
         when A_Subtype_Indication =>
            if not Is_Nil (Subtype_Constraint (Element)) then
               Anonymous_Subtype_Report;
            end if;
         when A_Discrete_Subtype_Definition
            | A_Discrete_Range
              =>
            case Discrete_Range_Kind (Element) is
               when Not_A_Discrete_Range =>
                  Failure ("Not a discrete range");
               when A_Discrete_Subtype_Indication =>
                  if not Is_Nil (Subtype_Constraint (Element)) then
                     Anonymous_Subtype_Report;
                  end if;
               when A_Discrete_Range_Attribute_Reference
                  | A_Discrete_Simple_Expression_Range
                    =>
                  Anonymous_Subtype_Report;
            end case;
         when others =>
            Failure ("Bad definition", Element);
      end case;
   end Process_Definition;


   -------------------------------
   -- Process_Access_Definition --
   -------------------------------

   procedure Process_Access_Definition (Element : in Asis.Definition) is
      use Asis, Asis.Elements;
      use Utilities;
      Encl : Asis.Declaration;
   begin
      if (Rule_Used
          and Usage_Flags'(All_Anonymous_Access => True,
                           others               => False))
          = No_Rule_Used
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Encl := Enclosing_Element (Element);
      case Element_Kind (Encl) is
         when A_Declaration =>
            case Declaration_Kind (Encl) is
               when A_Variable_Declaration | A_Return_Variable_Specification =>
                  Do_Report (D_Anonymous_Access_Variable, Encl);
               when A_Constant_Declaration | A_Return_Constant_Specification =>
                  Do_Report (D_Anonymous_Access_Constant, Encl);
               when A_Discriminant_Specification =>
                  Do_Report (D_Anonymous_Access_Discriminant, Encl);
               when A_Parameter_Specification =>
                  declare
                     use Asis.Declarations;
                     Decl : constant Asis.Declaration := Enclosing_Element (Encl);  -- Normally, the SP declaration
                  begin
                     case Declaration_Kind (Decl) is
                        when A_Procedure_Declaration
                           | A_Null_Procedure_Declaration
                           | A_Function_Declaration
                           | An_Expression_Function_Declaration   -- Ada 2012
                           | An_Entry_Declaration
                           | A_Procedure_Renaming_Declaration
                           | A_Function_Renaming_Declaration
                           | A_Generic_Procedure_Declaration
                           | A_Generic_Function_Declaration
                           | A_Formal_Procedure_Declaration
                           | A_Formal_Function_Declaration
                           =>
                           Do_Report (D_Anonymous_Access_Parameter, Encl);

                        when A_Procedure_Body_Declaration
                           | A_Function_Body_Declaration
                           =>
                           if Is_Nil (Corresponding_Declaration (Decl)) then
                              Do_Report (D_Anonymous_Access_Parameter, Encl);
                           end if;

                        when Not_A_Declaration =>
                           Assert (Element_Kind (Decl) = A_Definition,
                                   "Process_Access_Definition: unexpected parameter context (4)",
                                   Decl);
                           -- Must be part of a named or anonymous access to subprogram
                           -- We could put a more sophisticated Assert just to make sure, but there are many possible
                           -- contexts, so it is quite complicated, and the only risk is a redundant error message.
                           Do_Report (D_Anonymous_Access_Parameter, Encl);

                        when others =>
                           Failure ("Process_Access_Definition: unexpected parameter context (4)", Decl);
                     end case;
                  end;
               when A_Function_Declaration
                  | A_Formal_Function_Declaration
                  | An_Expression_Function_Declaration   -- Ada 2012
                  | A_Function_Body_Declaration
                  | A_Function_Body_Stub
                  | A_Function_Renaming_Declaration
                  | A_Generic_Function_Declaration
                  =>
                  -- This happens for functions whose result type is an anonymous access type
                  -- Theses are handle by rule Return_Type, so ignore here
                  null;
               when A_Formal_Object_Declaration =>
                  case Mode_Kind (Encl) is
                     when A_Default_In_Mode | An_In_Mode =>
                        Do_Report (D_Anonymous_Access_Constant, Encl);
                     when An_In_Out_Mode =>
                        Do_Report (D_Anonymous_Access_Variable, Encl);
                     when Not_A_Mode | An_Out_Mode =>
                        Failure ("Process_Access_Definition: bad mode");
                  end case;

               when An_Object_Renaming_Declaration =>
                  -- Renaming of an element of an anonymous access type
                  null;

               when others =>
                  Failure ("Process_Access_Definition: unexpected declaration for an access definition", Encl);
            end case;

         when A_Definition =>
            case Definition_Kind (Encl) is
               when A_Component_Definition =>
                  Do_Report (D_Anonymous_Access_Component, Enclosing_Element (Encl));
                  -- Enclosing_Element because we want the declaration
               when A_Type_Definition =>
                  -- the access definition is burried into some other type definition
                  -- like in "access function return access integer"
                  -- but we are interested only in the topmost declaration
                  null;
               when others =>
                  Failure ("Declarations: unexpected definition for an access definition", Encl);
            end case;
         when others =>
            Failure ("Declarations: unexpected element for an access definition", Encl);
      end case;
   end Process_Access_Definition;


   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Element : in Asis.Declaration) is
      use Asis, Asis.Elements, Asis.Statements, Utilities;
   begin
      if not (Rule_Used (D_Handlers) or Rule_Used (D_Non_Joint_CE_NE_Handler)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Statement_Kind (Element) is
         when An_Accept_Statement =>
            Check_Handlers (Accept_Body_Exception_Handlers (Element));
         when A_Block_Statement =>
            Check_Handlers (Block_Exception_Handlers (Element));
         when An_Extended_Return_Statement =>
            Check_Handlers (Extended_Return_Exception_Handlers (Element));
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
         Do_Report (D_Child_Unit, Unit_Declaration (Unit));
      end if;
   end Process_Unit;

begin  -- Rules.Declarations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
   Framework.Variables.Register (Limited_Initialization'Access,
                                 Rule_Id & ".LIMITED_INITIALIZATION");
end Rules.Declarations;
