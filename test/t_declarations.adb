with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO, Ada.Finalization;
with X_Declarations.Child;
with X_Declarations_Locations;
procedure T_declarations is     -- library procedure, no_spec_procedure
   procedure Test_Anonymous_Subtype is separate;   -- separate, no_spec_procedure
   procedure Test_Self_SP;                         -- not_library_procedure, local_procedure
   procedure Test_Self_SP           is separate;   -- separate
   procedure Test_Constructors      is separate;   -- separate, no_spec_procedure

   type I1 is range 1 .. 10;      -- signed_type, integer_type
   type I2 is mod 128;            -- binary_modular_type, modular_type, integer_type
   type I3 is mod 127;            -- non_binary_modular_type, modular_type, integer_type
   VI1_1 : I1;                    -- variable, scalar_variable, uninitialized_variable
   VI1_2 : I1 range 1..2;         -- variable, scalar_variable, uninitialized_variable, anonymous_subtype_declaration

   type Fl is digits 5;                    -- float_type
   type Fx1 is delta 0.1 range 0.0 .. 1.0; -- ordinary_fixed_type_no_small, ordinary_fixed_type, fixed_type
   type Fx2 is delta 0.1 digits 5;         -- decimal_fixed_type, fixed_type

   type Enum is (A, B, 'c', D, 'e');       -- enumeration_type, character_literal x2
   V_Enum : Enum := A;                     -- variable, scalar_variable, initialized_variable

   task T1 is                     -- single_task, task_variable, task, non_ravenscar_task
     entry E (I : Integer := 1);  -- task_entry, defaulted_parameter
   end T1;
   task body T1 is
      procedure P is              -- not library procedure, task_body procedure, local procedure, no_spec_procedure
      begin
         null;                    -- null_procedure_body, null_procedure
      end;
   begin
      null;
   exception                      -- handlers
      when others =>
         null;
   end T1;

   task type T2 (X : Integer) is  -- task_type, task, non_ravenscar_task, task_discriminant, discriminant
     entry E;                     -- task_entry
   end T2;
   task body T2 is
   begin
      null;
   end T2;

   task type T3;                  -- task_type, task, non_ravenscar_task
   task body T3 is
   begin
      null;
   end T3;

   VT3 : T3;                      -- variable, task_variable, non_ravenscar_task

   protected P1 is                                     -- single_protected, protected_variable, protected
      entry E1 (I : out Integer; J : in out Integer);  -- protected_entry, out_parameter, in_out_parameter
      entry E2;                                        -- multiple_protected_entries, protected_entry
   end P1;
   protected body P1 is
      entry E1 (I : out Integer; J : in out Integer) when True  is --out_parameter, in_out_parameter
      begin
         null;
      exception                      -- handlers
         when others =>
            null;
      end E1;
      entry E2 when True is
      begin
         null;
      end E2;
   end P1;

   protected type P2 (X : Integer := 0) is  -- protected_type, protected, protected_discriminant, defaulted_discriminant, discriminant
      entry E1;                             -- protected_entry
      entry E2;                             -- multiple_protected_entries, protected_entry
   private
      I : Integer;                          -- uninitialized_protected_component
      J : Integer := 0;                     -- initialized_protected_component
   end P2;
   protected body P2 is
      entry E1 when True is
      begin
         null;
      end E1;
      entry E2 when True is
      begin
         null;
      end E2;
   end P2;

   VP2 : P2 (0);           -- variable, protected_variable, anonymous_subtype_declarations

   E : exception;         -- exception
   NN1 : constant := 1;   -- named_number
   NN2 : constant := 1.0; -- named_number

   type Acc1;                      -- incomplete_type
   type Acc1 is access Integer;    -- access_type
   type Acc2 is access procedure;  -- access_subprogram_type, access_type
   type Acc3 is access T2;         -- access_nondef_discriminated_type, access_task_type, access_type
   type Acc4 is access P2;         -- access_def_discriminated_type, access_protected_type, access_type

   type Der_Task is new T2;        -- derived_type
   type Acc5 is access Der_Task;   -- access_nondef_discriminated_type, access_task_type, access_type

   type Acc6 is access all Integer;                 -- access_type, access_all_type
   type Acc7 is access constant Integer;            -- access_type, access_constant_type
   type Acc8 is access Ada.Text_IO.File_Type;       -- access_language_type, access_type;
   type Acc9 is access Ada.Finalization.Controlled; -- access_language_type, access_type;

   I,J,K : aliased Integer;               -- variable x3, aliased_variable x3, scalar_variable x3, uninitialized_variable x3, multiple_names
   C : aliased constant Character := ' '; -- constant, aliased_constant

   type Rec1 is tagged;                                   -- tagged_incomplete_type
   type Rec1 is tagged null record;                       -- null_tagged_type, tagged_type, record_type
   type Rec2 (X : Integer) is tagged limited null record; -- null_tagged_type, tagged_type, record_type, discriminant
   type Rec3 is null record;                              -- null_ordinary_record_type, ordinary_record_type, record_type
   type Rec4 (X : Integer := 0) is                        -- ordinary_record_type, record_type, defaulted_discriminant, discriminant
      record
         case X is               -- variant_part
            when 0 =>
               I : Integer;      -- uninitialized_record_component
            when others =>
               J : Integer := 0; -- initialized_record_component
         end case;
      end record;
   type Rec5 is null record;  -- null_ordinary_record_type, ordinary_record_type, record_type
   type Rec6 is record        -- null_ordinary_record_type, ordinary_record_type, record_type
      null;
   end record;
   type Rec7 is            -- ordinary_record_type, record_type
      record
         I : Integer;      -- uninitialized_record_component
         J : Integer := 0; -- initialized_record_component
      end record;
   Vclass : Rec1'Class          := Rec1'(null record);        -- variable, class_wide_variable, tagged_variable
   Cclass : constant Rec1'Class := Rec1'(null record);        -- constant, class_wide_constant
   VRec1  : Rec1;                                             -- variable, tagged_variable, uninitialized_variable
   VRec3  : Rec3;                                             -- variable, ordinary_record_variable, uninitialized_variable
   VRec4  : Rec4;                                             -- variable, ordinary_record_variable, uninitialized_variable
   type Acc_Rec2 is access Rec2;                              -- access_nondef_discriminated_type, access_type
   type Acc_Rec3 is access Rec4;                              -- access_def_discriminated_type, access_type

   type Arr1 is array (1 .. 10) of Character;                 -- constrained_array_type, array, anonymous_subtype_declaration
   type Arr2 is array (Positive range <>) of Integer'Base;    -- unconstrained_array_type, array
   type Arr3 is new Arr2 (1..10);                             -- derived_type, anonymous_subtype_declaration x2
   subtype Subarr21 is Arr2;                                  -- subtype, unconstrained_subtype
   subtype Subarr22 is Arr2 (1 .. 3);                         -- subtype, anonymous_subtype_declaration
   subtype Subarr23 is Subarr22;                              -- subtype, unconstrained_subtype
   type Arr4 is new Subarr22;                                 -- derived_type
   VArr1 : array (1 .. 10) of Character;                      -- variable, single_array, constrained_array_variable, array, uninitialized_variable, anonymous_subtype_declaration
   Varr2 : Arr2 := (1, 2, 3);                                 -- variable, unconstrained_array_variable, array, initialized_variable
   Carr1 : constant Arr2 := Varr2;                            -- constant, unconstrained_array_constant, array
   Varr3 : array (Positive range <>) of Integer := (1, 2, 3); -- variable, single_array, unconstrained_array_variable, array, initialized_variable
   Varr4 : Subarr21 := (1,2, 3);                              -- variable, unconstrained_array_variable, array, initialized_variable
   Varr5 : Subarr23;                                          -- variable, constrained_array_variable, array, uninitialized_variable
   Carr2 : constant Subarr23 := Varr5;                        -- constant, constrained_array_constant, array
   type Acc_Arr1  is access Arr1;                              -- access_constrained_array_type, access_type
   type Acc_Arr2  is access Arr2;                              -- access_unconstrained_array_type, access_type
   type Acc_Arr22 is access Subarr22;                          -- access_constrained_array_type, access_type
   type Acc_Arr3  is access Arr3;                              -- access_constrained_array_type, access_type
   type Acc_Arr4  is access Arr4;                              -- access_constrained_array_type, access_type

   type Der1 is new Rec1 with null record;                   -- null_extension, extension, tagged_type, record_type
   type Der2 (Y : Integer) is new Rec1 with null record;     -- null_extension, extension, tagged_type, record_type, discriminant
   type Der3 (Y : Integer) is new Rec2 (Y) with null record; -- null_extension, extension, tagged_type, record_type, discriminant, anonymous_subtype_declaration
   type Der4 is new Rec3;                                    -- derived_type
   VDer1 : Der1;                                             -- variable, tagged_variable, uninitialized_variable
   VDer4 : Der4;                                             -- variable, ordinary_record_variable, uninitialized_variable

   type T_Float is digits 5;                                 -- float_type
   type T_Fixed1 is delta 0.01 range 0.0 .. 1.0;             -- ordinary_fixed_type_with_small, ordinary_fixed_type, fixed_type
   for T_Fixed1'Small use 0.01;
   type T_Fixed2 is delta 0.01 digits 7;                     -- decimal_fixed_type, fixed_type

   generic                                                          -- Not Library Generic_Procedure, generic
      I : Integer := 1;                                             -- defaulted_generic_parameter
   procedure P (J : Integer := 1; K : in out Float; L : out Float); -- Defaulted_Parameter, In_Out_Parameter, Out_Parameter
   procedure P (J : Integer := 1; K : in out Float; L : out Float) is begin null; end; -- null_procedure_body, null_procedure

   package Pack1 is private end Pack1;              -- not library package, empty_visible_part, empty_private_part
   package body Pack1 is
   end Pack1;

   package Pack2 is                                 -- not library package
      type Priv1 is private;                        -- Non_Limited_Private_Type
      type Priv2 (<>) is limited private;           -- Limited_Private_Type, Unknown_Discriminant, Discriminant
      type Ext1 is new Rec1 with private;           -- Private_Extension
      type Abs1 is abstract tagged private;         -- Tagged_Private_Type, Non_Limited_Private_Type, Abstract_Type
      type Abs2 is abstract tagged limited private; -- Tagged_Private_Type, Limited_Private_Type, Abstract_Type
      type Int1 is interface;                       -- Interface_Type
      procedure P (X : Abs1) is abstract;           -- Not Library Procedure, Public Procedure, Local Procedure, Abstract_Procedure
      function  F (Y : Abs2) return Integer is abstract;   -- Abstract_Function
      function  "+" (L : Abs1) return Integer is abstract; -- Operator, Abstract_Operator, Abstract_Function
      Deferred : constant Priv1;                    -- Constant, Deferred_Constant
      procedure P_As_Body;                          -- Not Library Procedure, Public Procedure, Local Procedure
      function  F_As_Body return Integer;
      function F_Expr (I : Integer) return Integer is -- Expression_Function
          (I+1);
      type Acc_Priv2 is access Priv2;               -- access_type (not access_unknown_discriminated_type because of full type)
   private
      type Priv1 is new Integer;                    -- Derived_Type
      type Priv2 is new Integer;                    -- Derived_Type
      type Ext1 is new Rec1 with record             -- Extension, Tagged_Type, Record_Type
         I : Integer;                               -- Uninitialized_Record_Component
      end record;
      type Abs1 is abstract tagged null record;     -- Null_Tagged_Type, Tagged_Type, Record_Type, Abstract_Type
      type Abs2 is abstract tagged limited          -- Tagged_Type, Record_Type, Abstract_Type
         record
            X : Integer;                            -- Uninitialized_Record_component
         end record;
      procedure Proc1;                              -- Not Library Procedure, Private Procedure, Local Procedure
      Deferred : constant Priv1 := 0;               -- Constant
   end Pack2;
   package body Pack2 is
      type Abs3 is abstract new Abs2 with null record;   -- Null_Extension, Extension, Tagged_Type, Record_Type, Abstract_Type
      procedure Proc1 is
      begin
         null;                                           -- null_procedure_body, Null_Procedure
      end Proc1;
      procedure Proc2 is                                 -- not library procedure, Own procedure, local procedure, no_spec_procedure
      begin
         declare
            procedure Proc3 is                           -- Not Library Procedure, Local Procedure, Block Procedure, no_spec_procedure
            begin
               null;                                     -- null_procedure_body, Null Procedure
            end Proc3;
         begin
            null;
         end;
      end Proc2;
      procedure P_As_Body renames Test_Self_SP;
      function F_Hidden return Integer is                -- No_Spec_Function
      begin
         return 0;
      end F_Hidden;
      function  F_As_Body return Integer renames F_Hidden;
   begin                                                   -- package_statements
      null;
   exception                                               -- handlers
      when Numeric_Error =>                                -- non_joint_CE_NE_handler
         null;
      when others =>
         null;
   end Pack2;

   function "+" (L : Arr2) return Arr2 is                -- operator #00046, no_spec_function
   begin
      return L;
   end "+";

   function "+" (X, Y : Integer) return Integer is       -- operator, predefined_operator, no_spec_function, multiple_names
   begin
      return I : Integer := 1 do
         I := I + 1;
      exception                                          -- handlers
         when others =>
            null;
      end return;
   exception                                             -- handlers
      when others =>
         null;
   end "+";

   function "-" (X, Y : Integer) return Integer;         -- Operator, Predefined_operator, multiple_names
   function "-" (X, Y : Integer) return Integer is       -- Multiple_names
   begin
      return 1;
   end "-";

   generic                                                                    -- Not Library Generic_Package, generic
      Global : in out Integer;                                                -- in_out_generic_parameter
      type T1 is private;                                                     -- formal type
      type T2(<>) is private;                                                 -- formal type
      with procedure Formal_P1;                                               -- formal_procedure
      with procedure Formal_P2 is <>;                                         -- formal_procedure, box_defaulted_formal_procedure
      with procedure Formal_P3 is Test_Self_SP;                               -- formal_procedure, name_defaulted_formal_procedure
      with procedure Formal_P4 is null;                                       -- formal_procedure, null_defaulted_formal_procedure
      with function  Formal_F1 return Integer;                                -- formal_function
      with function  Formal_F2 return Integer is <>;                          -- formal_function, box_defaulted_formal_function
      with function  Formal_F3 return Integer is Pack2.F_As_Body;             -- formal_function, name_defaulted_formal_function
      with package EF is new Ada.Numerics.Generic_Elementary_Functions (<>);  -- formal_package
   package Test_Formals is private end;                                       -- empty_visible_part, empty_private_part
   package body Test_Formals is
      procedure Inner is begin null; end;                                     -- not library procedure, in_generic procedure, own procedure, local procedure, no_spec_procedure, null_procedure_body, null_procedure
      type Acc_T1 is access T1;                                               -- access_formal_type, access_type
      type Acc_T2 is access T2;                                               -- access_unknown_discriminated_type, access_formal_type, access_type;
   begin
      null;                                                                   -- package statements
   end Test_Formals;

   subtype Int1 is Integer range 1..10;                                 -- subtype
   subtype Int2 is Integer;                                             -- subtype, unconstrained_subtype
   V_Int1 : Int1;                                                       -- variable, scalar_variable, uninitialized_variable
   V_Int2 : Int2 range 1..10;                                           -- variable, scalar_variable, uninitialized_variable, anonymous_subtype_declaration

   procedure Predefined_Operator is separate;                           -- separate, no_spec_procedure

   type Al1 is array (Int1) of aliased Character;                       -- constrained_array_type, array, aliased_array_component
   type Al2 is array (Positive range <>) of aliased Character;          -- unconstrained_array_type, array, aliased_array_component
   Al3 : array (Int1) of aliased Character := (others => ' ');          -- variable, single_array, aliased_array_component, constrained_array_variable, array, initialized_variable

   type Al4 is                                                          -- ordinary_record_type, record_type
      record
         F1 : Integer := 0;                                             -- initialized record component
         F2 : aliased Integer := 1;                                     -- initialized_record_component, aliased_record_component
      end record;

   protected Al5 is                                                     -- single_protected, protected_variable, protected
   private
      Y : aliased Integer := 2;                                         -- initialized_protected_component, aliased_protected_component
   end Al5;
   protected body Al5 is
   end Al5;

   procedure Null_2005 is null;                                         -- null_procedure_declaration, null_procedure, not library_procedure, local_procedure
   function "=" (L : Rec1; R : Rec1) return Boolean is                  -- operator, predefined_operator, equality_operator, no_spec_function
   begin
      return False;
   end "=";
begin
   begin                                                                -- null_procedure_body, null_procedure
      null;
   exception                                                            -- handlers
      when Constraint_Error | Numeric_Error =>
         null;
      when others =>
         null;
   end;
exception                                                               -- handlers
   when Constraint_Error =>                                             -- non_joint_CE_NE_handler
      null;
   when others =>
      null;
end T_declarations;
