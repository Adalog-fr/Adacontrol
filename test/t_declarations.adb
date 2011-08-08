with Ada.Numerics.Generic_Elementary_Functions;
with X_Declarations.Child;
with X_Declarations_Locations;
procedure T_declarations is
   type I1 is range 1..10;      -- signed_type, integer_type
   type I2 is mod 128;          -- modular_type, integer_type

   type Fl is digits 5;                    -- float_type
   type Fx1 is delta 0.1 range 0.0 .. 1.0; -- ordinary_fixed_type, fixed_type
   type Fx2 is delta 0.1 digits 5;         -- decimal_fixed_type, fixed_type

   type Enum is (A, B, 'c', D, 'e');  -- enumeration_type, character_literal x2

   task T1 is                     -- single_task, task
     entry E (I : Integer := 1);  -- task_entry, defaulted_parameter
   end T1;
   task body T1 is
      procedure P is              -- task_body procedure, nested procedure, local procedure
      begin
         null;                    -- null_procedure
      end;
   begin
      null;
   exception                      -- handlers
      when others =>
         null;
   end T1;

   task type T2 (X : Integer) is  -- task_type, task, discriminant
     entry E;                     -- task_entry
   end T2;
   task body T2 is
   begin
      null;
   end T2;

   protected P1 is                                     -- single_protected, protected
      entry E (I : out Integer; J : in out Integer);   -- protected_entry, out_parameter, in_out_parameter
   end P1;
   protected body P1 is
      entry E (I : out Integer; J : in out Integer) when True  is --out_parameter, in_out_parameter
      begin
         null;
      end E;
   end P1;

   protected type P2 (X : Integer := 0) is  -- protected_type, protected, defaulted_discriminant
      entry E;                              -- protected_entry
   private
      I : Integer;                          -- uninitialized_protected_field
      J : Integer := 0;                     -- initialized_protected_field
   end P2;
   protected body P2 is
      entry E when True is
      begin
         null;
      end E;
   end P2;

   E : exception;         -- exception
   NN1 : constant := 1;   -- named_number
   NN2 : constant := 1.0; -- named_number

   type Acc1 is access Integer;    -- access_type
   type Acc2 is access procedure;  -- access_subprogram_type, access_type
   type Acc3 is access T2;         -- access_task_type, access_type
   type Acc4 is access P2;         -- access_protected_type, access_type

   type Der_Task is new T2;        -- derived_type
   type Acc5 is access Der_Task;   -- access_task_type, access_type

   I,J,K : aliased Integer;               -- aliased, multiple_names, uninitialized_variable, variable
   C : aliased constant Character := ' '; -- aliased

   type Rec1 is tagged null record;                       -- null_tagged_type, record_type
   type Rec2 (X : Integer) is tagged limited null record; -- null_tagged_type, record_type, discriminant
   type Rec3 is null record;                              -- null_ordinary_record_type, record_type
   type Rec4 (X : Integer := 0) is                        -- ordinary_record_type, record_type, defaulted_discriminant
      record
         case X is               -- variant_part
            when 0 =>
               I : Integer;      -- uninitialized_record_field
            when others =>
               J : Integer := 0; -- initialized_record_field
         end case;
      end record;
   type Rec5 is null record;  -- null_ordinary_record_type, record_type
   type Rec6 is record        -- null_ordinary_record_type, record_type
      null;
   end record;
   type Rec7 is            -- ordinary_record_type, record_type
      record
         I : Integer;      -- uninitialized_record_field
         J : Integer := 0; -- initialized_record_field
      end record;
   type Arr1 is array (1..10) of Character;           -- constrained_array_type, array
   type Arr2 is array (Positive range <>) of Integer; -- unconstrained_array_type, array
   VArr1 : array (1..10) of Character;                -- single_array, array, uninitialized_variable, variable

   type Der1 is new Rec1 with null record;                   -- null_extension, record_type
   type Der2 (Y : Integer) is new Rec1 with null record;     -- null_extension, record_type, discriminant
   type Der3 (Y : Integer) is new Rec2 (Y) with null record; -- null_extension, record_type, discriminant
   type Der4 is new Rec3;                                    -- derived_type

   type T_Float is digits 5;                                 -- float_type
   type T_Fixed1 is delta 0.01 range 0.0 .. 1.0;             -- ordinary_fixed_type, fixed_type
   type T_Fixed2 is delta 0.01 digits 7;                     -- decimal_fixed_type, fixed_type

   generic                                                          -- Nested_Generic_Procedure, generic
      I : Integer := 1;                                             -- defaulted_generic_parameter
   procedure P (J : Integer := 1; K : in out Float; L : out Float); -- Defaulted_Parameter, Out_Parameter, In_Out_Parameter
   procedure P (J : Integer := 1; K : in out Float; L : out Float) is begin null; end; -- null_procedure

   package Pack1 is end Pack1;                      -- nested_package
   package body Pack1 is
   end Pack1;

   package Pack2 is                                 -- nested_package
      type Priv1 is private;                        -- Non_Limited_Private_Type
      type Priv2 is limited private;                -- Limited_Private_Type
      type Ext1 is new Rec1 with private;           -- Non_Limited_Private_Extension
      type Abs1 is abstract tagged private;         -- Non_Limited_Private_Type, Abstract_Type
      type Abs2 is abstract tagged limited private; -- Non_Limited_Private_Type, Abstract_Type
      procedure P (X : Abs1) is abstract;           -- Public Procedure, Nested Procedure, Local Procedure, Abstract_Procedure
      function  F (Y : Abs2) return Integer is abstract; -- Abstract_Function
   private
      type Priv1 is new Integer;                    -- Derived_Type
      type Priv2 is new Integer;                    -- Derived_Type
      type Ext1 is new Rec1 with null record;       -- Null_Extension, Record_Type
      type Abs1 is abstract tagged null record;     -- Null_Tagged_Type, Tagged_Type, Record_Type, Abstract_Type
      type Abs2 is abstract tagged limited          -- Tagged_Type, Record_Type, Abstract_Type
         record
            X : Integer;                            -- Uninitialized_Record_Field
         end record;
      procedure Proc1;                              -- Private Procedure, Nested Procedure, Local Procedure
   end Pack2;
   package body Pack2 is
      type Abs3 is abstract new Abs2 with null record; -- Null_Extension, Extension, Tagged_Type, Record_Type, Abstract_Type
      procedure Proc1 is                                -- Own procedure, nested procedure, local procedure
      begin
         null;                                          -- Null_Procedure
      end Proc1;
      procedure Proc2 is                                -- Own procedure, nested procedure, local procedure
      begin
         declare
            procedure Proc3 is                          -- Nested Procedure, Local Procedure, Block Procedure
            begin
               null;                                    -- Null Procedure
            end Proc3;
         begin
            null;
         end;
      end Proc2;
   begin
      null;
   end Pack2;

   package Pack3 renames Pack2;                          -- renaming, non_identical_renaming
   generic package Generic_Elementary_Functions
      renames Ada.Numerics.Generic_Elementary_Functions; -- renaming, Not_Operator_Renaming

   procedure Sep is separate;

   Tab : array (1..10) of Integer;                       -- uninitialized_variable, variable

   function "+" (X, Y : Integer) return Integer is
   begin
      return 1;
   end "+";

   function "-" (X, Y : Integer) return Integer;         -- Predefined_operator
   function "-" (X, Y : Integer) return Integer is
   begin
      return 1;
   end "-";

   function F1  (X, Y : Integer) return Integer renames "+";            -- renaming, operator_renaming, non_identical_renaming, multiple_names
   function F2  (X, Y : Integer) return Integer renames Standard."+";   -- renaming, operator_renaming, non_identical_renaming, multiple_names
   function "*" (X, Y : Integer) return Integer renames Standard."*";   -- renaming, operator_renaming, multiple_names

   generic                                                                    -- Nested_Generic_Function, generic
      Global : in out Integer;                                                -- in_out_generic_parameter
      with procedure Formal_P;                                                -- formal_procedure
      with function Formal_F return Integer;                                  -- formal_function
      with package EF is new Ada.Numerics.Generic_Elementary_Functions (<>);  -- formal_package
   function Test_Formals return Integer;
   function Test_Formals return Integer is
   begin
      return 0;
   end Test_Formals;

   subtype Int is Integer range 1..10;                                  -- subtype

   Arr : Integer renames X_Declarations.Arr (1);                        -- renaming, non_identical_renaming, not_operator_renaming
   function Succ (X : Integer) return Integer renames Integer'Succ;     -- renaming, non_identical_renaming, not_operator_renaming
   function "/" (X, Y : Integer) return Integer renames Standard."+";   -- renaming, operator_renaming, non_identical_renaming, non_identical_operator_renaming, multiple_names

   procedure Predefined_Operator is separate;                                 -- separate
begin
   null;                                                                -- null_procedure
end T_declarations;
