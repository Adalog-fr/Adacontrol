with Unchecked_Deallocation, Unchecked_Conversion;
with Ada.Finalization;
package X_Declarations is
   function ItoF is new Unchecked_Conversion (Integer, Float);           -- Nested_Function_Instantiation
   type A_Integer is access Integer;                                     -- Access_Type

   package Pack is                                                       -- Nested_Package
      procedure Free is new Unchecked_Deallocation (Integer, A_Integer); -- Nested_Procedure_Instantiation
   end Pack;

   generic                                                               -- Generic, Nested_Generic_Package, empty_visible_part
   package Gen_Pack is
   end Gen_Pack;

   package Inst is new Gen_Pack;                                         -- Nested_Package_Instantiation

   Arr : array (1..10) of Integer;                                       -- Array, single_array, variable, constrained_array_variable

   package Af renames Ada.Finalization;                                 -- not_operator_renaming, non_identical_renaming, library_unit_renaming, renaming
   type Cont1 is new Ada.Finalization.Controlled with null record;      -- controlled_type
   type Cont2 is new Af.Limited_Controlled with                         -- controlled_type
      record
         I : Integer;                                                   -- Uninitialized_record_field
      end record;
end X_Declarations;
