with Unchecked_Deallocation, Unchecked_Conversion;
with Ada.Finalization;
package X_Declarations is
   function ItoF is new Unchecked_Conversion (Integer, Float);           -- Not library Function_Instantiation
   type A_Integer is access Integer;                                     -- Access_Type

   package Pack is                                                       -- Not library Package
      procedure Free is new Unchecked_Deallocation (Integer, A_Integer); -- Not library Procedure_Instantiation
   end Pack;

   generic                                                               -- Not library Generic_Package, Generic
   package Gen_Pack is                                                   -- empty_visible_part
   end Gen_Pack;

   package Inst is new Gen_Pack;                                         -- Not library Package_Instantiation

   Arr : array (1..10) of Integer;                                       -- variable, single_array, constrained_array_variable, Array, uninitialized_variable, anonymous_subtype_declaration

   package Af renames Ada.Finalization;
   type Cont1 is new Ada.Finalization.Controlled with null record;      -- controlled_type, null_extension, extension, tagged_type, record_type
   type Cont2 is new Af.Limited_Controlled with                         -- controlled_type, extension, tagged_type, record_type
      record
         I : Integer;                                                   -- Uninitialized_record_component
      end record;
end X_Declarations;
