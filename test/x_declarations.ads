with Unchecked_Deallocation, Unchecked_Conversion;
package X_Declarations is
   function ItoF is new Unchecked_Conversion (Integer, Float);           -- Nested_Function_Instantiation
   type A_Integer is access Integer;                                     -- Access_Type

   package Pack is                                                       -- Nested_Package
      procedure Free is new Unchecked_Deallocation (Integer, A_Integer); -- Nested_Procedure_Instantiation
   end Pack;

   generic                                                               -- Generic, Nested_Generic_Package
   package Gen_Pack is
   end Gen_Pack;

   package Inst is new Gen_Pack;                                         -- Nested_Package_Instantiation

   Arr : array (1..10) of Integer;                                       -- Array, single_array
end X_Declarations;
