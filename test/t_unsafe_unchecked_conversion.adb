with Unchecked_Conversion;
procedure T_Unsafe_Unchecked_Conversion is
   type T1 is range 1 .. 10;
   type T2 is (A, B, C, D);
   function Inst1 is new Unchecked_Conversion (T1, T2);      -- No size specified

   type T3 is range 1 .. 10;
   for T3'Size use 32;
   function Inst2 is new Unchecked_Conversion (T3, Integer); -- One size not specified

   type T4 is
      record
         V : Integer;
      end record;
   for T4'Size use 32;
   function Inst3 is new Unchecked_Conversion (T3, T4);      -- OK

   type T5 is array (1..1) of Integer;
   for T5'Size use T3'Size;
   function Inst4 is new Unchecked_Conversion (T5, T3);      -- OK

   type T6 is range 1 .. 10;
   for T6'Size use 8;
   function Inst5 is new Unchecked_Conversion (T6, T4);      -- Different sizes
begin
   null;
end T_Unsafe_Unchecked_Conversion;
