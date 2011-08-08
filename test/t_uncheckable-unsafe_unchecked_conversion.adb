with Unchecked_Conversion;
separate (T_Uncheckable)
procedure Unsafe_Unchecked_Conversion is
   type T1 is
      record
         V : Integer;
      end record;
   for T1'Size use 32;
   type T2 is range 1 .. 10;
   for T2'Size use Unknown'Size;
   function Inst is new Unchecked_Conversion (T1, T2);      -- Uncheckable, False positive
begin
   null;
end Unsafe_Unchecked_Conversion;
