separate (T_Representation_Clauses)
procedure Fractional_Size is
   type T1 is range 1 .. 10;
   type T2 is range 1 .. 10;
   type T3 is range 1 .. 10;
   type T4 is range 1 .. 10;

   for T1'Size use 8;               -- 'Size
   for T2'Size use 9;               -- 'Size, not multiple, non_power2
   for T3'Size use 2 ** 5;          -- 'Size
   for T4'Size use T3'Size*2 - 1;   -- 'Size, not multiple

   V1 : T1;
   V2 : T2;

   for V1'Size use 16;              -- object 'Size, 'Size
   for V2'Size use T3'Size * 2;     -- object 'Size, 'Size
begin
   null;
end Fractional_Size;
