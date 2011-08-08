with System;
separate (T_Uncheckable)
procedure Representation_Clauses is
   type T1 is range 1 .. 10;
   for T1'Size use Integer'Size;   -- Uncheckable, fractional_size

   type T2 is
      record
         I : Integer;
         J : Integer;
         K : Integer;
         L : Integer;
      end record;
   for T2 use
      record
         I at 0 range 0 .. 31;
         J at Integer'Size / System.Storage_Unit range 0 .. 31; -- uncheckable, non_contiguous record
         K at 8 range Integer'Size - Integer'Size .. 31;        -- uncheckable, non_contiguous record
         L at 12 range 0 .. Integer'Size - 1;                   -- uncheckable, non_contiguous record
      end record;
   for T2'Size use 4*Integer'Size;   -- Uncheckable, fractional_size
begin
   null;
end Representation_Clauses;
