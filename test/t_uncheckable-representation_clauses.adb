with System;
separate (T_Uncheckable)
procedure Representation_Clauses is
   type T1 is range 1 .. 10;
   for T1'Size use Unknown'Size;   -- Uncheckable, fractional_size

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
         J at Unknown'Size / System.Storage_Unit range 0 .. 31; -- uncheckable, non_contiguous record
         K at 8 range Unknown'Size - Unknown'Size .. 31;        -- uncheckable, non_contiguous record
         L at 12 range 0 .. Unknown'Size - 1;                   -- uncheckable, non_contiguous record
      end record;
   for T2'Size use 4*Unknown'Size;   -- Uncheckable, fractional_size
begin
   null;
end Representation_Clauses;
