separate (T_Uncheckable)
procedure Record_Declarations is
   type T1 is
      record
         S : String (1 .. 10);          -- unaligned array
      end record;
   for T1 use
      record
         S at 0 range Unknown'Size .. Unknown'Size + 10 * Character'Size - 1;  -- uncheckable, false positive
      end record;
   type T2 is
      record
         I : Integer;                   -- aligned range
      end record;
   for T2 use
      record
         I at 0 range Unknown'Size .. Unknown'Size + Integer'Size - 1;         -- uncheckable, false positive
      end record;
begin
   null;
end Record_Declarations;
