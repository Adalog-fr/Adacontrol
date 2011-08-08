separate (T_Representation_Clauses)
procedure Non_Contiguous_Record is
   type Rec1 is
      record
         I : Integer;
         S : String (1 .. 3);
         C : Character;
      end record;

   -- Normal clause
   for Rec1 use                     -- record repr
      record
         I at 0 range 8 .. 39;      -- gap at beginning
         S at 6 range 0 .. 23;      -- gap before
         C at 6 range 33 .. 40;     -- gap before, gap at end
      end record;
   for Rec1'Size use 90;            -- 'Size, not multiple

   type Rec2 is
      record
         I : Integer;
         S : String (1 .. 3);
         C : Character;
      end record;
   for Rec2'Size use 96;            -- 'Size

   -- Unordered components
   for Rec2 use                     -- record
      record
         C at 6 range 33 .. 40;     -- gap before, gap at end
         S at 6 range 0 .. 23;      -- gap before
         I at 0 range 8 .. 39;      -- gap at beginning
      end record;
begin
   null;
end Non_Contiguous_Record;
