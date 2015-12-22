separate (T_Representation_Clauses)
procedure Non_Contiguous_Unaligned is
   type Rec1 is
      record
         I : Integer;
         S : String (1 .. 3);
         C : Character;
         F : Float;
      end record;

   -- Normal clause
   for Rec1 use                     -- record, bit_order
      record
         I at 0 range 8 .. 39;      -- gap at beginning
         S at 6 range 0 .. 23;      -- gap before
         C at 6 range 33 .. 40;     -- gap before, unaligned
         F at 6 range 41 .. 72;     -- unaligned, gap at end
      end record;
   for Rec1'Size use 122;            -- 'Size, not multiple, non_power2

   type Rec2 is
      record
         I : Integer;
         S : String (1 .. 3);
         C : Character;
      end record;
   for Rec2'Size use 96;            -- 'Size, non_power2

   -- Unordered components
   for Rec2 use                     -- record, bit_order
      record
         C at 6 range 33 .. 40;     -- gap before, gap at end, unaligned
         S at 6 range 0 .. 23;      -- gap before
         I at 0 range 8 .. 39;      -- gap at beginning
      end record;
begin
   null;
end Non_Contiguous_Unaligned;
