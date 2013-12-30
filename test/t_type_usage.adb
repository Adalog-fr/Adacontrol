procedure T_Type_Usage is
   type Enum1 is (A, B, C);
   type Enum2 is (I, V, X, C);
   for Enum2 use (I => 1, V => 5, X => 10, C => 100);
   type Enum3 is (Yes, No, Maybe);
   for Enum3'Size use 32;
   type Enum4 is new Enum2;
   for Enum4'Size use 32;

   type Modular is mod 256;

   E : Enum1;
   Int : Integer := Integer'First; -- OK

   type Arr is array (Enum1 range <>, Modular range <>, Positive range <>) of Character; -- Index_Mod, Index_Integer
   V1 : array (Modular) of Integer;          -- Index_Mod
   V2 : array (1..10)   of Integer;          -- Index_Integer
begin
   E   := Enum1'Last;      -- OK
   E   := Enum1'First;     -- First
   Int := Integer'Last;    -- Last
   Int := Enum2'Pos (V);   -- Pos_Repr
   Int := Enum3'Pos (Yes); -- Pos_Size
   Int := Enum4'Pos (V);   -- Pos_Repr, Pos_Repr_Size
end;
