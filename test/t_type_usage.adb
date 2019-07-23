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

   type Index is range 1 .. 10;
   type Table_Pack1 is array (Index) of Character with Pack;
   type Table_Pack2 is array (Index) of Character with Pack => True;
   type Table_Pack3 is array (Index) of Character with Pack => False;
   type Table_Size1 is array (Enum1) of Character with Size => 100; -- Size
   type Table_Size2 is array (Enum1) of Character; -- Size
   for Table_Size2'Size use 100;
   type Table_Size3 is array (Integer range 1..10) of Character with Size => 10*Character'Size; -- Index_Integer
   type Table_Component_Size1 is array (Index) of Character with Component_Size => 10;
   T   : Table_Pack1;
   Id  : Index;
   Rep : Integer;
begin
   E   := Enum1'Last;      -- OK
   E   := Enum1'First;     -- First
   Int := Integer'Last;    -- Last
   Int := Enum2'Pos (V);   -- Pos_Repr
   Int := Enum3'Pos (Yes); -- Pos_Size
   Int := Enum4'Pos (V);   -- Pos_Repr, Pos_Repr_Size
   Id  := Table_Pack1'First;   -- Pack
   Id  := Table_Pack2'First;   -- Pack
   Id  := Table_Pack3'First;   -- OK
   Rep := Enum2'Enum_Rep(V);
   Id  := Table_Component_Size1'First;   -- Component_Size
end;
