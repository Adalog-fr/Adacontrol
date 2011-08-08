separate (T_Object_Declarations)
procedure Test_Min_Integer_Span is
   V1 : Integer;
   V2 : Integer range 1 .. 4;                -- check, count
   C1 : constant Integer := 1;
   C2 : constant Integer range 1 .. 4 := 1;  -- check, count

   type Modular is mod 255;
   V3 : Modular;
   V4 : Modular range 1 .. 5;                -- check, count
   C3 : constant Modular := 0;
   C4 : constant Modular range 1 .. 5 := 1;  -- count

   type Small is range 1 .. 8;
   V5 : Small;                               -- check

   Tab : array (1 .. 10) of Integer;

   type Enum is (A, B, C, D, E, F, G, H, I, J, K, L);
   V6 : Enum;

begin
   null;
end Test_Min_Integer_Span;
