separate (T_Expressions)
procedure Test_Range is
   C : constant Integer := 5;
   subtype Index1 is Integer range 1 .. 10;
   type Index2 is range 1 .. 10;
   type Index3 is new Index2 range 1..5;

   type TTab1 is array (1 .. 10) of Integer;   -- universal_range
   type TTab2 is array (1..C) of Integer;
   Tab1 : array (1..10) of Integer;            -- universal_range
   Tab2 : array (1..C) of Integer;
begin
   for I in 1 .. 100 loop                      -- universal_range
      null;
   end loop;
   for I in C .. 100 loop
      null;
   end loop;
end Test_Range;
