separate (T_Assignments)
procedure Sliding is

   function F return String is
   begin
      return "ABCD";
   end F;
   procedure Make_Other_Group is begin null; end;  -- To prevent the subrule repeated from complaining
begin
One_Dimensional :
   declare
      S1 : String (1 .. 10);
      S2 : String (2 .. 11);
      subtype From_2 is String (2 .. 11);
      I,J : Integer := 1;
   begin
      S1 := (1 => 'a', others => ' ');   -- OK
      S2 := From_2'("1234567890");
      Make_Other_Group;
      S1 := (3 => 'a', others => ' ');   -- OK
      Make_Other_Group;
      S1 := F;                           -- OK (not static)
      Make_Other_Group;
      S1 := (2 .. 11 => ' ');            -- Sliding
      Make_Other_Group;
      S1 := (2 => ' ', 6 .. 11 => ' ' , 3 .. 5 => ' ');                            -- Sliding
      Make_Other_Group;
      S1 := (4 .. 10 => ' ', 3 => ' ', 1 .. 2 => ' ');                             -- OK
      Make_Other_Group;
      S1 := (2 => 'a', 3 .. 5 => 'c', 8 => 'd', 11 => 'b', 6 | 7 | 9 | 10 => 'z'); -- Sliding
      S2 := (1 => 'a', 3 .. 5 => 'c', 8 => 'd', 2  => 'b', 6 | 7 | 9 | 10 => 'z'); -- Sliding
      Make_Other_Group;
      S1 := From_2'("1234567890");        -- Sliding
      Make_Other_Group;
      S1 := From_2 (S1);                  -- Sliding
      Make_Other_Group;
      S1 := S2;                           -- Sliding
      S2 := "2345678901";                 -- Sliding

      J := I + 1;
      S1 (I .. I + 2) := S2 (J .. J + 2); -- Sliding (almost static)
   end One_Dimensional;

Two_Dimensional :
   declare
      type Tab1 is array (Integer range <>, Integer range <>) of Integer;
      T1 : Tab1 (1 .. 3, 1 .. 3);
      T2 : Tab1 (2 .. 4, 2 .. 4);
   begin
      T1 := (1 .. 3 => (1 .. 3 => 0));                        -- OK
      Make_Other_Group;
      T1 := (1 => (0,2,3), 2 => (4,5,6), 3 => (7,8,9));       -- OK
      Make_Other_Group;
      T1 := (2 => (0, 2, 3), 3 => (4, 5, 6), 4 => (7, 8, 9)); -- Sliding
      T2 := T1;                                               -- Sliding
   end Two_Dimensional;
end Sliding;
