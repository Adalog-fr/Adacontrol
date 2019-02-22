separate (T_Simplifiable_Statements)
procedure Test_For_In_For_For_Of is
   S1 : String (1 .. 10);
   S2 : String (1 .. 10);
   X  : Character;

begin
   L1 : for I in S1'Range loop  -- for_in_for_for_of
      X := S1 (I);
      X := S1 (I);
      X := S1 (L1.I);
      declare
         Inx : Integer renames I;
      begin
         X := S1 (Inx);
      end;
   end loop L1;

   for I in 1 .. 9 loop
      X := S1 (I + 1);
   end loop;

   for I in Integer range 1 .. 10 loop
      X := S1 (I);
      X := S2 (I);
   end loop;

   for I in S1'Range loop
      X := S1 (I);
      declare
         Inx : Integer renames I;
      begin
         X := S2 (Inx);
      end;
   end loop;

   for I in 1 .. 10 loop
      null;
   end loop;
end Test_For_In_For_For_Of;
