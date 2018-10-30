separate (T_Simplifiable_Statements)
procedure Test_Loop is
   I : Integer := 1;
begin
   while True loop -- loop
      null;
   end loop;
   while 1=1 loop -- loop
      null;
   end loop;
   while False loop -- never executed
      null;
   end loop;
   while I < 2 loop -- while_for_for
      I := I + 1;
   end loop;
   loop             -- loop_for_while
      exit when I = 1;
   end loop;
   L1 : loop
      loop
         exit L1;
      end loop;
   end loop L1;
end Test_Loop;
