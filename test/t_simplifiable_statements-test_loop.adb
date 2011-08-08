separate (T_Simplifiable_Statements)
procedure Test_Loop is
   I : Integer := 1;
begin
   while True loop -- loop
      null;
   end loop;
   while False loop -- never executed
      null;
   end loop;
   while I < 2 loop
      I := I + 1;
   end loop;
end Test_Loop;
