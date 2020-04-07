separate (T_Simplifiable_Statements)
procedure Test_True_If is
   One : constant Integer := 1;
   I   : Integer          := 0;
begin
   I := 1;

   I := 4;

   if I = One then    -- not static
      I := 1;
   else I := 2;
   end if;

   if I = One then    -- not static
      I := 1;
   
   else I := 2;
   end if;

   
   if I = One then -- not static
      I := 1;
   else
      I := 4;
   end if;

end Test_True_If;
