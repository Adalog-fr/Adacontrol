separate (T_Simplifiable_Statements)
procedure Test_Null is
   I : Integer;
begin
   if I = 1 then
        -- null
      null;
   
   end if;

   if I = 1 then
      <<Label1>> null;
         -- null
   end if;

   if I = 1 then
         -- null
      <<Label2>> null;
   end if;

   if I = 1 then
      I := 2;
        -- null
   end if;

   if I = 1 then
        -- null
      I := 2;
   end if;
end Test_Null;
