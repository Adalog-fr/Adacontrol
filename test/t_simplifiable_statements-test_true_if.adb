separate (T_Simplifiable_Statements)
procedure Test_True_If is
   One : constant Integer := 1;
   I   : Integer          := 0;
begin
   if One = 1 then    -- true if
      I := 1;
   else               -- dead
      I := 2;
   end if;

   if One = 2 then    -- dead
      I := 3;
   else               -- true if
      I := 4;
   end if;

   if I = One then    -- not static
      I := 1;
   elsif One = 1 then -- true if
      I := 2;
   elsif One = 2 then -- dead
      I := 3;
   else               -- dead
      I := 4;
   end if;

   if I = One then    -- not static
      I := 1;
   elsif One = 2 then -- dead
      I := 3;
   elsif One = 1 then -- true if
      I := 2;
   else               -- dead
      I := 4;
   end if;

   if One = 2 then    -- dead
      I := 3;
   elsif I = One then -- not static
      I := 1;
   else
      I := 4;
   end if;

end Test_True_If;
