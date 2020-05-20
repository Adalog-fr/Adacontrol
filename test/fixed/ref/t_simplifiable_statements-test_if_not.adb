separate (T_Simplifiable_Statements)
procedure Test_If_Not is
   B : Boolean;
begin
   if not B then               -- negative condition
      null;
   
   end if;

   if ((B /= False)) then      -- negative condition
      null;
   
   end if;

   if B then
      null;
   
   end if;

   if B = False then
      null;
   
   end if;

   if B /= False then          -- Replaceable by case
      null;
   
   
   end if;

   if B /= False then          -- Replaceable by case
      null;
   
   end if;

   if not B then
      null;
   end if;

end Test_If_Not;
