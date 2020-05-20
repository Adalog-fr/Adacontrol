separate (T_Simplifiable_Statements)
procedure Test_If_Not is
   B : Boolean;
begin
   if not B then               -- negative condition
      null;
   else                        -- empty else path
      null;
   end if;

   if ((B /= False)) then      -- negative condition
      null;
   else                        -- empty else path
      null;
   end if;

   if B then
      null;
   else                        -- empty else path
      null;
   end if;

   if B = False then
      null;
   else                        -- empty else path
      null;
   end if;

   if B /= False then          -- Replaceable by case
      null;
   elsif B /= False then       -- Equivalent condition
      null;
   else                        -- empty else path
      null;
   end if;

   if B /= False then          -- Replaceable by case
      null;
   elsif B /= False then       -- Equivalent condition
      null;
   end if;

   if not B then
      null;
   end if;

end Test_If_Not;
