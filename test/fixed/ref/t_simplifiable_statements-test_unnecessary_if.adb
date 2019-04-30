separate (T_Simplifiable_Statements)
function Test_Unnecessary_If return Boolean is
   V1 : Boolean;
   V2 : Boolean;
   V3 : Integer;
begin
   V1 := V3 = 1;
   return V3 = 1;
   V1 := V3 <= 1;
   return V3 < 1;
   V1 := V3 = 1;
   if V3 = 1 then         -- Unnecessary, same value
      V1 := True;
   else
      V1 := True;
   end if;

   if V3 = 1 then         -- Unnecessary, same value
      return False;
   else
      return False;
   end if;

   return not (V3 = 1 and V1);
   return not (V3 = 1 or else V1);
   if V3 = 1 then         -- OK, not same variable
      V1 := True;
   else
      V2 := False;
   end if;

   if V3 = 1 then         -- OK, not static
      V1 := True;
   else
      V1 := V3 = 1;
   end if;

end Test_Unnecessary_If;
