separate (T_Simplifiable_Expressions)
procedure Test_Logical is
   X : Boolean := True;

   function Y return Boolean is
   begin
      return True;
   end Y;

   A : Boolean := Y = False;               -- Should trigger
   B : constant Boolean := Y = False;      -- Should trigger

   procedure Z (P : in Boolean := Y = False) is -- Should trigger
   begin
      A := B;
   end Z;

   type T is mod 5;
   I : T;

begin

   if X = False and then Y = True then  -- Should trigger (three times)
      Z;
   elsif X = False then                   -- Should trigger
      X := Y = False;                     -- Should trigger
   elsif X /= False then                  -- Should trigger
      null;
   elsif X = True then                    -- Should trigger
      null;
   elsif X /= True then                   -- Should trigger
      null;
   elsif False = X then                   -- Should trigger
      X := Y = False;                     -- Should trigger
   elsif False /= X then                  -- Should trigger
      null;
   elsif True = X then                    -- Should trigger
      null;
   elsif True /= X then                   -- Should trigger
      null;
   end if;

   while Y = False and X = False loop     -- Should trigger (twice)
      A := B;
   end loop;

   if not (I = 3) then                    -- Should trigger
      null;
   end if;
   I := not I;
   I := not (I and 2);

end Test_Logical;

