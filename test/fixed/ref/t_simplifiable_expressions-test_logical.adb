separate (T_Simplifiable_Expressions)
procedure Test_Logical is
   X : Boolean := True;

   function Y return Boolean is
   begin
      return True;
   end Y;

   A : Boolean := Y = False;                    -- Not Expr
   B : constant Boolean := Y = False;           -- Not Expr

   procedure Z (P : in Boolean := Y = False) is -- Not Expr
   begin
      A := B;
   end Z;

   type T is mod 5;
   I : T;
   C : constant T := 2;

begin

   if X = False and then Y = True then        -- Not Expr, Just Expr, Parentheses
      Z;
   elsif X = False then                         -- Not Expr
      X := Y = False;                           -- Not Expr
   elsif X /= False then                        -- Just Expr
      null;
   elsif X = True then                          -- Just Expr
      null;
   elsif X /= True then                         -- Not Expr
      null;
   elsif False = X then                         -- Not Expr
      X := Y = False;                           -- Not Expr
   elsif False /= X then                        -- Just Expr
      null;
   elsif True = X then                          -- Just Expr
      null;
   elsif True /= X then                         -- Not Expr
      null;
   end if;

   while Y = False and X = False loop           -- Not Expr (twice)
      A := B;
   end loop;

   if not (I = 3) then                          -- Not Expr
      null;
   end if;
   I := not I;
   I := not (I and 2);

   if I < 1 then                   -- Redundant
      null;
   elsif I > C then              -- Redundant
      null;
   end if;

   if I <= 1 then            -- Redundant
      null;
   elsif I > C then         -- Redundant
      null;
   end if;

   if Test_Logical.I <= 1 then        -- Redundant
      null;
   end if;

   if I >= 1 then        -- Redundant
      null;
   end if;
end Test_Logical;
