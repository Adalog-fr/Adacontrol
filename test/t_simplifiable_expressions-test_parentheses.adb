separate (T_Simplifiable_Expressions)
procedure Test_Parentheses is
   I : Integer := 1;   -- OK
   J : Integer := (1); -- Should Trigger
   B : Boolean;
   procedure P (X : Integer) is
   begin
      null;
   end P;
begin
   -- Statements

   I := (1); -- Should Trigger
   I := 1;   -- OK

   if (B) then     -- Should Trigger
      null;
   elsif (B) then  -- Should Trigger
      null;
   end if;
   if B then       -- OK
      null;
   elsif B then    -- OK
      null;
   end if;

   case (B) is     -- Should Trigger
      when others =>
         null;
   end case;
   case B is       -- OK
      when others =>
         null;
   end case;

   P ((1)); -- Should Trigger
   P (1);   -- OK

   -- Arithmetic
   I := 1 + (2*3);       -- Should Trigger
   I := (1) + 2*3;       -- Should Trigger
   I := 1 * (2+3);       -- OK
   I := 1 * ("+"(2, 3)); -- Should Trigger
   I := "*"(1, (2+3));   -- Should Trigger
   I := 3 + abs (I);     -- Should Trigger
   I := 3 + (abs I);     -- Should Trigger
   I := I ** (3);        -- Should Trigger
   I := I ** (3+1);      -- OK
   I := (I+1) ** 3;      -- OK

   -- Simple logical
   B := I = 1 or (I = 2 or  I = 3);  -- OK (removing parentheses changes associativity)
   B := I = 1 or (I = 2 and I = 3);  -- OK
   B := not (B);                     -- Should Trigger
   B := not (B and B);               -- OK
   B := (not B) and B;               -- Should Trigger

   -- Short circuits
   B := I = 1 or (I = 2 and then I = 3);        -- OK
   B := I = 1 or else (I = 2 and then I = 3);   -- OK
   B := I = 1 and then (I = 2 and then I = 3);  -- Should Trigger
   B := I = 1 and then (I = 2);                 -- Should Trigger
   B := I = 1 and then (B or B);                -- OK
   B := (I = 1) and then B;                     -- Should Trigger
   B := I = 1 and then (B);                     -- Should Trigger
   B := I = 1 and then ("or" (B, B));           -- Should Trigger
end Test_Parentheses;
