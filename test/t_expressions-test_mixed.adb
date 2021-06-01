separate (T_Expressions)
procedure Test_Mixed is
   A, B, C, D : constant := 1;
   I, J : Integer;
   L : Boolean;
begin
   -- Errors:
   I := A + B - C + D;                        -- Unparenthesized_mixed (x2)
   I := (A + B) - C + D;                      -- Unparenthesized_mixed
   I := -A + B;                               -- Unparenthesized_mixed (x2)
   I := A + C * D;                            -- Unparenthesized_mixed
   L := A = 3 or B = 2;                       -- Unparenthesized_mixed (x2), or, or_boolean
   L := A = 3 and then B = 2;                 -- Unparenthesized_mixed (x2), and_then
   L := L or I in 1 .. 10;                    -- or, or_boolean, Unparenthesized_mixed, in
   L := L or else L or else I not in 1 .. 10; -- or_else (x2), Unparenthesized_mixed, not_in

   -- Special case unary:
   I := -J mod 2;                             -- Unparenthesized_mixed
   I := I - J mod 2;                          -- Unparenthesized_mixed
   I := -J mod 2 + 3;                         -- Unparenthesized_mixed
   I := -(J mod 2);
   I := (-J) mod 2;

   -- OK:
   I := A + B + C + D;
   I := ((A + B) - C) + D;
   I := (-A) + B;
   I := A + (C * D);
end Test_Mixed;
