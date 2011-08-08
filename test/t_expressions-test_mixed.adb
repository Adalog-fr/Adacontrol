separate (T_Expressions)
procedure Test_Mixed is
   A, B, C, D : constant := 1;
   I : Integer;
begin
   -- Errors:
   I := A + B - C + D;
   I := (A + B) - C + D;
   I := -A + B;
   I := A + C * D;

   -- OK:
   I := A + B + C + D;
   I := ((A + B) - C) + D;
   I := (-A) + B;
   I := A + (C * D);
end Test_Mixed;
