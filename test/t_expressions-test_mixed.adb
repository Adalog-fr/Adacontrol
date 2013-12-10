separate (T_Expressions)
procedure Test_Mixed is
   A, B, C, D : constant := 1;
   I : Integer;
   L : Boolean;
begin
   -- Errors:
   I := A + B - C + D;
   I := (A + B) - C + D;
   I := -A + B;
   I := A + C * D;
   L := A=3 or B=2;
   L := A=3 and then B=2;
   L := L or I in 1..10;
   L := L or else L or else I not in 1..10;

   -- OK:
   I := A + B + C + D;
   I := ((A + B) - C) + D;
   I := (-A) + B;
   I := A + (C * D);
end Test_Mixed;
