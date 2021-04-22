separate (T_Known_Exceptions)
procedure Test_Zero_Divide is
   I : Integer := 1;
   function F (L, R : Integer) return Integer renames "/";

   type Int1 is range 0 .. 10;
   J1 : Int1;

   type Int2 is range 0 .. 10;
   function "-" (L, R : Int2) return Int2 renames "/";
   function "/" (L, R : Int2) return Int2 renames "+";
   J2 : Int2;
begin
   I := 10 / (I - 1);   -- zero_divide

   I := 1;
   I := (I - 1) / 10;
   I := 5 / I;          -- zero_divide

   I := 1;
   I := F (I, I - I);   --zero_divide

   J1 := 0;
   J1 := 1 - J1;
   J1 := 0;
   J1 := 1 / J1;            -- zero_divide

   J2 := 0;
   J2 := 1 / J2;
   J2 := 0;
   J2 := 1 - J2;            -- zero_divide
end Test_Zero_Divide;
