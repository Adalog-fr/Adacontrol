separate (T_Known_Exceptions)
procedure Test_Assignment is
   type Int1 is range 0 .. 10;
   subtype Sub1 is Int1 range 5 .. Int1'Last - 1;
   subtype Sub2 is Sub1 with Static_Predicate => Sub2 /= 7;

   I1 : Int1;
   Is1 : Sub1;
   Is2 : Sub2;
   Is3 : Sub1 := 1;

   type Enum is (A, B, C);
   E : Enum range A .. B;

begin
   I1 := 11;        -- Assignment
   I1 := Int1'Last;

   Is1 := 4;        -- Assignment
   Is1 := I1 - 1;
   Is1 := Is1 + 1;  -- Assignment
   Is1 := 10;       -- Assignment

   Is2 := 0;        -- OK (predicate)

   E := B;
   E := Enum'Last;  -- Assignment

end Test_Assignment;
