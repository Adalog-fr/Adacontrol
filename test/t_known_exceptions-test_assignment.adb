separate (T_Known_Exceptions)
procedure Test_Assignment is
   type Int1 is range 0 .. 10;
   subtype Sub1 is Int1 range 5 .. Int1'Last - 1;
   subtype Sub2 is Sub1 with Static_Predicate => Sub2 /= 7;
   type Mod1 is mod 2 ** 8;
   subtype Sub3 is Mod1 range 1 .. 10;

   I1 : Int1;
   Is1 : Sub1;
   Is2 : Sub2;
   Is3 : Sub1 := 1;     -- Assignment
   M   : Sub3;

   type Enum is (A, B, C);
   E : Enum range A .. B;

   type Ptr1 is access Integer;
   P1  : Ptr1;
   type Ptr2 is not null access all Integer;
   P2  : Ptr2;          -- Assignment (no initialization)
   subtype Ptr3 is not null Ptr1;
   P3  : Ptr3 := new Integer;
   --  type Ptr4 is new not null Ptr1;                        -- This test commented out because of infinite loop with
   --  P4  : Ptr4;          -- Assignment (no initialization) -- Gnat < Pro 220w
   P5  : not null Ptr1; -- Assignment (no initialization)

begin
   I1 := 11;            -- Assignment
   I1 := Int1'Last;

   Is1 := 4;            -- Assignment
   Is1 := I1 - 1;
   Is1 := Is1 + 1;      -- Assignment
   Is1 := 10;           -- Assignment

   Is2 := 0;            -- OK (predicate)

   M := -1;             -- Assignment
   M := 250 + 50;       -- Assignment

   E := B;
   E := Enum'Last;      -- Assignment

   P2 := null;          -- Assignment
   P2 := Ptr2 (P1);     -- Assignment
   P1 := new Integer;
   P2 := Ptr2 (P1);
   P1 := null;
   P3 := null;          -- Assignment
   P3 := P1;            -- Assignment
   --  P4 := Ptr4 (P1);     -- Assignment

end Test_Assignment;
