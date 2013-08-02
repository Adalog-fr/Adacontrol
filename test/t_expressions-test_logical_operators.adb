pragma Ada_2012;
separate (T_Expressions)
procedure Test_Logical_Operators is
   B1, B2 : Boolean;
   I      : Integer;
   subtype St is Integer range 11..20;
begin
   B1 := B1 and B2;       -- And
   B1 := B1 or B2;        -- Or
   B1 := B1 xor B2;       -- Xor
   B1 := B1 and then B2;  -- And_Then
   B1 := B1 or else B2;   -- Or_Else

   B1 := I     in St;                -- In     (in type)
   B1 := I not in St;                -- Not_In (in type)
   B1 := I     in 1..10;             -- In     (in range)
   B1 := I not in 1 .. 10;           -- Not_In (in range)
   B1 := I     in 1 .. 10 | St | 5;  -- In     (in list, Ada 2012)
   B1 := I not in 1 .. 10 | St | 5;  -- Not_In (in list, Ada 2012)
end Test_Logical_Operators;
