separate (T_Expressions)
procedure Test_Logical_Operators is
   B1, B2 : Boolean;
   I      : Integer;
   subtype St is Integer range 11 .. 20;

   type Byte is mod 256;
   O1, O2 : Byte := 17;

   type Set is array (Byte) of Boolean;
   S1, S2 : Set;
begin
   B1 := not B2;          -- Not
   B1 := B1 and B2;       -- And, And_Boolean
   B1 := B1 or B2;        -- Or,  Or_Boolean
   B1 := B1 xor B2;       -- Xor, Xor_Boolean
   B1 := B1 and then B2;  -- And_Then
   B1 := B1 or else B2;   -- Or_Else

   B1 := I     in St;                -- In     (in type)
   B1 := I not in St;                -- Not_In (in type)
   B1 := I     in 1..10;             -- In     (in range)
   B1 := I not in 1 .. 10;           -- Not_In (in range)
   B1 := I     in 1 .. 10 | St | 5;  -- In     (in list, Ada 2012)
   B1 := I not in 1 .. 10 | St | 5;  -- Not_In (in list, Ada 2012)

   O1 := O1 and O2;    -- And, And_Binary
   O1 := O1 or  O2;    -- Or,  Or_Binary
   O1 := O1 xor O2;    -- Xor, Xor_Binary

   S1 := S1 and S2;    -- And, And_Array
   S1 := S1 or  S2;    -- Or,  Or_Array
   S1 := S1 xor S2;    -- Xor, Xor_Array
end Test_Logical_Operators;
