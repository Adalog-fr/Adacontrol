separate (T_Expressions)
procedure Test_Logical_Operators is
   B1, B2 : Boolean;
begin
   B1 := B1 and B2;       -- And
   B1 := B1 or B2;        -- Or
   B1 := B1 xor B2;       -- Xor
   B1 := B1 and then B2;  -- And_Then
   B1 := B1 or else B2;   -- Or_Else
end Test_Logical_Operators;
