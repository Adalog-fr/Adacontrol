separate (t_simplifiable_expressions)
procedure Test_Membership is
   I1, I2 : Integer;
   I3     : Integer renames I1;

   Cond : Boolean;
begin
   Cond := I1 in 1 | 5 | 10 | 0 | 9;          -- Replaceable by in
   Cond := I1 in 1 | 12 .. 15;                                                       -- Replaceable by in
   Cond := I1 = 1 or (I2 in 2 | 3);                                                   -- Replaceable by in (subexpression)
   Cond := I1 = 1 or (I1 = 2 or I2 = 3);
   Cond := I1 in 1 | 2 or I2 = 3;                                                    -- Replaceable by in
   Cond := I1 = 1 and (I1 in 5 | 10);                                                 -- Replaceable by in (subexpression)
   Cond := I1 in 1 .. 2 | 3 .. 5;                                          -- Replaceable by in
   Cond := I1 in 1 .. 2 | 3 .. 5;                                          -- Replaceable by in
   Cond := I1 in 1 .. 2 or (I1 < 5 and I2 >= 3);
   Cond := I1 in 1 .. 2 or (I1 > 3 or  I1 <= 5);

   Cond := I1 not in 1 | 5 | 10 | 0 | 9; -- Replaceable by not in
   Cond := I1 not in 1 | 12 .. 15;                                                 -- Replaceable by not in
   Cond := I1 /= 1 and (I2 not in 2 | 3);                                              -- Replaceable by not in (subexpression)
   Cond := I1 /= 1 or (I1 not in 5 | 10);                                              -- Replaceable by not in (subexpression)
end Test_Membership;
