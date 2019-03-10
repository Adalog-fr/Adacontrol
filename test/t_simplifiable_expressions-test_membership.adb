separate (t_simplifiable_expressions)
procedure Test_Membership is
   I1, I2 : Integer;
   I3     : Integer renames I1;

   Cond : Boolean;
begin
   Cond := ((I1) = 1) or (I1 = 5 or I1 = 10) or I3 = 0 or Test_Membership.I1 = 9;          -- Replaceable by in
   Cond := I1 = 1 or I1 in 12 .. 15;                                                       -- Replaceable by in
   Cond := I1 = 1 or (I2 = 2 or I2 = 3);                                                   -- Replaceable by in (subexpression)
   Cond := I1 = 1 or (I1 = 2 or I2 = 3);
   Cond := I1 = 1 or  I1 = 2 or I2 = 3;                                                    -- Replaceable by in
   Cond := I1 = 1 and (I1 = 5 or I1 = 10);                                                 -- Replaceable by in (subexpression)
   Cond := I1 in 1 .. 2 or (I1 >= 3 and I1 <= 5);                                          -- Replaceable by in
   Cond := I1 in 1 .. 2 or (I1 <= 5 and I1 >= 3);                                          -- Replaceable by in
   Cond := I1 in 1 .. 2 or (I1 < 5 and I2 >= 3);
   Cond := I1 in 1 .. 2 or (I1 > 3 or  I1 <= 5);

   Cond := ((I1) /= 1) and (I1 /= 5 and I1 /= 10) and I3 /= 0 and Test_Membership.I1 /= 9; -- Replaceable by not in
   Cond := I1 /= 1 and I1 not in 12 .. 15;                                                 -- Replaceable by not in
   Cond := I1 /= 1 and (I2 /= 2 and I2 /= 3);                                              -- Replaceable by not in (subexpression)
   Cond := I1 /= 1 or (I1 /= 5 and I1 /= 10);                                              -- Replaceable by not in (subexpression)
end Test_Membership;
