-- Ada.Calendar inherited
with X_With_Clauses_1.Child2;
separate (T_With_Clauses) procedure Sep is
   use X_With_Clauses_1;
   Z : Ada.Calendar.Time;
begin
   Child2;
end Sep;
