with X_With_Clauses_1.Child2;   -- Regular
separate (T_With_Clauses.Plain)

with Ada.Calendar;
procedure Sep is                -- Ada.Calendar inherited
   use X_With_Clauses_1;
   Z : Ada.Calendar.Time;
begin
   Child2;
end Sep;
