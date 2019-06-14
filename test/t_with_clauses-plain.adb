with Text_IO;                 -- Regular
with Ada.Text_IO;             -- Regular, Redundant
with Ada.Calendar;            -- Regular, Unnecessary: Used in separate only
with X_With_Clauses_1.Child1; -- Regular, Unnecessary
with System,                  -- Regular, Multiple names
     X_With_Clauses_1.Child2; -- OK: Used in unit and in separate
separate (T_With_Clauses)
procedure Plain is
   use X_With_Clauses_1;
   use Text_IO, System;
   pragma Priority (Priority'First);
   procedure Sep is separate;
begin
   New_Line;
   X_With_Clauses_1.Child2;
end Plain;
