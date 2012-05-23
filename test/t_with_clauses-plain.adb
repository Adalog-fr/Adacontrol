with Text_IO;
with Ada.Text_IO;          -- Redundant
with Ada.Calendar;         -- Unnecessary: Used in separate only
with                       -- Multiple names
  X_With_Clauses_1.Child1, -- Unnecessary
  X_With_Clauses_1.Child2; -- OK: Used in unit and in separate
with System;               -- OK: Used in pragma
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
