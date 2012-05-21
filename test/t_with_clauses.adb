with Text_IO;
with Ada.Text_IO;          -- Redundant
with Ada.Calendar;         -- Used in separate only
with                       -- Multiple names
  X_With_Clauses_1.Child1, -- Unused
  X_With_Clauses_1.Child2; -- Used in unit and in separate
with System;               -- Used in pragma
procedure T_With_Clauses is
   use X_With_Clauses_1;
   use Text_IO, System;
   pragma Priority (Priority'First);
   procedure Sep is separate;
begin
   New_Line;
   X_With_Clauses_1.Child2;
end T_With_Clauses;
