with Text_IO;                 -- Regular
             -- Regular, Redundant
            -- Regular, Unnecessary: Used in separate only
 -- Regular, Unnecessary
with System;
with X_With_Clauses_1.Child2; -- OK: Used in unit and in separate
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
