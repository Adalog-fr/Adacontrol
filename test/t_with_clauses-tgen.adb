with X_With_Clauses_Gen;
with X_With_Clauses_1;     -- Unnecessary (not necessary for default of instantiation)
with X_With_Clauses_2;     -- OK (used for <> default of instantiation)
with System;               -- OK (used in aspect clause)
separate (T_With_Clauses)
procedure Tgen is
   use X_With_Clauses_2;
   package Inst is new X_With_Clauses_Gen (1);

   type T is range 1 .. 10 with Size => 8 * System.Storage_Unit;
begin
   null;
end TGen;
