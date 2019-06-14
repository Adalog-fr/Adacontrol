with System;               -- Regular, OK, used in aspect clause
separate (T_With_Clauses)
procedure Taspect is
 type T is range 1..10 with Size => 8 * System.Storage_Unit;
begin
   null;
end Taspect;
