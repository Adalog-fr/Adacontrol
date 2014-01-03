with Ada.Text_IO;
with Ada.Calendar;
use Ada.Text_IO;
use type Ada.Text_IO.File_Type;
use Ada.Calendar;
use type Ada.Calendar.Time;
with Ada.Numerics;                 -- out of order
pragma Elaborate (Ada.Text_IO);
use Ada.Numerics;                  -- out of order
pragma Elaborate (Ada.Numerics);

separate (T_Unit_Pattern)
procedure Test_Context_Clauses_Order is
begin
   null;
end;
