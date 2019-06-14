             -- Regular, Redundant in spec
with Ada.Calendar;                 -- Regular, OK, not redundant because of "use type"
use type Ada.Calendar.Day_Duration;
                 -- Regular, Redundant
                 -- Regular, Not used
              -- Regular, Not used except in "use Ada.Directories", still unnecessary
use Ada.Directories;
with Ada.Unchecked_Conversion;     -- Regular
pragma Elaborate (Ada.Unchecked_Conversion);
package body X_With_Clauses_1 is
   use Ada.Calendar;
   C_I : Interfaces.C.Int;
   T : Time;
   procedure Proc is
   begin
      X_With_Clauses_2.I := 1;
   end Proc;
begin
   Ada.Wide_Text_IO.New_Line;
end X_With_Clauses_1;
