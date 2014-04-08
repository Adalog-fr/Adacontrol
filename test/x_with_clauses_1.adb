with Ada.Wide_Text_IO;             -- Redundant in spec
with Ada.Calendar;                 -- OK, not redundant because of "use type"
use type Ada.Calendar.Day_Duration;
with Ada.Calendar;                 -- Redundant
with Ada.Numerics;                 -- Not used
with Ada.Directories;              -- Not used except in "use Ada.Directories", still unnecessary
use Ada.Directories;
with Ada.Unchecked_Conversion;
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
