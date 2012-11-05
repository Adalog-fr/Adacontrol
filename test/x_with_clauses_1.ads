pragma Ada_05;
with X_With_Clauses_2;             -- Can be moved to body
with Ada.Strings;                  -- Unused, could be used by children
with Ada.Characters.Handling;
with Ada.Characters.Handling;      -- Redundant
with Ada.Exceptions;               -- Can be changed to private with
with Ada.Calendar;
private with Ada.Command_Line;
private with Interfaces.C;         -- Can be moved to body (remove private)
package X_With_Clauses_1 is
   procedure Proc;
   X : Ada.Characters.Handling.ISO_646;
   package Pack is
   private
      T : Ada.Calendar.Time;
   end Pack;
private
   E : Ada.Exceptions.Exception_Id;
   S : Ada.Command_Line.Exit_Status;
end X_With_Clauses_1;
