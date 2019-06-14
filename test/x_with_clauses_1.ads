with X_With_Clauses_2;             -- Regular, Can be moved to body
with Ada.Strings;                  -- Regular, Unused, could be used by children
with Ada.Characters.Handling;      -- Regular
with Ada.Characters.Handling;      -- Regular, Redundant
with Ada.Exceptions;               -- Regular, Can be changed to private with
with Ada.Calendar;                 -- Regular
private with Ada.Command_Line;     -- Private with
private with Interfaces.C;         -- Private with, Can be moved to body (remove private)
with Ada.Wide_Text_IO;             -- Regular, Unused in spec, redundant and used in body
with Ada.Directories;              -- Regular, Unused, could be used by children
with Ada.Unchecked_Conversion;     -- Regular
package X_With_Clauses_1 is
   procedure Proc;
   X : Ada.Characters.Handling.ISO_646;
   package Pack is
   private
      T : Ada.Calendar.Time;
   end Pack;
   function I_To_F is new Ada.Unchecked_Conversion (Integer, Float);
private
   E : Ada.Exceptions.Exception_Id;
   S : Ada.Command_Line.Exit_Status;
end X_With_Clauses_1;
