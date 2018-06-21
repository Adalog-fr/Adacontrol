with X_With_Clauses_2;             -- Can be moved to body
                  -- Unused, could be used by children
with Ada.Characters.Handling;
      -- Redundant
private with Ada.Exceptions;               -- Can be changed to private with
with Ada.Calendar;
private with Ada.Command_Line;
private with Interfaces.C;         -- Can be moved to body (remove private)
with Ada.Wide_Text_IO;             -- Unused in spec, redundant and used in body
              -- Unused, could be used by children
with Ada.Unchecked_Conversion;
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
