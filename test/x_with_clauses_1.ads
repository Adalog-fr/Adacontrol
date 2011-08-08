with X_With_Clauses_2;             -- Can be moved to body
with Ada.Strings;                  -- Unused, could be used by children
with Ada.Characters.Handling;
with Ada.Characters.Handling;      -- Redundant
package X_With_Clauses_1 is
   procedure Proc;
   X : Ada.Characters.Handling.ISO_646;
end X_With_Clauses_1;
