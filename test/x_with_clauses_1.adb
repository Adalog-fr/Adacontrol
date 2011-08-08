with Ada.Wide_Text_IO;             -- Redundant in spec
package body X_With_Clauses_1 is
   procedure Proc is
   begin
      X_With_Clauses_2.I := 1;
   end Proc;
end X_With_Clauses_1;
