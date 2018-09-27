with Ada.Command_Line, Ada.Numerics;
package T_unnecessary_use_clause is
   use Ada.Numerics;  -- Unused: Command_Line unused, possible use in child

   My_Pi : constant := Pi;

   procedure Sep;
end T_unnecessary_use_clause;
