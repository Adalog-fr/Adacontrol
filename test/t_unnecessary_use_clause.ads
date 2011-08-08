with Ada.Command_Line, Ada.Numerics;
package T_unnecessary_use_clause is
   use Ada.Command_Line, Ada.Numerics;

   My_Pi : constant := Pi;

   procedure Sep;
end T_unnecessary_use_clause;
