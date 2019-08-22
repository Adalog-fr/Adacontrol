with Ada.Command_Line, Ada.Numerics;
package T_unnecessary_use_clause is
   use Ada.Command_Line, Ada.Numerics;  -- Unused: Command_Line unused, possible use in child

   My_Pi : constant := Pi;

   procedure Sep;

   procedure Max_Replacement;

   package Parent_Pack is
      Data : Integer;
   end Parent_Pack;
end T_unnecessary_use_clause;
