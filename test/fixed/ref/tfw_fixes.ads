private with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded;

use Ada.Text_IO;


package Tfw_Fixes is
   pragma PAGE; 
   pragma PAGE;

   procedure Hello;

private

   procedure Console_Log (S : String) renames Put_Line;

   int : Integer;
   X : Integer := Int;
   function "and" (L, R : in Integer) return Integer is (1);

end Tfw_Fixes;
