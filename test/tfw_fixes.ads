with Ada.Text_IO, Ada.Strings.Unbounded;
with Xfw_Fixes.Child;
with Ada.Wide_Text_IO, Ada.Strings.Wide_Unbounded;

use Ada.Text_IO;
use Xfw_Fixes.Child;

package Tfw_Fixes is
   pragma Page; pragma PAGE;

   procedure Hello;

private

   procedure Console_Log (S : String) renames Put_Line;

   int : Integer;
   X : Integer := Integer (Int);
   function "aNd" (L, R : in Integer) return Integer is (1);

end Tfw_Fixes;
