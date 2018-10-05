with System.Storage_Elements;
use System.Storage_Elements;
package body X_Reduceable_Scope is

   ----------------
   -- Needs_Body --
   ----------------

   procedure Needs_Body is
      V : Ada.Text_IO.Count;
      C : Storage_Count;
   begin
      V := V + 1;
      C := C + 1;
   end Needs_Body;

   V : Float;
begin
   V := Pi;
end X_Reduceable_Scope;
