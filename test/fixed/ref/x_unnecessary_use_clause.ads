with Text_IO, Ada.Strings.Unbounded, Ada.Numerics;
use Text_IO, Ada.Strings.Unbounded; -- Movable: use for Text_IO can be moved to body, Unused: Numerics not used
package  X_Unnecessary_Use_Clause is
   type Int_1 is range 1 .. 10;
   function "+" (L, R : Int_1) return Int_1;

   procedure Put_Line (S : Unbounded_String);
end X_Unnecessary_Use_Clause;
