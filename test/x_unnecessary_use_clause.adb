package body X_Unnecessary_Use_Clause is
   function "+" (L, R : Int_1) return Int_1 is
   begin
      return 2;
   end "+";

   procedure Put_Line (S : Unbounded_String) is
   begin
      New_Line;
   end Put_Line;
end X_Unnecessary_Use_Clause;
