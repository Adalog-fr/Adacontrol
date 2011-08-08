package body X_No_Operator_Usage is
   package body Internal is
      function "+" (L, R : X2) return X2 is
      begin
         return 1;
      end "+";
   end Internal;

   function "abs" (L : X4) return X4 is
   begin
      return L;
   end "abs";
end X_No_Operator_Usage;
