package X_No_Operator_Usage is
   type X1 is new Integer;                 -- no operators

   package Internal is
      type X2 is new X1 range 1 .. 10;
      function "+" (L, R : X2) return X2;
   end Internal;

   type X3 is private;
   type X4 is private;
   function "abs" (L : X4) return X4;
private
   type Intermediate1 is range 1 .. 10;    -- no operators
   subtype Intermediate2 is Intermediate1 range 3..5;
   type X3 is new intermediate2;           -- no operators
   type X4 is range 1 .. 10;
end X_No_Operator_Usage;
