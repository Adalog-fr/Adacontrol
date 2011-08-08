separate (T_Expressions)
procedure Test_Prefixed_Operator is
   I, J : Integer;
   type Int is new Integer;
   A, B : Int;
   P, Q : Float;
begin
   -- Errors:
   I := "+" (J, 1);                                         -- <>
   A := T_Expressions.Test_Prefixed_Operator."-" (B, A);    -- <>
   P := "+" (P, Q);                                         -- digits, <>

   -- OK:
   I := J + 1;
   A := B - A;
   P := P + Q;
end Test_Prefixed_Operator;
