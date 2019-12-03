separate (T_simplifiable_expressions)
procedure test_if is
   B : Boolean;
   V : Integer := 0;
begin
   V := (if V /= 0      then 1 else 0);  -- Negative_condition
   V := (if not (V = 0) then 1 else 0);  -- Negative_condition, not on comparison

end test_if;
