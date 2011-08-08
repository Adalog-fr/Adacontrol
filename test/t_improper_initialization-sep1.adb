separate (T_Improper_Initialization)
procedure Sep1 (Var1, Var2 : out Integer) is      -- not safely initialized
begin
   Var1 := 1;
end Sep1;
