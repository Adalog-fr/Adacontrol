separate (T_Simplifiable_Statements)
procedure Test_Block is
   I : Integer;
begin
   I := 1;

   I := 1;

   L : begin
      I := 1;
   end L;

   declare
      J : Integer;
   begin
      I := 1;
   end;

   begin
      I := 1;
   exception
      when others => null;
   end;
end Test_Block;
