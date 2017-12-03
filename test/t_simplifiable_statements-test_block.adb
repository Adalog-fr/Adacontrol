separate (T_Simplifiable_Statements)
procedure Test_Block is
   I : Integer;
begin
   begin        -- Simplifiable block
      I := 1;
   end;

   declare      -- Simplifiable block
   begin
      I := 1;
   end          -- (with a strange semicolon)
   ;

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
