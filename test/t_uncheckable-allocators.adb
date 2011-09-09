separate (T_Uncheckable)
procedure Allocators is
   procedure P (X : access Integer := new Integer'(1)) is  -- Uncheckable
   begin
      null;
   end;
begin
   P;
end Allocators;
