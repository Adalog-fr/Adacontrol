separate (T_Uncheckable)
procedure Allocators is
   procedure P (X : access Integer := new Integer'(1)) is
   begin
      null;
   end;
begin
   null;
end Allocators;
