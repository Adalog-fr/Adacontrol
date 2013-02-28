separate (T_Max_Nesting)
procedure Sep is
   procedure Test_Separate is separate;  -- separate > 0
   procedure All2 is -- all > 1
   begin
      null;
   end;
begin
   null;
end Sep;
