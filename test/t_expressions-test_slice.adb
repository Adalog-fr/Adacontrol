separate (T_Expressions)
procedure Test_Slice is
   X : String := "abcd";
begin
   if X (Natural range 2 .. 3) = "bc" then   -- Slice
      null;
   end if;

   if X (3) = 'c' then       -- OK
      null;
   end if;
end Test_Slice;
