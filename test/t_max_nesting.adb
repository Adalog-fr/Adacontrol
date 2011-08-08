procedure T_max_nesting is
   procedure Nest2 is
      procedure Nest3 is
         procedure Nest4 is
         begin
            null;
         end;
      begin
         begin
            null;
         end;
      end;
   begin
      null;
   end;
   procedure Sep is separate;
begin
   null;
end T_max_nesting;

