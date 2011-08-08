procedure Test_Case_112 is
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
end Test_Case_112;

