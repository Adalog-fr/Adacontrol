procedure T_Max_Nesting is
   procedure Test_Generic is separate;
   procedure Test_Task    is separate;

   procedure All1 is
      procedure All2;          -- All > 1
      procedure All2 is
         procedure All3 is     -- All > 2
            procedure All4 is  -- All > 2
            begin
               null;
            end All4;
         begin
            null;
         end All3;
      begin
         begin                 -- All > 2
            null;
         end;
      end All2;
   begin
      null;
   end All1;
   procedure Sep is separate;
begin
   null;
end T_Max_Nesting;

