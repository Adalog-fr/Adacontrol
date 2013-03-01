separate (T_Max_Nesting)
procedure Test_Generic is
   generic                    -- All > 1
   procedure Gen0_All2;

   procedure Gen0_All2 is
   begin
      null;
   end Gen0_All2;

   generic                    -- All > 1
   package All2 is
      generic                 -- All > 2
      procedure Gen1_All3;
   end All2;

   package body All2 is
       procedure Gen1_All3 is
         generic              -- All > 2, Generic > 1
         procedure Gen2_All4;
         procedure Gen2_All4 is
         begin
            null;
         end Gen2_All4;
      begin
         null;
      end Gen1_All3;
   end All2;
begin
   null;
end Test_Generic;
