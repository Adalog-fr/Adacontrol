package body X_Declarations_Locations is

   procedure In_Own is -- own, nested
   begin
      null;            -- null_procedure_body, null_procedure
   end;

   procedure In_Visible is
      procedure In_Local is -- local, nested
      begin
         null;              -- null_procedure_body, null_procedure
      end;
   begin
      null;                 -- null_procedure_body, null_procedure
   end In_Visible;

   procedure In_Private is
   begin
      declare
         procedure In_Block; -- nested, local, block
         procedure In_Block is
         begin
            null;            -- null_procedure_body, null_procedure
         end;
      begin
         null;
      end;
   end In_Private;

end X_Declarations_Locations;
