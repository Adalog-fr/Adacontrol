package body X_Declarations_Locations is

   procedure In_Own is -- own, nested, no_spec_procedure
   begin
      null;            -- null_procedure_body, null_procedure
   end;

   procedure In_Visible is
      procedure In_Local is -- local, nested, no_spec_procedure
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

   generic                  -- not library generic_package, generic
   package Gen is
      procedure P;          -- not library public in_generic procedure, not library procedure, in_generic procedure, public procedure
   private
      procedure Q;          -- not library procedure, in_generic procedure, private procedure
   end Gen;

   package body Gen is
      procedure P is begin null; end;  -- null_procdure_body, null_procedure
      procedure Q is begin null; end;  -- null_procdure_body, null_procedure
   end Gen;
end X_Declarations_Locations;
