separate (T_Exception_Propagation)
procedure X_Interface is
   procedure Proc is begin null; end;

   procedure Proc_C1 with Convention => C;
   procedure Proc_C1 is begin null; end; -- Interface

   procedure Proc_C2;
   pragma Convention (C, Proc_C2);
   procedure Proc_C2 is                  -- Interface
   begin
      null;
   exception
      when others =>
         raise;
   end Proc_C2;

   procedure Proc_C3;
   pragma Convention (C, Proc_C3);
   procedure Proc_C3 is                  -- Interface
      use Ada.Exceptions;
   begin
      null;
   exception
      when others =>
         Raise_Exception (Constraint_Error'Identity);
   end Proc_C3;

   generic
   function F return Integer;
   pragma Convention (C, F);
   function F return Integer is           -- Interface
   begin
      return 0;
   end F;

   procedure Proc_C_OK;
   pragma Convention (C, Proc_C_OK);
   procedure Proc_C_OK is
   begin
      null;
   exception
      when others =>
         null;
   end Proc_C_OK;
begin
   null;
end X_Interface;
