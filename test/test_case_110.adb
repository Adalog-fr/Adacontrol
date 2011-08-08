procedure Test_Case_110 is
   X : Integer;
   type T is
      record
         X : Integer;
      end record;
   procedure P (X : Integer) is
   begin
      null;
   end;
   procedure Q is
      X: Integer;
   begin
      null;
   end;
   Test_Case_110 : Integer;

   package Pack is
      Z : Integer;
   end Pack;

   package body Pack is
   begin
      declare
         Z : Float;
      begin
         null;
      end;
   end Pack;
begin
   declare
      X : Integer;
   begin
      null;
   end;

   begin
      null;
   exception
      when Y : Constraint_Error =>
         declare
            Y : Integer;
         begin
            null;
         end;
      when X : others =>
         null;
   end;
end Test_Case_110;

