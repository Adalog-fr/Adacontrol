with Ada.Text_IO;
procedure Test_Case_107 is
   use Ada.Text_Io;
   generic
      X,Y : Integer := 0;
   procedure Gen;
   procedure Gen is
   begin
      null;
   end Gen;

   procedure Inst is new Gen (1);

   procedure Proc (X, Y : Integer := 0) is
   begin
      null;
   end Proc;

   procedure Proc (X, Y : Float := 0.0) is
   begin
      null;
   end Proc;
begin
   Proc (1);
   Proc (Y => 1.0);
end Test_Case_107;

