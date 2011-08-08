with Ada.Text_IO;
procedure T_default_parameter is
   use Ada.Text_Io;

   generic
      X,Y : Integer := 0;
   package Gen is end Gen;

   package Inst1 is new Gen (1);

   generic
      with package Pack is new Gen (1);
   package Gen_Gen is end Gen_Gen;

   package Inst2 is new Gen_Gen (Inst1);

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
end T_default_parameter;

