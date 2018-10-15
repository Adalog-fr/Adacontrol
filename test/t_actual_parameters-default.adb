with Ada.Text_IO;
separate (T_Actual_Parameters)
procedure Default is
   use Ada.Text_Io;

   generic
      X,Y : Integer := 0;
   package Gen is end Gen;

   package Inst1 is new Gen (1); -- X used, Y not used

   generic
      with package Pack is new Gen (X => 1);  -- X used, Y not used
   package Gen_Gen is end Gen_Gen;

   package Inst2 is new Gen_Gen (Inst1);

   generic
      with function "<" (L, R  : Integer) return Boolean is <>;
      Max : in Integer := 100;
   procedure P;
   procedure P is
   begin
      null;
   end;

   procedure Q is new P;
   procedure R is new P ("<");           -- "<" not used, Max used
   procedure S is new P (Max => 10);     -- "<" used,     Max not used

   procedure Proc (X, Y : Integer := 0) is
   begin
      null;
   end Proc;

   procedure Proc (X, Y : Float := 0.0) is
   begin
      null;
   end Proc;

begin
   Proc (1);            -- X used, Y not used
   Proc (Y => 1.0);     -- X not used, Y used
end Default;

