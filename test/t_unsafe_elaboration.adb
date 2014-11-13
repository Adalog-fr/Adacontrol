with X_Unsafe_Elaboration.Normal1;
with X_Unsafe_Elaboration.Normal2;
with X_Unsafe_Elaboration.Normal3;
with X_Unsafe_Elaboration.Preelab;
with X_Unsafe_Elaboration.Normal_Elab;
with X_Unsafe_Elaboration.Not_Called;
with Ada.Text_IO;
pragma Elaborate (X_Unsafe_Elaboration.Normal_Elab);
package body T_Unsafe_Elaboration is
   use X_Unsafe_Elaboration;

   function F return Character is
      use Normal2;
   begin
      Proc;
      return 'F';
   end F;

   procedure Indirect is
      I : Character;
   begin
      I := F;
   end Indirect;

   task T1;
   task body T1 is
      package Inst2 is new X_Unsafe_Elaboration.Normal3.Gen;
   begin
      null;
   end T1;

   procedure Exported1 is
   begin
      Not_Called.Proc;
   end Exported1;

   I : Integer := Inst1.FG;     -- Ticket #38
   procedure Exported2 is
   begin
      null;
   end Exported2;

begin
   Indirect;
   Normal1.Proc;
   Preelab.Proc;
   Normal_Elab.Proc;
   Exported2;
   Ada.Text_IO.Put_Line ("End of test");
end T_Unsafe_Elaboration;
