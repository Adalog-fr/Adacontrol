-- Test case: local (indirect) instantiation
with Ada.Text_Io;
pragma Elaborate_All (Ada.Text_Io);

with Xfw_Inhibit;
procedure Tfw_Inhibit_6 is
   generic
   procedure Indirect;
   procedure Indirect is
      procedure Proc is new Xfw_Inhibit;
   begin
      Proc;
   end Indirect;

   procedure Inst is new Indirect;
begin
   Inst;
end Tfw_Inhibit_6;
