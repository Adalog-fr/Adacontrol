-- Test case: local (direct) instantiation
with Ada.Text_Io;
pragma Elaborate_All (Ada.Text_Io);

with Xfw_Inhibit;
procedure Tfw_Inhibit_4 is
   procedure Proc is new Xfw_Inhibit;
begin
   Proc;
end Tfw_Inhibit_4;
