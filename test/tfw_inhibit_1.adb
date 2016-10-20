-- Test case: not inhibited
with Ada.Text_Io;
pragma Elaborate_All (Ada.Text_Io);

with Tfw_Inhibit_5;
procedure Tfw_Inhibit_1 is
begin
   Tfw_Inhibit_5;
end Tfw_Inhibit_1;
