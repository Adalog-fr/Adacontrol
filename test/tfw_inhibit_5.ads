-- Test case: library instantiation
with Ada.Text_Io;
pragma Elaborate_All (Ada.Text_Io);

with Xfw_Inhibit;
procedure Tfw_Inhibit_5 is new Xfw_Inhibit;
