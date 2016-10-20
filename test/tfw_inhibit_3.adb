-- Test case: inhibited for entities and pragmas
with Ada.Text_Io;
pragma Elaborate_All (Ada.Text_Io);

procedure Tfw_Inhibit_3 is
begin
   null;
end Tfw_Inhibit_3;
