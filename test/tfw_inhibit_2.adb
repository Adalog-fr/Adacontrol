-- Test case: inhibited for entities, not pragma
with Ada.Text_Io;
pragma Elaborate_All (Ada.Text_Io);

procedure Tfw_Inhibit_2 is
begin
   null;
end Tfw_Inhibit_2;
