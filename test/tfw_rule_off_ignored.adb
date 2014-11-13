-- Objective: check derogation (derogation ignored case)

with Ada.Text_Io;

pragma Elaborate_All (Ada.Text_Io);--## rule line off pragmas

pragma Elaborate_All (Ada.Text_Io);

--## rule off pragmas
pragma Elaborate_All (Ada.Text_Io);
--## rule on pragmas

pragma Elaborate_All (Ada.Text_Io);

procedure Tfw_Rule_Off_Ignored is
begin
   null;
end Tfw_Rule_Off_Ignored;
