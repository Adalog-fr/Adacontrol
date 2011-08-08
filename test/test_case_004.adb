with Ada.Text_Io;

pragma Elaborate_All (Ada.Text_Io);--## rule line off pragmas

pragma Elaborate_All (Ada.Text_Io);

--## rule off pragmas
pragma Elaborate_All (Ada.Text_Io);
--## rule on pragmas

pragma Elaborate_All (Ada.Text_Io);

procedure Test_Case_004 is
begin
   null;
end Test_Case_004;
