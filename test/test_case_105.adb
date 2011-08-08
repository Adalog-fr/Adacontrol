with Ada.Text_Io;

pragma Elaborate_All (Ada.Text_Io);--## rule line off pragmas

pragma Elaborate_All (Ada.Text_Io);

--## rule off pragmas
pragma Elaborate_All (Ada.Text_Io);
--## rule on pragmas

pragma Elaborate_All (Ada.Text_Io);

pragma Inconnu;
pragma TODO;

procedure Test_Case_105 is
begin
   null;
end Test_Case_105;
