with Ada.Text_Io;

pragma Elaborate_All (Ada.Text_Io);--## rule line off pragmas

pragma Elaborate_All (Ada.Text_Io);

--## rule off pragmas
pragma Elaborate_All (Ada.Text_Io);
--## rule on pragmas

pragma Elaborate_All (Ada.Text_Io);

pragma Inconnu;
pragma TODO;

procedure T_pragmas is
   pragma Inline (T_pragmas);
   pragma Annotate (Test_Case_105); -- A GNAT pragma
begin
   null;
end T_pragmas;
