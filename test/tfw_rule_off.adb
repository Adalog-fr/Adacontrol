with Ada.Text_Io;

pragma Elaborate_All (Ada.Text_Io);--## rule line off pragmas ## some comment

pragma Elaborate_All (Ada.Text_Io);

--## rule off pragmas
pragma Elaborate_All (Ada.Text_Io);
--## rule on pragmas

pragma Elaborate_All (Ada.Text_Io);

procedure Tfw_Rule_off is
   pragma Inline (Tfw_Rule_Off); --## rule line off "special name"
begin
   null;
end Tfw_Rule_Off;
