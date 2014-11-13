-- Objective: check derogation (user defined derogation keywords)

with Ada.Text_Io;

pragma Elaborate_All (Ada.Text_Io);--%DEROG rule line off pragmas -- some comment

pragma Elaborate_All (Ada.Text_Io);

--%DEROG rule off pragmas
pragma Elaborate_All (Ada.Text_Io);
--%DEROG rule on pragmas

pragma Elaborate_All (Ada.Text_Io);

procedure Tfw_Rule_Off_Tags is
   pragma Inline (Tfw_Rule_Off_Tags); --%DEROG rule line off "special name"
begin
   null;
end Tfw_Rule_Off_Tags;
