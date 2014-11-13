-- Objective: check derogation (derogation inverted case)
with Ada.Text_Io;

pragma Elaborate_All (Ada.Text_Io);--## rule line off pragmas ## some comment

pragma Elaborate_All (Ada.Text_Io);

--## rule off pragmas
pragma Elaborate_All (Ada.Text_Io);
--## rule on pragmas

pragma Elaborate_All (Ada.Text_Io);

procedure Tfw_Rule_Off_Inverted is
   pragma Inline (Tfw_Rule_Off_Inverted); --## rule line off "special name"

   --## rule off "special name"
   procedure P is
      pragma Inline (P);
   begin
      null;
   end;
   --## rule on "special name"

   procedure Q is
      pragma Inline (Q);
   begin
      null;
   end;
begin
   null;
end Tfw_Rule_Off_Inverted;
