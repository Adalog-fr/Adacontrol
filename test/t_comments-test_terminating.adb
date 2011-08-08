with Ada.Text_IO; use Ada.Text_IO;
separate (T_Comments)
procedure Test_Terminating is
  S : constant String := """--JUNK"; --
   -- Not a terminating comment
   procedure Beginbegin is
   begin -- beginbegin
      null;
   end; -- end comment
begin  -- Test_Terminating
   Beginbegin --
   ;          -- junk
   null
   ;          -- not ok
   Put_Line ("-- essai");
   null;  -- not junk
   null;  -- not ok
end Test_Terminating;
