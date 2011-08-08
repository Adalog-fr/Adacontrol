with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Case_101 is
   F : File_Type;
   procedure Put_Line (I : Integer) is
   begin
      null;
   end;
   procedure Put_Line (S : string) is
   begin
      null;
   end;
begin
   null;

exception
   when others =>
      Ada.Text_IO.Put_Line ("");
      Put_Line (F, "");
      Put_Line ("");
      Put_Line (3);
end Test_Case_101;



