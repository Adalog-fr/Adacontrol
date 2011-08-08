with Ada.Text_IO; use Ada.Text_IO;
procedure T_entity_inside_exception is
   F : File_Type;
   procedure Put_Line (I : Integer) is
   begin
      null;
   end;
   procedure Put_Line (S : string) is
   begin
      null;
   end;

   X : Integer;
begin
   X := Integer'Value (Integer'Image (X));
exception
   when others =>
      Ada.Text_IO.Put_Line ("");
      Put_Line (F, "");
      Put_Line ("");
      Put_Line (3);
      X := Integer'Value (Integer'Image (X));
end T_entity_inside_exception;



