with Ada.Text_Io, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;
procedure T_entities is
   F : File_Type;
   S1 : String := Float'Image (1.0);
   S2 : String := Integer'Image (Integer'First);
   procedure Sep is separate;
   package F_IO is new Float_Io (Float);
   X  : Float := 0.0;
   S3 : constant String := X'Img;
begin
   Put ("Hello world");
   Put (F, "Hello world");
   Put (F, 3);    -- not in Text_IO
   F_IO.Put (X);
end T_entities;
