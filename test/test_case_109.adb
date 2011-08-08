with Ada.Text_Io, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;
procedure Test_Case_109 is
   F : File_Type;

begin
   Put ("Hello world");
   Put (F, "Hello world");
   Put (F, 3);    -- not in Text_IO
end Test_Case_109;
