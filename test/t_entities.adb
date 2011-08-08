with Ada.Text_Io, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;
procedure T_entities is
   F : File_Type;
   S1 : String := Float'Image (1.0);              -- 'Image
   S2 : String := Integer'Image (Integer'First);  -- 'Image, standard.integer'first
   procedure Sep is separate;
   package F_IO is new Float_Io (Float);
   X  : Float := 0.0;
   S3 : constant String := X'Img;                 -- 'Img

   generic
      with function "<" (L, R  : Integer) return Boolean is <>;
      Max : in Integer := 100;
   procedure P;
   procedure P is
   begin
      null;
   end;

   procedure Q is new P;
   procedure R is new P ("<");                    -- "<"

   I : Standard.Integer;

begin
   Put ("Hello world");                           -- Ada.Text_Io.Put{Standard.String}
   Put (F, "Hello world");                        -- Ada.Text_Io.Put
   Put (F, 3);                                    -- OK, not in Text_IO
   F_IO.Put (X);                                  -- Ada.Text_Io.Float_Io.Put
   if I < I then                                  -- "<"
      null;
   end if;

end T_entities;
