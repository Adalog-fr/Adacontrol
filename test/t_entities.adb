with Ada.Text_Io, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
procedure T_entities is
   function I_To_I is new Ada.Unchecked_Conversion (Integer, Integer);

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

   I : Standard.Integer := I_To_I (5);            -- Unchecked_Conversion

   type Rec is
      record
         I : Integer;
      end record;
   VR  : Rec;
   Ren : Integer renames VR.I;                    -- T_Entities.VR, T_Entities.Rec.I

   Dur1 : Duration;                               -- Standard.Duration

begin
   Put ("Hello world");                           -- Ada.Text_Io.Put{Standard.String}
   Put (F, "Hello world");                        -- Ada.Text_Io.Put
   Put (F, 3);                                    -- OK, not in Text_IO
   F_IO.Put (X);                                  -- Ada.Text_Io.Float_Io.Put
   if I < I then                                  -- "<"
      null;
   end if;

   VR   := (I => 0);                              -- T_Entities.VR, T_Entities.Rec.I
   VR.I := 0;                                     -- T_Entities.VR, T_Entities.Rec.I
   VR.I := Ren;                                   -- T_Entities.VR (x2), T_Entities.Rec.I (x2)
   Ren  := 0;                                     -- T_Entities.VR, T_Entities.Rec.I

   declare
      I    : Integer;                             -- block standard.integer
      Dur1 : Duration;                            -- OK Standard.Duration, in block
   begin
      I := Integer'First;                         -- Standard.Integer'first, block standard.Integer
   end;

   I := I_To_I (I);                               -- Ada.Unchecked_Conversion
end T_entities;
