with Ada.Text_IO.Text_Streams; use Ada.Text_IO, Ada.Text_IO.Text_Streams;
separate (T_Expressions)
procedure Test_Conversions is
   subtype Int10 is Integer range 1 .. 10;
   type Int10_New is new Int10;
   type Int10_Underived is range 1..10;
   subtype Str10 is String (Positive range 1 .. 10);
   type Str10_New is new Str10;
   type Str10_Underived is array (Int10_New) of Character;

   type Tag1 is tagged null record;
   type Tag2 is new Tag1 with null record;
   type Tag3 is new Tag2 with null record;

   I1, I2 : Integer;
   INew   : Int10_New;
   IUnd   : Int10_Underived;
   F1, F2 : Float;
   S1, S2 : Str10;
   SN     : Str10_New;
   SU     : Str10_Underived;
   T1     : Tag1;
   T2     : Tag2;
   T3     : Tag3;

   procedure P1 (X : in Integer) is
   begin
      null;
   end;
   procedure P2 (X : out Integer) is
   begin
      null;
   end;
   procedure P3 (X : in out Integer) is
   begin
      null;
   end;
begin
   I1 := Integer (I2);         -- range <> type_conversion, type conversion
   I2 := Int10 (I1);           -- range <> type_conversion, type conversion
   S1 := String (S2);          -- <> array type_conversion, type conversion
   S2 := Str10 (S1);           -- <> array type_conversion, type conversion
   SN := Str10_New (S1);       -- <> array type_conversion, type conversion
   SU := Str10_Underived (S1); -- <> array type_conversion, type conversion, <> array underived_conversion

   I1 := Integer (F1);            -- digits range type_conversion, type_conversion, <> range underived_conversion
   F2 := Float (I2);              -- range <> type_conversion, type conversion
   INew := Int10_New (I1);        -- range <> type_conversion, type conversion
   IUnd := Int10_Underived (I1);  -- range <> type_conversion, type conversion, <> range underived_conversion

   P1 (Integer (I1));   -- range <> type_conversion, type conversion
   P2 (I1);
   P2 (Integer (I1));   -- parameter_view_conversion, range <> type_conversion, type conversion
   P3 (I1);
   P3 (Integer (I1));   -- parameter_view_conversion, range <> type_conversion, type conversion

   String'Read (Stream (Standard_Input), S1);          -- complex_parameter (x2), Function_Call (x2)
   String'Read (Stream (Standard_Input), String (S1)); -- complex_parameter (x2), Function_Call (x2), parameter_view_conversion, <> array type_conversion, type_conversion

   T1 := Tag1 (T2);            -- type_conversion, <> tagged upward_conversion
   T1 := Tag1 (T3);            -- type_conversion, <> tagged upward_conversion
   T3 := Tag3(Tag1'Class(T3)); -- type_conversion x2, <> tagged_downward_conversion, <> tagged upward_conversion
end Test_Conversions;
