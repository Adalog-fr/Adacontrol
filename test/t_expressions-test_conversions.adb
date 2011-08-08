with Ada.Text_IO.Text_Streams; use Ada.Text_IO, Ada.Text_IO.Text_Streams;
separate (T_Expressions)
procedure Test_Conversions is
   subtype Int10 is Integer range 1 .. 10;
   subtype Str10 is String (Positive range 1..10);
   I1, I2 : Integer;
   F1, F2 : Float;
   S1, S2 : Str10;

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
   I1 := Integer (I2);  -- range <> type_conversion, <> <> type conversion
   I2 := Int10 (I1);    -- range <> type_conversion, <> <> type conversion
   S1 := String (S2);   -- <> <> type_conversion, <> array type_conversion
   S2 := Str10 (S1);    -- <> <> type_conversion, <> array type_conversion

   I1 := Integer (F1);  -- digits range type_conversion, <> <> type_conversion
   F2 := Float (I2);    -- range <> type_conversion, <> <> type conversion

   P1 (Integer (I1));   -- range <> type_conversion, <> <> type conversion
   P2 (I1);
   P2 (Integer (I1));   -- <> <> parameter_view_conversion, range <> type_conversion, <> <> type conversion
   P3 (I1);
   P3 (Integer (I1));   -- <> <> parameter_view_conversion, range <> type_conversion, <> <> type conversion

   String'Read (Stream (Standard_Input), S1);          -- complex_parameter (x2)
   String'Read (Stream (Standard_Input), String (S1)); -- complex_parameter (x2), <> <> parameter_view_conversion, <> <> type_conversion, <> array type_conversion
end Test_Conversions;
