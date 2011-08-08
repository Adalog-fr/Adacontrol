separate (T_Expressions)
procedure Test_Conversions is
   subtype Int10 is Integer range 1 .. 10;
   subtype Str10 is String (Positive range 1..10);
   I1, I2 : Integer;
   S1, S2 : Str10;
begin
   I1 := Integer (I2);  -- type_conversion
   I2 := Int10 (I1);    -- type_conversion
   S1 := String (S2);   -- type_conversion
   S2 := Str10 (S1);    -- type_conversion
end Test_Conversions;
