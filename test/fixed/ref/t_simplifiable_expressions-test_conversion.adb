separate (T_Simplifiable_Expressions)
procedure Test_Conversion is
   X : Integer;
   Y : Positive;
   C : constant := 1;
   D : Duration;
   F : Float;
begin
   X := Y;      -- unnecessary conversion
   X := Y;     -- unnecessary conversion
   X := Positive (Y - 1);
   X := Y - 1;  -- unnecessary conversion
   X := Natural (Y);
   Y := Positive (X);

   X := 1;                -- unnecessary conversion
   X := C;                -- unnecessary conversion
   X := Integer'Size;     -- unnecessary conversion
   X := Integer'Size / 8; -- unnecessary conversion
   X := Integer (3.5);

   F := Float (D + 10.0);
   F := Float (D / 10.0);
   F := Float (C);
   D := D + 10.0;  -- unnecessary conversion
   D := Duration (D * 10.0);
   D := Duration (D / 10.0);
end Test_Conversion;
