separate (T_Simplifiable_Expressions)
procedure Test_Conversion is
   X : Integer;
   Y : Positive;
begin
   X := Integer (Y);      -- unnecessary conversion
   X := Positive (Y);     -- unnecessary conversion
   X := Positive (Y - 1);
   X := Integer (Y - 1);  -- unnecessary conversion
   X := Natural (Y);
   Y := Positive (X);
end Test_Conversion;
