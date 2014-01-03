separate (T_Expressions)
procedure Test_Fixed_Multiplying_Op is
   type Fx  is delta 0.1 range 0.0 .. 1.0;
   type Dec is delta 0.1 digits 5;

   Vfx  : Fx;
   Vdec : Dec;
   Vdur : Duration;
   I    : Integer;
begin
   Vfx := Vfx * Vfx;           -- fixed point multiplying operator, unconv_fixed_op
   Vfx := Fx (Vfx / Vfx);      -- fixed point multiplying operator, type_conversion
   Vfx := Vfx * I;             -- fixed point multiplying operator
   Vfx := I   * Vfx;           -- fixed point multiplying operator
   Vfx := Vfx / I;             -- fixed point multiplying operator
   Vfx := Vfx * 2.0;           -- fixed point multiplying operator
   Vfx := Vfx * 2;             -- fixed point multiplying operator

   Vdec := Dec (Vdec * Vdec);  -- fixed point multiplying operator, type_conversion
   Vdec := Vdec / Vdec;        -- fixed point multiplying operator, unconv_fixed_op
   Vdec := Vdec * I;           -- fixed point multiplying operator
   Vdec := I   * Vdec;         -- fixed point multiplying operator
   Vdec := Vdec / I;           -- fixed point multiplying operator
   Vdec := Vdec * 2.0;         -- fixed point multiplying operator
   Vdec := Vdec * 2;           -- fixed point multiplying operator

   Vdur := Vdur * Vdur;        -- fixed point multiplying operator, unconv_fixed_op
   Vdur := Vdur / Vdur;        -- fixed point multiplying operator, unconv_fixed_op
   Vdur := Vdur * I;           -- fixed point multiplying operator
   Vdur := I   * Vdur;         -- fixed point multiplying operator
   Vdur := Vdur / I;           -- fixed point multiplying operator
   Vdur := Vdur * 2.0;         -- fixed point multiplying operator
   Vdur := Vdur * 2;           -- fixed point multiplying operator

end Test_Fixed_Multiplying_Op;
