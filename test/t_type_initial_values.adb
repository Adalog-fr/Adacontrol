with X_Initial_Values;
procedure T_Type_Initial_Values is
   type Bool is new Boolean;   -- No constant
   type Int is new Integer;    -- No matching constant

   type T1 is range 1 .. 10;   -- No matching constant
   type T2 is (A, B, C);
   type T3 is range 0 .. 100;
   type T4 is new String (1..3);  -- No constant

   C1 : constant Int := 0;
   INit_T1 : constant T_Type_Initial_Values.T1 := 2;
   Init_T2 : constant T2 := B;
   C2, Init_T3, C3 : constant T3 := 3;

   package Pack is
      Init_T4 : constant T4 := "ABC";
      type T5 is digits 5;        -- No constant
   end Pack;

   Init_T5 : constant Pack.T5 := 0.0;
begin
   null;
end T_Type_Initial_Values;
