separate (T_Style)
procedure Literals is
   -- Integer literals
   I1   : constant Integer := 0;
   I2   : constant Integer := 10_000;
   I3   : constant Integer := 1_0000;                       -- Numeric_Literal

   I4   : constant Integer := 1_2_3_4_5_6;                  -- Numeric_Literal

   IE1  : constant Integer := 1E9;
   IE2  : constant Integer := 1_000E6;
   IE3  : constant Integer := 1_0000E5;                     -- Numeric_Literal


   -- Real literals
   R1   : constant Float   := 0.0;
   R2   : constant Float   := 3.141_592;
   R3   : constant Float   := 1.61_80_33;                   -- Numeric_Literal

   RE1  : constant Float   := 1.0E-9;
   RE2  : constant Float   := 0.314_159E1;
   RE3  : constant Float   := 16_18.03_39E-3;               -- Numeric_Literal


   -- Based literals
   BI1  : constant Integer :=  2#1111_1111#;
   BI2  : constant Integer :=  8#0000_0177#;                -- OK (not specified)
   BI3  : constant Integer := 16#0000_00FF#;

   BI4  : constant Integer :=  7#0064_3502#;                -- Numeric_Literal (not allowed)
   BI5  : constant Integer := 13#003A_5BC8#;                -- Numeric_Literal (not allowed)

   BIE1 : constant Integer :=  2#0000_1010#E6;
   BIE2 : constant Integer := 16#0000_000A#E6;

   BR1  : constant Float   :=  2#1111_1111.1111_1111#;
   BR2  : constant Float   := 16#0000_00FF.FF00_0000#;
   BR3  : constant Float   := 16#41_89_AF_3B.FF_01_00_00#;  -- Numeric_Literal

   BR4  : constant Float   :=  7#0064_3502.2053_4600#;      -- Numeric_Literal (not allowed)

   BRE1 : constant Float   :=  2#0100_0001.1100_0000#E6;
   BRE2 : constant Float   := 16#C0_AF_E3.70_06#E1;         -- Numeric_Literal
begin
   null;
end Literals;
