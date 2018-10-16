with Ada.Unchecked_Conversion;
separate (T_Actual_Parameters)
procedure Entity is
   --## rule off Pos all_I all_C ## not the objective of this test
   A, B, C : Integer;
   Ren : integer renames A;
   procedure P (X : Integer := A; Y : Integer := B) is
   begin
      null;
   end;
   procedure Q (X : Integer) is null;

   function F return Integer is (3);
   function F (X : Integer) return Integer is
   begin
      return X**2;
   end F;
   function G return Integer is
   begin
      return 8652;
   end G;


   generic
      V : Integer;
      with function F return Integer is <>;
   procedure Gen;
   procedure Gen is begin null; end Gen;

   procedure Inst1 is new Gen (0);    -- Gen/F/F_No_Param (default)
   procedure Inst2 is new Gen (V => B, F => G); -- Gen/V/B, All_I
   procedure Inst3 is new Gen (1, F); -- Gen/F/F_No_Param

   function F_To_I is new Ada.Unchecked_Conversion (Float, Integer);
begin
   P;            -- P/X/A (default value), all/Y/B (default value)
   P (Ren);      -- all/Y/B (default value), P/X/A
   P (B, A);     -- P/X/B
   P (B);        -- all/Y/B (default value), P/X/B
   P (B, B);     -- P/X/B, all/Y/B
   P (C, A);     -- OK
   P (F, A);     -- calls/X/F_no_param
   P (F (1), C); -- OK, not the right F
   Q (F);        -- calls/X/F_no_param
   Q (F (1));    -- OK, not the right F

   A := F(F_To_I (1.0));  -- Instance
end Entity;
