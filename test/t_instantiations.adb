with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with X_Instantiations;
procedure T_instantiations is

   type String_Access is access String;
   type String_Access_2 is access String;

   type Integer_Access is access Integer;
   type Natural_Access is access Natural;

   -- T2 never matched

   procedure Free_1 is             -- T1, T4, T5, local_instantiation
      new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Free_2 is             -- T1, T4, local_instantiation
      new Ada.Unchecked_Deallocation (String, String_Access_2);

   procedure Free_3 is             -- T1, T3, local_instantiation
      new Ada.Unchecked_Deallocation (Standard.Integer, Integer_Access);

   procedure Free_4 is             -- T1, T3, local_instantiation
      new Ada.Unchecked_Deallocation (Standard.Natural, Natural_Access);

   function To_Integer_Access is   -- T6, local_instantiation
      new Ada.Unchecked_Conversion (String_Access, Integer_Access);

   generic
      type T1 is private;
      type T2 is private;
   procedure G1;
   procedure G1 is begin null; end;

   procedure P11 is new G1 (Integer, Float);
   procedure P12 is new G1 (Float,   Integer);
   procedure P13 is new G1 (Integer, Integer);  -- Repeat1, Repeat2
   procedure P14 is new G1 (Float,   Float);    -- Repeat1, Repeat2
   procedure P15 is new G1 (Integer, Float);    -- Repeat1, Repeat2

   procedure Proc is begin null; end;
   procedure Proc1 is begin null; end;

   generic
      type T1 is private;
      with procedure P is Proc;
      type T2 is private;
   procedure G2;
   procedure G2 is begin null; end;

   procedure P21 is new G2 (Integer, Proc, Float);
   procedure P22 is new G2 (Integer, Proc, Float);        -- Repeat4
   procedure P23 is new G2 (T2 => Float, T1 => Integer);  -- Repeat4
   procedure P24 is new G2 (Integer, Proc1, Float);       -- Repeat4
   procedure P25 is new G2 (Integer, Proc1, Float);       -- Repeat4, Repeat5
begin
   declare
      procedure L1 is new G1 (Duration, Duration);
      procedure L2 is new G1 (Duration, Integer);     -- Repeat1, Repeat2, Repeat3
   begin
      null;
   end;
end T_instantiations;
