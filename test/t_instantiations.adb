with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with X_Instantiations;
procedure T_instantiations is
   generic procedure AUD renames Ada.Unchecked_Deallocation;

   type String_Access is access String;
   type String_Access_2 is access String;

   type Integer_Access is access Integer;
   type Natural_Access is access Natural;

   procedure Free_1 is             -- T1, T4, T5, local_instantiation
      new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Free_2 is             -- T1, T4, local_instantiation
      new Ada.Unchecked_Deallocation (String, String_Access_2);

   procedure Free_3 is             -- T1, T3, local_instantiation
      new Ada.Unchecked_Deallocation (Standard.Integer, Integer_Access);

   procedure Free_4 is             -- T1, T2, T3, local_instantiation
      new Ada.Unchecked_Deallocation (Standard.Natural, Natural_Access);

   type Tagged_T is tagged null record;
   type Tagged_Access is access Tagged_T'class;
   subtype Class_T is Tagged_T'Class;
   procedure Free_5 is             -- T1, T8, local_instantiation
      new Ada.Unchecked_Deallocation (Tagged_T'class, Tagged_Access);

   procedure Free_6 is             -- T1, T8, local_instantiation
      new Ada.Unchecked_Deallocation (Class_T, Tagged_Access);

   procedure Free_7 is             -- T1, T2, T3, local_instantiation
     new AUD (Standard.Natural, Natural_Access);

   function To_Integer_Access is   -- T6, local_instantiation
      new Ada.Unchecked_Conversion (String_Access, Integer_Access);

   function To_Mod is new Ada.Unchecked_Conversion (Integer, Float); -- T7, local_instantiation

   package Pack is
      type Priv is private;
      type Extend is new Tagged_T with private;
   private
      type Priv is new Integer;
      type Extend is new Tagged_T with null record;
   end Pack;

   function Cheater1 is new Ada.Unchecked_Conversion (Pack.Priv,   Integer);  -- T9, local_instantiation
   function Cheater2 is new Ada.Unchecked_Conversion (Pack.Extend, Tagged_T); -- T9, local_instantiation

   type Derived is new Duration;
   function Duration_To_Integer is new Ada.Unchecked_Conversion (Derived, Integer);  -- T10

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

   generic
      type T1 is private;
      Val : String := "";
   procedure G3;
   procedure G3 is begin null; end;
   procedure IG31 is new G3 (Integer);
   procedure IG32 is new G3 (Integer, "Some Message");     -- Repeat6
   procedure IG33 is new G3 (Integer, "Something else");   -- Repeat6
   procedure IG34 is new G3 (Float, "Some Message");
   procedure IG35 is new G3 (Float, "Some Other Message"); -- Repeat6

   procedure P21 is new G2 (Integer, Proc, Float);
   procedure P22 is new G2 (Integer, Proc, Float);         -- Repeat4
   procedure P23 is new G2 (T2 => Float, T1 => Integer);   -- Repeat4
   procedure P24 is new G2 (Integer, Proc1, Float);        -- Repeat4
   procedure P25 is new G2 (Integer, Proc1, Float);        -- Repeat4, Repeat5

   generic
      type T1 is private;
      type T2 is private;
      type T3 is private;
   procedure G4;
   procedure G4 is begin null; end;
   procedure IG41 is new G4 (Integer, Integer, Integer);
   procedure IG42 is new G4 (Integer, Float,   Integer);   -- Repeat 7

   generic
   package GP is
      generic
      package GPGP is
      end GPGP;
   end GP;

   package body GP is
      package body GPGP is
      end GPGP;
   end GP;

   package IP is new GP;
   package IPIP is new IP.GPGP; -- GenGen
begin
   declare
      procedure L1 is new G1 (Duration, Duration);
      procedure L2 is new G1 (Duration, Integer);     -- Repeat1, Repeat2, Repeat3
   begin
      null;
   end;
end T_instantiations;
