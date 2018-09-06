with X_Usage;
with Calendar;
with Ada.Exceptions, Ada.Task_Identification;
with System;
procedure T_Usage is
   procedure Guards     is separate;   -- procedure
   procedure Parameters is separate;   -- procedure

   type Rec is
      record
         V : Integer;
      end record;
   type Acc is access Rec;

   Local : Integer;

   generic
      X : Integer;
   package Gen is
      Y1 : Integer := 0;
      Y2 : Integer;
      Y3 : Integer := 0;
      Z : constant Integer := 1;
      function "+" (Left : in Calendar.Time;
                    Right : in Duration) return Calendar.Time
                    renames Calendar."+";
   private
      Y4 : Integer;
   end Gen;

   package body Gen is
   begin
      Gen.Y2 := X;
      Gen.Y4 := X;
   end Gen;

   package Inst1 is new Gen (0);
   package Inst2 is new Gen (1);

   package Pack is
      A1, A2 : Integer;
      A3 : Integer range 1..10 := 1;
      A4 : Integer range 1..10;
      B : constant Integer := 1;
      C : Integer renames A1;
      D,E,F : Integer;

      I1 : constant := 1;
      I2 : constant := 2;
      R : constant := 3.0;

      VR1, VR2 : Rec;
      Ac : Acc;

      type Arr_Acc is array (1..10) of Acc;
      type Arr_Arr_Acc is array (1..10) of Arr_Acc;
      V1 : Acc;
      Const  : constant Acc := new Rec;
      Ren_C  : Integer renames Const.V;
      V2 : Arr_Acc;
      V3 : Arr_Arr_Acc;
      Y : array (1..10) of Arr_Acc;
      Z : array (1..10) of Arr_Acc;

      -- Some nasty subtypes and/or derived types...
      V4 : Natural;
      type Acc_Int is access Integer;
      type Derived is new Acc;
      V5 : Derived;
      V6 : Integer renames VR2.V;
   private
      XX : Integer;
      YY : Integer;
   end Pack;
   package body Pack is
   begin
      YY := 0;
   end Pack;
   use Pack;

   procedure P (X : Integer; Y : out Integer; Z : in out Integer) is
   begin
      null;
   end P;

   -- Special case for access types
   generic
      with package P1 is new Gen (0);
      with package P2 is new Gen (<>);
   package Gen_Gen is end Gen_Gen;

   package body Gen_Gen is
   begin
      P1.Y1 := P2.Y3;
   end Gen_Gen;

   package Inst_Inst is new Gen_Gen (Inst1, Inst2);

   use X_Usage;
begin
   Ac.V := 0;            -- Read of Ac

   Pack.VR1.V := 1;      -- Write of VR1
   V6         := 1;      -- Write of VR2
   Pack.A1 := Pack.A3;   -- Write of Pack.A1, Read of Pack.A3
   A2 := C;              -- Write of Pack.A2, Read of Pack.A1
   A2 := I2;
   Const.V := 1;         -- A constant on the LHS
   Ren_C   := 1;         -- A nastier constant on the LHS
   P (D, E, F);
   Inst1.Y1 := Inst2.Y3;

   X_Usage.Not_Included := 0;

   -- Exceptions
   begin
      raise E1;
   exception
      when E1 =>
         null;
   end;

   Ada.Exceptions.Raise_Exception (E2'Identity);

   begin
      null;
   exception
      when E3 =>
         null;
   end;
   -- Nothing on E4


   -- Tasks
   T1.E;
   abort T1;
   select
      X_Usage.T2.E;
   or delay 1.0;
   end select;
   Ada.Task_Identification.Abort_Task (T3'Identity);
   -- Nothing on T4 (the following should do nothing)
   if Ada.Task_Identification.Is_Terminated (T4'Identity) then
      null;
   end if;

   -- Protected
   P1.E;
   select
      X_Usage.P2.E;
   or delay 1.0;
   end select;
   if P3.F then
      null;
   end if;

   -- Pseudo constants
Pseudo_Const:
   declare
      Empty  : String (1 .. 0);
      S      : String (1 .. 10);
      I      : Integer range 1 .. 0;
      I2     : Integer range 1 .. 0;
      J      : constant Integer := 0;
      type Tab is array (Positive range <>) of Integer;
      subtype None is Tab (1 .. 0);
      type None_Der is new None;
      Nothing : None_Der;
      Something : Tab (1..10);
   begin
      if S = Empty then
         null;
      end if;
      if I = I then
         null;
      end if;
      if Something = Tab (Nothing) then
         null;
      end if;
   end Pseudo_Const;

   -- Types
   declare
      type T1 is (A, B, C);
      type T2 is range 1 .. 10;
      V1 : T2;

      subtype S2 is T2;
      subtype S3 is T2;
      V2 : S2;
   begin
      null;
   end;

   -- Procedure and functions
   declare
      procedure P1 is begin null; end;
      procedure P2;
      procedure P2 is begin null; end;
      function F1 return Integer;
      function F1 return Integer is begin return 0; end;
      function F2 return Integer is begin return 0; end;

      type Acc_Proc is access procedure;

      AP : Acc_Proc;
      AF : System.Address;

      I : Integer;
   begin
      P1;
      I := F1;
      Ap := P2'Access;
      Af := F2'Address;
   end;

   -- Generics
   declare
      generic
      package Gen1 is
         procedure P;
         procedure Q;
      end Gen1;
      package body Gen1 is
         procedure P is begin null; end;
         procedure Q is begin null; end;
      end Gen1;

      generic
      package Gen2 is
         procedure P;
      end Gen2;
      package body Gen2 is
         procedure P is begin null; end;
      end Gen2;

      package Inst is new Gen1;
   begin
      Inst.P;
   end;
end T_Usage;
