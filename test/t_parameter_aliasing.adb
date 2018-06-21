procedure T_parameter_aliasing is
   package Wrapper is
      type By_Copy is private;     -- A by copy type that is not subject to Ada 2012 aliasing checks
   private
      type By_Copy is new Character;
   end Wrapper;
   use Wrapper;
   type BC_String is array (Positive range <>) of By_Copy;

   type By_Ref is tagged null record;

   procedure Proc_CC  (X : out By_Copy;   Y : in out By_Copy) is begin null; end;
   procedure Proc_CC2 (X : in  By_Copy;   Y : out    By_Copy) is begin null; end;
   procedure Proc_CCC (X : out By_Copy;   Y : in     By_Copy; Z : in By_Copy) is begin null; end;
   procedure Proc_RRR (X : out By_Ref;    Y : in     By_Ref;  Z : in By_Ref)  is begin null; end;
   procedure Proc_CC3 (                   Y : in     By_Copy; Z : in By_Copy; X : out By_Copy) is begin null; end;
   procedure Proc_RR3 (                   Y : in     By_Ref;  Z : in By_Ref;  X : out By_Ref)  is begin null; end;
   procedure Proc_SC  (X : out BC_String; Y : out    By_Copy) is begin null; end;

   function  Func_CC  (X : out By_Copy;   Y : in out By_Copy) return Integer is begin return 1; end;
   Result : Integer;

   function "+" (C : By_Copy; I : Integer) return By_Copy is
   begin
      return C;
   end "+";
begin
Simple_Cases :
   declare
      package Pack1 is
         X : By_Copy;
      end Pack1;
      package Pack2 renames Pack1;

      I,J : By_Copy;
      Alias1 : By_Copy renames I;

      R1, R2 : By_Ref;
   begin
      Proc_CC (I, I);                         -- Aliasing
      Proc_CCC (I, I, I);                     -- OK (by copy type)
      Proc_RRR (R1, R1, R1);                  -- Aliasing
      Proc_CC (I, J);                         -- OK
      Proc_CCC(I,  J,  J);                    -- OK
      Proc_RRR(R1, R2, R2);                   -- OK
      Proc_CCC(I,  I,  J);                    -- OK (by copy type)
      Proc_RRR(R1, R1, R2);                   -- Aliasing
      Proc_CC3(    I,  J,  I);                -- OK (by copy type)
      Proc_RR3(   R1, R2, R1);                -- Aliasing
      Proc_CC2(J+1, J);                       -- OK
      Proc_CC (X => I, Y => J);               -- OK
      Proc_CC (X => Simple_Cases.I, Y => I);  -- Aliasing

      Proc_CC (I, By_Copy(I));                -- Aliasing

      Proc_CC (I, Alias1);                    -- Aliasing
      Proc_CC (Pack1.X, Pack2.X);             -- Aliasing

      Result := Func_CC (I, I);                         -- Aliasing
      Result := Func_CC (X => Simple_Cases.I, Y => I);  -- Aliasing
      Result := Func_CC (I, J);                         -- OK
   end Simple_Cases;

Selectors:
   declare
      type Rec1 is
         record
            I, J : By_Copy;
         end record;

      type Rec2 is
         record
            I, J : By_Copy;
            K, L : Rec1;
         end record;

      procedure Proc_R1C (X : out Rec1; Y : in out By_Copy) is begin null;  end;

      procedure Proc_R2C (X : out Rec2; Y : in out By_Copy) is begin null; end;

      R1, R2 : Rec2;
      Alias2 : Rec2 renames R1;
      Alias3 : Rec1 renames Alias2.K;
   begin
      Proc_CC (R1.I, R1.I);                   -- Aliasing
      Proc_CC (R1.I, R1.J);                   -- OK
      Proc_CC (R1.I, R2.I);                   -- OK

      Proc_R2C (R1,     R1.I);                -- Aliasing
      Proc_R2C (R1,     R1.K.I);              -- Aliasing
      Proc_R2C (R1,     R1.K.J);              -- Aliasing
      Proc_R2C (Alias2, R1.K.J);              -- Aliasing
      Proc_R2C (R1,     Alias2.K.J);          -- Aliasing
      Proc_R2C (R1,     Alias3.J);            -- Aliasing

      Proc_R1C (R1.K, R1.K.J);                -- Aliasing
      Proc_R1C (R1.L, R1.K.J);                -- OK
   end Selectors;


Indexing:
   declare
      type Rec is
         record
            S : BC_String (1..10);
         end record;
      X   : Rec;
      I,J : Integer;
      Tab1 : array (1..10) of Rec;
      type Enum is (A, B);
      Tab2 : array (Enum) of By_Copy;
      E : Enum;
   begin
      Proc_SC (X.S,               X.S(1));            -- Aliasing
      Proc_CC (X.S(3),            X.S(4));            -- OK (static indexing)
      Proc_CC (X.S(3),            X.S(10#3#));        -- Aliasing
      Proc_CC (X.S(I),            X.S(J));            -- Possible aliasing
      Proc_CC (Tab2 (A),          Tab2 (B));          -- OK (static indexing)
      Proc_CC (Tab2 (Indexing.A), Tab2 (Indexing.B)); -- OK (static indexing)
      Proc_CC (Tab2 (A),          Tab2 (a));          -- Aliasing
      Proc_CC (Tab2 (A),          Tab2 (E));          -- Possible aliasing
      Proc_SC (Tab1(I).S,         Tab1(J).S(3));      -- Possible aliasing
      Proc_SC (Tab1(I).S(3..5),   Tab1(J).S(3));      -- Possible aliasing
   end Indexing;

Dereferences:
   declare
      type Acc is access all BC_String (1..10);

      function F return Acc is begin return null; end;
      function G return Acc renames F;

      procedure Proc_AS (X : out Acc; Y : out BC_String) is begin null; end;

      type Rec is
         record
            S : aliased BC_String (1..10);
            A : Acc;
            B : Acc;
         end record;

      --   X : Character;
      R : Rec;
      A : Acc;
      B : Acc renames R.A;
      C : Acc := R.S'Access;
   begin
      Proc_SC (A.all,   A(3));                      -- Aliasing
      Proc_SC (G.all,   F(3));                      -- Unlikely aliasing
      Proc_SC (B.all,   R.A(3));                    -- Aliasing
      Proc_SC (R.A.all, R.A(3));                    -- Aliasing
      Proc_SC (R.A.all, R.B(3));                    -- Unlikely aliasing
      Proc_AS (A,       A.all);                     -- Unlikely aliasing
      Proc_CC (X => F.all(3), Y => F(3));           -- Unlikely aliasing
      Proc_SC (Y => Dereferences.F(3), X => F.all); -- Unlikely aliasing
      Proc_SC (C.all, R.S(3));                      -- Unlikely aliasing (but true here)
   end Dereferences;

   -- The following case is not currently detected.
   -- Test is there for future improvements
Dispatching:
   declare
      package Pack is
         type T is tagged null record;
         procedure Proc_T (A : out T; B : out T);
      end Pack;
      package body Pack is
         procedure Proc_T (A : out T; B : out T) is begin null; end;
      end Pack;
      use Pack;
      X : T;
      Y : T'Class := X;
      Z : T'Class := Y;
   begin
      Proc_T (Y, Z);
      Proc_T (Y, Y);                          -- Aliasing (not detected)
      Proc_T (A => Y, B => Y);                -- Aliasing (not detected)
   end Dispatching;

   Mantis_0000006 :
   declare
      procedure P (O : in out Integer; X, Y : in out Integer) is begin null; end;
      type Tab1 is array (1 .. 10) of Integer;
      type Tab2 is array (1 .. 10, 1 .. 10) of Integer;
      type PTab1 is access Tab1;
      type PTab2 is access Tab2;
      A : Ptab1;
      B : Ptab2;
      O : Integer;
   begin
      P (O, A (1), B (1, 1));                 -- Unlikely aliasing
   end Mantis_0000006;

   Mantis_0000013 :
   declare
      type T is tagged
         record
            I : By_Copy;
         end record;
      type T_Acc is access T;
      type TC_Acc is access T'Class;
      type D is new T with null record;

      VT : T;
      VTA : T_Acc;
      VTAC : TC_Acc;

      procedure P1 (X, Y : in out By_Copy) is
      begin
         null;
      end P1;

      procedure P2 (L : access T'Class; R : By_Copy) is
      begin
         null;
      end P2;

      procedure P3 (L : By_Copy; R : access T'Class) is
      begin
         null;
      end P3;

      procedure Q (X, Y : access T) is
      begin
         P1 (X.I, Y.I);                       -- Unlikely aliasing
      end Q;

      procedure R (X, Y : access T'Class) is
      begin
         P1 (X.I, Y.I);                       -- Unlikely aliasing
         P2 (X,   X.I);
         P3 (X.I, X);
      end R;

   begin
      P1 (VT.I, VT.I);                        -- Aliasing
      P1 (VTA.I, VTA.I);                      -- Aliasing
      P1 (VTAC.I, VTAC.I);                    -- Aliasing

      P1 (VT.I, VTA.I);                       -- Unlikely aliasing
      P1 (VTA.I, VTAC.I);                     -- Unlikely aliasing
   end Mantis_0000013;
end T_parameter_aliasing;
