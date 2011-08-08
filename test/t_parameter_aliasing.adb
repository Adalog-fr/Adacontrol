procedure T_parameter_aliasing is

   procedure Proc_CC (X : out Character; Y : in out Character) is begin null; end;
   procedure Proc_SC (X : out String;    Y : out    Character) is begin null; end;

begin
Simple_Cases :
   declare
      package Pack1 is
         X : Character;
      end Pack1;
      package Pack2 renames Pack1;

      I,J : Character;
      Alias1 : Character renames I;
   begin
      Proc_CC (I, I);                         -- Aliasing
      Proc_CC (I, J);                         -- OK
      Proc_CC (X => I, Y => J);               -- OK
      Proc_CC (X => Simple_Cases.I, Y => I);  -- Aliasing

      Proc_CC (I, Character(I));              -- Aliasing

      Proc_CC (I, Alias1);                    -- Aliasing
      Proc_CC (Pack1.X, Pack2.X);             -- Aliasing
   end Simple_Cases;

Selectors:
   declare
      type Rec1 is
         record
            I, J : Character;
         end record;

      type Rec2 is
         record
            I, J : Character;
            K, L : Rec1;
         end record;

      procedure Proc_R1C (X : out Rec1; Y : in out Character) is begin null;  end;

      procedure Proc_R2C (X : out Rec2; Y : in out Character) is begin null; end;

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
            S : String (1..10);
         end record;
      X   : Rec;
      I,J : Integer;
      Tab1 : array (1..10) of Rec;
      type Enum is (A, B);
      Tab2 : array (Enum) of Character;
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
      type Acc is access all String (1..10);

      function F return Acc is begin return null; end;
      function G return Acc renames F;

      procedure Proc_AS (X : out Acc; Y : out String) is begin null; end;

      type Rec is
         record
            S : aliased String (1..10);
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
      Proc_SC (G.all,   F(3));                      -- Possible aliasing
      Proc_SC (B.all,   R.A(3));                    -- Aliasing
      Proc_SC (R.A.all, R.A(3));                    -- Aliasing
      Proc_SC (R.A.all, R.B(3));                    -- Unlikely aliasing
      Proc_AS (A,       A.all);                     -- Unlikely aliasing
      Proc_CC (X => F.all(3), Y => F(3));           -- Possible aliasing
      Proc_SC (Y => Dereferences.F(3), X => F.all); -- Possible aliasing
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
      Proc_T (Y, Y);                          -- Aliasing
      Proc_T (A => Y, B => Y);                -- Aliasing
   end Dispatching;
end T_parameter_aliasing;

