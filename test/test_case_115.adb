procedure Test_Case_115 is

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
      Tab : array (1..10) of Rec;
   begin
      Proc_SC (X.S,      X.S(1));             -- Aliasing
      Proc_CC (X.S(1),   X.S(2));             -- Possible aliasing
      Proc_SC (Tab(1).S, Tab(2).S(1));        -- Possible aliasing
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
      Proc_SC (A.all,   A(1));                -- Aliasing
      Proc_SC (G.all,   G(1));                -- Possible aliasing
      Proc_SC (B.all,   R.A(1));              -- Aliasing
      Proc_SC (R.A.all, R.A(1));              -- Aliasing
      Proc_SC (R.A.all, R.B(1));              -- Unlikely aliasing
      Proc_AS (A,       A.all);               -- Unlikely aliasing
      Proc_CC (X => F.all(1), Y => F(1));     -- Possible aliasing
      Proc_SC (Y => F(1),     X => F.all);    -- Possible aliasing
      Proc_SC (C.all, R.S(1));
   end Dereferences;

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
   end Dispatching;
end Test_Case_115;

