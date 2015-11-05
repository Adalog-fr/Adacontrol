procedure T_Generic_Aliasing is

   -- =================== Tests for certain  ===================
   -- Objects
   generic
      O1 : Integer;
      O2 : in Integer;
      O3 : in out Integer;
      O4 : in out Integer;
   package Geno1 is end Geno1;
   V1 : aliased Integer;
   V2 : aliased Integer;
   V3 : aliased Integer;
   V4 : Integer renames V3;

   package Insto1 is new Geno1 (V1,   V1, V2, V3); -- OK
   package Insto2 is new Geno1 (V1,   V2, V2, V3); -- OK
   package Insto3 is new Geno1 (V1,   V2, V3, V3); -- Variable_Aliasing
   package Insto4 is new Geno1 (V1+1, 45, V4, V3); -- Variable_Aliasing

   -- Types
   generic
      type T1 (<>) is private;
      type T2 (<>) is private;
   procedure Gent;
   procedure Gent is begin null; end Gent;
   type Tag1 is tagged null record;
   type Tag2 is tagged null record;

   procedure Instt1 is new Gent (Integer,    Float);        -- OK
   procedure Instt2 is new Gent (Natural,    Positive);     -- Type_Aliasing;
   procedure Instt3 is new Gent (Integer,    Integer'Base); -- Type_Aliasing;
   procedure Instt4 is new Gent (Tag1,       Tag1'Class);   -- OK;
   procedure Instt5 is new Gent (Tag1'Class, Tag2'Class);   -- OK;
   procedure Instt6 is new Gent (Tag1'Class, Tag1'Class);   -- Type_Aliasing;

   -- Procedures
   procedure Proc1 is begin null; end Proc1;
   procedure Proc2 is begin null; end Proc2;
   procedure Ren1 renames Proc1;

   generic
      with procedure Proc1 is T_Generic_Aliasing.Proc2;
      with procedure Proc2 is <>;
   procedure Genpr;
   procedure Genpr is begin null; end Genpr;

   procedure Instp1 is new Genpr (Proc1, Proc2);   -- OK
   procedure Instp2 is new Genpr (Proc1, Proc1);   -- Subprogram_Aliasing
   procedure Instp3 is new Genpr (Proc1, Ren1);    -- Subprogram_Aliasing
   procedure Instp4 is new Genpr (Proc2 => Proc1); -- OK
   procedure Instp6 is new Genpr (Proc2 => Proc2); -- Subprogram_Aliasing
   procedure Instp7 is new Genpr (Proc2);          -- Subprogram_Aliasing
   procedure Instp8 is new Genpr;                  -- Subprogram_Aliasing

   -- Functions
   type Enum is (A, B, C);
   function "+"   (L : Enum) return Enum is begin return B; end "+";
   function Plus  (L : Enum) return Enum renames "+";
   function Succ1 (L : Enum) return Enum renames Enum'Succ;
   function Succ2 (L : Enum) return Enum renames Enum'Succ;
   function Noparm return Enum is begin return A; end Noparm;
   function Plus  (L : Integer) return Integer renames "+";


   generic
      type T is (<>);
      with function F1 (L : T) return T;
      with function F2 (L : T) return T;
   procedure Genf1;
   procedure Genf1 is begin null; end Genf1;

   procedure Instf11 is new Genf1 (Enum,    "+",          Enum'Succ);     -- OK
   procedure Instf12 is new Genf1 (Enum,    "+",          Plus);          -- Subprogram_Aliasing
   procedure Instf13 is new Genf1 (Enum,    Enum'Succ,    Enum'Succ);     -- Subprogram_Aliasing
   procedure Instf14 is new Genf1 (Enum,    Enum'Succ,    Enum'Pred);     -- OK
   procedure Instf15 is new Genf1 (Enum,    Enum'Succ,    Succ1);         -- Subprogram_Aliasing
   procedure Instf16 is new Genf1 (Enum,    Succ1,        Succ2);         -- Subprogram_Aliasing
   procedure Instf17 is new Genf1 (Integer, Integer'Succ, Positive'Succ); -- Subprogram_Aliasing
   procedure Instf18 is new Genf1 (Integer, "+",          "+");           -- Subprogram_Aliasing
   procedure Instf19 is new Genf1 (Integer, "+",          Plus);          -- Subprogram_Aliasing

   generic
      with function F1 return Enum;
      with function F2 return Enum;
   procedure Genf2;
   procedure Genf2 is begin null; end Genf2;

   procedure Instf21 is new Genf2 (Noparm, A);            -- OK
   procedure Instf22 is new Genf2 (A, A);                 -- Subprogram_Aliasing

   generic
      with function F1 return String;
      with function F2 return String;
   procedure Genf3;
   procedure Genf3 is begin null; end Genf3;

   -- Packages
   generic
      with package Pack1 is new Geno1 (V1, V1, V2, V3);
      with package Pack2 is new Geno1 (<>);
   function Genpa return Integer;
   function Genpa return Integer is begin return 0; end Genpa;
   package Insto5 renames Insto1;

   function Instpa1 is new Genpa (Insto1, Insto2); -- OK
   function Instpa2 is new Genpa (Insto1, Insto1); -- Package_Aliasing
   function Instpa3 is new Genpa (Insto1, Insto5); -- Package_Aliasing

   -- =================== Tests for possible and unlikely  ===================

   type Acc_Int is access all Integer;
   P1 : Acc_Int := V2'Access;
   P2 : Acc_Int := new Integer;
   Tab : array (1 .. 3) of Integer;

   package Insto6 is new Geno1 (V1, V1, P1.all, P2.all);       -- Unlikely variable
   package Insto7 is new Geno1 (V1, V1, P1.all, P1.all);       -- Certain variable
   package Insto8 is new Geno1 (V1, V1, Tab (V1), Tab (V2));   -- Possible variable

   type Acc_Proc is access procedure;
   Proc_Ptr : Acc_Proc;
   procedure Instap1 is new Genpr (Proc1,        Proc_Ptr.all);  -- Unlikely procedure
   procedure Instap2 is new Genpr (Proc_Ptr.all, Proc_Ptr.all);  -- Certain procedure

   -- Special case, Gnat dependent: implementation defined attributes
   -- whose prefix is a variable
   generic
      with function F1 return String;
      with function F2 return String;
   procedure P;
   procedure P is begin null; end;

   V5 : Integer;
   V6 : Integer;
   V7 : String (1 .. 10);

   procedure I1 is new P (V5'Img, V5'Img);              -- Certain subprogram
   procedure I2 is new P (V5'Img, V6'Img);              -- OK
   procedure I3 is new P (V7 (1)'Img, V7 (2)'Img);      -- OK
   procedure I4 is new P (V7 (1)'Img, V7 (1)'Img);      -- Certain subprogram
   procedure I5 is new P (V7 (V1)'Img, V7 (V2)'Img);    -- Possible subprogram

begin
   null;
end T_Generic_Aliasing;
