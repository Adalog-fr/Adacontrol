procedure T_Non_Static is
   -- Misc. types
   type Enum is (A, B, C);
   type Rec is
      record
         I : Integer;
      end record;
   type Acc is access Integer;

   -- Non static values:
   V  : Integer;
   VA : Acc;
   type Ptr_Proc is access procedure;
   Ptr : Ptr_Proc;

   -- Check objects
   V1 : Integer;
   V2 : Integer := 3 + 2;
   V3 : Integer := V;                  -- Variable_Initialization
   V4 : Enum := A;
   V5 : Enum := Enum'Succ (A);
   V6 : Enum := V5;                    -- Variable_Initialization
   V7 : Rec  := (I => 1);
   V8 : Acc;
   V9 : Acc := null;
   V10 : Acc := new Integer;           -- Variable_Initialization

   C1 : constant Integer := 2 ** 4;
   C2 : constant Integer := V;         -- Constant_Initialization


   -- Check Index_Constraint
   type Tab1 is array (Integer range <>)    of Integer;
   type Tab2 is array (Integer range 1..10) of Integer;
   type Tab3 is array (Integer range 1..V)  of Integer; -- Index_Constraint

   T1 : Tab1 (1..10);
   T2 : array (1..10) of Integer;
   T3 : Tab1 (1..V);                                    -- Index_Constraint
   T4 : array (1..V) of Integer;                        -- Index_Constraint

   subtype Foo is Integer range 1..V;                   -- OK (not index or discr. constraint)

   -- Check Discriminant_Constraint
   type D (X : Integer) is null record;
   Y : D(1);
   X : D(V);                                            -- Discriminant_Constraint

   -- Check Instantiation
   generic
      X : Integer;
      Y : Integer := V;
   procedure Gen1;
   procedure Gen1 is begin null; end;

   procedure Inst11 is new Gen1 (1, 2);
   procedure Inst12 is new Gen1 (V, V);                   -- Instantiation x2
   procedure Inst13 is new Gen1 (1);                      -- Instantiation

   generic
      X : in out Integer;
   procedure Gen2;
   procedure Gen2 is begin null; end;

   procedure Inst21 is new Gen2 (V);
   procedure Inst22 is new Gen2 (T_Non_Static.V);
   procedure Inst23 is new Gen2 (T2(V));                  -- Instantiation
   procedure Inst24 is new Gen2 (Va.all);                 -- Instantiation

   procedure Static is begin null; end;
   procedure Ren1 renames Static;
   procedure Ren2 renames Ptr.all;

   generic
      with procedure FP is Static;
   procedure Gen3;
   procedure Gen3 is begin null; end;

   procedure Inst31 is new Gen3 (Static);
   procedure Inst32 is new Gen3;
   procedure Inst33 is new Gen3 (Ren1);
   procedure Inst34 is new Gen3 (Ptr.all);               -- Instantiation
   procedure Inst35 is new Gen3 (Ren2);                  -- Instantiation
begin
   null;
end T_Non_Static;
