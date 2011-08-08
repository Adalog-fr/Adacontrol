procedure T_Non_Static is

   -- Non static values:
   V : Integer;
   type Ptr_Proc is access procedure;
   Ptr : Ptr_Proc;

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
   procedure Inst12 is new Gen1 (V, V);                   -- Instantiation
   procedure Inst13 is new Gen1 (1);                      -- Instantiation

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
