pragma Ada_2012;
separate (T_derivations)
procedure From is
   -- Check plain types
   type T1 is new Integer;                -- From: Standard.Integer, category RANGE
   type T2 is new T1 range 1 .. 10;       -- From: Standard.Integer, category RANGE

   type T3 is tagged
      record
         F : Float;
      end record;
   type T4 is new T3 with null record;    -- From: T3
   type T5 is new T4                      -- From: T4, T3
     with record
      I : Integer;
   end record;

   type T6 is range 1 .. 10;
   type T7 is new T6;                     -- From: category RANGE
   type T8 is (One, Two, Three);
   type T9 is new T8;                     -- From: category ()

   type T10 is new Float;                 -- From: package Standard

   generic
      type F1 is new Integer;             -- From: Standard.Integer, category RANGE
      type F2 is new T2;                  -- From: Standard.Integer, category RANGE
      type F3 is new T5 with private;     -- From: T4, T3
   procedure P;
   procedure P is
      type DF1 is new F1;                 -- From: Standard.Integer, category RANGE
      type DF2 is new F2;                 -- From: Standard.Integer, category RANGE
   begin null; end P;

   -- Check subtypes
   subtype S1 is Integer range 1 .. 10;
   type S2 is new S1;                     -- From: Standard.Integer, category RANGE
   subtype S3 is S2 range 1 .. 5;
   type S4 is new S3;                     -- From: S3, Standard.Integer, category RANGE
   type S5 is new S3'Base;                -- From: Standard.Integer, category RANGE
   type S6 is new S4;                     -- From: S3, Standard.Integer, category RANGE

   -- Check interfaces and progenitors
   type I1 is interface;
   type I2 is interface;
   type II1 is new I1 and I2 with null record;    -- From: I1, I2; Max_Parents: more than 1
   type II2 is interface and I1 and I2;           -- From: I1, I2; Max_Parents: more than 1

   type MI1 is new T5  and I1  with null record;  -- From: T4, T3, I1;     Max_Parents: more than 1
   type MI2 is new MI1 and I2  with null record;  -- From: T4, T3, I1, I2; Max_Parents: more than 1
   type MI3 is new MI1         with null record;  -- From: T4, T3, I1
   type MI4 is new MI2 and II2 with null record;  -- From: T4, T3, I1, I2; Max_Parents: more than 1

   -- Check tasks and POs
   task type Task1 is
   end Task1;
   task body Task1 is begin null; end;
   type Task_D1 is new Task1;                     -- From: category TASK

   type Ilim is limited interface;
   type Itask is task interface;
   task type Task2 is new Ilim and Itask with     -- From: Ilim; Max_Parents: more than 1
   end Task2;
   task body Task2 is begin null; end;

   protected type PO1 is
      procedure P;
   end PO1;
   protected body PO1 Is
      procedure P is begin null; end;
   end PO1;
   type PO_D1 is new PO1;                         -- From: category PROTECTED

   protected type PO2 is new Ilim with            -- From: Ilim;
        procedure P;
   end PO2;
   protected body PO2 Is
      procedure P is begin null; end;
   end PO2;
begin
   null;
end From;
