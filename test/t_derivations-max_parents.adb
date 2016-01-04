pragma Ada_2012;
separate (T_Derivations)
procedure Max_Parents is
   type I1 is interface;
   type I2 is interface;
   type Tag1 is tagged null record;

   type T1 is new I1                 with null record;
   type T2 is new Tag1               with null record;
   type T3 is new Tag1 and I1        with null record;   -- More than 1 parent
   type T4 is new I1   and I2        with null record;   -- More than 1 parent
   type T5 is new Tag1 and I1 and I2 with null record;   -- More than 2 parents

   -- Synchronized
   type LI1 is limited interface;
   type LI2 is limited interface;
   type LI3 is limited interface;

   task S1 is new LI1 with end S1;               -- OK
   task body S1 is begin null; end;

   task type S2 is new LI1 and LI2 with          -- More than 1 parent
      entry E;
   end S2;
   task body S2 is begin accept E; end S2;

   protected S3 is new LI1 and LI2 and LI3 with  -- More than 2 parents
      procedure P;
   end S3;
   protected body S3 is procedure P is begin null; end; end S3;

   protected type S4 is new LI1 with             -- OK
        procedure P;
   end S4;
   protected body S4 is procedure P is begin null; end; end S4;
begin
   null;
end Max_Parents;
