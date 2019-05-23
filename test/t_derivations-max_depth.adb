separate (T_Derivations)
procedure Max_Depth is
   -- Tagged
   type I0 is interface;
   type I1 is interface and I0;
   type I2 is interface and I1;

   type T0 is tagged null record;
   subtype ST0 is T0;
   type T1 is new T0 and I0 with null record;
   type T2 is new T0 and I1 with null record;
   type T3_A is new T0 and I2 with null record;                             -- max 2, got 3
   type T3_B is new T0 and I2 with null record;                             -- max 2, got 3
   type T3_C is new T0 and I1 and I2 with null record;                      -- max 2, got 3
   type T3_D is new T0 and I2 and I1 with null record;                      -- max 2, got 3
   type T3_E is new ST0 and I2 and I1 with null record;                     -- max 2, got 3
   type T4 is new T3_B and I2 with null record;                             -- max 2, got 4
   type T5 is new T4 and I2 with null record;                               -- max 4, got 5

   -- Untagged
   type Int1 is new Integer;
   type Int2 is new Int1;                                                   -- max 1, got 2
   type Int3 is new Int2;                                                   -- max 1, got 3
   type Int4 is new Int3;                                                   -- max 1, got 4
   type Int5 is new Int4'Base;                                              -- max 1, got 5

   -- Task
   type TaInterface0 is task interface;
   type ILim0 is limited interface;
   type ILim1 is limited interface and ILim0;
   type ILim2 is limited interface and ILim1;

   task type Ta0 is
   end Ta0;
   task body Ta0 is
   begin
      null;
   end Ta0;
   subtype STa0 is Ta0;

   type Ta1_1 is new Ta0;                                                   -- max 0, got 1

   type Ta1_2 is new STa0;                                                  -- max 0, got 1

   task type Ta1_3 is new TaInterface0 with                                 -- max 0, got 1
     end Ta1_3;
   task body Ta1_3 is
   begin
      null;
   end Ta1_3;

   task type Ta1_4 is new ILim0 with                                        -- max 0, got 1
     end Ta1_4;
   task body Ta1_4 is
   begin
      null;
   end Ta1_4;

   task type Ta2 is new ILim2 with                                          -- max 1, got 3
      entry P;
   end Ta2;
   task body Ta2 is
   begin
      null;
   end Ta2;

   -- Protected
   protected type PT0 is
   end PT0;

   protected body PT0 is
   end PT0;
   subtype SPT0 is PT0;

   type PT1_1 is new PT0;

   type PT1_2 is new SPT0;

   protected type PT1_3 is new ILim0 with
     end PT1_3;

   protected body PT1_3 is
   end PT1_3;

   protected type PT1_4 is new ILim2 with                                   -- max 1, got 3
     end PT1_4;

   protected body PT1_4 is
   end PT1_4;

   -- Task interface
   type TaInterface1 is task interface and TaInterface0;                    -- max 0, got 1
   type TaInterface2 is task interface and TaInterface1;                    -- max 0, got 2
   type TaInterface3 is task interface and TaInterface2;                    -- max 0, got 3

   -- Protected interface
   type ProtectedInterface0 is protected interface;
   type ProtectedInterface1 is protected interface and ProtectedInterface0;
   type ProtectedInterface2 is protected interface and ProtectedInterface1; -- max 0, got 2
   type ProtectedInterface3 is protected interface and ProtectedInterface2; -- max 0, got 3

   -- Synchronized interface
   type SyncInterface is synchronized interface and ILim2;                  -- max 0, got 3

   -- Generic
   type TI0 is range 1 .. 10;
   type TI1 is new TI0;
   type TI2 is new TI1;                                                     -- max 1, got 2

   generic
      type F1 is new TI2;
   procedure Gen;

   procedure Gen is
      type D1 is new Integer;
      type D2 is new F1;                                                    -- From GenImpl: max 1, got 3
      type D3 is new T2 with null record;                                   -- max 2, got 3; From GenImpl: max 2, got 3
   begin
      null;
   end Gen;

   procedure GenImpl is new Gen (F1 => TI2);

   generic
      type F1 is new TI1;
   package GenPack is
      type D1 is new F1;                                                    -- From GenPackImpl: max 1, got 3
      type D2 is new D1;                                                    -- max 1, got 2; from GenPackImpl: max 1, got 4
      type D4 is new I2 with null record;                                   -- max 2, got 3; from GenPackImpl: max 1, got 3
   end GenPack;

   package GenPackImpl is new GenPack (TI2);
begin
   null;
end Max_Depth;
