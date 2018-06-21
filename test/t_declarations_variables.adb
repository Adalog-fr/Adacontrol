procedure T_Declarations_Variables is
   package Pack is
      type Lim1 is limited private;
      function F1 return Lim1;

      type Lim2 is tagged limited null record;
      type Lim3 is new Lim2 with null record;
      type Lim4 is limited new Lim3 with null record;
   private
      type Lim1 is new Integer;
   end Pack;

   package body Pack is
      function F1 return Lim1 is
      begin
         return 2;
      end F1;
   end Pack;
   use Pack;

   type Lim5 is limited record
      A : Integer;
      B : Float;
   end record;

   task type TT;
   task body TT is
   begin
      null;
   end TT;

   protected type PP is
      procedure P;
   end PP;
   protected body PP is
      procedure P is
      begin
         null;
      end P;
   end PP;
   V11 : Lim1;                      -- Uninitialized_Variable (Limited_Initialization = On)
   V12 : Lim1 := F1;

   V21 : Lim2;                      -- Uninitialized_Variable (Limited_Initialization = On)
   V22 : Lim2 := (null record);
   V23 : Lim2 := (others => <>);

   V31 : Lim3;                      -- Uninitialized_Variable (Limited_Initialization = On)
   V32 : Lim3 := (null record);
   V33 : Lim3 := (others => <>);

   V41 : Lim4;                      -- Uninitialized_Variable (Limited_Initialization = On)
   V42 : Lim4 := (null record);
   V43 : Lim4 := (others => <>);

   V51 : Lim5;                      -- Uninitialized_Variable (Limited_Initialization = On)
   V52 : Lim5 := (A => 1, B => 2.4);

   VT : TT;                         -- OK
   VP : PP;                         -- OK
begin
   null;
end T_Declarations_Variables;
