package body T_Max_Primitives is

   --------
   -- P1 --
   --------

   procedure P1 (X : in out PT) is separate;

   --------
   -- F1 --
   --------

   function F1 (X : PT) return Integer is
   begin
      return 1;
   end F1;

   ---------
   -- Ta0 --
   ---------

   task body Ta0 is
   begin
      null;
   end Ta0;

   ---------
   -- Ta1 --
   ---------

   task body Ta1 is
   begin
      null;
   end Ta1;

   ----------
   -- PrT0 --
   ----------

   protected body PrT0 is
   end PrT0;

   ----------
   -- PrT1 --
   ----------

   protected body PrT1 is
   end PrT1;

   --------
   -- P1 --
   --------

   procedure P1 (T : TagT1) is
   begin
      null;
   end P1;

   package body Generic_Package is
      procedure P4 (X : Der) is null;

      type ImplType is new TagT1 and GI1 and GI2 with null record;
   end Generic_Package;

   type Custom is new TagT1 and GI1 and GI2 with null record;

   package Impl is new Generic_Package (Custom);                     -- BigDer max 3, got 5; Der max 5, got 8
end T_Max_Primitives;
