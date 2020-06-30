with X_Max_Primitives; use X_Max_Primitives;

package T_Max_Primitives is
   -- Untagged
   type Int1 is new Integer;                             --  max 3, got 5

   procedure Int1_P1 (I : Int1) is null;
   procedure Int1_P2 (I : Int1) is null;
   procedure Int1_P3 (I : Int1) is null;
   procedure Int1_P4 (I : Int1) is null;
   procedure Int1_P5 (I : Int1) is null;

   type Enum is (Foo, Bar);                              --  max 3, got 5

   procedure Int1_P1 (I : Enum) is null;
   procedure Int1_P2 (I : Enum) is null;
   procedure Int1_P3 (I : Enum) is null;
   procedure Int1_P4 (I : Enum) is null;
   procedure Int1_P5 (I : Enum) is null;

   -- Tagged
   type PT is private;                                   --  max 3, got 4

   procedure P1 (X : in out PT);
   procedure P2 (X : in out PT) is null;
   function F1 (X : PT) return Integer;
   function F2 (X : PT) return Integer is (F1 (X) + 1);

   type I1 is interface;

   procedure P1 (X : in out I1) is abstract;
   procedure P2 (X : in out I1) is null;

   type I2 is interface and I1;                          --  max 3, got 4 x2
   function F1 (X : I2) return Integer is abstract;
   function F2 (X : I2) return Integer is abstract;

   -- Tasks and protected

   type SI1 is synchronized interface;
   type SI2 is synchronized interface;

   procedure PSI1_1 (T : SI1) is abstract;
   procedure PSI1_2 (T : SI1) is abstract;
   procedure PSI2_1 (T : SI2) is abstract;
   procedure PSI2_2 (T : SI2) is abstract;


   task type Ta0 is
   end;

   task type Ta1 is new SI1 and SI2 with                 --  max 3, got 4 x2
   end;

   overriding procedure PSI1_1 (T : Ta1) is null;
   overriding procedure PSI1_2 (T : Ta1) is null;
   overriding procedure PSI2_1 (T : Ta1) is null;
   overriding procedure PSI2_2 (T : Ta1) is null;

   protected type PrT0 is
   end;

   protected type PrT1 is new SI1 and SI2 with           --  max 3, got 4 x2
   end;

   overriding procedure PSI1_1 (T : PrT1) is null;
   overriding procedure PSI1_2 (T : PrT1) is null;
   overriding procedure PSI2_1 (T : PrT1) is null;
   overriding procedure PSI2_2 (T : PrT1) is null;

   -- Generic
   type TagT1 is tagged null record;
   type GI1 is interface;
   type GI2 is interface;

   procedure P1 (T : TagT1);
   procedure P2 (T : TagT1) is null;
   procedure P3 (T : TagT1) is null;

   procedure GI1_P1 (T : GI1) is null;
   procedure GI1_P2 (T : GI1) is null;

   procedure GI2_P1 (T : GI2) is null;
   procedure GI2_P2 (T : GI2) is null;

   generic
      type GT is new TagT1 and GI1 and GI2 with private;
   package Generic_Package is
      procedure P4 (X : out GT) is null;

      type Der is new GT with null record;               --  max 3, got 8; max 5, got 8
      not overriding procedure P4 (X : Der);
      overriding procedure P1 (X : Der) is null;
      overriding procedure P2 (X : Der) is null;
      overriding procedure P3 (X : Der) is null;

      type BigDer is new TagT1 and GI1 with null record; --  max 3, got 5 x2
   end Generic_Package;

   type PT2 is new GI1 and GI2 with private;             --  max 3, got 4
   type PT3 is private;
   type PT4 is new TagT1 and GI1 with private;           --  max 3, got 5

   procedure P1_PT3 (T : PT3) is null;
   procedure P2_PT3 (T : PT3) is null;
   procedure P3_PT3 (T : PT3) is null;
   procedure P4_PT3 (T : PT3) is null;

   -- Incomplete types
   type IT;
   type ITT;                                             --  max 3, got 5 x2

   procedure P1_IT (T : IT) is null;
   procedure P2_IT (T : IT) is null;
   procedure P3_IT (T : IT) is null;
   procedure P4_IT (T : IT) is null;

   type IT is null record;                               --  max 3, got 4
   type ITT is tagged null record;

   procedure P1_ITT (T : ITT) is null;
   procedure P2_ITT (T : ITT) is null;
   procedure P3_ITT (T : ITT) is null;
   procedure P4_ITT (T : ITT) is null;
   procedure P5_ITT (T : ITT) is null;
private
   type PT is tagged record                              --  max 3, got 4
      I : Integer;
   end record;

   type PT2 is new TagT1 and GI1 and GI2 with record     --  max 5, got 7
      I : Integer;
   end record;

   type T1 is new PT with null record;                   --  max 3, got 4

   procedure P1 (X : in out T1) is null;

   type PT3 is range 1 .. 10;                            --  max 3, got 5

   type PT4 is new TagT1 and GI1 with null record;       --  max 3, got 5

   procedure P5_PT3 (T : PT3) is null;
end T_Max_Primitives;
