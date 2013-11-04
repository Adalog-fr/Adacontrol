procedure T_Parameter_Declarations (A, B, C, D, E : Integer) is

   procedure P1 (A : Integer; B : in out Float; C : out Integer);
   procedure P1 (A : Integer; B: in out Float; C : out Integer) is
   begin
      null;
   end P1;

   procedure P2 (A : Integer := 0; B: Float := 0.0) is
   begin
      null;
   end P2;

   procedure P3 (A, B, C, D, E, F : Integer) is
   begin
      null;
   end P3;

   procedure P4 (A, B, C, D, E, F : out Integer) is
   begin
      null;
   end P4;

   procedure P5 (A, B, C, D, E, F : in out Integer) is
   begin
      null;
   end P5;

   procedure P6 (A : Integer; B : access Integer) is
   begin
      null;
   end P6;

   procedure P7 (A : access Integer; B : access Integer) is
   begin
      null;
   end P7;

   function F1 (A : Integer; B: Float) return integer;
   function F1 (A : Integer; B: Float) return integer is
   begin
      return 0;
   end F1;

   function F2 (A : Integer := 0) return integer is
   begin
      return 0;
   end F2;

   function F3 return Boolean is
   begin
      return False;
   end F3;

   protected type Pt is
      entry E1 (A, B, C : out Integer; D, E, F: Float := 0.0);
      entry E2 (A, B, C : Integer);
      procedure PP1 (A, B, C : Integer; D, E, F: Float; G : out Float);
      procedure PP2 (A, B, C : Integer);
      function PF1 (A, B, C : Integer; D, E, F: Float) return integer;
      function PF2 (A, B, C : Integer) return integer;
   end Pt;
   protected body Pt is
      entry E1 (A, B, C : out Integer; D, E, F: Float := 0.0) when True is
      begin
         null;
      end;
      entry E2 (A, B, C : Integer) when True is
      begin
         null;
      end;
      procedure PP1 (A, B, C : Integer; D, E, F: Float; G : out Float) is
      begin
         null;
      end;
      procedure PP2 (A, B, C : Integer) is
      begin
         null;
      end;
      function PF1 (A, B, C : Integer; D, E, F: Float) return integer is
      begin
         return 0;
      end;
      function PF2 (A, B, C : Integer) return integer is
      begin
         return 0;
      end;
   end Pt;

   task Ta is
      entry E1 (A, B, C : Integer; D, E, F: Float);
      entry E2 (A, B, C : Integer);
   end Ta;
   task body Ta is
   begin
      accept E1 (A, B, C : Integer; D, E, F: Float);
      accept E2 (A, B, C : Integer);
   end Ta;

   generic
   procedure Gen (A : Integer := 0; B: Float := 0.0; C : Integer := 0);
   procedure Gen (A : Integer := 0; B: Float := 0.0; C : Integer := 0) is
   begin
      null;
   end;

   procedure Inst is new Gen;

   type Tag is tagged null record;
   subtype Tag_Wide is Tag'Class;

   procedure PCW1 (P1 : Tag'Class) is
   begin
      null;
   end PCW1;

   procedure PCW2 (P1, P2 : Tag_Wide) is
   begin
      null;
   end PCW2;

begin
   null;
end T_Parameter_Declarations;
