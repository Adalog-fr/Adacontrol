procedure T_Max_Parameters (A, B, C, D, E : Integer) is

   procedure P1 (A : Integer; B: Float; C : Integer);
   procedure P1 (A : Integer; B: Float; C : Integer) is
   begin
      null;
   end P1;

   procedure P2 (A : Integer; B: Float) is
   begin
      null;
   end P2;

   function F1 (A : Integer; B: Float) return integer;
   function F1 (A : Integer; B: Float) return integer is
   begin
      return 0;
   end F1;

   function F2 (A : Integer) return integer is
   begin
      return 0;
   end F2;

   protected type Pt is
      entry E1 (A, B, C : Integer; D, E, F: Float);
      entry E2 (A, B, C : Integer);
      procedure PP1 (A, B, C : Integer; D, E, F: Float);
      procedure PP2 (A, B, C : Integer);
      function PF1 (A, B, C : Integer; D, E, F: Float) return integer;
      function PF2 (A, B, C : Integer) return integer;
   end Pt;
   protected body Pt is
      entry E1 (A, B, C : Integer; D, E, F: Float) when True is
      begin
         null;
      end;
      entry E2 (A, B, C : Integer) when True is
      begin
         null;
      end;
      procedure PP1 (A, B, C : Integer; D, E, F: Float) is
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
   procedure Gen (A : Integer; B: Float; C : Integer);
   procedure Gen (A : Integer; B: Float; C : Integer) is
   begin
      null;
   end;

   procedure Inst is new Gen;
begin
   null;
end T_Max_Parameters;
