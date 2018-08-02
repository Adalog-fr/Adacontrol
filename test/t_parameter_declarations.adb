procedure T_Parameter_Declarations (A, B, C, D, E : Integer) is     -- Error: not 2 for procedure

   procedure P1 (A : out Integer) is                                -- Error: not 2 for procedure, single out
   begin
      null;
   end P1;

   procedure P2 (A : Integer; B : in out Float; C : out Integer);   -- Error: not 2 for procedure, single out (with in out)
   procedure P2 (A : Integer; B : in out Float; C : out Integer) is
   begin
      null;
   end P2;

   procedure P3 (A : Integer := 0; B: Float := 0.0) is              -- Warning: not in 0..1 for procedure
   begin
      null;
   end P3;

   procedure P4 (A, B, C, D, E, F : Integer) is                     -- Error: not 2 for procedure, warning in not in 0..5 for procedure
   begin
      null;
   end P4;

   procedure P5 (A, B, C, D, E, F : out Integer) is                 -- Error: not 2 for procedure, warning out not in 0..5 for procedure
   begin
         null;
   end P5;

   procedure P6 (A, B, C, D, E, F : in out Integer) is              -- Error: not 2 for procedure, warning in out not in 0..5 for procedure
   begin
      null;
   end P6;

   procedure P7 (A : Integer; B : access Integer) is                -- Warning: not in 0..1 for procedure
   begin
      null;
   end P7;

   procedure P8 (A : access Integer; B : access Integer) is         -- Warning: not in 0..1 for procedure, warning access not in 0..1 for procedure
   begin
      null;
   end P8;

   function F1 (A : Integer; B : Float) return integer;             -- Warning: not in 0..1 for function
   function F1 (A : Integer; B: Float) return integer is
   begin
      return 0;
   end F1;

   function F2 (A : Integer := 0) return integer is
   begin
      return 0;
   end F2;

   function F3 return Boolean is                                    -- Error: not in 1..2 for function
   begin
      return False;
   end F3;

   protected type Pt is
      entry E1 (A, B, C : out Integer; D, E, F: Float := 0.0);           -- Error: not in 2..4 for protected entry
      entry E2 (A, B, C : Integer);                                      -- Warning: not in 0..1 for protected entry
      procedure PP1 (A, B, C : Integer; D, E, F: Float; G : out Float);  -- Error: not in 2..4 for protected procedure
      procedure PP2 (A, B, C : Integer);                                 -- Warning: not in 0..2 for protected procedure
      function PF1 (A, B, C : Integer; D, E, F: Float) return integer;   -- Error: not in 2..4 for protected function, warning in not in 0..5 for protected function
      function PF2 (A, B, C : Integer) return integer;                   -- Warning: not in 0..2 for protected function
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
      entry E1 (A, B, C : Integer; D, E, F: Float);                         -- Error: not in 2..4 for task entry, warning: in not in 0..5 for task entry
      entry E2 (A, B, C : Integer);
   end Ta;
   task body Ta is
   begin
      accept E1 (A, B, C : Integer; D, E, F: Float);
      accept E2 (A, B, C : Integer);
   end Ta;

   generic
   procedure Gen (A : Integer := 0; B: Float := 0.0; C : Integer := 0);     -- Error: not 2 for procedure, warning: defaulted not in 0..2 for procedure
   procedure Gen (A : Integer := 0; B: Float := 0.0; C : Integer := 0) is
   begin
      null;
   end;

   procedure Inst is new Gen;                                               -- Error: not 2 for procedure, warning: defaulted not in 0..2 for procedure

   package Class_Package is
      type Tag is tagged null record;
      subtype Tag_Wide is Tag'Class;
      function F4 (A : Tag; B, C, D : Integer) return Integer;              -- Error: not 1..2 for function
      function F5 (A, B : Tag; C, D : Integer) return Integer;              -- Error: not 1..2 for function, warning tagged no in 0..1 for dispatching function
   end Class_Package;

   package body Class_Package is
      function F4 (A : Tag; B, C, D : Integer) return Integer is
      begin
         return 0;
      end F4;

      function F5 (A, B : Tag; C, D : Integer) return Integer is
      begin
         return 0;
      end F5;
   end Class_Package;
   use Class_Package;

   procedure PCW1 (P1 : Tag'Class) is                                       -- Error: not 2 for procedure, warning class wide not 0 for procedure
   begin
      null;
   end PCW1;

   procedure PCW2 (P1, P2 : Tag_Wide) is                                    -- Warning: not in 0..1 for procedure, class wide not 0 for procedure
   begin
      null;
   end PCW2;

begin
   null;
end T_Parameter_Declarations;
