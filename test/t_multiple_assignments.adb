procedure T_Multiple_Assignments is
   type Rec is
      record
         A, B, C : Integer;
      end record;
   type Tab is array (1 .. 5) of Integer;
   type TabRec is array (1..3) of Rec;
   type RecRec is
      record
         X : Rec;
         Y : Tab;
         Z : TabRec;
      end record;

   R : Rec;
   T : Tab;
   RR : RecRec;
   B : Boolean;

   Rbis  : Rec     renames R;
   RABis : integer renames RBis.A;
   RBBis : Integer renames R.B;
begin
   -- Simple cases
   T (1) := 1;

   R.A := 1;
   R.B := 2;
   R.C := 3;   -- Groupable1, Groupable2, Count

   T (1) := 1;   -- Repeated
   T (2) := 2;
   null;
   T (3) := 3;
   T (4) := 4;   -- Groupable1

   if False then
      null;
   end if;

   R.A := 1;
   R.B := 2;

   RR.X.A     := 1;   -- Check
   RR.Y (2)   := 2;
   RR.Z (1).B := 3;  -- Groupable1, Groupable2, Count
   RR.Z (2).C := 4;  -- Groupable1

   RABis   := 1;     -- Repeated
   RBBis   := 2;     -- Repeated
   Rbis.C := 3;      -- Groupable1, Groupable2, Count

   begin
      R.A := 1;
      if B then
         R.B := 3;
      end if;
      R.C := 3;
   end;

   -- subtypes and derived types
   declare
      subtype ST is Rec;
      type DT is new Rec;
      type DST is new ST;

      S  : St;
      D  : Dt;
      DS : Dst;
   begin
      S.A := 1;
      S.B := 2;
      S.C := 3;   -- Groupable1, Groupable2, Count

      D.A := 1;
      D.B := 2;
      D.C := 3;   -- Groupable1, Groupable2, Count

      DS.A := 1;
      DS.B := 2;
      DS.C := 3;   -- Groupable1, Groupable2, Count
   end;

   -- Check no triggering on limited variables
   declare
      type Rl is limited
         record
            X, Y, Z : Integer;
         end record;

      type Arrl is array (1 .. 3) of Rl;
      Vrl : Rl;
      Varrl : Arrl;
      Varr2 : array (1 .. 3) of Rl;
   begin
      Vrl.X := 1;
      Vrl.Y := 2;
      Vrl.Z := 3;

      Varrl (1).X := 1;
      Varrl (2).X := 1;
      Varrl (3).X := 1;

      Varr2 (1).X := 1;
      Varr2 (2).X := 1;
      Varr2 (3).X := 1;
   end;

   -- Simple variable
Bl: declare
      X : Integer;
      Y : Positive;
      Z : Integer renames X;
   begin
      X := 1;
      Y := 1;
      X := 1;                             -- Repeated
      T_Multiple_Assignments.Bl.X := 1;   -- Repeated
      Z := 1;                             -- Repeated
      T_Multiple_Assignments.Bl.Z := 1;   -- Repeated
   end Bl;

   -- Tagged types
   declare
      type T1 is tagged
         record
            I1 : Integer;
         end record;
      type T2 is new T1 with
         record
            I2 : Integer;
         end record;
      V1 : T1;
      V2 : T2;
   begin
      V1.I1 := 1;  -- Groupable2, Count
      V2.I1 := 1;
      V2.I2 := 2;  -- Groupable1, Groupable2, Count
   end;
end T_Multiple_Assignments;
