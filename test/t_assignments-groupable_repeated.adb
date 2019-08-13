separate (T_Assignments)
procedure Groupable_Repeated is
   type Rec is
      record
         A, B, C : Integer;
      end record;
   type Small_Rec is
      record
         C1, C2 : Integer;
      end record;
   type Tab is array (1 .. 5) of Integer;
   type TabRec is array (1 .. 3) of Rec;
   type MatRec is array (1 .. 3, 1 .. 4) of Rec;
   type RecRec is
      record
         X : Rec;
         Y : Tab;
         Z : TabRec;
      end record;

   R  : Rec;
   SR : Small_Rec;
   T  : Tab;
   M  : MatRec;
   RR : RecRec;
   B  : Boolean;

   Rbis  : Rec     renames R;
   RABis : integer renames RBis.A;
   RBBis : Integer renames R.B;
begin
   -- Simple cases
   T (1) := 1;

   R.A := 1;
   R.B := 2;
   R.C := 3;        -- Groupable1, Groupable2, Count

   T (1) := 1;      -- Repeated
   T (2) := 2;
   null;
   T (3) := 3;
   T (4) := 4;
   T (5) := 5;      -- Groupable1, Groupable2, Count

   M (1, 1).A := 1;
   M (1, 1).B := 2; -- Groupable1
   M (1, 2)   := R;
   M (1, 3)   := R;
   M (1, 4)   := R;
   M (2, 1).A := 1;
   M (2, 2)   := R;
   M (2, 3)   := R;
   M (2, 4)   := R;
   M (2, 4)   := R; -- Repeated
   M (3, 1)   := R;
   M (3, 2)   := R;
   M (3, 3)   := R; -- Groupable2 (on M)

   if False then
      null;
   end if;

   R.A := 1;
   R.B := 2;

   RR.X.A     := 1;
   RR.Y (2)   := 2;
   RR.Z (1).B := 3; -- OK by transitivity
   RR.Z (2).C := 4; -- OK by transitivity

   RABis  := 1;     -- Repeated
   RBBis  := 2;     -- Repeated
   Rbis.C := 3;     -- Groupable1, Groupable2, Count

   if False then
      null;
   end if;

   RR.X.A     := 1;
   RR.Y (2)   := 2;
   RR.Z (1).A := 3;
   RR.Z (1).B := 4; -- Groupable1
   RR.Z (2).B := 4; -- Groupable1 (RR.Z)
   RR.Z (2).C := 5; -- Groupable1

   begin
      R.A := 1;
      if B then
         R.B := 3;
      end if;
      R.C := 3;
   end;

   SR.C1 := 1;      -- Small_Rec

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
      S.C := 3;     -- Groupable1, Groupable2, Count

      D.A := 1;
      D.B := 2;
      D.C := 3;     -- Groupable1, Groupable2, Count

      DS.A := 1;
      DS.B := 2;
      DS.C := 3;    -- Groupable1, Groupable2, Count
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
   Bl : declare
      X : Integer;
      Y : Positive;
      Z : Integer renames X;
   begin
      X := 1;
      Y := 1;
      X := 1;                                     -- Repeated
      T_Assignments.Groupable_Repeated.Bl.X := 1; -- Repeated
      Z := 1;                                     -- Repeated
      T_Assignments.Groupable_Repeated.Bl.Z := 1; -- Repeated
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
      V1.I1 := 1;           -- Groupable2, Small_Rec, Count
      V2.I1 := 1;
      V2.I2 := 2;           -- Groupable1, Groupable2, Small_Rec, Count
   end;

   -- Protected types         Mantis 0000009
   declare
      protected type Prot is
         procedure P;
      private
         F1 : Integer;
         F2 : Integer;
         F3 : Rec;
      end Prot;

      protected body Prot is
         procedure P is
         begin
            Prot.F1 := 1;
            Prot.F1 := 2;   -- Repeated
            Prot.F2 := 1;
            Prot.F3.A := 1;
            Prot.F3.B := 1; -- Groupable1
            null;
         end P;
      end Prot;
   begin
      null;
   end;

   -- Class-wide, not groupable
   Case_Tagged :
   declare
      package Pack is
         type Tag is tagged
            record
               I : Integer;
               J : Integer;
            end record;
      end Pack;

      procedure P (X : out Pack.Tag) is
      begin
         X.I := 1;
         X.J := 2;          -- Groupable1, Groupable2, Small_Rec
      end P;
      procedure Q (X : out Pack.Tag'Class) is
      begin
         X.I := 1;
         X.J := 2;          -- OK (class-wide)
      end Q;
   begin
      null;
   end Case_Tagged;
end Groupable_Repeated;
