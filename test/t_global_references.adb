procedure T_Global_References is
   Glob1 : Integer; -- Global reference
   Glob2 : Integer; -- Global reference
   Glob3 : Integer; -- Global reference
   Glob4 : Integer; -- OK
   Glob5 : Integer; -- Global reference
   Glob6 : Integer; -- Global reference (through renaming)
   Glob7 : Integer; -- Global reference (through generic)
   Glob8 : Integer renames Glob6;

   type Acc1 is access Integer;
   V_Acc1 : Acc1;

   type Rec1 is
      record
         I : Integer;
      end record;
   type Acc2 is access Rec1;
   V_Acc2 : Acc2;

   procedure P1 is
      Inner_V : Integer;  -- Global reference for Inner_T
                          -- not global reference for P1

      procedure Inner_P is
      begin
         Inner_V := 1;
         Inner_P;
      end Inner_P;

      task Inner_T;
      task body Inner_T is
      begin
         Inner_P;
      end Inner_T;
   begin
      Glob1 := Inner_V;
   end P1;

   procedure P2 is
   begin
      Glob2 := 1;
      Glob8 := 1;
   end P2;

   generic
      V : in out Integer;
   procedure Gen;
   procedure Gen is
   begin
      V := 1;
   end Gen;
   procedure P3 is new Gen (Glob7);

   protected type PT is
      procedure P;
      entry E;
   end PT;

   protected body PT is
      procedure P is
      begin
         Glob1 := 1;
      end P;
      entry E when True is
         Local : Integer;
      begin
         Local := Glob3;
      end E;
   end Pt;

   protected OP1 is
      procedure P1;
      procedure P2;
   end OP1;
   protected body OP1 is
      procedure P1 is
      begin
         Glob4 := 1;  -- Several references from same PO
         Glob5 := 1;  -- Several references from different PO
      end P1;
      procedure P2 is
      begin
         Glob4 := 1;
      end P2;
   end OP1;

   protected OP2 is
      procedure P1;
   end OP2;
   protected body OP2 is
      procedure P1 is
      begin
         Glob5 := 1;  -- Several references from different PO
      end P1;
   end OP2;

   task type T1;
   task body T1 is
      OP : PT;
   begin
      Glob1 := 1;
      P2;
      V_Acc1.all := 1;
      V_Acc2.I := 1;
      OP.E;
      P3;
   end T1;

   function Impure return Integer is
   begin
      Glob2 := 0;
      return Glob1;
   end Impure;

   ----------------------------------------------------
   -- Check for Atomic and Atomic_Components pragmas --
   ----------------------------------------------------
   Glob10 : Integer;
   pragma Atomic (Glob10);

   type Atom_T is new Integer;
   pragma Atomic (Atom_T);
   Glob11 : Atom_T;

   Glob12 : Integer := 0;
   pragma Atomic (Glob12);

   procedure Atom1 is
   begin
      Glob10 := Glob12;
      Glob11 := 0;
   end Atom1;

   procedure Atom2 is
      Local : Integer;
   begin
      Local := Glob10; -- OK
      Local := Integer (Glob11); -- OK
   end Atom2;

   procedure Atom3 is
   begin
      Glob10 := Glob12;     -- Error
      Glob11 := 0;          -- Error
   end Atom3;

   task type T2;
   task body T2 is
   begin
      Glob10 := Integer (Glob11);  -- Error Glob10, OK Glob11
   end T2;
begin
   null;
end T_Global_References;
