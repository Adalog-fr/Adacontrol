with Ada.Finalization;
separate (t_assignments)
procedure Possible_Target_Name is
   I    : Integer;
   S    : String (1 .. 10);
   TabI : array (1 .. 10) of Integer;
   Ren  : Integer renames I;
   Rt   : Integer renames TabI (2);

   type Rec is
      record
         Comp1 : Integer;
         Comp2 : Integer;
      end record;
   Vrec : Rec;
   Rr   : Integer renames Vrec.Comp1;
   TabR : array (1 .. 10) of Rec;

   type Ptr is access Integer;
   P1, P2 : Ptr;

   function F (X : Integer) return Integer is (1);
   function G (S : String) return Integer is (1);
begin
   I := Possible_Target_Name.I + 1; -- Triv_Target
   I := Ren + 1;                    -- Triv_Target
   I := Integer (Ren) + 1;          -- Triv_Target
   I := Integer'(Ren) + 1;          -- Triv_Target
   I := F (F (I + 1 - 3 * I + 2));  -- Triv_Target x2
   I := G ((1 .. I => ' '));        -- Triv_Target

   Vrec.Comp1 := 1 + Vrec.Comp1;    -- Not_Triv_Target
   Vrec.Comp1 := 1 + Vrec.Comp2;
   Vrec.Comp1 := Rr + 1;            -- Not_Triv_Target
   Rr         := Vrec.Comp1 + 1;    -- Triv_Target

   S (I)    := Character'Succ (S (I)); -- Not_Triv_Target
   S (I)    := Character'Succ (S (I + 1));
   Rt       := TabI (2) + 1;
   TabI (2) := Rt + 1;

   I := TabI (I);                      -- Triv_Target
   I := TabI (I .. I + 1)'Length;      -- Triv_Target x2
   I := TabI (I .. I + 1) (I);         -- Triv_Target x3
   I := TabR (I).Comp2;                -- Triv_Target

   P1.all := P1.all + 1;               -- Not_Triv_Target
   P2.all := P1.all + 1;

   P1 := new Integer;
   P2 := new Integer'(I);              -- Repeated
   P1 := new Integer'(P1.all);         -- Triv_Target
end Possible_Target_Name;
