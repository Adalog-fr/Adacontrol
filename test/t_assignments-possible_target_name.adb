with Ada.Finalization;
separate (t_assignments)
procedure Possible_Target_Name is
   I : Integer;
   S : String (1 .. 10);
   type Rec is
      record
         Comp1 : Integer;
         Comp2 : Integer;
      end record;
   R : Rec;
   function F (X : Integer) return Integer is (1);
   function G (S : String) return Integer is (1);
begin
   I       := Possible_Target_Name.I + 1; -- Triv_Target
   R.Comp1 := 1 + R.Comp1;                -- small_rec, Not_Triv_Target
   R.Comp1 := 1 + R.Comp2;                -- Repeated
   S (I)   := Character'Succ (S (I));     -- Not_Triv_Target
   S (I)   := Character'Succ (S (I + 1));
   I       := F (F (I + 1 - 3 * I + 2));  -- Repeated, Triv_Target x2
   I       := G ((1 .. I => ' '));        -- Repeated
end Possible_Target_Name;
