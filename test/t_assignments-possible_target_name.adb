separate (t_assignments)
procedure Possible_Target_Name is
   I    : Integer;
   II   : Integer;
   S    : String (1 .. 10);
   TabI : array (1 .. 10) of Integer;
   Ren  : Integer renames II;
   Rt   : Integer renames TabI (2);

   type Rec1 is
      record
         Comp1 : Integer;
         Comp2 : Integer;
      end record;
   type Rec2 is
      record
         Rec : Rec1;
      end record;
   Vrec1 : Rec1;
   Vrec2 : Rec2;
   Rr    : Integer renames Vrec2.Rec.Comp1;
   TabR  : array (1 .. 10) of Rec1;

   type Ptr is access Integer;
   P1, P2 : Ptr;

   function F (X : Integer) return Integer is (1);
   function G (S : String) return Integer is (1);
begin
   I  := I + 1;                                              -- OK (1 character)
   II := Possible_Target_Name.II + 1;                        -- Triv_Target
   II := Ren + 1;                                            -- Triv_Target, Repeated
   II := Integer (Ren) + 1;                                  -- Triv_Target, Repeated
   II := Integer'(Ren) + 1;                                  -- Triv_Target, Repeated
   II := F (F (II + 1 - 3 * II + 2));                        -- Triv_Target x2, Repeated
   II := G ((1 .. II => ' '));                               -- Triv_Target, Repeated

   Vrec1.Comp1     := 1 + Vrec1.Comp1;                       -- OK (2 components), Small_Rec
   Vrec2.Rec.Comp1 := 1 + Vrec2.Rec.Comp1;                   -- Not_Triv_Target, Small_Rec x2
   Vrec2.Rec.Comp1 := 1 + Vrec2.Rec.Comp2;                   -- OK, Repeated
   Vrec2.Rec.Comp1 := Rr + 1;                                -- Not_Triv_Target, Repeated
   Rr              := Vrec2.Rec.Comp1 + 1;                   -- Triv_Target, Repeated

   Possible_Target_Name.S (II)   := Character'Succ (S (II)); -- Not_Triv_Target
   Possible_Target_Name.S (II)   := Character'Succ (S (II + 1));
   Rt                            := TabI (2) + 1;            -- Triv_Target
   Possible_Target_Name.TabI (2) := Rt + 1;                  -- Not_Triv_Target, Repeated

   II := TabI (II);                                          -- Triv_Target, Repeated
   II := TabI (II .. II + 1)'Length;                         -- Triv_Target x2, Repeated
   II := TabI (II .. II + 1) (II);                           -- Triv_Target x3, Repeated
   II := TabR (II).Comp2;                                    -- Triv_Target, Repeated

   TabI (1)       := TabI (1) + 1;                           -- OK (2 elements)
   TabR (1).Comp1 := TabR (1).Comp1 + 2;                     -- Not_Triv_Target, Small_Rec

   Possible_Target_Name.P1.all := P1.all + 1;                -- Not_Triv_Target
   Possible_Target_Name.P2.all := P1.all + 1;

   P1 := new Integer;
   P2 := new Integer'(II);
   P1 := new Integer'(P1.all);                               -- Triv_Target, Repeated
end Possible_Target_Name;
