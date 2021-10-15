separate (T_Known_Exceptions)
procedure Test_Discriminant is
   I : Integer;

   type Rec1 (D : Character := 'a') is
      record
         case D is
            when 'a'              => A : Integer;
            when 'b' .. 'e' | 'z' => B : Integer;
            when others => null;
         end case;
      end record;
   subtype Rec1_A     is Rec1 ('a');
   subtype Rec1_B     is Rec1 ('b');
   subtype Rec1_B_Bis is Rec1_B;

   function Build_Rec1 return Rec1 is
   begin
      return ('b', 1);
   end Build_Rec1;

   V1   : Rec1 := ('a', 1);
   V1a  : Rec1_A;
   V1b1 : Rec1 := Build_Rec1;  -- Discriminant value is dynamic
   V1b2 : Rec1_B_Bis;

   type Rec2 (D1 : Integer := 1; D2 : Character := 'a') is
      record
         case D1 is
            when 1 =>
               case D2 is
                  when 'a' =>
                     Inner1 : Rec1 (D2);
                  when 'x' =>
                     Inner2 : Rec1; -- use default for discriminant
                  when others =>
                     null;
               end case;
            when 2 =>
               C : Integer;
            when others =>
               null;
         end case;
      end record;
   V2 : Rec2;

   subtype Rec2c is Rec2 (1, 'a');
   V2c : Rec2c;
   Alias : Integer renames V2c.C; -- Discrim_Error D1

   type Rec3 (D3 : Character := 'a') is
      record
         R21 : Rec2 (1, D3);
         case D3 is
            when 'a' =>
               R22 : Rec2 (1, D3);
            when others =>
               null;
         end case;
      end record;
   V3 : Rec3;

begin
   I := V1.A;
   I := V1.B;                     -- Discrim_Error D
   I := V1a.A;
   I := V1a.B;                    -- Discrim_Error D
   I := V1b1.A;                   -- Unknown
   I := V1b1.B;                   -- Unknown
   I := V1b2.A;                   -- Discrim_Error D
   I := V1b2.B;

   V1 := (D => 'd', B => 1);
   I  := V1.A;                    -- Discrim_Error D
   I  := V1.B;

   V1 := (D => 'z', B => 1);
   I  := V1.A;                    -- Discrim_Error D
   I  := V1.B;

   V1 := (D => 'x');
   I  := V1.A;                    -- Discrim_Error D
   I  := V1.B;                    -- Discrim_Error D

   V1 := Build_Rec1;
   if V1.D in 'b'..'c' then
      I  := V1.A;                    -- Discrim_Error D
      I  := V1.B;
   end if;

   -- V2 is (1, 'a') by default
   I := V2.Inner1.A;
   I := V2.Inner1.B;               -- Discrim_Error D
   V2.Inner1 := V1;

   V2 := (1, 'b');
   I := V2.Inner1.A;               -- Discrim_Error D2, D
   I := V2.Inner1.B;               -- Discrim_Error D2
   V2.Inner1 := V1;                -- Discrim_Error D2

   V2 := (1, 'x', V1);
   I := V2.Inner2.A;               -- Discrim_Error D2, D
   I := V2.Inner2.B;               -- Discrim_Error D2
   V2.Inner2 := V1;                -- Discrim_Error D2

   V2 := (1, 'a', ('a', 0));
   I := V2.Inner1.A;
   I := V2.Inner1.B;               -- Discrim_Error D
   V2.Inner1 := V1;

   V2 := (D1 => 2, D2 => 'a', C => 0);
   I := V2.Inner1.A;               -- Discrim_Error D1
   I := V2.Inner1.B;               -- Discrim_Error D1, D
   V2.Inner1 := V1;                -- Discrim_Error D1
   I := V2.C;

   V2 := (D1 => 2, D2 => 'b', C => 0);
   I := V2.Inner1.A;               -- Discrim_Error D2, D1, D
   I := V2.Inner1.B;               -- Discrim_Error D2, D1
   V2.Inner1 := V1;                -- Discrim_Error D2, D1

   I := V3.R21.Inner1.A;
   I := V3.R21.Inner1.B;           -- Discrim_Error D
   I := V3.R22.Inner1.A;
   I := V3.R22.Inner1.B;           -- Discrim_Error D

   V3 := (D3 => 'b', R21 => V2);

   I := V3.R21.Inner1.A;           -- Discrim_Error D, D2
   I := V3.R21.Inner1.B;           -- Discrim_Error D2
   I := V3.R22.Inner1.A;           -- Discrim_Error D, D2, D3
   I := V3.R22.Inner1.B;           -- Discrim_Error D2, D3

   -- Check acces through renaming
   declare
      V3  : Rec2 (1, 'a');
      Ren : Rec1 renames V3.Inner1;
      RenA : Integer renames Ren.A;
      RenB : Integer renames Ren.B; -- Discrim_Error D
   begin
      I := Ren.A;
      I := Ren.B;                   -- Discrim_Error D
   end;

   -- Check access discriminants/components
   -- Checking discriminants of the target of a pointer is not possible,
   -- but if it is a discriminant, we can check that it is not null (access subrule)
   -- (we don't track other components). This check is mainly to make sure that
   -- access components and discriminant do not crash the rule
   declare
      type Rec31 is
         record
            Ptr : access Rec1;
         end record;
      type Rec32 (Ptr : access Rec1) is null record;

      V31 : Rec31 := (Ptr => new Rec1'('a', 0));
      V32 : Rec32 := (Ptr => new Rec1'('a', 0));
      V33 : Rec32 (null);
   begin
      I := V31.Ptr.A;
      I := V31.Ptr.B;
      I := V32.Ptr.A;
      I := V32.Ptr.B;

      I := V32.Ptr.A;
      I := V32.Ptr.B;

      I := V33.Ptr.A;   -- Access_Error
      I := V33.Ptr.B;   -- Access_Error
   end;

   -- Formal parameters and local objects
   declare
      procedure Proc (Unconstr : Rec1; Constr : Rec1_A) is
         Loc1 : Rec1   := Unconstr;
         Loc2 : Rec1_A := Constr;
         Loc3 : Rec1;
      begin
         I := Unconstr.A;
         I := Unconstr.B;
         I := Constr.A;
         I := Constr.B; -- Discrim_Error D

         I := Loc1.A;   -- Unknown
         I := Loc1.B;   -- Unknown
         I := Loc2.A;
         I := Loc2.B;   -- Discrim_Error D
         I := Loc3.A;
         I := Loc3.B;   -- Discrim_Error D
      end Proc;
   begin
      null;
   end;

   -- Protected procedure
   declare
      protected type Pt is
         procedure P (Unconstr : Rec1; Constr : Rec1_A);
      end Pt;
      protected body Pt is
         procedure P (Unconstr : Rec1; Constr : Rec1_A) is
            Loc1 : Rec1   := Unconstr;
            Loc2 : Rec1_A := Constr;
            Loc3 : Rec1;
         begin
            I := Unconstr.A;
            I := Unconstr.B;
            I := Constr.A;
            I := Constr.B; -- Discrim_Error D

            I := Loc1.A;   -- Unknown
            I := Loc1.B;   -- Unknown
            I := Loc2.A;
            I := Loc2.B;   -- Discrim_Error D
            I := Loc3.A;
            I := Loc3.B;   -- Discrim_Error D
         end P;
      end Pt;
   begin
      null;
   end;

   -- Check inherited discriminants
   declare
      type Enum is (A, B);
      type Rec1 (DRec1 : Enum) is tagged
         record
            case DRec1 is
               when A =>
                  I : Integer;
               when B =>
                  F : Float;
            end case;
         end record;

      type Rec2 (DRec2 : Enum) is new Rec1 (Drec1 => Drec2) with null record;

      type Rec3 (DRec3 : Enum) is
         record
            case Drec3 is
               when A =>
                  C : Rec2 (Drec2 => DRec3);
               when B =>
                  D : Rec1 (Drec1 => Drec3);
            end case;
         end record;

      type Rec4  (DRec4 : Enum) is
         record
            E : Rec1 (Drec1 => Drec4);
         end record;

      VF : Float;
      R3 : Rec3 (Drec3 => A);
      R4 : Rec4 (Drec4 => A);
   begin
      VF := R3.C.F;   -- Discrim_Error DRec1
      VF := R3.D.F;   -- Discrim_Error Drec3
      VF := R4.E.F;   -- Discrim_Error DRec1
   end;

   -- Check class-wide
   declare
      type Untagged is
         record
            C1 : Integer;
         end record;
      type Tag1 (D : access Untagged) is tagged null record;
      subtype St1 is Tag1 (null);

      procedure P1 (X : Tag1'Class; Y : St1) is
      begin
         I := X.D.all.C1;   -- unknown
         I := X.D.C1;       -- unknown
         I := Y.D.all.C1;   -- null access
         I := Y.D.C1;       -- null access
      end P1;

   begin
      null;
   end;

   -- Check access to discriminants from subprogram
   declare
      procedure P2 is
         LocalV1 : constant Rec1 := V1;
         LocalV2 : constant Rec1 := ('a', 0);
         LocalV3 : Rec1 ('a') := V1;
         LocalV4 : Rec1 := V1a;

         C : Character range 'b' .. 'z';
      begin
         C := V1.D;          -- Unknown
         C := LocalV1.D;    -- Unknown
         C := LocalV2.D;    -- Constraint_Error
         C := LocalV3.D;    -- Constraint_Error
         C := LocalV4.D;    -- Constraint_Error
         LocalV4 := (D => 'b', B => 0);
         C := LocalV4.D;
      end P2;
   begin
      null;
   end;
end Test_Discriminant;
