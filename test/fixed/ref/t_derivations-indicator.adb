separate (T_Derivations)
procedure Indicator is
   generic
      type T is private;
   procedure Gen (X : T);
   procedure Gen (X : T ) is begin null; end Gen;

   package Pack is
      type Int is range 1 .. 10;
      procedure Pi_OK (X : Int);
      procedure Pi_KO (X : Int);
      function F_KO return Int;


      type Tt is tagged null record;
      procedure Pt (X : Tt);

   end Pack;

   package Pack_Tag Is
      type Dert is new Pack.Tt with null record;           -- From: category  TAGGED
      overriding procedure Pt (X : Dert);                             -- Indicator: Overriding missing tagged
      not overriding procedure Ptno (X, Y : Dert);                        -- Indicator: Not overriding missing tagged
   end Pack_Tag;

   package body Pack_Tag Is
      overriding procedure Pt (X : Dert) is                -- OK (Overriding unspecified) tagged
      begin
         null;
      end Pt;
      procedure Ptno (X, Y : Dert) is       -- Indicator: not Overriding forbidden tagged
      begin
         null;
      end Ptno;
   end Pack_Tag;

   type Deri1 is new Pack.Int;                             -- From: Category RANGE
   overriding procedure Pi_OK (X : Deri1);
   overriding procedure Pi_KO (X : Deri1);                            -- Indicator: Overriding missing untagged
   overriding function F_KO return Deri1;                             -- Indicator: Overriding missing untagged

   type Deri2 is new Deri1;                                -- From: category RANGE; Depth: greater than 1
   procedure Notov (X : Deri2);                            -- OK (not primitive)
   overriding procedure Pi_OK (X : Deri2) renames Notov;   -- OK (indicator given, primitive rename as body, untagged)

   package body Pack is
      procedure Pi_OK (X : Int) is null;
      procedure Pi_KO (X : Int) is null;
      function F_KO return Int is begin return 1; end F_KO;

      procedure Pt (X : Tt) is null;
   end Pack;

   -- Bodie of specifications above
   overriding procedure Pi_OK (X : Deri1) is null;         -- OK, indicator given
   overriding procedure Pi_KO (X : Deri1) is null;                    -- Indicator: Overriding missing untagged (null as body)
   overriding function  F_KO return Deri1 is separate;                -- Indicator: Overriding missing untagged (separate body)
   procedure Notov (X : Deri2) is null;


   package Make_Prim is
      type Deri3 is new Deri2;                             -- From: category RANGE; Depth: greater than 1
      not overriding procedure New_Prim (X : Deri3);                      -- Indicator: Not overriding missing untagged
      package Nested is
         procedure Not_Prim (X : Deri3);
      end Nested;
      overriding procedure Pi_Ko (X : Deri3) renames Nested.Not_Prim; -- Indicator: Overriding missing untagged
      not overriding procedure Pi2_Ko (X : Deri3) is null;                -- Indicator: not overriding missing untagged (body as spec)
      not overriding procedure Pi3_Ko is new Gen (Deri3);                 -- Indicator: Not overriding missing untagged
   end Make_Prim;

   package body Make_Prim is
      not overriding procedure New_Prim (X : Deri3) is     -- OK
      begin
         null;
      end;
      package body Nested is
         procedure Not_Prim (X : Deri3) is null;
      end Nested;
   end Make_Prim;

   overriding procedure Pi_KO is new Gen (Deri2);                     -- Indicator: Overriding missing untagged


   -- Case of private types
   package Pack_Priv is
      type Tn is private;
      type Tt is tagged private;
      procedure Prim (X : Tn) is null;
      procedure Prim (X : Tt) is null;

   private
      type Tn is new Integer;                              -- From: STANDARD.Integer, category RANGE
      type Tt is new Pack.Tt with null record;             -- From: category TAGGED
   end Pack_Priv;

   -- Case of task/protected
   package For_Inteface is
      type I is limited interface;
      procedure P (X : in out I) is abstract;
   end For_Inteface;

   task type Tt1 is new For_Inteface.I with
      entry P;                    -- Indicator: overriding missing task entry (not implemented yet)
      entry Q;                    -- Indicator: Not overriding missing task entry (not implemented yet)
   end Tt1;

   task body Tt1 is
   begin
      null;
   end Tt1;

   task Tt2 is new For_Inteface.I with
      overriding entry P;         -- OK
      not overriding entry Q;     -- OK
   end Tt2;

   task body Tt2 is
   begin
      null;
   end Tt2;

   protected Pt1 is new For_Inteface.I with
      overriding procedure P;                -- Indicator: overriding missing protected
      not overriding procedure Q;                -- Indicator: Not overriding missing protected
   end Pt1;

   protected body Pt1 is
      procedure P is null;
      procedure Q is null;
      procedure R;
      procedure R is null;
      procedure S is null;
   end Pt1;

   protected type Pt2 is new For_Inteface.I with
      overriding procedure P;     -- OK
      not overriding procedure Q; -- OK
   end Pt2;

   protected body Pt2 is
      procedure P is null;        -- Ok (not checked in protected body)
      procedure Q is null;        -- Ok (not checked in protected body)
   end Pt2;

      type TagT1 is tagged null record;
   type GI1 is interface;
   type GI2 is interface;

   generic
      type GT is new TagT1 and GI1 and GI2 with private; --  Derivations: Type derived from category TAGGED, More than 2 parents (3)
   package Generic_Package is
      procedure P4 (X : out GT) is null;                 -- Ok (not primitive)

      type Der is new GT with null record;               --  Derivations: Type derived from category TAGGED
      not overriding procedure P4 (X : Der) is null;                    --  Indicator: Missing not overriding indicator

      type BigDer is new TagT1 and GI1 with null record; --  Derivations: Type derived from category TAGGED, More than 1 parents (2)
   end Generic_Package;
begin
   null;
end Indicator;
