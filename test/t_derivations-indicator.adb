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
      procedure Pt (X : Dert);                             -- Indicator: Overriding missing tagged
      procedure Ptno (X, Y : Dert);                        -- Indicator: Not overriding missing tagged
   end Pack_Tag;

   package body Pack_Tag Is
      overriding procedure Pt (X : Dert) is                -- OK (Overriding unspecified) tagged
      begin
         null;
      end Pt;
      not overriding procedure Ptno (X, Y : Dert) is       -- Indicator: not Overriding forbidden tagged
      begin
         null;
      end Ptno;
   end Pack_Tag;

   type Deri1 is new Pack.Int;                             -- From: Category RANGE
   overriding procedure Pi_OK (X : Deri1);
   procedure Pi_KO (X : Deri1);                            -- Indicator: Overriding missing untagged
   function F_KO return Deri1;                             -- Indicator: Overriding missing untagged

   type Deri2 is new Deri1;                                -- From : category RANGE
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
   procedure Pi_KO (X : Deri1) is null;                    -- Indicator: Overriding missing untagged (null as body)
   function  F_KO return Deri1 is separate;                -- Indicator: Overriding missing untagged (separate body)
   procedure Notov (X : Deri2) is null;


   package Make_Prim is
      type Deri3 is new Deri2;                             -- From: category RANGE
      procedure New_Prim (X : Deri3);                      -- Indicator: Not overriding missing untagged
      package Nested is
         procedure Not_Prim (X : Deri3);
      end Nested;
      procedure Pi_Ko (X : Deri3) renames Nested.Not_Prim; -- Indicator: Overriding missing untagged
      procedure Pi2_Ko (X : Deri3) is null;                -- Indicator: not overriding missing untagged (body as spec)
      procedure Pi3_Ko is new Gen (Deri3);                 -- Indicator: Not overriding missing untagged
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

   procedure Pi_KO is new Gen (Deri2);                     -- Indicator: Overriding missing untagged
begin
   null;
end Indicator;
