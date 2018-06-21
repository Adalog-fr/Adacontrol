procedure T_Aspects is
   procedure P
   with Pre  => True,                                     -- DBC_Aspects
        Post => False,                                    -- DBC_Aspects
        convention => C;                                  -- Other_Aspects

   procedure P is begin null; end P;

   type T is tagged null record;
   procedure Op (This : T) with
     Pre'Class => 1=1,                                    -- DBC_Aspects
     Post'Class => False;                                 -- DBC_Aspects

   procedure Op (This : T) is begin null; end Op;

   type Packed is array (1..10) of Boolean with Pack;     -- Other_Aspects, Implicit_Aspects
begin
   null;
end T_Aspects;
