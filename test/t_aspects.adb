pragma Ada_2012;
procedure T_Aspects is
   procedure P
   with Pre  => True,
        Post => False,
        convention => C;

   procedure P is begin null; end P;
   
   type T is tagged null record;
   procedure Op (This : T) with
     Pre'Class => 1=1,
     Post'Class => False;
   
   procedure Op (This : T) is begin null; end Op;
   
begin
   null;
end T_Aspects;
