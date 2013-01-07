separate (T_uncheckable)
procedure Parameter_Aliasing is
   package Pack is
      type T is tagged null record;
      procedure P1 (X : T);
      procedure P2 (X, Y : in out T);
      procedure P3 (X : T; Y : Integer);
   end Pack;
   package body Pack is
      procedure P1 (X : T) is begin null; end;
      procedure P2 (X, Y : in out T) is begin null; end;
      procedure P3 (X : T; Y : Integer) is begin null; end;
   end Pack;
   use Pack;

   V : T'Class := T'(null record);
   I : Integer;
   C : constant Integer := 1;
begin
   P1 (V);
   P2 (V, V);           -- Possible false negative
   P3 (V, 1);
   P3 (V, C);
   P3 (V, I);           -- Possible false negative
end Parameter_Aliasing;
