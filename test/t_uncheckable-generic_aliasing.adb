separate (T_Uncheckable)
procedure Generic_Aliasing is
   generic
      with function Intf (L : Integer) return Integer;
      with function Flof (L : Float)   return Float;
   procedure Genp;
   procedure Genp is begin null; end Genp;

   procedure Instp is new Genp ("+", "+");    -- False positive
begin
   null;
end Generic_Aliasing;
