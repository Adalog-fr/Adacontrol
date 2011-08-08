separate (T_Uncheckable)
procedure Declarations is
   type Int is range 1 .. 10;
   type Mod1 is mod Int'Size;  -- Uncheckable
begin
   null;
end Declarations;
