separate (T_Uncheckable)
procedure Style is
   function Succ (I : in Integer) return Integer renames Integer'Succ;   -- Uncheckable
   function "-" (L, R : in Integer) return Integer renames Standard."+"; -- Uncheckable
   J : Integer;
begin
   J := Integer'Succ (J); -- Not checked
   J := J + 1;            -- Not checked
end Style;
