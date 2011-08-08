separate (T_Uncheckable)
procedure Global_References is
begin
   Dyn_Ren_Proc;              -- Uncheckable
   Dyn_Proc.all;              -- Uncheckable
   Dispatch (Dyn_Tagged);     -- Uncheckable
end Global_References;
