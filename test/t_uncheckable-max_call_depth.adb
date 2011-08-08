separate (T_Uncheckable)
procedure Max_Call_Depth is
begin
   Dyn_Ren_Proc;              -- Uncheckable
   Dyn_Proc.all;              -- Uncheckable
   Dispatch (Dyn_Tagged);     -- Uncheckable
end Max_Call_Depth;
