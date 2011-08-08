separate (T_Uncheckable)
procedure Max_Call_Depth is
   procedure In_C;
   pragma Import (C, In_C);
begin
   Dyn_Ren_Proc;              -- Uncheckable
   Dyn_Proc.all;              -- Uncheckable
   Dispatch (Dyn_Tagged);     -- Uncheckable
   In_C;                      -- Uncheckable
end Max_Call_Depth;
