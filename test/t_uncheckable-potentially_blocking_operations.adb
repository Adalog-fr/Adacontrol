separate (T_Uncheckable)
procedure Potentially_Blocking_Operations is
   protected Prot is
      procedure P;
   end Prot;
   protected body Prot is
      procedure P is
      begin
         Dyn_Ren_Proc;              -- Uncheckable
         Dyn_Proc.all;              -- Uncheckable
         Dispatch (Dyn_Tagged);     -- Uncheckable
      end P;
   end Prot;
begin
   null;
end Potentially_Blocking_Operations;
