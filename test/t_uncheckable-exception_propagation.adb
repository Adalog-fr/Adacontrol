separate (T_Uncheckable)
procedure Exception_Propagation is
   type Acc_Proc is access procedure;

   procedure Make_CB_L0 (CB : Acc_Proc) is
   begin
      Cb.all;
   end Make_CB_L0;

   Ptr1 : Acc_Proc;
begin
   Make_Cb_L0 (Ptr1.all'Access);             -- Uncheckable
   Make_Cb_L0 (Dyn_Ren_Proc'Access);         -- Uncheckable x2
   Make_CB_L0 (X_Uncheckable_Proc'Access);
   Dispatch (Dyn_Tagged);                    -- Uncheckable
end Exception_Propagation;
