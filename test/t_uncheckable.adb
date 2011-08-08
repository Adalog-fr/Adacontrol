with X_Uncheckable_Proc; -- Missing body
with X_Uncheckable;
procedure T_Uncheckable is
   use X_Uncheckable;

   procedure Case_Statement              is separate;
   procedure Directly_Accessed_Globals   is separate;
   procedure Exception_Propagation       is separate;
   procedure Max_Call_Depth              is separate;
   procedure Style                       is separate;
   procedure Unsafe_Unchecked_Conversion is separate;
begin
   null;
end T_Uncheckable;
