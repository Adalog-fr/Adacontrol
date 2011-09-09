separate (T_Uncheckable)
procedure Directly_Accessed_Globals is
   function F return Integer is
   begin
      return 1;
   end F;

   package Pack is
      type TT is tagged null record;
      procedure Dispatch (X : in TT);
   end Pack;

   package body Pack is
      I : Integer renames F;
      J : Integer;
      K : TT;
      procedure P is
      begin
         J := I;                   -- Uncheckable: I is dynamic renaming
         K := K;
      end P;

      procedure Q is
      begin
         Dispatch (TT'Class (K));
      end Q;

      procedure Dispatch (X : in TT) is
      begin
         J := J;
      end Dispatch;
   end Pack;
begin
   null;
end Directly_Accessed_Globals;
