separate (T_Uncheckable)
procedure Directly_Accessed_Globals is
   function F return Integer is
   begin
      return 1;
   end F;

   package Pack is
   end Pack;
   package body Pack is
      I : Integer renames F;
      J : Integer;
      procedure P is
      begin
         J := I;
      end P;
   end Pack;
begin
   null;
end Directly_Accessed_Globals;
