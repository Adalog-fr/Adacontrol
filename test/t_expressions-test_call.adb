separate (T_Expressions)
procedure Test_Call is
   function F (X : Integer) return Integer is
   begin
      return X;
   end F;

   procedure P (X : Integer) is
   begin
      null;
   end;

   I : Integer;
begin
   P (I);
   P (2);
   I := F(I);
   P (F (I));      -- Complex_Parameter
   P (I + 1);      -- Complex_Parameter
   P (F (I + 1));  -- Complex_Parameter (x2)
end Test_Call;
