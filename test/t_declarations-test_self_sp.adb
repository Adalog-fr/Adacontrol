separate (T_Declarations)
procedure Test_Self_Sp is                       -- self_calling_procedure, nested procedure, local procedure
   function F (X : Integer) return Integer is   -- self_calling_function
   begin
      return F (X);
   end;

   function G (X : Integer) return Integer is
   begin
      return F (X);
   end;

   procedure P is                               -- nested procedure, local procedure
   begin
      Test_Self_Sp;
   end P;
begin
   Test_Self_Sp;
end Test_Self_Sp;
