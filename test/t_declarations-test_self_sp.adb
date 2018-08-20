separate (T_Declarations)
procedure Test_Self_Sp is                       -- self_calling_procedure, nested procedure, local procedure
   function F (X : Integer) return Integer is   -- self_calling_function, no_spec_function
   begin
      return F (X);
   end;

   function Ok_Recurs (X : Integer) return Integer is  -- OK (ignored due to pattern)
   begin
      return Ok_Recurs (X);
   end Ok_Recurs;

   function G (X : Integer) return Integer is   -- relay_function, no_spec_function
   begin
      return F (X);
   end;

   procedure P is                               -- nested procedure, local procedure, relay_procedure, no_spec_procedure
   begin
      Test_Self_Sp;
   end P;

   package Pack is                              -- not_library_package, relay_package
      package Inner is                          -- not_library_package, empty_visible_part
      end Inner;
   end Pack;
begin
   Test_Self_Sp;
end Test_Self_Sp;
