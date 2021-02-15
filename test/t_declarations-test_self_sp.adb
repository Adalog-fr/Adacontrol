separate (T_Declarations)
procedure Test_Self_Sp is                       -- not library procedure, local procedure, self_calling_procedure
   function F (X : Integer) return Integer is   -- no_spec_function, self_calling_function
   begin
      return F (X);
   end;

   function Ok_Recurs (X : Integer) return Integer is  -- OK (ignored due to pattern)
   begin
      return Ok_Recurs (X);
   end Ok_Recurs;

   function G (X : Integer) return Integer is   -- no_spec_function, relay_function
   begin
      return F (X);
   end;

   procedure P is                               -- not library procedure, local procedure, no_spec_procedure, relay_procedure
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
