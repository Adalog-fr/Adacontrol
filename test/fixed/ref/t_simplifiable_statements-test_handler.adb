separate (T_Simplifiable_Statements)
procedure Test_Handler is
   I : Integer;
begin
   -- No when others
   begin
      null;
   exception
      when Constraint_Error =>
         null;
      
      when Storage_Error =>
         I := 1;
         raise;
   end;

   -- Trivial when others
   begin
      null;
   exception
      when Constraint_Error =>
         null;
      
      when Storage_Error =>
         I := 1;
         raise;
      
   end;

   -- Non trivial when others
   begin
      null;
   exception
      when Constraint_Error =>
         null;
      when Tasking_Error =>
         raise;
      when Storage_Error =>
         I := 1;
         raise;
      when others =>
         I := 1;
   end;
end Test_Handler;
