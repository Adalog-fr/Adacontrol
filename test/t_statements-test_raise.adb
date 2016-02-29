with X_Statements_Raise;
with Ada.Exceptions; use Ada.Exceptions;
separate (T_Statements)
procedure Test_Raise is
   C1 : constant Exception_Id := Constraint_Error'Identity;
   C2 : constant Exception_Id := C1;

   package Pack1 is
      False_External : exception;
   end Pack1;
   package body Pack1 is separate;

   package Pack2 is new X_Statements_Raise;

   task T is
      entry E;
   end T;
   task body T is
   begin
      raise Constraint_Error;  -- raise, raise_standard
      raise Tasking_Error;     -- raise, raise_standard, locally handled (in task)

      accept E do
         Raise_Exception (Constraint_Error'Identity, "message"); -- raise_standard, procedure_call
         Raise_Exception (C2);                                   -- raise_standard, procedure_call
         raise Constraint_Error;  -- raise, raise_standard
         raise Tasking_Error;     -- raise, raise_standard
         raise Storage_Error;     -- raise, raise_standard, locally handled (in accept)
      exception
         when Storage_Error =>
            null;                 -- null;
      end E;
   exception
      when Tasking_Error =>
         raise;                   -- raise, reraise
   end T;

   E : exception;
begin
   raise E;                       -- raise, Raise_nonpublic

   raise Pack2.Exported;          -- raise, raise_foreign
   raise Constraint_Error;        -- raise, raise_standard, locally handled (in proc)
   raise Program_Error;           -- raise, raise_standard

   declare                        -- block, unnamed block, declare_block, effective_declare_block
      package Pack2 is end Pack2;
      package body Pack2 is
      begin
         raise Constraint_Error;  -- raise, raise_standard, locally handled (in proc)
         raise Tasking_Error;     -- raise, raise_standard, locally handled (in package)
         raise Program_Error;     -- raise, raise_standard
      exception
         when Occur: Tasking_Error =>
            Reraise_Occurrence (Occur); -- reraise, procedure_call
      end Pack2;
   begin
      raise Storage_Error;  -- raise, raise_standard, locally handled (in block)
      raise Numeric_Error;  -- raise, raise_standard, locally handled (in block), => Constraint_Error
      raise Tasking_Error;  -- raise, raise_standard, locally handled (in proc)
      raise Program_Error;  -- raise, raise_standard
   exception
      when Constraint_Error =>
         null;              -- null
      when Storage_Error =>
         null;              -- null
   end;

   begin                    -- block, unnamed block
      raise Program_Error;  -- raise, raise_standard, locally handled (in block)
   exception
      when others =>        -- null_when_others
         null;              -- null
   end;

exception
   when Constraint_Error =>
      null;                 -- null
   when Tasking_Error =>
      null;                 -- null
end Test_Raise;
