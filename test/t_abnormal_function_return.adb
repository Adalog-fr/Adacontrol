with Ada.Exceptions; use Ada.Exceptions;
with Text_IO; use Text_IO;
function T_Abnormal_Function_Return return Integer is
   function F1 return Integer is
   begin
      return 0;
      null;                                         -- Should trigger
   exception
      when Tasking_Error =>
        Put_Line ("tasking_error");                 -- Should trigger
      when others =>
         null;                                      -- Should trigger
   end F1;
begin
   begin
      begin
         begin
            return 1;   -- OK
         end;
      end;
   end;
exception
   when Constraint_Error =>
      Raise_Exception (Constraint_Error'Identity);   -- OK
   when Program_Error =>
      null;
      raise;                                         -- OK
   when Storage_Error =>
      null;
      raise Storage_Error;                           -- OK
   when Occur : Tasking_Error =>
      Reraise_Occurrence (Occur);                    -- OK
   when others =>
      return 0;                                      -- OK
end T_Abnormal_Function_Return;
