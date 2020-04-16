with Ada.Exceptions; use Ada.Exceptions;
with Text_IO; use Text_IO;
function T_Abnormal_Function_Return return Integer is
   function F1 return Integer is
   begin
      return 0;
      null;                                         -- Sequence of statements
   exception
      when Tasking_Error =>
        Put_Line ("tasking_error");                 -- Sequence of statements
      when Constraint_Error =>
         begin
            return 1;
         exception
            when Constraint_Error =>
               begin
                  null;                            -- Sequence of statements
               end;
         end;
      when others =>
         null;                                      -- Sequence of statements
   end F1;

   function F2 (I : Integer) return Integer is
   begin
      if True then
         return 1;
      else
         return 2;
      end if;
   exception
      when Constraint_Error =>
         case I is
            when 1 =>
               if True then                          -- "else" path
                  return 0;
               elsif True then
                  null;                              -- Sequence of statements
               end if;
            when 2 =>
               if True then
                  return 0;
               elsif True then
                  return 1;
               else
                  return 2;
               end if;
            when 3 =>
               begin
                  raise;
               end;
            when 4 =>
               null;                                 -- Sequence of statements
            when 5 =>
               return I : Integer do
                  I := 0;
               end return;
            when 6 =>
               <<Hell>>                              -- exitable extended return
               return I : Integer do
                  I := 0;
                  goto Hell;
               end return;
            when others =>
               raise Program_Error;
         end case;
   end F2;

   function F3_OK return Integer is
      procedure No_Ret1;
      pragma No_Return (No_Ret1);
      procedure No_Ret1 is
      begin
         raise Constraint_Error;
      end No_Ret1;
      procedure No_Ret2 with No_Return is
      begin
         raise Constraint_Error;
      end No_Ret2;

   begin
      if True then
         return 1;
      elsif True then
         Raise_Exception (Constraint_Error'Identity);
      elsif True then
         No_Ret1;
      else
         No_Ret2;
      end if;
   end F3_OK;

   function F4_Loops return Integer is
      Z : Integer := 10;
   begin
      if True then
         for I in 1 .. 10 loop                -- Sequence of statements
            null;
         end loop;
      elsif True then
         while Z > 3 loop                     -- Sequence of statements
            return 1;
         end loop;
      elsif True then
         <<L1>>                               -- Sequence of statements
         loop
            null;
            goto L1;
         end loop;
      elsif True then
         loop                                 -- OK
            <<L2>>
            null;
            goto L2;
         end loop;
      else
         loop                                 -- OK
            return 1;
         end loop;
      end if;
   end F4_Loops;


begin
   begin
      begin
         begin
            return 1;
         exception
            when Constraint_Error =>
               return 2;
         end;
      end;
   end;
exception
   when Constraint_Error =>
      Raise_Exception (Constraint_Error'Identity);
   when Program_Error =>
      null;
      raise;
   when Storage_Error =>
      null;
      raise Storage_Error;
   when Occur : Tasking_Error =>
      Reraise_Occurrence (Occur);
   when others =>
      return 0;
end T_Abnormal_Function_Return;
