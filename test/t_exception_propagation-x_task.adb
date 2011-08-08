separate (T_Exception_Propagation)
procedure X_Task is
   task T1;
   task body T1 is -- Task
   begin
      null;
   end T1;

   task T2;
   task body T2 is -- OK
   begin
      null;
   exception
      when others =>
         null;
   end T2;

   task T3;
   task body T3 is -- Task
   begin
      null;
   exception
      when others =>
         raise;
   end T3;
begin
   null;
end X_Task;
