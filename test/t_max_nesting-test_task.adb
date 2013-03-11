separate (T_Max_Nesting)
procedure Test_Task is
   task Global;           -- All > 1
   task body Global is
      task type T1;       -- All > 2, task > 0
      task body T1 is
         task T2 is       -- All > 2, task > 1
         end T2;
         task body T2 is
         begin
            null;
         end T2;
      begin
         null;
      end T1;

      task T3 is          -- All > 2, Task > 0
      end T3;
      task body T3 is
         V : T1;          -- OK
      begin
         null;
      end T3;
   begin
      null;
   end Global;
begin
   null;
end Test_Task;
