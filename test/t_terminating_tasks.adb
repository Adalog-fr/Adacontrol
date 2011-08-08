procedure T_Terminating_Tasks is

   -----------------------------
   -- Checking last statement --
   -----------------------------

   -- A_Single_Task_Definition
   task Single_Task_Without_Unconditional_Loop is
      entry E;
   end Single_Task_Without_Unconditional_Loop;
   -- A_Task_Body_Definition
   --  without any unconditional loop
   task body Single_Task_Without_Unconditional_Loop is
      I : Integer := 0;
   begin
      accept E;
      I := I + 1;
   end Single_Task_Without_Unconditional_Loop;


   -- A_Task_Type_Definition
   task type Task_Type_Without_Unconditional_Loop is
      entry E;
   end Task_Type_Without_Unconditional_Loop;
   -- A_Task_Body_Definition
   --  without any unconditional loop
   task body Task_Type_Without_Unconditional_Loop is
      I : Integer := 0;
   begin
      accept E;
      I := I + 1;
   end Task_Type_Without_Unconditional_Loop;


   -- A_Task_Type_Definition
   task type Task_Type_With_Non_Ending_Unconditional_Loop is
      entry E;
   end Task_Type_With_Non_Ending_Unconditional_Loop;
   -- A_Task_Body_Definition
   --  with an unconditional loop but not as the last statement
   task body Task_Type_With_Non_Ending_Unconditional_Loop is
      I : Integer := 0;
   begin
      accept E;
      loop
         I := I + 1;
         if I > 50 then
            I := 0;
         end if;
      end loop;
      I := 0;
   end Task_Type_With_Non_Ending_Unconditional_Loop;


   --------------------------------
   -- Checking `exit' statements --
   --------------------------------

   -- A_Task_Type_Definition
   task type Task_Type_With_Exited_Unconditional_Loop is
      entry E;
   end Task_Type_With_Exited_Unconditional_Loop;
   -- A_Task_Body_Definition
   --  with an exited unconditional loop as the last statement
   task body Task_Type_With_Exited_Unconditional_Loop is
      I : Integer := 0;
   begin
      accept E;
      loop                                                      -- should trigger
         I := I + 1;
         exit when I = 10;                                      -- from here
      end loop;
   end Task_Type_With_Exited_Unconditional_Loop;


   -- A_Task_Type_Definition
   task type Task_Type_With_Exited_Labeled_Unconditional_Loop is
      entry E;
   end Task_Type_With_Exited_Labeled_Unconditional_Loop;
   -- A_Task_Body_Definition
   --  with an exited labeled unconditional loop as the last statement
   task body Task_Type_With_Exited_Labeled_Unconditional_Loop is
      I : Integer := 0;
   begin
      accept E;
  Unconditional_Loop:
      loop                                                      -- should trigger
         I := I + 1;
     Inner_For_Loop:
         for J in Integer range 1..5 loop
            I := I + 1;
            exit Unconditional_Loop when I > 50;                -- from here
         end loop Inner_For_Loop;
      end loop Unconditional_Loop;
   end Task_Type_With_Exited_Labeled_Unconditional_Loop;


   -- A_Task_Type_Definition
   task type Task_Type_With_Exited_Unconditional_Loop_And_Terminate is
      entry E;
   end Task_Type_With_Exited_Unconditional_Loop_And_Terminate;
   -- A_Task_Body_Definition
   --  with an exited unconditional loop and a terminate alternative
   task body Task_Type_With_Exited_Unconditional_Loop_And_Terminate is
      I : Integer := 0;
   begin
      loop                                                      -- should trigger
         I := I + 1;
         select
            accept E;
            I := I + 2;
            exit when (I mod 7) = 0;                            -- from here
         or
            terminate;                                          -- from here
         end select;
         if I > 50 then
            I := 0;
         end if;
      end loop;
   end Task_Type_With_Exited_Unconditional_Loop_And_Terminate;


   -- A_Task_Type_Definition
   task type Task_Type_With_Non_Exited_Labeled_Unconditional_Loop is
      entry E;
   end Task_Type_With_Non_Exited_Labeled_Unconditional_Loop;
   -- A_Task_Body_Definition
   --  with a non-exited labeled unconditional loop as the last statement
   task body Task_Type_With_Non_Exited_Labeled_Unconditional_Loop is
      I : Integer := 0;
   begin
      accept E;
  Unconditional_Loop:
      loop                                                      -- should not trigger
         I := I + 1;
     Inner_For_Loop:
         for J in Integer range 1..5 loop
            I := I + 1;
            exit Inner_For_Loop when I > 50;
         end loop Inner_For_Loop;
         if I > 50 then
            I := 0;
         end if;
      end loop Unconditional_Loop;
   end Task_Type_With_Non_Exited_Labeled_Unconditional_Loop;


   ---------------------------------------
   -- Checking `terminate' alternatives --
   ---------------------------------------

   -- A_Task_Type_Definition
   task type Task_Type_With_Terminate is
      entry E;
   end Task_Type_With_Terminate;
   -- A_Task_Body_Definition
   --  with a terminate alternative
   task body Task_Type_With_Terminate is
      I : Integer := 0;
   begin
      loop                                                      -- should trigger
         select
            accept E;
            I := I + 1;
         or
            terminate;                                          -- from here
         end select;
         if I > 50 then
            I := 0;
         end if;
      end loop;
   end Task_Type_With_Terminate;

begin
   null;
end T_Terminating_Tasks;
