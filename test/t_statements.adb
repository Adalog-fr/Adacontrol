with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Vectors;
procedure T_statements is
   procedure Test_Raise is separate;
   procedure Test_Hard_Bounds is separate;

   type Acc_Proc is access procedure;
   Ptr_Proc : Acc_Proc := Test_Raise'Access;

   task T1 is
      entry E1;
      entry E2;
   end T1;
   task body T1 is
   begin
      select             -- Selective_Accept
         accept E1 do    -- Accept
            requeue E2;  -- Requeue
         end E1;
      or
         accept E2 do   -- Accept
            return;     -- Accept_Return
         end E2;
      or terminate;     -- Terminate
      end select;
   end T1;

   I    : Integer;
   Bool : Boolean;

   type Enum is (VA, VB, VC);
   Venum : Enum;

   procedure P is
   begin
      return;           -- Procedure_Return
   end P;

   function F return Integer is
   begin
      if I = 3 then
         return 1;      -- Function_Return OK (first return)
      else
         return 2;      -- Function_Return
      end if;

 B1:  declare           -- Block, Declare_Block, Effective_Declare_Block
         function Ff return Integer is
         begin
            if I = 1 then   -- No_Else
               return 1;    -- Function_Return OK (first return)
            end if;
            return X : Integer do  -- Function_Return
               X := 1;
            end return;
         end Ff;
      begin
         I := 0;
      end B1;
   exception
      when Constraint_Error =>
         return 1;
      when others =>    -- Exception_Others
         if I = 3 then
            return 1;   -- OK Function_Return (first return)
         else
            return 2;   -- Function_Return
         end if;
   end F;

   function G1 return Integer is
   begin
      if I = 2 then
         return X : Integer do  -- Extended_Return
            null;               -- null, OK Function_Return (first return)
         end return;
      else
         return 2;              -- Function_Return
      end if;
   end G1;

   function G2 return Integer is
   begin
      if I = 2 then
         return 2;              -- Function_Return OK (first return)
      else
         return X : Integer do  -- Extended_Return, Function_Return
            null;               -- Null
         end return;
      end if;
   end G2;
   protected Prot is
      entry E;
   end Prot;
   protected body Prot is
      entry E when True is
      begin
         return;        -- Entry_Return
      end E;
   end Prot;

   package Dispatching is
      type Object is tagged null record;
      procedure Proc (X : Object);
      function  Func (X : Object) return Integer;
   end Dispatching;
   package body Dispatching is
      I : Integer;
      procedure Proc (X : Object) is
         procedure Nested is
         begin
            I := Func (Object'Class (X));     -- Dispatching_call, redispatching_call
         end Nested;
      begin
         Proc (Object'Class (X));             -- Dispatching_call, redispatching_call, procedure_call
      end Proc;
      function  Func (X : Object) return Integer is
      begin
         return 0;
      end Func;
   end Dispatching;

   Disp : Dispatching.Object'Class := Dispatching.Object'(null record);

   function F_Ext_Return return Integer is
   begin
      For_Exit:                          -- For_Loop, For_In_Loop
      for I in Integer range 1..10 loop
         return X : Integer do           -- Function_Return OK (first return), Loop_Return, Extended_Return, Exited_Extended_Return
            if I = 3 then                -- No_Else
               return;                   -- Exited_Extended_Return OK (not exiting), Function_Return, Loop_Return
            end if;
            exit For_Exit;               -- Exit, Exit_For_Loop, Unconditional_Exit, Named_Exit
         end return;
         goto False_Continue;            -- Goto, Goto_Not_Continue
         <<False_Continue>>              -- Labelled
         delay 1.0;                      -- Delay
      end loop For_Exit;
      return X : Integer do              -- Function_Return, Extended_Return, Exited_Extended_Return
	goto Hell;                       -- Goto, Goto_Not_Continue
      end return;
  <<Hell>>                               -- Function_Return, Labelled
      return 1;
   end F_Ext_Return;

begin
   delay 1.0;            -- Delay
   delay until Clock;    -- Delay_Until
   goto Next;            -- Goto, Goto_Not_Continue
   <<Next>> null;        -- Null, Labelled
   abort T1;              -- Abort

   loop                  -- Simple_Loop, Unnamed_Simple_Loop
      exit;              -- Exit, Exit_Plain_Loop, Unconditional_Exit, Unnamed_Loop_Exited
      goto Not_Continue; -- Goto, Goto_Not_Continue
      <<Not_Continue>>   -- Labelled, goto (OK Goto_Not_Continue)
      goto Continue;
      <<Continue>> null; -- Labelled, null
   end loop;
B:                      -- Simple_Loop
   loop
      exit;                -- exit, exit_plain_loop, unconditional_exit, Unnamed_exit,
      exit B;              -- Multiple_exit, exit, exit_plain_loop, unconditional_exit, named_exit
      exit T_Statements.B; -- Multiple_exit, exit, Exit_Plain_Loop, unconditional_exit, named_exit, exit_expanded_name.
      goto Reloop;         -- Goto (OK Goto_Not_Continue)
      <<Reloop>>           -- Labelled
   end loop B;

   select               -- Asynchronous select
      T1.E1;
   then abort
      <<Label>> null;   -- Null, Labelled
   end select;

   case I is
      when 1 =>         -- Null_Case_Path
         null;          -- Null;
      when others =>    -- Case_Others_Null, Null_Case_Path
         null;          -- Null;
   end case;

   case I is
      when 1 =>         -- Null_Case_Path
         null;          -- Null
      when others =>    -- Case_Others
         I := 1;
   end case;

   case Venum is        -- OK
      when VA =>        -- Null_Case_Path
         null;          -- Null
      when VB =>        -- Null_Case_Path
         null;          -- Null
      when VC => null;  -- Null_Case_Path, Null
   end case;

   if I = 0 then           -- No_Else, Null_If_Path
      null;                -- Null
   end if;

   if I = 0 then           -- No_Else, If_Elsif, Null_If_Path
      null;                -- Null
   elsif I = 1 then        -- Null_If_Path
     null;                 -- Null
   end if;

B2:begin                   -- Block
      null;                -- Null
   exception
      when others =>       -- Exception_Others_Null
         null;             -- Null
   end B2;

   while Bool  loop null; end loop;  -- While_Loop, Null, Null_Loop_Body

   for I in 1..10 loop null; end loop;               -- For_Loop, For_In_Loop, Untyped_For, Untyped_For_In, Null, Null_Loop_Body
   for I in Integer range 1..10 loop null; end loop; -- For_Loop, For_In_Loop, Null, Null_Loop_Body

   --
   -- Check Unnamed_Multiple_Loop
   --

   for I in Integer range 1 .. 10 loop   -- For_Loop, For_In_Loop, Unnamed_For_Loop
      P;
   end loop;

   for I in Integer range 1 .. 10 loop   -- For_Loop, For_In_Loop, Unnamed_Multiple_Loop
      P;                                 -- procedure_call
      while I /= 0 loop                  -- While_Loop, Unnamed_While_Loop, Unnamed_Multiple_Loop, Null_Loop_Body
         null;                           -- Null
      end loop;
   end loop;


   L1 : for I in Integer range 1 .. 10 loop  -- For_Loop, For_In_Loop
      Venum := VA;
      while Venum /= VA loop             -- While_Loop, Unnamed_While_Loop, Unnamed_Multiple_Loop, Null_Loop_Body
         null;                           -- Null
      end loop;
   end loop L1;

   for I in Integer range 1 .. 10 loop   -- For_Loop, For_In_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop
      Venum := VA;
      L2: while Venum /= VA loop         -- While_Loop, Null_Loop_Body
         null;                           -- Null
      end loop L2;
   end loop;

L3: for I in Integer range 1 .. 10 loop  -- For_Loop, For_In_Loop
      Venum := VA;
      L4: while Venum /= VA loop         -- While_Loop, Null_Loop_Body
         null;                           -- Null
      end loop L4;
   end loop L3;

   for I in Integer range 1 .. 10 loop   -- For_Loop, For_In_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop
      Venum := VA;
      L5: while Venum /= VA loop         -- While_Loop, Null_Loop_Body
         null;                           -- Null
      end loop L5;
      while Venum /= VA loop             -- While_Loop, Unnamed_While_Loop, Unnamed, Null_Loop_Body
         null;                           -- Null
      end loop;
   end loop;

   -- Check for .. of loops
   declare                               -- Block, Unnamed_Block, Declare_Block, Effective_Declare_Block
      S : String (1 .. 10);
      type Dim3 is array (1 .. 3, 1 .. 4, 1 .. 5) of Integer;
      Mult1 : Dim3;
      Mult2 : array (1 .. 2, 1 .. 2) of Integer;
   begin
      for C of S loop                          -- For_Loop, For_Of_Loop, Unnamed_For_Loop, Untyped_For, Untyped_For_Of, Null_Loop_Body
         null;                                 -- Null
      end loop;

      L6 : for C : Character of S (2 .. 8) loop  -- For_Loop, For_Of_Loop, Null_Loop_Body
         null;                                   -- Null
      end loop L6;

      L7: for E1 : Integer of Mult1 loop         -- For_Loop, For_Of_Loop
         L8: for E2 : Integer of Mult2 loop      -- For_Loop, For_Of_Loop, Null_Loop_Body
            null;                                -- Null
         end loop L8;
      end loop L7;
   end;

   -- Check for .. iterator loops
   declare                               -- Block, Unnamed_Block, Declare_Block, Effective_Declare_Block
      package Vects is new Ada.Containers.Vectors (Positive, Integer);
      use Vects;
      V : Vects.Vector;
   begin
      for C in Iterate (V) loop          -- For_Loop, For_Iterator_loop, Unnamed_For_Loop, Null_Loop_Body
         null;                           -- Null;
      end loop;
   end;

   -- Check nesting of bodies

   for I in Integer range 1 .. 10 loop        -- For_Loop, For_In_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop
      begin                                   -- Block, Simple_Block, Unnamed_Block, Unnamed_Simple_Block
         for J in Integer range 1 .. 10 loop  -- For_Loop, For_In_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop, Null_Loop_Body
            null;                             -- Null
         end loop;
      end;
   end loop;

   for I in Integer range 1 .. 10 loop        -- For_Loop, For_In_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop, Null_Loop_Body
      begin                                   -- Block, Unnamed_Block
         null;                                -- Null
      exception
         when Constraint_Error =>
            for J in Integer range 1 .. 10 loop -- For_Loop, For_In_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop, Null_Loop_Body
               null;                            -- Null
            end loop;
      end;
   end loop;

   for I in Integer range 1 .. 10 loop          -- For_Loop, For_In_Loop, Unnamed_For_Loop
      declare                                   -- Block, Unnamed_Block, Declare_Block, Effective_Declare_Block
         procedure Proc is
         begin
            for J in Integer range 1 .. 10 loop -- For_Loop, For_In_Loop, Unnamed_For_Loop, Null_Loop_Body
               null;                            -- Null
            end loop;
         end Proc;
      begin
         null;                                  -- Null
      end;
   end loop;

   declare                                      -- Block, Unnamed_Block, Declare_Block, Effective_Declare_Block
      task T2 is
         entry E;
      end T2;

      task body T2 is
      begin
         for I in Integer range 1 .. 10 loop       -- For_Loop, For_In_Loop, Unnamed_For_Loop
            accept E do                            -- Accept
               for J in Integer range 1 .. 10 loop -- For_Loop, For_In_Loop, Unnamed_For_Loop, Null_Loop_Body
                  null;                            -- Null
               end loop;
            end E;
         end loop;

         T1.E1;                                    -- Entry_Call
         Prot.E;                                   -- Entry_Call
         select                                    -- Conditional_Entry_Call
            T1.E1;                                 -- Entry_Call
         else
            null;                                  -- Null
         end select;
         select                                    -- Timed_Entry_Call
            T1.E1;                                 -- Entry_Call
         or delay 1.0;                             -- Delay
         end select;
      end T2;
   begin
      null;                                        -- Null
   end;

   LL1:loop                                        -- Simple_Loop
      exit LL1 when True;                          -- Exit, Exit_Plain_Loop, Named_Exit
      declare                                      -- Block, Unnamed_Block, Declare_Block, Effective_Declare_Block
         procedure Test is
            X : Integer := 0;
         begin
            Outer: loop                            -- simple_loop
               declare                             -- Block, Unnamed_Block, Declare_Block, Effective_Declare_Block
                  procedure P is
                     task T is
                        entry E;
                     end T;

                     task body T is
                     begin
                        TL:loop                    -- simple_loop
                           accept E do             -- Accept
                              for J in Integer range 1 .. 10 loop -- For_Loop, For_In_Loop, Unnamed_For_Loop
                                 if J = 5 then                    -- No_Else
                                    return;                       -- Loop_Return, accept_return
                                 end if;
                              end loop;
                           end E;
                           exit TL;                -- Exit, Exit_Plain_Loop, unconditional_exit, Named_Exit
                        end loop TL;
                     end T;
                  begin
                     return;                       -- procedure return
                  end P;
               begin
                  null;                           -- Null
               end;
               exit when True;                    -- Exit, Exit_Plain_Loop, unnamed exit
               XL1: for I in 1..10 loop           -- For_Loop, For_In_Loop, Untyped_For, Untyped_For_In
                  exit Outer;                     -- multiple_exits, Exit, Exit_Plain_Loop, unconditional_exit, Named_Exit
                  while X /= 0 loop               -- While_Loop, Unnamed_While_Loop, Unnamed_Multiple_Loop
                     exit when X = 2;             -- Exit, Exit_while_loop, Unnamed_Loop_Exited
                     exit Outer when False;       -- multiple_exit, Exit, Exit_Plain_Loop, Named_Exit
                     exit XL1;                    -- Exit, Exit_for_loop, unconditional_exit, Named_Exit
                  end loop;
                  exit;                           -- multiple_exits, Exit, exit_for_loop, unconditional_exit, unnamed_exit
               end loop XL1;
            end loop Outer;
         end Test;
      begin
         exit LL1;                                -- multiple_exits, Exit, Exit_Plain_Loop, unconditional_exit, Named_Exit
      exception
         when Constraint_Error =>
            exit;                                 -- multiple_exits, Exit, Exit_Plain_Loop, unconditional_exit, unnamed_exit
      end;
   end loop LL1;

   begin                                          -- Block, Simple_Block, Unnamed_Block, Unnamed_Simple_Block
      null;                                       -- null
   end;
   declare                                        -- Block, Unnamed_Block, Declare_Block      pragma Optimize (Time);
      use Ada.Calendar;
   begin
      null;                                       -- null
   end;
   declare                                        -- Block, Unnamed_Block, Declare_Block, Effective_Declare_Block
      pragma Optimize (Time);
      use Ada.Calendar;
      type T is range 1 .. 10;
      I : T;
   begin
      null;                                       -- null
   end;
   declare                                        -- Block, unnamed_block, declare_block, simple_block, unnamed_simple_block
   begin
      null;                                       -- null
   end;

   Dispatching.Proc (Disp);                       -- Dispatching call, procedure_call

IPC:                                              -- Block, Declare Block, Effective Declare Block
   declare
      package Pack is
         type Orig is
            record
               I : Integer;
            end record;
         procedure Inh   (X : out Orig);
         procedure Redef (X : out Orig);

         type Der is new Orig;
         procedure Redef (X : out Der);
      end Pack;
      package body Pack is
         procedure Inh   (X : out Orig) is
         begin
            X.I := 1;
         end Inh;

         procedure Redef (X : out Orig) is
         begin
            X.I := 1;
         end Redef;

         procedure Redef (X : out Der) is
         begin
            X.I := 1;
         end Redef;
      end Pack;
      use Pack;

      O : Orig;
      D : Der;
      type Acc_Proc_Param is access procedure (X : out Pack.Orig);
      Ptr : Acc_Proc_Param := Pack.Inh'Access;
   begin
      Inh (O);       -- procedure_call
      Redef (O);     -- procedure_call
      Inh (D);       -- Inherited_Procedure_Call, procedure_call
      Redef (D);     -- procedure_call
      Ptr (O);       -- procedure_call, dynamic_procedure_call
      Ptr.all (O);   -- procedure_call, dynamic_procedure_call
   end IPC;

   <<Back>>            -- labelled, procedure_call, dynamic_procedure_call
   Ptr_Proc.all;
   <<Here>> goto Here; -- labelled, goto, goto_not_continue, backward_goto
   goto Back;          -- goto, goto_not_continue, backward_goto
end T_statements;
