with Ada.Calendar; use Ada.Calendar;
procedure T_statements is
   task T1 is
      entry E1;
      entry E2;
   end T1;
   task body T1 is
   begin
      select             -- Selective_Accept
         accept E1 do
            requeue E2;  -- Requeue
         end E1;
      or
         accept E2 do
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
         return 1;
      else
         return 2;      -- Function_Return
      end if;

 B1:  declare           -- Block
         function Ff return Integer is
         begin
            if I = 1 then   -- No_Else
               return 1;
            end if;
         end Ff;
      begin
         I := 0;
      end B1;
   exception            -- Exception_Handler
      when Constraint_Error =>
         return 1;
      when others =>    -- Exception_Others
         if I = 3 then
            return 1;   -- Function_Return
         else
            return 2;   -- Function_Return
         end if;
   end F;

   protected Prot is
      entry E;
   end Prot;
   protected body Prot is
      entry E when True is
      begin
         return;        -- Entry_Return
      end E;
   end Prot;

   E : exception;

   package Dispatching is
      type Object is tagged null record;
      procedure Proc (X : Object);
      function  Func (X : Object) return Integer;
   end Dispatching;
   package body Dispatching is
      procedure Proc (X : Object) is
      begin
         null;
      end Proc;
      function  Func (X : Object) return Integer is
      begin
         return 0;
      end Func;
   end Dispatching;

   Disp : Dispatching.Object'Class := Dispatching.Object'(null record);
begin
   delay 1.0;           -- Delay
   delay until Clock;   -- Delay_Until
   goto Next;           -- Goto
   <<Next>> null;
   abort T1;             -- Abort

   loop
      exit;             -- Unconditional_Exit, Exit, Unnamed_Loop_Exited
   end loop;
B:
   loop
      exit;             -- Unnamed_exit, unconditional_exit, exit
      exit B;           -- Multiple_exit, unconditional_exit, exit
   end loop B;

   select               -- Asynchronous select
      T1.E1;
   then abort
      null;
   end select;

   case I is
      when 1 =>
         null;
      when others =>    -- Case_Others_Null
         null;
         null;
   end case;

   case I is
      when 1 =>
         null;
      when others =>    -- Case_Others
         I := 1;
   end case;

   case Venum is        -- OK
      when VA =>
         null;
      when VB =>
         null;
      when VC => null;
   end case;

   raise Constraint_Error; -- Raise_Standard

   raise E;                -- Raise

   if I = 0 then           -- No_Else
      null;
   end if;

   if I = 0 then           -- No_Else
      null;
   elsif I = 1 then
     null;
   end if;

   if I = 0 then           -- OK
      null;
   else
      null;
   end if;

   if I = 0 then           -- OK
      null;
   elsif I = 1 then
     null;
   else
      null;
   end if;

B2:begin                   -- Block
      null;
   exception               -- Exception_Handler
      when others =>       -- Exception_Others_Null
         null;
   end B2;

   begin                   -- Block, Unnamed_Block
      null;
   exception               -- Exception_Handler
      when others =>       -- Exception_Others
         I := 1;
         raise;            -- Reraise
   end;

   while Bool  loop null; end loop;  -- While_Loop
   while True  loop null; end loop;  -- While_True, While_Loop
   while False loop null; end loop;  -- While_Loop

   for I in 1..10 loop null; end loop;               -- Untyped_For
   for I in Integer range 1..10 loop null; end loop; -- OK

   --
   -- Check Unnamed_Multiple_Loop
   --

   for I in Integer range 1 .. 10 loop             -- OK
      P;
   end loop;

   for I in Integer range 1 .. 10 loop             -- Unnamed
      P;
      while I /= 0 loop  -- While_Loop                          -- Unnamed
         null;
      end loop;
   end loop;


L1: for I in Integer range 1 .. 10 loop            -- OK
      Venum := VA;
      while Venum /= VA loop  -- While_Loop                    -- Unnamed
         null;
      end loop;
   end loop L1;

   for I in Integer range 1 .. 10 loop             -- Unnamed
      Venum := VA;
      L2: while Venum /= VA loop   -- While_Loop
         null;
      end loop L2;
   end loop;

L3: for I in Integer range 1 .. 10 loop            -- OK
      Venum := VA;
      L4: while Venum /= VA loop   -- While_Loop
         null;
      end loop L4;
   end loop L3;

   for I in Integer range 1 .. 10 loop             -- Unnamed
      Venum := VA;
      L5: while Venum /= VA loop    -- While_Loop                 -- OK
         null;
      end loop L5;
      while Venum /= VA loop     -- While_Loop                    -- Unnamed
         null;
      end loop;
   end loop;

   -- Check nesting of bodies

   for I in Integer range 1 .. 10 loop             -- Unnamed
      begin                                        -- Block, Unnamed_Block
         for J in Integer range 1 .. 10 loop       -- Unnamed
            null;
         end loop;
      end;
   end loop;

   for I in Integer range 1 .. 10 loop             -- Unnamed
      begin                                        -- Block, Unnamed_Block
         null;
      exception                                    -- Exception_Handler
         when Constraint_Error =>
            for J in Integer range 1 .. 10 loop    -- Unnamed
               null;
            end loop;
      end;
   end loop;

   for I in Integer range 1 .. 10 loop             -- OK
      declare                                      -- Block, Unnamed_Block
         procedure Proc is
         begin
            for J in Integer range 1 .. 10 loop    -- OK
               null;
            end loop;
         end Proc;
      begin
         null;
      end;
   end loop;

   declare                                        -- Block, Unnamed_Block
      task T2 is
         entry E;
      end T2;

      task body T2 is
      begin
         for I in Integer range 1 .. 10 loop       -- OK
            accept E do
               for J in Integer range 1 .. 10 loop -- OK
                  null;
               end loop;
            end E;
         end loop;
         select                                    -- Conditional_Entry_Call
            T1.E1;
         else
            null;
         end select;
         select                                    -- Timed_Entry_Call
            T1.E1;
         or delay 1.0;                             -- Delay
         end select;
      end T2;
   begin
      null;
   end;

   LL1:loop
      exit LL1 when True;                          -- Exit
      declare                                      -- Block, Unnamed_Block
         procedure Test is
            X : Integer := 0;
         begin
            Outer: loop
               declare                             -- Block, Unnamed_Block
                  procedure P is
                     task T is
                        entry E;
                     end T;

                     task body T is
                     begin
                        TL:loop
                           accept E do
                              for J in Integer range 1 .. 10 loop
                                 if J = 5 then     -- No_Else
                                    return;        -- Loop_Return, accept_return
                                 end if;
                              end loop;
                           end E;
                           exit TL;                -- unconditional_exit, exit
                        end loop TL;
                     end T;
                  begin
                     return;                       -- procedure return
                  end P;
               begin
                  null;
               end;
               exit when True;                     -- unnamed exit, exit
               XL1: for I in 1..10 loop            -- untyped for
                  exit Outer;                      -- multiple_exits, unconditional_exit, exit
                  while X /= 0 loop                -- While_Loop, Unnamed
                     exit when X = 2;              -- Exit_while_loop, Unnamed_Loop_Exited
                     exit Outer when False;        -- multiple_exit, exit
                     exit XL1;                     -- Exit_for_loop, unconditional_exit
                  end loop;
                  exit;                            -- multiple_exits, exit_for_loop, unconditional_exit, unnamed_exit
               end loop XL1;
            end loop Outer;
         end Test;
      begin
         exit LL1;                                 -- multiple_exits, unconditional_exit, exit
      end;
   end loop LL1;

   Dispatching.Proc (Disp);                        -- Dispatching call
   I := Dispatching.Func (Disp);                   -- Dispatching call
end T_statements;
