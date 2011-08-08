with Ada.Calendar; use Ada.Calendar;
procedure T_statements is
   procedure Test_Raise is separate;

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
         return 1;      -- OK (first return)
      else
         return 2;      -- Function_Return
      end if;

 B1:  declare           -- Block, Declare_Block, Effective_Declare_Block
         function Ff return Integer is
         begin
            if I = 1 then   -- No_Else
               return 1;
            end if;
         end Ff;
      begin
         I := 0;
      end B1;
   exception
      when Constraint_Error =>
         return 1;
      when others =>    -- Exception_Others
         if I = 3 then
            return 1;   -- OK (first return)
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

   package Dispatching is
      type Object is tagged null record;
      procedure Proc (X : Object);
      function  Func (X : Object) return Integer;
   end Dispatching;
   package body Dispatching is
      procedure Proc (X : Object) is
      begin
         null;            -- Null
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
   <<Next>> null;       -- Null, Labelled
   abort T1;             -- Abort

   loop                 -- Simple_Loop, Unnamed_Simple_Loop
      exit;             -- Unconditional_Exit, Exit, Unnamed_Loop_Exited
   end loop;
B:                      -- Simple_Loop
   loop
      exit;                -- Unnamed_exit, unconditional_exit, exit
      exit B;              -- Multiple_exit, unconditional_exit, exit
      exit T_Statements.B; -- Multiple_exit, unconditional_exit, exit_expanded_name, exit
   end loop B;

   select               -- Asynchronous select
      T1.E1;
   then abort
      <<Label>> null;   -- Null, Labelled
   end select;

   case I is
      when 1 =>
         null;          -- Null;
      when others =>    -- Case_Others_Null
         null;          -- Null;
   end case;

   case I is
      when 1 =>
         null;          -- Null
      when others =>    -- Case_Others
         I := 1;
   end case;

   case Venum is        -- OK
      when VA =>
         null;          -- Null
      when VB =>
         null;          -- Null
      when VC => null;  -- Null
   end case;

   if I = 0 then           -- No_Else
      null;                -- Null
   end if;

   if I = 0 then           -- No_Else, If_Elsif
      null;                -- Null
   elsif I = 1 then
     null;                -- Null
   end if;

B2:begin                   -- Block
      null;                -- Null
   exception
      when others =>       -- Exception_Others_Null
         null;             -- Null
   end B2;

   while Bool  loop null; end loop;  -- While_Loop, Unnamed_While_Loop, Null

   for I in 1..10 loop null; end loop;               -- For_Loop, Unnamed_For_Loop, Untyped_For, Null
   for I in Integer range 1..10 loop null; end loop; -- For_Loop, Unnamed_For_Loop, Null

   --
   -- Check Unnamed_Multiple_Loop
   --

   for I in Integer range 1 .. 10 loop   -- For_Loop, Unnamed_For_Loop
      P;
   end loop;

   for I in Integer range 1 .. 10 loop   -- For_Loop, Unnamed_Multiple_Loop
      P;
      while I /= 0 loop                  -- While_Loop, Unnamed_While_Loop, Unnamed_Multiple_Loop
         null;                           -- Null
      end loop;
   end loop;


   L1 : for I in Integer range 1 .. 10 loop  -- For_Loop
      Venum := VA;
      while Venum /= VA loop             -- While_Loop, Unnamed_While_Loop, Unnamed_Multiple_Loop
         null;                           -- Null
      end loop;
   end loop L1;

   for I in Integer range 1 .. 10 loop   -- For_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop
      Venum := VA;
      L2: while Venum /= VA loop         -- While_Loop
         null;                           -- Null
      end loop L2;
   end loop;

L3: for I in Integer range 1 .. 10 loop  -- For_Loop
      Venum := VA;
      L4: while Venum /= VA loop         -- While_Loop
         null;                           -- Null
      end loop L4;
   end loop L3;

   for I in Integer range 1 .. 10 loop   -- For_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop
      Venum := VA;
      L5: while Venum /= VA loop         -- While_Loop
         null;                           -- Null
      end loop L5;
      while Venum /= VA loop             -- While_Loop, Unnamed_While_Loop, Unnamed
         null;                           -- Null
      end loop;
   end loop;

   -- Check nesting of bodies

   for I in Integer range 1 .. 10 loop        -- For_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop
      begin                                   -- Block, Unnamed_Block
         for J in Integer range 1 .. 10 loop  -- For_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop
            null;                             -- Null
         end loop;
      end;
   end loop;

   for I in Integer range 1 .. 10 loop        -- For_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop
      begin                                   -- Block, Unnamed_Block
         null;                                -- Null
      exception
         when Constraint_Error =>
            for J in Integer range 1 .. 10 loop -- For_Loop, Unnamed_For_Loop, Unnamed_Multiple_Loop
               null;                            -- Null
            end loop;
      end;
   end loop;

   for I in Integer range 1 .. 10 loop          -- For_Loop, Unnamed_For_Loop
      declare                                   -- Block, Unnamed_Block, Declare_Block, Effective_Declare_Block
         procedure Proc is
         begin
            for J in Integer range 1 .. 10 loop -- For_Loop, Unnamed_For_Loop
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
         for I in Integer range 1 .. 10 loop       -- For_Loop, Unnamed_For_Loop
            accept E do
               for J in Integer range 1 .. 10 loop -- For_Loop, Unnamed_For_Loop
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
      exit LL1 when True;                          -- Exit
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
                           accept E do
                              for J in Integer range 1 .. 10 loop -- For_Loop, Unnamed_For_Loop
                                 if J = 5 then                    -- No_Else
                                    return;                       -- Loop_Return, accept_return
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
                  null;                           -- Null
               end;
               exit when True;                    -- unnamed exit, exit
               XL1: for I in 1..10 loop           -- For_Loop, untyped for
                  exit Outer;                     -- multiple_exits, unconditional_exit, exit
                  while X /= 0 loop               -- While_Loop, Unnamed_While_Loop, Unnamed_Multiple_Loop
                     exit when X = 2;             -- Exit_while_loop, Unnamed_Loop_Exited
                     exit Outer when False;       -- multiple_exit, exit
                     exit XL1;                    -- Exit_for_loop, unconditional_exit
                  end loop;
                  exit;                           -- multiple_exits, exit_for_loop, unconditional_exit, unnamed_exit
               end loop XL1;
            end loop Outer;
         end Test;
      begin
         exit LL1;                                -- multiple_exits, unconditional_exit, exit
      exception
         when Constraint_Error =>
            exit;                                 -- multiple_exits, unconditional_exit, unnamed_exit, exit
      end;
   end loop LL1;

   begin                                          -- Block, Unnamed_Block
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

   Dispatching.Proc (Disp);                       -- Dispatching call
   I := Dispatching.Func (Disp);                  -- Dispatching call

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
   begin
      Inh (O);
      Redef (O);
      Inh (D);       -- Inherited_Procedure_Call
      Redef (D);
   end IPC;
end T_statements;
