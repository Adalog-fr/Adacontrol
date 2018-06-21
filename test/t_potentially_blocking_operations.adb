with Ada.Task_Identification, Ada.Synchronous_Task_Control;
with Ada.Text_Io, Ada.Sequential_IO;
procedure T_Potentially_Blocking_Operations is
   function Func return Boolean is
      use Ada.Text_IO;
   begin
      Put_Line ("Potentially blocking");
      return True;
   end Func;

   protected type Prot1 is
      entry E;
      procedure P;
   end Prot1;

   protected Prot2 is
      procedure P1;
      procedure P2;
   end Prot2;

   V1 : Prot1;
   protected body Prot1 is
      entry E when Func is    -- Potentially blocking in barrier
      begin
         requeue Prot1.E;  -- Internal requeue to same object, OK
         requeue V1.E;     -- Possible external requeue to same object
      end E;
      procedure P is
      begin
         Prot2.P2;
      end P;
   end Prot1;

   protected type Prot3 is
      procedure P;
   end Prot3;

   Obj : Prot3;

   protected body Prot3 is
      procedure P is
      begin
         Obj.P;           -- Possible external call to same object
      end P;
   end Prot3;

   procedure Proc1 (B : Boolean) is
   begin
      V1.E;
      if B then
         Proc1 (False);   -- Check recursion
      end if;
   end Proc1;

   procedure Proc2 is
      task T;
      task body T is
      begin
         null;
      end;
   begin
      null;
   end Proc2;

   procedure Proc3 is
   begin
      V1.P;
   end Proc3;

   procedure Proc4 renames T_Potentially_Blocking_Operations.Proc3;

   SO : Ada.Synchronous_Task_Control.Suspension_Object;

   generic
   procedure Gen_Proc;
   procedure Gen_Proc is
      use Ada.Synchronous_Task_Control;
   begin
      Suspend_Until_True (SO);
   end Gen_Proc;
   procedure Inst_Proc is new Gen_Proc;

   package Int_Io is new Ada.Text_IO.Integer_IO (Integer);
   package Seq_Io is new Ada.Sequential_IO (Integer);
   F : Seq_IO.File_Type;

   task type TT;
   task body TT is
   begin
      null;
   end TT;
   type TT_Access is access TT;

   -- a deep hidden task
   type Rec (B1 : Boolean; B2 : Boolean) is
      record
         X : Integer;
         case B1 is
            when True =>
               null;
            when False =>
               case B2 is
                  when True =>
                     null;
                  when False =>
                     T : TT;
               end case;
         end case;
      end record;
   type Rec_Access is access Rec;
   function Create_Rec return Rec is
   begin
      return V : Rec (False, False);
   end Create_Rec;

   protected body Prot2 is
      procedure Q;
      procedure P1 is
         use Ada.Text_IO, Int_IO, Seq_IO;
         S : String (1..10);
         I : Integer;
         X : TT;                    -- Task creation
         R : Rec (False, True);     -- Task creation
         A : TT_Access;
         B : Rec_Access;
      begin
         V1.E;                      -- Potentially blocking statement entry call
         Proc1 (True);              -- Potentially blocking call
         Proc2;                     -- Potentially blocking call
         Proc4;                     -- External call to same object
         Q;                         -- Potentially blocking call
         Inst_Proc;                 -- Potentially blocking call
         New_Line;                  -- Potentially blocking call
         Put (I);                   -- Potentially blocking call
         Put (S, I);                -- OK
         Open (F, In_File, "junk"); -- Potentially blocking call (from generic)
         delay 3.0;                 -- Potentially blocking statement
         A := new TT;               -- Task creation
         B := new Rec'(Create_Rec); -- Task creation through initialized allocator
      end P1;
      procedure P2 is
      begin
         null;
      end P2;
      procedure Q is
         use Ada.Task_Identification;
      begin
         Abort_Task (Current_Task);   -- Potentially blocking call
      end Q;
   end Prot2;

   -- Nasty forms of internal calls
   protected Prot4 is
      procedure Proc1;
      procedure Proc2;
   end Prot4;

   protected body Prot4 is
      procedure Proc1 is
         protected Prot5 is
            procedure Proc;
         end Prot5;

         protected body Prot5 is
            procedure Proc is
            begin
               Proc2;
               Prot4.Proc2;
            end;
         end Prot5;
      begin
         Prot5.Proc;
         Prot4.Proc1;
      end;

      procedure Proc2 is
      begin
         null;
      end;
   end Prot4;

   protected Prot5 is
      function F return Boolean;
   end Prot5;

   protected body Prot5 is
      function F  return Boolean is (Func);  -- Potentially blocking expression function
   end Prot5;

begin
   null;
end T_Potentially_Blocking_Operations;
