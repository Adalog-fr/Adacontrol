with Ada.Exceptions;
with X_Uncheckable;
procedure T_exception_propagation is
   use X_Uncheckable;
begin
Test_Parameter:
   declare
      type Acc_Proc is access procedure;
      type Acc_Registration_Proc is access procedure (CB : Acc_Proc);

      procedure Make_CB_L0 (CB : Acc_Proc) is
      begin
         Cb.all;
      end Make_CB_L0;

      procedure Make_CB_L1 (CB : Acc_Proc) is
      begin
         Cb.all;
      end Make_CB_L1;

      procedure Make_CB_L2 (CB : Acc_Proc) is
      begin
         Cb.all;
      end Make_CB_L2;

      procedure Make_CB_L3 (CB : Acc_Proc) is
      begin
         Cb.all;
      end Make_CB_L3;

     -- Level 0
      procedure Proc10 is
      begin
         null;
      end;

      -- Level 1
      procedure Proc11 is
         X : Integer;
         Y : Integer := X + 1;
      begin
         null;
      exception
         when others =>
            null;
      end;

      -- Level 2
      procedure Proc12 is
         X : Integer;
         Y : Integer := X;
      begin
         null;
      exception
         when others =>
            null;
      end;

      -- Level 3
      procedure Proc13 is
         X : Integer;
      begin
         null;
      exception
         when others =>
            null;
      end;

      procedure Proc2 is
      begin
         null;
      exception
         when others =>
            raise;
      end Proc2;

      procedure Proc3 is
         use Ada.Exceptions;
      begin
         null;
      exception
         when others =>
            Raise_Exception (Constraint_Error'Identity);
      end Proc3;

      procedure Proc_OK is
      begin
         null;
      exception
         when others =>
            null;
      end Proc_OK;

      generic
      procedure Gen1;
      procedure Gen1 is
      begin
         null;
      end Gen1;
      procedure Inst1 is new Gen1;

      generic
      procedure Gen2 (CB : Acc_Proc);
      procedure Gen2 (CB : Acc_Proc) is
      begin
         null;
      end Gen2;
      procedure Inst2 is new Gen2;

      generic
      package Gen3 is
         procedure Make_Cb2 (CB : Acc_Proc);
      end Gen3;
      package body Gen3 is
        procedure Make_cb2 (CB : Acc_Proc) is
        begin
           null;
        end Make_Cb2;
      end Gen3;
      package Inst3 is new Gen3;
      use Inst3;

      generic
         with procedure Gen_Param;
      procedure Gen4;
      procedure Gen4 is begin null; end;

      procedure Inst4 is new Gen4 (Proc10);
      procedure Inst5 is new Gen4 (Inst1);

      Ptr1 : constant Acc_Proc := Proc10'Access;
      Ptr2 : constant Acc_Registration_Proc := Make_CB_L0'Access;

   begin
      -- Level 0
      Make_Cb_L0 (Proc10'Access);                -- Propagating
      Make_Cb_L0 (Proc11'Access);                -- Not Propagating
      Make_Cb_L0 (Proc12'Access);                -- Not Propagating
      Make_Cb_L0 (Proc13'Access);                -- Not Propagating

       -- Level 1
      Make_Cb_L1 (Proc10'Access);                -- Propagating
      Make_Cb_L1 (Proc11'Access);                -- Propagating
      Make_Cb_L1 (Proc12'Access);                -- Not Propagating
      Make_Cb_L1 (Proc13'Access);                -- Not Propagating

        -- Level 2
      Make_Cb_L2 (Proc10'Access);                -- Propagating
      Make_Cb_L2 (Proc11'Access);                -- Propagating
      Make_Cb_L2 (Proc12'Access);                -- Propagating
      Make_Cb_L2 (Proc13'Access);                -- Not Propagating

        -- Level 3
      Make_Cb_L3 (Proc10'Access);                -- Propagating
      Make_Cb_L3 (Proc11'Access);                -- Propagating
      Make_Cb_L3 (Proc12'Access);                -- Propagating
      Make_Cb_L3 (Proc13'Access);                -- Propagating

      Make_Cb_L0 (Test_Parameter.Proc2'Access);  -- Propagating, qualified name
      Make_Cb_L0 (Proc3'Access);                 -- Propagating
      Make_Cb_L0 (Inst1'Access);                 -- Propagating, instantiation


      Make_Cb_L0 (Ptr1.all'Access);             -- Uncheckable
      Make_Cb_L0 (Dyn_Ren_Proc'Access);         -- Uncheckable
      Dispatch (Dyn_Tagged);                    -- Uncheckable

      Ptr2 (CB => Proc10'Access);               -- Access to registration proc
      Ptr2.all (Proc10'Access);                 -- id., explicit dereference

      Inst2 (Proc10'Access);                    -- Registration proc from generic

      Make_Cb2 (Proc10'Access);                 -- Registration proc part of generic
   end Test_Parameter;

Test_Convention:
   declare
      procedure Proc is begin null; end;

      procedure Proc_C1;
      pragma Convention (C, Proc_C1);
      procedure Proc_C1 is begin null; end;

      procedure Proc_C2;
      pragma Convention (C, Proc_C2);
      procedure Proc_C2 is
      begin
         null;
      exception
         when others =>
            raise;
      end Proc_C2;

      procedure Proc_C3;
      pragma Convention (C, Proc_C3);
      procedure Proc_C3 is
         use Ada.Exceptions;
      begin
         null;
      exception
         when others =>
            Raise_Exception (Constraint_Error'Identity);
      end Proc_C3;

      generic
      function F return Integer;
      pragma Convention (C, F);
      function F return Integer is
      begin
         return 0;
      end F;

      procedure Proc_C_OK;
      pragma Convention (C, Proc_C_OK);
      procedure Proc_C_OK is
      begin
         null;
      exception
         when others =>
            null;
      end Proc_C_OK;
   begin
      null;
   end Test_Convention;

Test_Task:
   declare
      task T1;
      task body T1 is -- Propagating
      begin
         null;
      end T1;

      task T2;
      task body T2 is -- Not propagating
      begin
         null;
      exception
         when others =>
            null;
      end T2;

      task T3;
      task body T3 is -- Propagating
      begin
         null;
      exception
         when others =>
            raise;
      end T3;
   begin
      null;
   end Test_Task;
end T_exception_propagation;

