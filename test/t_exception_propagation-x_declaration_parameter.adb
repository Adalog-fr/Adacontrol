separate (T_Exception_Propagation)
procedure X_Declaration_Parameter is
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
      X : Integer;                   -- declaration level 3
      Y : Integer := X + 1;          -- declaration level 1
   begin
      null;
   exception
      when others =>
         null;
   end;

   -- Level 2
   procedure Proc12 is
      X : Integer;          -- declaration level 3
      Y : Integer := X;     -- declaration level 2
   begin
      null;
   exception
      when others =>
         null;
   end;

   -- Level 3
   procedure Proc13 is
      X : Integer;          -- declaration level 3
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
      procedure Make_Cb2 (CB : Acc_Proc) is
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

   Ptr : constant Acc_Registration_Proc := Make_CB_L0'Access;  -- declaration level 3

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

   Make_Cb_L0 (X_Declaration_Parameter.Proc2'Access);  -- Propagating, qualified name
   Make_Cb_L0 (Proc3'Access);                 -- Propagating
   Make_Cb_L0 (Inst1'Access);                 -- Propagating, instantiation

   Ptr (CB => Proc10'Access);                 -- Access to registration proc
   Ptr.all (Proc10'Access);                   -- id., explicit dereference

   Inst2 (Proc10'Access);                     -- Registration proc from generic

   Make_Cb2 (Proc10'Access);                  -- Registration proc part of generic
end X_Declaration_Parameter;
