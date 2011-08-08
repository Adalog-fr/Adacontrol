procedure T_allocators is
   type Acc1 is access Integer;
   type Acc2 is access String;

   task type T1;
   task body T1 is
   begin
      null;
   end T1;
   type Acc_T1 is access T1;

   task type T2;
   task body T2 is
   begin
      null;
   end T2;
   type Acc_T2 is access T2;

   protected type P1 is
      procedure Proc;
   end P1;
   protected body P1 is
      procedure Proc is
      begin
         null;
      end Proc;
   end P1;
   type Acc_P1 is access P1;

   protected type P2 is
      procedure Proc;
   end P2;
   protected body P2 is
      procedure Proc is
      begin
         null;
      end Proc;
   end P2;
   type Acc_P2 is access P2;


   V1 : Acc1;
   V2 : Acc2;
   V3 : Acc_T1;
   V4 : Acc_T2;
   V5 : Acc_P1;
   V6 : Acc_P2;
begin
   V1 := new Integer;
   V1 := new Integer'(1);
   V2 := new String (1..3);
   V2 := new String'("Hello world!");
   V3 := new T1;
   V4 := new T2;
   V5 := new P1;
   V6 := new P2;
end T_allocators;
