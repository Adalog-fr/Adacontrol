procedure T_allocators is
   type Acc1 is access Integer;
   type Acc2 is access String;

   V1 : Acc1;
   V2 : Acc2;
begin
   V1 := new Integer;
   V1 := new Integer'(1);
   V2 := new String (1..3);
   V2 := new String'("Hello world!");
end T_allocators;
