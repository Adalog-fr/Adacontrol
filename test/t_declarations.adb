procedure T_declarations is
   task T1;
   task body T1 is
   begin
      null;
   end T1;

   task type T2;
   task body T2 is
   begin
      null;
   end T2;

   E : exception;

   type Acc1 is access Integer;
   type Acc2 is access procedure;

   I : aliased Integer;
   C : aliased constant Character := ' ';

   type Rec is tagged null record;
begin
   null;
end T_declarations;
