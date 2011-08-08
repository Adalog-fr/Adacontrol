package body X_Usage is
   task body TT1 is
   begin
      null;
   end TT1;

   task body T4 is
   begin
      null;
   end T4;

   protected body PT1 is
      entry E when True is
      begin
         null;
      end E;
      procedure P is
      begin
         null;
      end P;
      function F return Boolean is
      begin
         return True;
      end F;
   end PT1;

   protected body P4 is
      entry E when True is
      begin
         null;
      end E;
   end P4;
end X_Usage;
