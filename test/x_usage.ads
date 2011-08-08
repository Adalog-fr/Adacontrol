package X_Usage is
   Not_Included : Integer;

   E1, E2, E3, E4 : exception;

   task type TT1 is
      entry E;
   end TT1;
   type TT2 is new TT1;
   subtype TT3 is TT2;

   T1 : TT1;
   T2 : TT2;
   T3 : TT3;
   task T4;

   protected type PT1 is
      entry E;
      procedure P;
      function F return Boolean;
   end PT1;
   type PT2 is new PT1;
   subtype PT3 is PT2;

   P1 : PT1;
   P2 : PT2;
   P3 : PT3;
   protected P4 is
      entry E;
   end P4;
end X_Usage;
