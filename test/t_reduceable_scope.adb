with System;
procedure T_Reduceable_Scope (X : Integer) is
   Top : Integer;    -- Not movable

   package Pack1 is  -- Movable to P
      I : Integer;   -- Not movable
   end Pack1;
   procedure Unused is    -- Not used
   begin
      Pack1.I := 1;
   end Unused;
   procedure For_Access is
   begin
      null;
   end For_Access;

   package Pack2 is  -- Movable to P
      I : Integer;   -- Not movable because declared in package spec
   end Pack2;
   package body Pack2 is
      procedure P is -- Not movable
      begin
         Pack2.I := 1;
      end P;
   begin
      P;
   end Pack2;

   task T1 is  -- Not movable
      entry E; -- Not movable
   end T1;
   task body T1 is
   begin
      null;
   end T1;

   task type TT1 is  -- Not movable
      entry E;       -- Not movable
   end TT1;
   task body TT1 is
   begin
      null;
   end TT1;

   task type TT2 is  -- Movable to block
      entry E;       -- Not movable
   end TT2;
   task body TT2 is
   begin
      null;
   end TT2;

   T2 : TT1;  -- Not movable
begin

For_Loop : for I in 1..10 loop -- Not movable (For_Loop, I)
      null;
   end loop For_loop;
   <<L>> null; -- Not movable
   declare
      T3 : TT2; -- Not movable
   begin
      T1.E;
      T2.E;
      T3.E;
      goto L;
   end;
   declare
      I, J, K, L : Integer;  -- I movable to Inner1, J, L movable to P, K not movable

      procedure P is         -- Not used
         procedure Inner1 is -- Not used
         begin
            I := 1;
            L := 1;
         end Inner1;
         procedure Inner2 is -- Not movable
            type Proc_Ptr is access procedure;
            Addr : Proc_Ptr := For_Access'Access;
         begin
            L := 1;
            Top := 1;
         end Inner2;
      begin
         Inner2;
         J := 1;
      end P;

   begin
      K := 1;
   end;

   declare
      I, J, K, L : Integer;      -- I,L  movable to Inner1, J movable to P, K not movable

      procedure P is             -- Not used
         procedure Inner1 is     -- Not used
            procedure Inner11 is -- Not used
            begin
               L := 1;
            end;
         begin
            I := 1;
            L := 1;
         end Inner1;

         procedure Inner2 is -- Not movable
         begin
            Top := 1;
         end Inner2;
      begin
         Inner2;
         J := 1;
      end P;

   begin
      K := 1;
   end;
end T_Reduceable_Scope;
