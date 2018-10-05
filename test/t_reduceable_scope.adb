with X_Reduceable_Scope;
use  X_Reduceable_Scope;                         -- All uses qualified
procedure T_Reduceable_Scope (X : Integer) is
   procedure Use_Type is separate;               -- Not used

   Top : Integer;                                -- Not movable

   package Pack1 is                              -- Movable to Unused
      I : Integer;                               -- Not movable
   end Pack1;

   procedure Unused is                           -- Not used
      use Pack1;                                 -- Movable to block
   begin
      begin
         I := 1;
      end;
   end Unused;
   procedure For_Access is
   begin
      null;
   end For_Access;

   package Pack2 is                              -- Movable to P
      I  : Integer;                              -- Movable to body
      E1 : exception;                            -- Movable to body
   end Pack2;
   package body Pack2 is
      E2 : exception;                            -- OK (movable to P, but exceptions are only to_body)
      procedure P is                             -- Not movable
      begin
         Pack2.I := 1;
         raise E1;
         raise E2;
      end P;
   begin
      P;
   end Pack2;

   task T1 is                                    -- Not movable
      entry E;                                   -- Not movable
   end T1;
   task body T1 is
   begin
      null;
   end T1;

   task type TT1 is                              -- Not movable
      entry E;                                   -- Not movable
   end TT1;
   task body TT1 is
   begin
      null;
   end TT1;

   task type TT2 is                              -- Movable to block
      entry E;                                   -- Not movable
   end TT2;
   task body TT2 is
   begin
      null;
   end TT2;

   T2  : TT1;                                    -- Not movable

   type Param_T is range 1 .. 10;                -- Not movable
   procedure Proc (X : Param_T) is               -- Movable to Include_Gen, but not Gen_P
   begin
      null;
   end Proc;

   Count : Natural := 0;
   procedure Include_Gen is                      -- Not used
      generic
         with procedure P (X : Param_T) is Proc; -- This usage does not bring Proc in formals scope
      procedure Gen_P;                           -- Not used
      procedure Gen_P is
      begin
         Count := Count + 1;                     -- Used in generic => not movable
      end Gen_P;
      procedure Inst is new Gen_P;
   begin
      Inst;
   end Include_Gen;
begin

   For_Loop : for I in 1 .. 10 loop              -- Not movable (For_Loop, I)
      null;
   end loop For_loop;
   <<L>> null;                                   -- Not movable
   declare
      T3 : TT2;                                  -- Not movable
   begin
      T1.E;
      T2.E;
      T3.E;
      goto L;
   end;
   declare
      I, J, K, L : Integer;                      -- I movable to Inner1, J, L movable to P, K not movable
      generic procedure Pg;                      -- Movable to Inner1, but not block because generics are no_blocks
      procedure Pg is begin null; end;

      procedure P is                             -- Not used
         procedure Inner1 is                     -- Not used
         begin
            I := 1;
            L := 1;
            declare
               procedure Inst is new Pg;
            begin
               Inst;
            end;
         end Inner1;
         procedure Inner2 is                     -- Not movable
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
      I, J, K, L : Integer;                      -- I,L  movable to Inner1, J movable to P, K not movable

      procedure P is                             -- Not used
         procedure Inner1 is                     -- Not used
            procedure Inner11 is                 -- Not used
            begin
               L := 1;
            end;
         begin
            I := 1;
            L := 1;
         end Inner1;

         procedure Inner2 is                     -- Not movable
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

   declare                                       -- case of box-defaulted parameters, Mantis 0000038
      package Pack is
         procedure P;                            -- OK
      end Pack;
      use Pack;                                  -- OK

      generic
         with procedure P is <>;
      procedure Gen;
      procedure Gen is
      begin
         null;
      end;

      package body Pack is
         procedure P is
         begin
            null;
         end P;
      end Pack;

      procedure Inst is new Gen;
   begin
      Inst;
   end;

--     declare
--        type Enum is (A, B, C);                    -- Not movable
--        I : Integer;
--     begin
--        I := A'Image'Length;
--        begin
--           I := Enum'Pos (A);
--        end;
--     end;

   X_Reduceable_Scope.Needs_Body;
end T_Reduceable_Scope;
