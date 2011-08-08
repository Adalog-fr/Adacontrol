procedure T_Unsafe_Paired_Calls is
   procedure Not_A_Lock is begin null; end;

   procedure P is begin null; end;
   procedure V is begin null; end;

   procedure PP renames P;
   procedure VV renames V;

   procedure P (C : Character; X : Integer) is begin null; end;
   procedure V (C : Character; X : Integer := 1) is begin null; end;

   type Acc is access Integer;
   procedure P (S : in Acc) is begin null; end;
   procedure V (S : in Acc) is begin null; end;

   type Sema is null record;
   procedure P (S : in out Sema) is begin null; end;
   procedure V (S : in out Sema) is begin null; end;

   procedure MultP1 is begin null; end;
   procedure MultV1 is begin null; end;
   procedure MultP2 is begin null; end;
   procedure MultV2 is begin null; end;

   -- Check procedure body
   procedure P1 is   -- OK
   begin
      P;
      null;
      V;
   exception
      when others =>
         V;
   end P1;

   procedure P2 is   -- Error: no handler
   begin
      P;
      null;
      V;
   end P2;

   -- Check accept
   task T is
      entry E;
   end T;
   task body T is
   begin
      accept E do    -- OK
         P;
         null;
         V;
      exception
         when others =>
            V;
      end E;
      accept E do    -- Error: no handler
         P;
         null;
         V;
      end E;
   end T;

   S1, S2 : Sema;
   A1, A2 : constant Acc := new Integer;
   type Rec is
      record
         F : Sema;
      end record;
   R1, R2 : Rec;
begin

   --
   -- Checks without parameters
   --

   -- Check blocks, more complicated cases
   begin             -- OK
      P;
      null;
      V;
   exception
      when Constraint_Error =>
         V;
      when others =>
         V;
   end;

   begin             -- OK (renaming)
      PP;
      null;
      V;
   exception
      when Constraint_Error =>
         V;
      when others =>
         V;
   end;

   begin             -- OK (renaming)
      P;
      null;
      VV;
   exception
      when Constraint_Error =>
         VV;
      when others =>
         VV;
   end;

   begin             -- Error: no handler
      P;             -- Error: not paired
      null;
   end;

   begin
      null;
      V;             -- Error: not paired
   end;

   begin
      P;              -- Error: not paired
      null;
      Not_A_Lock;
   exception
      when others =>  -- Error: not paired
        Not_A_Lock;
   end;

   begin
      null;
      P;             -- Error: not first/last
      V;             -- Error: not first/last
      if True then
         P;          -- Error: not first/last
      else
         V;          -- Error: not first/last
      end if;
      null;
   end;

   begin             -- Error: no exception handler
      P;
      null;
      V;
   end;

   begin
      P;
      null;
      V;
   exception        -- Error: no when others
      when Constraint_Error =>
         V;
   end;

   begin
      P;
      null;
      V;
   exception
      when others => -- Error: no call to V
         null;
   end;

   begin
      P;
      null;
      V;
   exception
      when others => -- Error: several calls to V
         V;
         VV;
   end;

   begin
      P;
      begin
         P;          -- Error: nested calls
         V;
      exception
         when others =>
            V;
      end;
      V;
   exception
      when others =>
         V;
   end;

   --
   -- Checks with "in" parameter
   --
   begin               --OK
      P ('a', 1);
      null;
      V ('B', 1);
   exception
      when others =>
         V ('C');      -- OK, default value
   end;

   begin               --OK
      P (A1);
      null;
      V (A1);
   exception
      when others =>
         V (A1);
   end;

   begin
      P (A1);        -- Error: wrong values
      null;
      V (A2);        -- Error: wrong values
   exception
      when others => -- Error: wrong values
         V (A2);
   end;

   begin
      P ('a', 1);    -- Error: not same procedure
      null;
      V;             -- Error: not same procedure
   exception
      when others => -- Error: not same procedure
         V;
   end;

   begin
      P ('a', 1);    -- Error: wrong values
      null;
      V ('B', 2);    -- Error: wrong values
   exception
      when others => -- Error: wrong values
         V ('C', 3);
   end;

   begin             --OK (nested, but different values)
      P ('a', 1);
      begin
         P ('a', 2);
         V ('B', 2);
      exception
         when others =>
            V ('C', 2);
      end;
      V ('B', 1);
   exception
      when others =>
         V ('C', 1);
   end;

   begin
      P ('a', 1);
      begin
         P ('a', 1); --Error (nested, same values)
         V ('B', 1);
      exception
         when others =>
            V ('C', 1);
      end;
      V ('B', 1);
   exception
      when others =>
         V ('C', 1);
   end;


   --
   -- Checks with "in out" parameter
   --
   begin             --OK
      P (S1);
      null;
      V (S1);
   exception
      when others =>
         V (S1);
   end;

   begin
      P (S1);        -- Error: not same parameter
      null;
      V (S2);        -- Error: not same parameter
   exception
      when others => -- Error: not same parameter
         V (S2);
   end;

   begin
      P (R1.F);        -- Error: not same parameter
      null;
      V (R2.F);        -- Error: not same parameter
   exception
      when others => -- Error: not same parameter
         V (R1.F);
   end;

   begin
      P (S1);
      null;
      V (S1);
   exception
      when others => -- Error: wrong values
         V (S2);
   end;

   begin             --OK (nested, but different values)
      P (S1);
      begin
         P (S2);
         V (S2);
      exception
         when others =>
            V (S2);
      end;
      V (S1);
   exception
      when others =>
         V (S1);
   end;

   begin
      P (S1);
      begin
         P (S1);     --Error (nested, same values)
         V (S1);
      exception
         when others =>
            V (S1);
      end;
      V (S1);
   exception
      when others =>
         V (S1);
   end;

   -- Check multiple matches
   begin                -- OK
      MultP1;
      MultV1;
   exception
      when others => MultV2;
   end;

   begin                -- OK
      MultP2;
      MultV2;
   exception
      when others => MultV2;
   end;

   begin
      MultP2;           -- Error: not paired
      MultV1;           -- Error: not paired
   exception
      when others => MultV2;
   end;
end T_Unsafe_Paired_Calls;
