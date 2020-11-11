separate ( T_Unsafe_Paired_Calls)
procedure Simple is
   -- Check procedure body
   procedure P1 is   -- OK
   begin
      P;
      null;
      V;
      return;
   exception
      when others =>
         V;
   end P1;

   procedure P2 is
   begin
      P;
      null;
      V;
   end P2;   -- Error: no handler

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
         null;
      exception
         when others =>
            V;
      end E;
      accept E do
         P;
         null;
         V;
      end E;    -- Error: no handler
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
   loop
      begin             -- OK
         P;
         null;
         V;
      exception
         when Constraint_Error =>
            V;
            exit;
         when others =>
            V;
      end;
   end loop;

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

   begin
      P;
      null;          -- Error: no closing call
   end;              -- Error: no handler

   begin
      null;
      V;             -- Error: no matching opening call
   end;

   begin
      null;
   exception
      when others =>
         V;             -- Error: no matching opening call
   end;

   begin
      P;
      null;
      Not_A_Lock;
   exception         -- Error: no matching closing call
      when others => -- Error: no matching closing call
        Not_A_Lock;
   end;

   begin
      null;
      P;             -- Error: not first
      V;             -- Error: not last
      Case True is
         when True =>
            P;       -- Error: not valid nested if
         when False =>
            V;       -- Error: not valid nested if
      end case;
      null;
   end;

   begin
      P;
      null;
      V;
   end;              -- Error: no exception handler

   begin
      P;
      null;
      V;
   exception                 -- Error: no when others
      when Constraint_Error =>
         V;
      when Storage_Error =>  -- Error: no matching closing call
         P;                  -- not handled sequence of statements
   end;

   begin
      P;
      null;
      V;
   exception
      when others =>  -- Error: no matching closing call
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
      P (A1);
      null;
      V (A2);        -- Error: wrong values
   exception         -- Error: missing closing call
      when others => -- Error: missing closing call
         V (A2);     -- Error: wrong values
   end;

   begin
      P ('a', 1);
      null;
      V;             -- Error: not matching
   exception         -- Error: no closing call
      when others => -- Error: no closing call
         V;          -- Error: not matching
   end;

   begin
      P ('a', 1);
      null;
      V ('B', 2);    -- Error: not matching
   exception         -- Error: no closing call
      when others => -- Error: no closing call
         V ('C', 3); -- Error: not matching
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
         P ('a', 1); -- Error (nested, same values)
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
      P (S1);
      null;
      V (S2);        -- Error: not same parameter
   exception         -- Error: no closing call
      when others => -- Error: no closing call
         V (S2);     -- Error: not same parameter
   end;

   begin
      P (R1.F);
      null;
      V (R2.F);        -- Error: not same parameter
   exception           -- Error: no closing call
      when others =>   -- Error: no closing call
         V (R2.F);     -- Error: not same parameter
   end;

   begin
      P (S1);
      null;
      V (S1);
   exception
      when others =>   -- Error: no closing call
         V (S2);       -- Error: wrong values
   end;

   begin             -- OK (nested, but different values)
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
         P (S1);     -- Error (nested, same values)
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
      MultP2;
      MultV1;           -- Error: not matching
   exception            -- Error: no closing call
      when others =>
         MultV2;
   end;
end Simple;
