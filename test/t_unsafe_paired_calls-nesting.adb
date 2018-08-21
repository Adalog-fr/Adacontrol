separate ( T_Unsafe_Paired_Calls)
procedure Nesting is
   Cond1      : constant Boolean := True;
   Cond2      : constant Boolean := False;
   Cond3      :          Boolean := True;

   Must_Check : constant Boolean := Cond1 or Cond2;

   S1, S2     : Sema;
begin

   -- Simple if, OK
   begin
      if Cond1 then
         P;
      end if;
      null;
      if Cond1 then
         V;
      end if;
   exception
      when others =>
         if Cond1 then
            V;
         end if;
   end;

   -- Simple if, not same constant
   begin
      if Cond1 then
         P;
      elsif Cond2 then
         P;            -- Error: not valid nested if
      end if;
      null;
      if Cond2 then
         V;            -- Error: not matching
      end if;
   exception           -- Error: no matching closing call
      when others =>   -- Error: no matching closing call
         V;            -- Error: not matching
   end;

   -- Simple if, not constant
   begin
      if Cond3 then
         P;            -- Error: invalid nesting
      end if;
      null;
      if Cond3 then
         V;            -- Error: invalid nesting
      end if;
   exception
      when others =>
         V;            -- Error: not matching
   end;

   -- Simple if, several matches in handler
   begin
      if Cond1 then
         P;
      end if;
      null;
      if Cond1 then
         V;
         Cond3 := True;    -- Error: statements after closing block
      end if;
   exception
      when others =>
         if Cond1 then
            V;
            Cond3 := True; -- OK: statements after closing block in exception handler
         end if;
   end;

   -- Statements after closing block
   begin
      if Cond1 then
         P;
      end if;
      null;
      if Cond1 then
         V;
      end if;
   exception
      when others =>   -- several matches
         if Cond1 then
            V;
         end if;
         if Cond1 then
            V;
         end if;
   end;

   -- Complicated nesting, OK
   declare
      procedure Proc is
      begin
         if Must_Check then
            if Cond1 then
               P (S1);
            else
               P ('a', 1);
               null;             -- OK, extra uneffective statement
            end if;
            null;                -- OK, extra uneffective statement
         end if;

         null;

         if Must_Check then
            if Cond1 then
               V (S1);
               null;             -- OK, extra uneffective statement
               null;             -- OK, extra uneffective statement
            else
               V ('a', 1);
            end if;
         end if;
      exception
         when others =>
            if Must_Check then
               if Cond1 then
                  V (S1);
               else
                  V ('a', 1);
               end if;
            end if;
      end Proc;

   begin
      Proc;
   end;

   -- Complicated nesting, not OK
   declare
      procedure Proc is
      begin
         if Must_Check then
            if Cond1 then
               P (S1);
            else
               P ('a', 1);
            end if;
         end if;

         null;

         if Must_Check then
            if Cond2 then
               V (S1);              -- Error: not matching
            else
               V ('a', 1);          -- Error: not matching
            end if;
         end if;
      exception                     -- Error: no matching closing call x2 on S1 and 1
         when Storage_Error =>      -- Error: Several matches x2 on S1 and 1
            null;
            if Must_Check then
               if Cond1 then
                  V (S1);
               else
                  V ('a', 1);
               end if;
            end if;
            null;
            if Must_Check then
               if Cond1 then
                  V (S1);
               else
                  V ('a', 1);
               end if;
            end if;
            null;

         when Tasking_Error =>      -- Error: several matches on S1 only
            if Must_Check then
               if Cond1 then
                  V (S1);
               else
                  V ('a', 1);
               end if;
            end if;
            if Must_Check then
               if Cond1 then
                  V (S1);
               else
                  V ('a', 2);       -- Error: not matching
               end if;
            end if;
            if Must_Check then
               if Cond2 then
                  V (S1);           -- Error: not matching
               else
                  V ('a', 1);       -- Error: not matching
               end if;
            end if;

         when Constraint_Error =>   -- Error: no matching closing call x2 on S1 and 1
            if Must_Check then
               if Cond2 then
                  V (S2);           -- Error: not matching
               else
                  V ('a', 2);       -- Error: not matching
               end if;
            end if;

         when others =>             -- Error: no matching closing call x2 on S1 and 1
            if Cond1 then
               V (S1);              -- Error: not matching
            else
               V ('a', 1);          -- Error: not matching
            end if;
      end Proc;

   begin
      Proc;
   end;
end Nesting;
