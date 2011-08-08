with Ada.Exceptions; use Ada.Exceptions;
separate (T_Simplifiable_Statements)
procedure Test_Dead is
   I : Integer;
   C : constant Integer := 1;
begin
   if I = 1 then
      null;
   elsif C /= 1 then  -- always false
      null;
   else
      I := 1;
      Raise_Exception (Constraint_Error'Identity); -- Unreachable
      I := 2;
   end if;
   if C /= 1 then  -- always false
      I := 0;
      goto L;
   elsif I = 1 then
      I := 2;
   end if;

   while I = 1 loop
      I := 0;
      exit when I /= 1;
      exit when C = 1;  -- unreachable
      I := 3;
      exit;              -- unreachable
      I := 5;
      exit;
   end loop;
   while (C+3)/4 /= 1 loop    -- never executed
      I := I+1;
   end loop;

   for X in 1 .. C - 1 loop   -- never executed
      I := 3;
   end loop;
   for X in 1 .. C loop
      I := 3;
   end loop;
   for X in 1 .. C + 1 loop
      I := 3;
   end loop;

<<L>> goto L;            -- Unreachable
      return;            -- Unreachable
      I := 0;

      return;
end Test_Dead;
