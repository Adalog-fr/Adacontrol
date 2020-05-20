with Ada.Exceptions; use Ada.Exceptions;
separate (T_Simplifiable_Statements)
procedure Test_Dead is
   I    : Integer;
   RenI : Integer renames I;
   J    : Integer;
   RenJ : Integer renames J;
   C    : constant Integer := 1;
   Lim  : Integer := C;

   subtype ST1 is Integer range 5 .. 4;
   subtype ST2 is Integer range 2 * Lim .. 5 * Lim; -- 2..5

   S : String (1 .. 10);
   type S_Ptr is access String;
   V : S_Ptr;
   W : String renames V.all;
begin
   if I = 1 then
      null;
   elsif C /= 1 then                               -- always false
      null;
   else
      I := 1;
      Raise_Exception (Constraint_Error'Identity); -- Unreachable
      I := 2;
   end if;
   if C /= 1 then                                  -- always false
      I := 0;
      goto L1;
   elsif I = 1 then
      I := 2;
   end if;

   case I is
      when 1 .. 0 =>                               -- choices cover no value
         null;
      when C + 5 .. C + 3 =>                       -- choices cover no value
         I := 1;
      when ST1 =>                                  -- choices cover no value
         I := 2;
      when 8 .. 7 | 2 =>
         I := 3;
      when others =>
         I := 0;
   end case;

   case ST2 (I) is
      when 0 .. 1 =>                               -- choices cover no value
         null;
      when 2 | 3.. 5 =>                            -- OK (bounds deducted from subtype conversion)
         I := 1;
      when others =>
         null;
   end case;

   if S'Length = 0 then                            -- always false
      I := 1;
   end if;

   if V'Length = 0 then                            -- OK (dynamic, implicit dereference)
      I := 1;
   end if;

   if V.all'Length = 0 then                        -- OK (dynamic, explicit dereference)
      I := 1;
   end if;

   if W'Length = 0 then                            -- OK (dynamic, renaming of dereference)
      I := 1;
   end if;

   while I = 1 loop
      exit when I /= 1;
      exit when C = 1;                             -- unreachable
      I := 3;
      exit;                                        -- unreachable
      I := 5;
      exit;
   end loop;
   while (C + 3) / 4 /= 1 loop                     -- never executed
      I := I + 1;
   end loop;

   for X in 1 .. C - 1 loop                        -- never executed
      I := 3;
   end loop;
   for X in 1 .. C loop
      I := 3;
   end loop;
   for X in 1 .. C + 1 loop
      I := 3;
   end loop;

   <<L1>> goto L1;                                 -- OK (next statement is labelled)
   <<L2>> goto L2;                                 -- Unreachable
   return;                                         -- Unreachable
   I := 0;
   <<L3>>                                          -- no more unreachable from here

   -- Check equivalent branches
   if I = 1 then                                  -- (1)
      null;
   elsif I = C then                               -- Equivalent to (1)
      null;
   elsif I + J - 2 = 0 then                       -- (2)
      null;
   elsif I + J - (C + 1) = 0 then                 -- Equivalent to (2)
      null;
   elsif RenI + RenJ - 2 = 0 then                 -- Equivalent to (2)
      null;
   end if;

   if I + 2 > J + 3 then                          -- (3)
      null;
   elsif "+" (I, 2) > J + 3 then                  --  Equivalent to (3)
      null;
   elsif "+" (Right => 2, Left => I) > J + 3 then --  Equivalent to (3)
      null;
   elsif "+" (Right => I, Left => 2) > J + 3 then
      null;
   end if;

   if Integer'Pos (I) > Integer'Pos (1) then      --  (4)
      null;
   elsif Positive'Pos (I) > Integer'Pos (J) then
      null;
   elsif I'Image /= J'Image then                  --  (5)
      null;
   elsif Integer'Pos (I) > 1 then                 --  Equivalent to (4)
      null;
   elsif I'Image /= J'Image then                  --  Equivalent to (5)
      null;
   end if;

end Test_Dead;
