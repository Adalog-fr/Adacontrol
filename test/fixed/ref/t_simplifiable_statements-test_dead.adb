with Ada.Exceptions; use Ada.Exceptions;
separate (T_Simplifiable_Statements)
procedure Test_Dead is
   I : Integer;
   C : constant Integer := 1;
   Lim : Integer := C;
   subtype ST1 is Integer range 5 .. 4;
   subtype ST2 is Integer range 2 * Lim .. 5 * Lim; -- 2..5

   S : String (1 .. 10);
   type S_Ptr is access String;
   V : S_Ptr;
   W : String renames V.all;
begin
   if I = 1 then
      null;
   
   else
      I := 1;
      Raise_Exception (Constraint_Error'Identity); -- Unreachable
      
   end if;
   
   if I = 1 then
      I := 2;
   end if;

   case I is
      
      
      
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
      
   end loop;
   

   
   for X in 1 .. C loop
      I := 3;
   end loop;
   for X in 1 .. C + 1 loop
      I := 3;
   end loop;

   <<L1>> goto L1;                                 -- OK (next statement is labelled)
   <<L2>> goto L2;                                 -- Unreachable
   
   <<L3>>                                          -- no more unreachable from here
   return;
end Test_Dead;
