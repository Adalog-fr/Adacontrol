with Ada.Exceptions; use Ada.Exceptions;
separate (T_Simplifiable_Statements)
procedure Test_Dead is
   I : Integer;
   C : constant Integer := 1;
   subtype ST is Integer range 5 .. 4;

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
      I := 0;
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
