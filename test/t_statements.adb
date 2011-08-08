with Ada.Calendar; use Ada.Calendar;
procedure T_statements is
   task T is
      entry E;
   end T;
   task body T is
   begin
      accept E do
         requeue E;
      end E;
   end T;

   I : Integer;

begin
   delay 1.0;
   delay until Clock;
   goto Next;
   <<Next>> null;
   abort T;

   loop
      exit;  -- No name required here
   end loop;
B:
   loop
      exit;
      exit B;
   end loop B;

   select
      T.E;
   then abort
      null;
   end select;

   case I is
      when 1 =>
         null;
      when others =>
         null;
   end case;

   raise Constraint_Error;
end T_statements;
