separate (T_Known_Exceptions)
procedure Test_Raise_Expression is
   I : Integer;
   B : Boolean;
begin
   -- OK cases
   I := (case I is
            when 1 => raise Constraint_Error with "message",
            when 2 => raise Constraint_Error,
            when others => 1);
   B := (if I = 1 then True else raise Program_Error);
   B := I = 1 or else raise Program_Error;
   B := I = 1 or else (raise Program_Error);

   declare
      procedure P (X : Integer) with Pre => X /= 0 or else raise Constraint_Error is
      begin
         null;
      end;
   begin
      null;
   end;

   -- Error cases
   if I = 0 or else raise Constraint_Error then
      I := 1;
   end if;

   if I = 0 and then raise Constraint_Error then
      I := 1;
   end if;

   if raise Constraint_Error and then I = 0 then
      I := 1;
   end if;

   if (((raise Constraint_Error))) and then I = 0 then
      I := 1;
   end if;

   B := raise Constraint_Error;

   declare
      procedure P (X : Integer) with Pre => X /= 0 or else raise Constraint_Error is
      begin
         null;
      end;
   begin
      null;
   end;
end Test_Raise_Expression;
