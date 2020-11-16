separate (T_Known_Exceptions)
procedure Test_Raise_Expression is
   I : Integer;
   B : Boolean;

   function F1 return Boolean is (raise Constraint_Error);           -- OK
   function F2 return Boolean is (True and then raise Constraint_Error);  -- Known_Exception

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
      procedure P (X : Integer) with Pre => X /= 0 or else raise Constraint_Error is -- OK
      begin
         null;
      end;
   begin
      null;
   end;

   -- Error cases
   if I = 0 or else raise Constraint_Error then                                      -- Known_Exception
      I := 1;
   end if;

   if I = 0 and then raise Constraint_Error then                                     -- Known_Exception
      I := 1;
   end if;

   if raise Constraint_Error and then I = 0 then                                -- Known_Exception
      I := 1;
   end if;

   if (((raise Constraint_Error))) and then I = 0 then                          -- Known_Exception
      I := 1;
   end if;

   B := raise Constraint_Error;                                                 -- Known_Exception

   declare
      procedure P (X : Integer) with Pre => X /= 0 or else raise Constraint_Error is -- Known_Exception
      begin
         null;
      end;
   begin
      null;
   end;
end Test_Raise_Expression;
