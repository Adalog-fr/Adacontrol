pragma Ada_2012;
separate (T_Simplifiable_Statements)
function Test_If return Integer is
   I, J : Integer;
begin
   if I = 0 then
      null;
   else                    -- empty else path
      null;
   end if;

   if I = 0 then
      null;
   elsif I = 1 then
     null;
   else                    -- empty else path
      null;
   end if;

   if I < 0 then
      B1: begin
         I := 0;
         return 1;
      end B1;
   else                    -- Movable else
      I := 1;
   end if;

   if I < 0 then
      I := 0;
      return 1;
   elsif I = J then
      I := 3;
   else
      I := 1;
   end if;

   if I < 0 then
      I := 1;              -- Movable then
   else
      B2: begin
         B3: begin
            I := 0;
            raise Constraint_Error;
         end B3;
      end B2;
   end if;

   if I = 0 then   -- OK, same breaking statement
      raise Constraint_Error;
   else
      raise Storage_Error;
   end if;

   if I = 0 then   -- OK, same breaking statement
      return 1;
   else
      return X : Integer := 2
      do
         X := X + 1;
      end return;
   end if;
end Test_If;
