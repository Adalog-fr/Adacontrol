procedure T_Return_Statements is
   V : Integer;

   function River return Integer is
   begin
      return 1;                   -- OK, count
      return 1;                   -- OK, count
      begin
         return 1;                -- Found: depth, found: too many returns (counted with enclosing proc)
      exception
         when Constraint_Error => -- Error: no return (of River ;-) )
            null;
         when Tasking_Error =>
            return 1;            -- OK, count
            return 2;            -- OK, count
            return 3;            -- Error: too many returns, count
      end;
   exception
      when Constraint_Error =>  -- Error: no return
         null;
      when Program_Error =>
         return 1;              -- OK, count
         return 2;              -- OK, count
         return X : Integer do  -- Error: too many returns, count
            X := 1;
            return;             -- OK (nested inside extended return)
         end return;
   end River;

   task T is
      entry E;
   end T;
   task body T is
   begin
      accept E do
         null;
      exception
         when Program_Error =>
            return;             -- OK, count
            return;             -- OK, count
            return;             -- Error: too many returns, count
            return;             -- Error: too many returns, count
      end E;
   end T;

begin
   return;                      -- OK, count
   return;                      -- OK, count

   if V = 1 then
      return;                   -- Found Depth, Found too many returns
   end if;

   if V = 1 then
      for I in 1..10 loop
         return;                -- Error Depth, Error too many returns
      end loop;
   end if;

exception
   when Constraint_Error =>
      return;                    -- OK, count
   when Tasking_Error =>
      return;                    -- OK, count
      if V = 1 then
         return;                 -- Found Depth
      else
         return;                 -- Found Depth, Error too many returns
      end if;
end T_Return_Statements;
