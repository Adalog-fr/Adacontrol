procedure T_Max_Expression_Items is
   V   : Integer;
   B   : Boolean;
   Tab : array (1 .. 10) of Integer;

   function F (X : Integer) return Integer is
   begin
      return X + 1;                                               -- OK
   end F;
begin
   -- Simple cases
   V := 1;                                                        -- OK
   V := V + T_Max_Expression_Items.V + 2;                         -- 3: Count
   V := V + (2 * (V + 1));                                        -- 4: Search, Count
   V := V + (2 * (V + 1)) + Integer'First;                        -- 5: Check,  Count

   V := V + F (2 * V + 1) + 2;                                    -- 3,3: Count x2
   V := V + F (2 * V + 1 + F (3)) + 2 + F (4);                    -- 4,4 Search x2, Count x2


   -- if and case expressions
   V := (if V < 0 then
            1
         elsif V = 0 then
            V + 2 * (V + 1) + 2                                   -- 5: Check, Count
         else
            V + 2 * V + 1);                                       -- 4: Search, Count

   V := (Case (V + 6) * (V-1) is                                  -- 4: Check, Count (others branch determines complexity)
            when -1     => 1,
            when  0     => V + 2 * V + 1,                         -- 4: count
            when others => V + 2 * (V + 1) + 2);                  -- 5: Check, Count

   -- Aggregates
   Tab := (1, V + 1, Tab (1 + 2 + 3 + 4), others => <>);          -- 4: Search, Count (for indexing expression), aggregate OK
   Tab := (V + 1 + Tab (3) + 4,                                   -- 4: Search, Count
           others => (if V = 1 then 0 else 2 * Integer'(V) + 1)); -- 4: Search, Count

   -- Logical and Short circuits
   B := V > 0 and then (V = 1 or V = 2 or (V = 5 or else V = 6)); -- 10: Check, Count
end T_Max_Expression_Items;
