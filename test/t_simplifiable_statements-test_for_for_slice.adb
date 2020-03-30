separate (T_Simplifiable_Statements)
procedure Test_For_For_Slice is
   type T is array (Positive range <>) of Integer;
   V1, V2 : T (1 .. 10);
   Offset : constant Integer := 5;
begin

   -- Initialization case : V1 := (others => 0);
   for I in V1'Range loop          -- Replaceable, for_in_for_for_of
      V1 (I) := 0;
   end loop;

   -- Assignment case: V1 := V2
   for I in V1'Range loop          -- Replaceable
      V1 (I) := V2 (I);
   end loop;

   -- Sliding case: S1 (S1'First .. S1'Last - Offset + 1) := S1 (S1'First + Offset -1 .. S1'Last)
   for I in V1'First .. V1'Last - Offset + 1 loop   -- Replaceable
      V1 (I) := V1 (I + Offset - 1);
   end loop;

   -- Not replaceable:
   for I in V1'Range loop
      V1 (I) := V1 (I + I);        -- More than one reference to index
   end loop;
   for I in V1'Range loop
      V1 (I) := V1 (1 - I);        -- Index on RHS of "-"
   end loop;
   for I in V1'Range loop
      V1 (I) := V1 (I * 2);        -- Not "+" or "-"
   end loop;
   for I in V1'Range loop
      V1 (I) := I;                 -- use of Index not for indexing
   end loop;
   for I in V1'Range loop
      V1 (I) := V2 (I) + 1;        -- RHS is not simple indexing or value
   end loop;
   for I in V1'Range loop
      null;                        -- No assignment in loop
   end loop;

end Test_For_For_Slice;
