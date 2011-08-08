separate (T_Expressions)
procedure Test_Others is
   S : String (1 .. 4);
   type Nothing is null record;
   type Rec is tagged
      record
         I, J, K : Integer;
      end record;
   type Ext is new Rec with
      record
         A, B : Integer;
      end record;
   R : Rec;
   E : Ext;
   N : Nothing;
begin
   S := ('a', others => ' ');         -- Unqualified, array_others
   S := (1=> 'a', others => ' ');     -- Unqualified, array_others
   S := ('a', 'b', 'c', 'd');         -- Unqualified

   R := (1, 2, others => 0);          -- Unqualified, record_others
   R := (I => 1, others => 0);        -- Unqualified, record_others
   R := (1, 2, 3);                    -- Unqualified

   E := (R with 1, others => 0);      -- Unqualified, record_others
   E := (R with A => 1, others => 0); -- Unqualified, record_others
   E := (R with 1, 0);                -- Unqualified

   N := (null record);                -- Unqualified
end Test_Others;
