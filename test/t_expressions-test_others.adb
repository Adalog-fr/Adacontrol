separate (T_Expressions)
procedure Test_Others is
   S : String (1 .. Natural'(4));
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
   S := ('a', others => ' ');         -- array_aggregate, array_others, array_positional_others, array_partial_others, unqualified
   S := (1=> 'a', others => ' ');     -- array_aggregate, array_others, array_named_others, array_partial_others, unqualified
   S := ('a', 'b', 'c', 'd');         -- array_aggregate, Unqualified

   R := (1, 2, others => 0);          -- record_aggregate, record_others, record_partial_others, unqualified
   R := (I => 1, others => 0);        -- record_aggregate, record_others, record_partial_others, unqualified
   R := (1, 2, 3);                    -- record_aggregate, unqualified

   E := (R with 1, others => 0);      -- record_aggregate, extension_aggregate, record_others, record_partial_others, unqualified
   E := (R with A => 1, others => 0); -- record_aggregate, Unqualified, record_others, record_partial_others, extension_aggregate
   E := (R with 1, 0);                -- record_aggregate, extension_aggregate, unqualified

   N := (null record);                -- record_aggregate, unqualified
end Test_Others;
