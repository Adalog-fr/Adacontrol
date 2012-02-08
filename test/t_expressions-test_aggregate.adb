separate (T_Expressions)
procedure Test_Aggregate is
   type Arr is array (Integer'(1) .. 4) of Integer;
   type Rec is tagged
      record
         X, Y : Float;
      end record;
   type Der is new Rec with
      record
         Z : Float;
      end record;

   A : Arr;
   R : Rec;
   D : Der;
   One : Integer := 1;
begin
   A := Arr'(1 => 1, 2 .. 3 => 3, 4 => 4);    -- Array_Aggregate, Array_Range, Universal_Range
   A :=     (1 => 1, 2 => 2, 3 => 3, 4 => 4); -- Array_Aggregate, Unqualified
   A := Arr'(1, 2, 3, 4);                     -- Array_Aggregate
   A := Arr'(One..4 => 0);                    -- Array_Aggregate, Array_Range
   A := Arr'(1 | 3..4 | 2 => 0);              -- Array_Aggregate, Array_Range, Universal_Range
   A :=     (1, 2, 3, 4);                     -- Array_Aggregate, Unqualified
   A := Arr'(others => 0);                    -- Array_Aggregate, Array_Others
   A :=     (others => 0);                    -- Array_Aggregate, Unqualified, array_others
   A := Arr'(1, others => 0);                 -- Array_Aggregate, Array_Others, Array_Partial_Others
   A :=     (1 => 1, others => 0);            -- Array_Aggregate, Unqualified, Array_Others, Array_Partial_Others

   R := Rec'(X => 1.0, Y => 0.0);             -- Record_Aggregate
   R :=     (X => 1.0, Y => 0.0);             -- Record_Aggregate, Unqualified
   R := Rec'(0.0, 2.0);                       -- Record_Aggregate
   R :=     (0.0, 2.0);                       -- Record_Aggregate, Unqualified
   R :=     (others => 0.0);                  -- Record_Aggregate, Unqualified, Record_Others
   R :=     (0.0, others => 0.0);             -- Record_Aggregate, Unqualified, Record_Others, record_partial_others

   D := Der'(0.0, 1.0, 2.0);                  -- Record_Aggregate
   D :=     (0.0, 1.0, 2.0);                  -- Record_Aggregate, Unqualified
   D := Der'(R with 2.0);                     -- Record_Aggregate
   D :=     (R with 2.0);                     -- Record_Aggregate, Unqualified
end Test_Aggregate;
