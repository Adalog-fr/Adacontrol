separate (T_Expressions)
procedure Test_Aggregate is
   type Arr is array (1 .. 4) of Integer;
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
begin
   A := Arr'(1 => 1, 2 => 2, 3 => 3, 4 => 4); -- Array_Aggregate
   A :=     (1 => 1, 2 => 2, 3 => 3, 4 => 4); -- Array_Aggregate, Unqualified
   A := Arr'(1, 2, 3, 4);                     -- Array_Aggregate
   A :=     (1, 2, 3, 4);                     -- Array_Aggregate, Unqualified
   A := Arr'(others => 0);                    -- Array_Aggregate, Array_Others
   A :=     (others => 0);                    -- Array_Aggregate, Unqualified, array_others

   R := Rec'(X => 1.0, Y => 0.0);             -- Record_Aggregate
   R :=     (X => 1.0, Y => 0.0);             -- Record_Aggregate, Unqualified
   R := Rec'(0.0, 2.0);                       -- Record_Aggregate
   R :=     (0.0, 2.0);                       -- Record_Aggregate, Unqualified

   D := Der'(0.0, 1.0, 2.0);                  -- Record_Aggregate
   D :=     (0.0, 1.0, 2.0);                  -- Record_Aggregate, Unqualified
   D := Der'(R with 2.0);                     -- Record_Aggregate
   D :=     (R with 2.0);                     -- Record_Aggregate, Unqualified
end Test_Aggregate;
