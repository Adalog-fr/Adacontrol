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
   S := ('a', others => ' ');
   S := (1=> 'a', others => ' ');
   S := ('a', 'b', 'c', 'd');

   R := (1, 2, others => 0);
   R := (I => 1, others => 0);
   R := (1, 2, 3);

   E := (R with 1, others => 0);
   E := (R with A => 1, others => 0);
   E := (R with 1, 0);

   N := (null record);
end Test_Others;
