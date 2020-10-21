separate (T_Known_Exceptions)
procedure Test_Index is
   type Inx is range 1 .. 10;
   I : Inx := 5;
   type Arr1 is array (Inx) of Character;
   VA1 : Arr1;
   C : Character;
   Tab : array (2..10) of Arr1;

   VS1 : String (1 .. 10);
   J : Integer;

   type Enum is (One, Two, Three);
   E : Enum := Three;
   type Arr2 is array (Enum range One .. Two) of Integer;
   VE1 : Arr2;


begin
   VA1 (1) := VA1 (I);
   VA1 (2 * I + 1) := ' ';           -- Index_Error

   VE1 (E) := 0;                     -- Index_Error
   E := One;
   VE1 (One) := VE1 (Enum'Succ (E));
   VE1 (One) := VE1 (Enum'Pred (E)); -- Index_Error

   case J is
      when 1 =>
         VS1 (J) := '1';
      when 10 =>
         VS1 (J) := 'A';
      when 11 =>
         VS1 (J) := 'B';             -- Index_Error
      when others =>
         VS1 (J) := '?';
   end case;

   -- Check bounds from component iterator
   for S of Tab loop
      C := S (1);
      C := S (11);                   -- Index_Error
   end loop;

end Test_Index;
