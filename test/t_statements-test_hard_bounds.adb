separate (T_Statements)
procedure Test_Hard_Bounds is
--## rule off Loop_Stmts Sequential_Stmts  ## Don't pollute this test with other messages

   type Tab_T is array (1 .. 10) of Integer;
   subtype Int is Integer range 1 .. 10;
   Tab1 : array (1 .. 10) of Integer;
   Tab2 : array (Int) of Integer;
   Tab3 : Tab_T;
begin
   for V in 1 .. 10 loop           -- Hard_Bounds_Array_For_Loop
      Tab1 (V) := 1;
      Tab2 (V) := 1;
   end loop;
   for V in 1 .. 10 loop           -- Hard_Bounds_Array_For_Loop
      Tab1 (V + 2) := 1;
   end loop;
   for V in Int loop               -- Hard_Bounds_Array_For_Loop
      Tab1 (V) := 1;
   end loop;
   for V in Int range 1 .. 10 loop -- Hard_Bounds_Array_For_Loop
      Tab1 (V) := 1;
   end loop;


   -- OK
   for V in Tab1'First .. Tab1'Last loop
      Tab1 (V) := 1;
   end loop;

   for V in Tab1'First + 1 .. Tab1'Last - 1 loop
      Tab1 (V) := 1;
   end loop;

   for V in Tab_T'Range (1) loop
      Tab3 (V) := 1;
   end loop;
end Test_Hard_Bounds;
