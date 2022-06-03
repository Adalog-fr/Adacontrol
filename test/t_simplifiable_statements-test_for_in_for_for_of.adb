separate (T_Simplifiable_Statements)
procedure Test_For_In_For_For_Of is
   S1 : String (1 .. 10);
   S2 : String (1 .. 10);
   X  : Character;
   type Acc_Str is access String;
   Ptr : Acc_Str;
   Mat : array (1 .. 5, 1 .. 5) of Boolean;
begin
   L1 : for I in S1'Range loop         -- for_in_for_for_of x2
      X := S1 (I);
      X := S1 (I);
      X := S1 (L1.I);
      declare
         Inx : Integer renames I;
      begin
         X := S1 (Inx);
      end;
   end loop L1;

   for I in S1'Range loop              -- For_in_for_for_of x1, Not enough indexings
      X := S1(I);
   end loop;

   for I in 1 .. 10 loop                -- OK Index used in expression
      X := S1 (I + 1);
   end loop;

   for I in Ptr.all'Range loop         -- OK Indexing of dereference
      X := Ptr.all (I);
   end loop;

   for I in Integer range 1 .. 10 loop -- OK Several variables
      X := S1 (I);
      X := S2 (I);
   end loop;

   for I in S1'Range loop              -- OK Renaming, used with different variable
      X := S1 (I);
      declare
         Inx : Integer renames I;
      begin
         X := S2 (Inx);
      end;
   end loop;

   for I in 1 .. 10 loop               -- OK No indexing
      null;
   end loop;


   declare                             -- OK Same component of different variables
      type Rec is
         record
            Tab : String (1 .. 10);
         end record;
      V1, V2 : Rec;
   begin
      for I in V1.Tab'Range loop       -- for_for_slice x2
         V1.Tab (I) := V2.Tab (I);
      end loop;
   end;

   declare
      subtype String4 is String (1..4);
      function F return String4 is ("ABCD");
      C :  constant String4 := "ABCD";
   begin
      for I in 1 .. 4 loop             -- OK Indexing of function call
         X := F (I);
         X := F (I);
      end loop;
      for I in 1 .. 4 loop             -- for_in_for_for_of x2
         X := C (I);
         X := C (I);
      end loop;
   end;

   declare                             -- OK Case of arrays that depend on discriminants (5.5.2(6.1/4))
      subtype Inx is Natural range 0 .. 10;
      type Rec1 (L : Inx) is           -- No default value: OK
         record
            S : String (1 .. L);
         end record;
      type Rec2 (L : Inx := 0) is      -- Default value: assume mutable
         record
            S : String (1 .. L);
         end record;
      type Rec3 (B : Boolean := False) is
         record
            case B is
               when True =>
                  null;
               when False =>
                  S : String (Inx);
            end case;
         end record;

      V1 : Rec1 (5);
      V2 : Rec2;
      V3 : Rec3;
   begin
      for I in V1.S'Range loop         -- for_in_for_for_of x1, not full range
         X := V1.S (I);
         X := V1.S (I);
      end loop;
      for I in V2.S'Range loop -- OK mutable
         X := V2.S (I);
      end loop;
      for I in Inx loop  -- OK part of variant record
         X := V3.S (I);
      end loop;
   end;

   declare
      V4 : array (1 .. 10) of String (1 .. 10);
   begin
      for Str of V4 loop
         for I in Str'Range loop       -- for_in_for_for_of x1
            Str (I) := ' ';
         end loop;
      end loop;
   end;

   for I in Mat'Range (1) loop         -- OK multidimensional array
      for J in Mat'Range (2) loop
         Mat (I, J) := False;
      end loop;
   end loop;

   for I in 2 .. 10 loop               -- for_in_for_for_of x1, not full_range
      X := S1 (I);
      X := S1 (I);
   end loop;

   for I in S1'First .. S1'Last loop   -- for_in_for_for_of x2
      X := S1 (I);
      X := S1 (I);
   end loop;

   for I in S1'First .. S1'Last - 1 loop -- for_in_for_for_of x1, not full_range
      X := S1 (I);
      X := S1 (I);
   end loop;

   for I in S1'First .. S1'Last - 1 + 1 loop -- for_in_for_for_of x2
      X := S1 (I);
      X := S1 (I);
   end loop;

   declare
      S : String (1 .. 10);
   begin
      for I in 1 .. 10 loop  -- for_in_for_for_of x1
         S (I) := ' ';
      end loop;
   end;
   for I in 1 .. 10 loop  -- OK, variable inner to loop
      declare
         S : String (1 .. 10);
      begin
         S (I) := ' ';
      end;
   end loop;
end Test_For_In_For_For_Of;
