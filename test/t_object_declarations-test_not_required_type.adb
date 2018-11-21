with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Containers.Vectors;

separate (T_Object_Declarations)
procedure Test_Not_Required_Type is
   type My_Int is new Integer;
   package Int_Vect is new Ada.Containers.Vectors (My_Int, My_Int);
   type TS     is array (My_Int range <>) of Character;
   subtype Cts is Ts (1 .. 5);

   S   : array (My_Int range 1 .. 10) of Character;
   C   : Character;
   Vec : Int_Vect.Vector;
begin

   -- simple uses
   declare
      OK1  : My_Int;        -- OK (used as indexing)
      KO2  : My_Int;        -- NRT_Variable
      OK3  : My_Int;        -- OK (depends on OK1, twice)
      OK4  : My_Int;        -- OK (depends on OK1, later)
      OK5  : My_Int;        -- OK (depends on OK6 in initialization)
      OK6  : My_Int := OK5; -- OK (used as indexing)
      KO7  : My_Int;        -- NRT_Variable (depends on K08)
      K08  : My_Int := KO7; -- NRT_Variable
      OK9  : My_Int;        -- OK (Depends on OK_Formal)
      KO10 : My_Int;        -- NRT_Variable (depends on KO_Formal)
      OK11 : My_Int;        -- OK (used in slice)
      OK12 : My_Int;        -- OK (depends on OK11)

      procedure Proc (OK_Formal : My_Int; -- OK (used as indexing)
                      KO_Formal : My_Int) -- NRT_Variable
      is
      begin
         C := S (OK_Formal);
      end Proc;
   begin
      OK3 := 1;
      OK1 := OK3;
      OK1 := OK3;
      C := S (OK1);
      C := S (OK6);
      KO2 := 1;
      OK1 := My_Int (KO2);   -- Dependency does not traverse conversion
      OK1 := OK4;
      Proc (OK9, KO10);
      if S (1 .. OK11 + 1) = "  " then
         OK11 := OK12;
      end if;
   end;

   -- use from expressions
   declare
      OK1 : My_Int;               -- OK (used as indexing, complex expression)
      OK2 : constant My_Int := 2; -- OK (used as indexing, complex expression)
      OK3 : My_Int;               -- OK (depends on OK1, complex expression)
      KO3 : My_Int;               -- NRT_Variable (does not depend on OK1, because of user defined operator)
      OK4 : My_Int;               -- OK (depends on OK1, complex expression)
      OK5 : Count;                -- OK (depends on non-application function)
      KO5 : Count;                -- NRT_Count (depends on application function)
      OK6 : Count;                -- OK (depends on non-application function)
      OK7 : My_Int;               -- OK (depends on required function parameter)
      function "-" (L : My_Int;   -- NRT_Variable
                    R : My_Int)   -- OK (used in indexing)
                    return My_Int is
      begin
         C := S (R);
         return L;
      end "-";

      function Local_Length return Count is
      begin
         return Line_Length;
      end Local_Length;
   begin
      OK3 := 1;
      OK1 := (OK3 + 1) * OK4;
      OK1 := KO3 - OK7;
      C := S (2 * OK1 + OK2);
      OK5 := Line_Length;
      KO5 := Local_Length;
      Set_Line_Length (OK6 + 3);
   end;

   -- same without indexing
   declare
      KO1 : My_Int;               -- NRT_Variable
      KO2 : constant My_Int := 2; -- NRT_Constant
      KO3 : My_Int;               -- NRT_Variable
   begin
      KO3 := 1;
      KO1 := (KO3 + 1) * KO2;
   end;

   -- Not same scope
   declare
      OK1 : My_Int;            -- OK (used as indexing)
      KO2 : My_Int;            -- NRT_Variable
   begin
      declare
         OK3 : My_Int;         -- OK (depends on OK1)
         KO4 : My_Int := 3;    -- NRT_Variable (depends on KO2)
      begin
         OK1 := OK3;
         KO2 := KO4;
         declare
            OK5 : My_Int;      -- OK (depends on OK3)
            KO6 : My_Int := 5; -- NRT_Variable (depends on KO4)
         begin
            OK3 := OK5;
            KO4 := KO6;
         end;

      end;
      C := S (OK1 + 3);
   end;

   declare
      OK1 : My_Int;       -- OK (used as indexing)
      KO1 : My_Int;       -- NRT_Variable
   begin
      declare
         OK2 : My_Int;    -- OK (depends on OK3)
         KO2 : My_Int;    -- NRT_Variable
      begin
         declare
            OK3 : My_Int; -- OK (depends on OK1)
            KO3 : My_Int; -- NRT_Variable
         begin
            OK1 := OK3;
            OK3 := OK2;

            KO1 := KO3;
            KO3 := KO2;
         end;
      end;
      C := S (OK1);
   end;

   -- loops
   declare
      Indirect : array (1 .. 10) of My_Int;
      Ten      : constant My_Int := 10;       -- NRT_Constant
   begin
      for I in 1 .. Ten loop                  -- NRT_Variable
         null;
      end loop;
      L1 : for I in My_Int range 1 .. 10 loop -- NRT_Variable
         for J in 1 .. I loop                 -- NRT_Variable
            null;
         end loop;
      end loop L1;
      for I of Indirect loop                  -- NRT_Constant
         null;
      end loop;

      for I in My_Int range 1 .. 10 loop -- OK (depends on J)
         for J in 1 .. I loop            -- OK (used in indexing)
            C := S (J);
         end loop;
      end loop;
      for I of Indirect loop             -- OK (used in indexing)
         C := S (I);
      end loop;

      for V of Vec loop                  -- NRT_Constant
         null;
      end loop;
      for V of Vec loop                  -- OK (used in indexing)
         C := S (V);
      end loop;
   end;

   -- Depending on several variables
   declare
      OK1 : My_Int;         -- OK (used in indexing)
      OK2 : My_Int;         -- OK (used in indexing)
      KO3 : My_Int := OK1;  -- NRT_Variable
   begin
      declare
         OK4 : My_Int := 3; -- OK (depends on OK1, OK2, and KO3)
      begin
         OK1 := OK4;
         OK2 := OK4;
         KO3 := OK4;
      end;
      C := S (OK1);
      C := S (OK2);
   end;

   -- Attributes
   declare
      OK1 : My_Int; -- OK (in expression using array 'First)
      KO1 : My_Int; -- NRT_Variable (in expression using non array 'First)
      OK2 : My_Int; -- OK (in expression using array 'Last, complex expression)
      KO2 : My_Int; -- NRT_Variable (in expression using non array 'Last, complex expression)
   begin
      OK1 := OK1 + S'First;
      KO1 := KO1 + My_Int'First;

      OK2 := OK2 * (1 + Cts'Last) / 3;
      KO2 := KO2 * (1 + My_Int'Last - 2) / 3;
   end;

   -- Circularities
   declare
      OK1 : My_Int;   -- OK (used in indexing)
      OK2 : My_Int;   -- OK (depends on V1)
      KO1 : My_Int;   -- NRT_Variable
      KO2 : My_Int;   -- NRT_Variable
   begin
      OK1 := OK2;
      OK2 := OK1;

      KO1 := KO2;
      KO2 := KO1;

      C := S (OK1);
   end;

   declare
      OK1 : My_Int;       -- OK (used in indexing)
      KO1 : My_Int;       -- NRT_Variable
   begin
      declare
         OK2 : My_Int;    -- OK (depends on OK3)
         KO2 : My_Int;    -- NRT_Variable (depends on KO3)
      begin
         declare
            OK3 : My_Int; -- OK (depends on OK1)
            KO3 : My_Int; -- NRT_Variable (depends on KO1)
         begin
            OK3 := OK2;
            OK1 := OK3;

            KO3 := KO2;
            KO1 := KO3;
         end;
      end;
      C := S (OK1);
   end;
end Test_Not_Required_Type;
