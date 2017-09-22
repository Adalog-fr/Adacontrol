with System;
separate (T_Style)
procedure Exposed_Literal is
   II : constant Integer   := 4;                         -- OK
   FX : constant Float     := 4.5;                       -- OK
   NI : constant           := 5;                         -- OK
   NF : constant           := 4.5;                       -- OK
   SS : constant String    := "AdaControl";              -- OK
   CC : constant Character := 'a';                       -- OK
   I  : Integer := 5;                                    -- OK
   F  : Float;
   S  : String (1 .. NI);
   C  : Character;
   type Enum is (A);
   for Enum use (A => 5);                                -- OK
   type Rec is
      record
         A, B, C : Integer;
      end record;
   for Rec use
      record
         A at 0 range 0 .. 31;                             -- OK
         B at 4 range 0 .. 31;                             -- OK
         C at 8 range 0 .. 31;                             -- OK
      end record;

   type T is range 1 .. 10;                              -- OK
   subtype Sub is T range 1 .. 5;                        -- OK
   type Arr is array (T, T) of Sub;

   V : Long_Long_Integer := +9_223_372_036_854_775_807;  -- OK
   type Big_Mod is mod System.Max_Binary_Modulus;
   BM : constant Big_Mod := 16#FFFF_FFFF_FFFF_FFFF#;     -- OK

   pragma JUNK ("ABCD");                                 -- OK
   Char_Var  : Character := 'A';                         -- OK
   Float_Var : Float     := 3.0;                         -- Exposed_Literal (real)

   procedure P (C : in Character := 'A') is              -- OK
   begin
      null;
   end P;

   procedure P (F : in Float := 3.0) is                  -- Exposed_Literal (real)
   begin
      null;
   end P;
begin
   I := 0;                                               -- OK
   I := 2;                                               -- Exposed_Literal (integer)
   I := Arr'Length (2);                                   -- OK

   declare
      F1 : Float := 1.0;                                 -- OK
      F2 : Float := -1.0;                                -- OK
      F3 : Float := 2.0;                                 -- Exposed_Literal (real)
      F4 : Float := -2.0;                                -- Exposed_Literal (real)
   begin
      F := 2.0;                                          -- OK (Statement)
   end;

   S := "rosen";                                         -- OK
   if S = "" then                                        -- OK
      S := "12345";                                      -- Exposed_Literal (string)
   elsif S = "12345" then                                -- Exposed_Literal (string)
      C := 'A';                                          -- Exposed_Literal (character)
   end if;
   S(5) := S(1);                                         -- OK
   S (I + 5) := S (1);                                   -- Exposed_Literal (integer)
   S (I + 6) := S (1);                                   -- OK
   S (I + 7) := S (1);                                   -- OK
   S         := (1 => C, 2 .. 3 => CC, 4 | 5 => C);      -- OK
   I := I**2;                                            -- OK
   I := I**(2);                                          -- OK
   I := 2**I;                                            -- Exposed_Literal (integer)
   V := +9_223_372_036_854_775_807;                      -- Exposed_Literal (integer)
   V := -9_223_372_036_854_775_808;                      -- Exposed_Literal (integer)  (Mantis 0000030)
   V := -1;                                              -- OK

   for I in 1 .. 10 loop                                 -- Exposed_Literal (integer)
      null;
   end loop;

end Exposed_Literal;
