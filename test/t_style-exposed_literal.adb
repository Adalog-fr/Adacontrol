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

   V : Long_Long_Integer := +9_223_372_036_854_775_807;  -- OK
  
  pragma JUNK ("ABCD");                                  -- OK
begin
   I := 0;                                               -- OK
   I := 2;                                               -- Exposed_Literal (integer)
   F := 1.0;                                             -- OK
   F := 2.0;                                             -- Exposed_Literal (real)
   S := "rosen";                                         -- OK
   if S = "" then                                        -- OK
      S := "12345";                                      -- Exposed_Literal (string)
   elsif S = "12345" then                                -- Exposed_Literal (string)
      C := 'A';                                          -- Exposed_Literal (character)
   end if;
   S(5) := S(1);                                         -- OK
   S (I + 5) := S (1);                                   -- Exposed_Literal (integer)
   I := I**2;                                            -- OK
   I := I**(2);                                          -- OK
   I := 2**I;                                            -- Exposed_Literal (integer)
   V := +9_223_372_036_854_775_807;                      -- Exposed_Literal (integer)  
   
   for I in 1 .. 10 loop                                 -- Exposed_Literal (integer)
      null;
   end loop;

end Exposed_Literal;
