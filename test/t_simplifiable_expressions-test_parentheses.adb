separate (T_Simplifiable_Expressions)
procedure Test_Parentheses is
   S1 : String := (1 .. 4 => 'A');  -- OK

   type Rec is
      record
         F1 : Integer;
         F2 : Boolean;
      end record;
   R1 : Rec;

   type D_Rec (Discr : Boolean) is null record;
   R2 : D_Rec (True);                                   -- OK
   R3 : D_Rec ((False));                                -- Should Trigger
   R4 : D_Rec (for Some C of S1 => C = 'A');            -- OK
   R5 : D_Rec ((for Some C of S1 => C = 'A'));          -- Should Trigger
   R6 : D_Rec (Discr => (for Some C of S1 => C = 'A')); -- OK

   I  : Integer := 1;                               -- OK
   J  : Integer := (1);                             -- Should Trigger
   B1 : Boolean := (if True then True else False);  -- OK
   B2 : Boolean := (for all C of S1 => C = 'A');    -- OK
   B3 : Boolean := (for Some C of S1 => C = 'A');   -- OK

   B : Boolean;
   procedure P (X : Integer; Y : Integer := 0) is
   begin
      null;
   end P;
   function F1 return Integer is (1);                            -- OK
   function F2 return Integer is ((1));                          -- Should Trigger
   function F3 return Boolean is (for all C of S1 => C = 'A');   -- OK
   function F4 return Boolean is ((for all C of S1 => C = 'A')); -- Should Trigger

   generic
      B : Boolean;
   procedure Gen;
   procedure Gen is begin null; end Gen;

   -- Instantiations
   procedure Inst1 is new Gen (True);                                 -- OK
   procedure Inst2 is new Gen ((True));                              -- Should Trigger
   procedure Inst3 is new Gen (B => (True));                         -- Should Trigger
   procedure Inst4 is new Gen (if True then True else False);        -- OK
   procedure Inst5 is new Gen ((if True then True else False));      -- Should Trigger
   procedure Inst6 is new Gen (B => (if True then True else False)); -- OK
begin

   -- Statements
   I  := (1);                             -- Should Trigger
   I  := 1;                               -- OK
   B1 := (if True then True else False);  -- OK
   B2 := (for all C of S1 => C = 'A');    -- OK
   B3 := (for Some C of S1 => C = 'A');   -- OK

   if (B) then                               -- Should Trigger
      null;
   elsif (B) then                            -- Should Trigger
      null;
   elsif (if True then True else False) then --OK
      null;
   end if;
   if B then     -- OK
      null;
   elsif B then  -- OK
      null;
   end if;

   case (B) is     -- Should Trigger
      when others =>
         null;
   end case;
   case B is       -- OK
      when others =>
         null;
   end case;

   P ((1));                                 -- Should Trigger
   P ((((1))));                             -- Should Trigger  x3
   P ((if True then 1 else 2));             -- Should Trigger
   P (X => (if True then 3 else 4));        -- OK
   P ((if True then 5 else 6), 1);          -- OK
   P (if True then 7 else 8);               -- OK
   P (1);                                   -- OK
   B := not (if B then B else not B);       -- OK
   B := not ((if B then B else not B));     -- Should Trigger
   B := "not" (if B then B else not B);     -- OK
   B := "not" ( (if B then B else not B) ); -- Should Trigger


   -- Arithmetic
   I := 1 + (2*3);       -- Should Trigger
   I := (1) + 2*3;       -- Should Trigger
   I := 1 * (2+3);       -- OK
   I := 1 * ("+"(2, 3)); -- Should Trigger
   I := "*"(1, (2+3));   -- Should Trigger
   I := 3 + abs (I);     -- Should Trigger
   I := 3 + (abs I);     -- Should Trigger
   I := I ** (3);        -- Should Trigger
   I := I ** (3+1);      -- OK
   I := (I+1) ** 3;      -- OK

   -- Simple logical
   B := I = 1 or (I > 2 or  I = 3);  -- OK (removing parentheses changes associativity)
   B := I = 1 or (I = 2 and I = 3);  -- OK
   B := not (B);                     -- Should Trigger
   B := not (B1 and B2);             -- OK
   B := (not B) and B;               -- Should Trigger

   -- Short circuits
   B := I = 1 or (I = 2 and then I = 3);        -- OK
   B := I = 1 or else (I = 2 and then I = 3);   -- OK
   B := I = 1 and then (I = 2 and then I = 3);  -- Should Trigger
   B := I = 1 and then (I = 2);                 -- Should Trigger
   B := I = 1 and then (B1 or B2);              -- OK
   B := (I = 1) and then B;                     -- Should Trigger
   B := I = 1 and then (B);                     -- Should Trigger
   B := I = 1 and then ("or" (B1, B2));         -- Should Trigger

   -- Aggregates
   S1 := (('a'), 'b', others => (' '));                     -- Should Trigger x2
   S1 := (1 => ('a'), 2 => 'b', others => (' '));           -- Should Trigger x2
   S1 := ((if B1 then 'a' else 'b'), others => ' ');        -- OK
   S1 := (((if B1 then ('a') else 'b')), others => (' ')); -- Should Trigger x3

   R1 := (1, True);                           -- OK
   R1 := ((1), (True));                       -- Should Trigger x2
   R1 := ((1), F2 => (True));                 -- Should Trigger x2
   R1 := (1, (for all C of S1 => C = 'A'));   -- OK
   R1 := (1, ((for all C of S1 => C = 'A'))); -- Should Trigger

   -- Membership
   B := (I in 1 .. 10) or (J not in 1 .. 10);    -- Should Trigger x2
   B := (I in 1 .. 10) =  (J not in 1 .. 10);    -- OK
   B := (B) in False .. False;                   -- Should Trigger
   B := (I+1) in 1 .. 10;                        -- Should Trigger
   B := (B1 or B2) in False .. False;            -- OK
   B := (B in False .. False) in False .. False; -- OK
   B := B or (B in False .. False);              -- Should Trigger

   -- Ranges with logical operators (not simple expression)
   for L in False .. (I = 3) loop                -- OK
      null;
   end loop;
   for L in 0 .. (I + 3) loop                    -- Should Trigger
      null;
   end loop;
   B := B in False .. (I = 3);                   -- OK
   B := I in 0     .. (I + 3);                   -- Should Trigger

end Test_Parentheses;
