procedure T_simplifiable_expressions is

   procedure Test_Range is
      Tab : array (1..2, 1..2) of Integer;

      package Pack1 is
         subtype T is Integer range 1..2;
      end Pack1;

      Package P1 renames Pack1;

      package Pack2 is
         subtype T is Integer range 1..2;
      end Pack2;

      package Pack is
         type T is range 1..10;
      end Pack;
      use Pack;

      type Atype is (a,b,c,d,e);
      type Btype is range 1 .. 10;
      type Ctype is range 1 .. 10;

      subtype PST1 is Positive range 1 .. 10;
      subtype PST2 is Positive range 1 .. 10;

      Ix1 : constant PST1 := 1;
      Ix2 : constant PST1 := 2;

      ASA : array (PST1) of String (PST1);
      ASB : array (1 .. 10) of String (1..10);

      type Array_Type is array  (PST1 range <>) of String (1..10);
      type Array_Type1 is array (PST1 range <>) of String (PST1'FIRST .. PST1'LAST); -- Should trigger
      type Array_Type2 is array (PST1 range <>) of String (PST1'RANGE);              -- Should trigger
      type Array_Type3 is array (PST1 range <>) of String (PST1'FIRST .. PST2'LAST); -- OK
      type Array_Type4 is new Array_Type3 (1..2);

      subtype ST1A is Array_Type (PST1'FIRST .. PST1'LAST); -- Should trigger
      subtype ST2A is Array_Type (PST1'RANGE);              -- Should trigger
      subtype ST3A is Array_Type (PST1'FIRST .. PST2'LAST); -- OK

      SA1 : ST1A;
      SA2 : ST2A;

      A_Var : Positive := 5;

      Renamed  : String (1..10);
      Renaming : String renames Renamed;

      generic
         type S is range <>;
      package Gen is end Gen;
      package body Gen is
         type Array_T is array (S range <>) of S;
         V : Array_T (S'Range); -- Should trigger
      begin
         for I in S'First..S'Last loop -- Should trigger
            null;
         end loop;
      end Gen;

   begin
      for I in Tab'First(1)..Tab'Last(2) loop -- OK
         null;
      end loop;

      for I in Pack1.T'First..Pack2.T'Last loop -- OK
         null;
      end loop;

      for I in Pack1.T'First..P1.T'Last loop -- Should trigger
         null;
      end loop;

      for X in Pack.T'First .. T'Last loop -- Should trigger
         null;
      end loop;
      for X in Pack.T'First .. Pack.T'Last loop -- Should trigger
         null;
      end loop;
      for X in T'Base'First .. T'Last loop -- OK
         null;
      end loop;
      for X in Btype'FIRST .. Btype (Ctype'LAST) loop -- OK
         null;
      end loop;
      for X in Btype (CType'FIRST) .. Btype'LAST loop -- OK
         null;
      end loop;
      for X in Btype'FIRST .. Btype'LAST-1 loop -- OK
         null;
      end loop;
      for X in Btype'FIRST-1 .. Btype'LAST loop -- OK
         null;
      end loop;
      for X in Btype'LAST .. Btype'FIRST loop -- OK
         null;
      end loop;
      for X in Btype'FIRST .. Btype'LAST loop -- Should trigger
         null;
      end loop;
      for X in Btype'RANGE loop -- Should trigger
         null;
      end loop;
      for X in Btype loop -- OK
         null;
      end loop;
      for X in Atype'FIRST .. Atype'LAST loop -- Should trigger
         null;
      end loop;

      for X in ASA'FIRST .. ASA'LAST loop         -- Should trigger
         ASA(X)(ASA(X)'RANGE) := (others => ' '); -- OK
      end loop;
      for X in ASA'RANGE loop                                      -- OK
         ASA(X)(ASA(X)'FIRST .. ASA(X)'LAST) := (others => ' ');   -- Should trigger
         ASA(X)(ASA(X)'FIRST .. ASA(Ix1)'LAST) := (others => ' '); -- OK
         ASA(X)(ASA(X)'FIRST .. A_Var) := (others => ' ');         -- OK
         ASA(X)(A_Var .. ASA(X)'LAST) := (others => ' ');          -- OK
      end loop;
      ASA (ASA'FIRST (1) .. ASA'LAST (1)) := (others => (others => ' ')); -- Should trigger
      ASA (ASA'RANGE (1)) := (others => (others => ' '));                 -- OK
      ASA(1)(ASA(Ix1)'FIRST .. ASA(Ix1)'LAST) := (others => ' ');         -- Should trigger
      ASA(1)(ASA(Ix1)'FIRST .. ASA(Ix2)'LAST) := (others => ' ');         -- OK

      SA1 (ST1A'FIRST .. ST1A'LAST) := (others => (others => ' ')); -- Should trigger
      SA1 (ST1A'FIRST .. ST2A'LAST) := (others => (others => ' ')); -- OK
      SA2 (ST1A'FIRST .. ST1A'LAST) := (others => (others => ' ')); -- Should trigger

      ASA (PST1) := (others => (others => ' '));                    -- OK
      ASA (PST1'FIRST .. PST1'LAST) := (others => (others => ' ')); -- Should trigger
      ASA (PST2'FIRST .. PST2'LAST) := (others => (others => ' ')); -- Should trigger
      ASA (PST1'FIRST .. PST2'LAST) := (others => (others => ' ')); -- OK

      ASB (PST1) := (others => (others => ' '));                              -- OK
      ASB (PST1'BASE'FIRST .. PST1'BASE'LAST) := (others => (others => ' ')); -- Should trigger
      ASB (PST1'BASE'FIRST .. PST1'LAST) := (others => (others => ' '));      -- OK
      ASB (PST1'FIRST .. PST2'LAST) := (others => (others => ' '));           -- OK
      ASB (ASB'FIRST .. ASB'LAST) := (others => (others => ' '));             -- Should trigger

      for I in Renaming'First..Renaming'Last loop  -- Should trigger
         null;
      end loop;

      for I in Renaming'Range loop -- OK
         null;
      end loop;

      for I in Array_Type4'range loop  -- OK
         null;
      end loop;
   end Test_Range;

   procedure Test_Logical is
      X : Boolean := True;

      function Y return Boolean is
      begin
         return True;
      end Y;

      A : Boolean := Y = False;               -- Should trigger
      B : constant Boolean := Y = False;      -- Should trigger

      procedure Z (P : in Boolean := Y = False) is -- Should trigger
      begin
         A := B;
      end Z;

   begin

      if X = False and then Y = True then    -- Should trigger (twice)
         Z;
      elsif X = False then                   -- Should trigger
         X := Y = False;                     -- Should trigger
      elsif X /= False then                  -- Should trigger
         null;
      elsif X = True then                    -- Should trigger
         null;
      elsif X /= True then                   -- Should trigger
         null;
      elsif False = X then                   -- Should trigger
         X := Y = False;
      elsif False /= X then                  -- Should trigger
         null;
      elsif True = X then                    -- Should trigger
         null;
      elsif True /= X then                   -- Should trigger
         null;
      end if;

      while Y = False and X = False loop     -- Should trigger (twice)
         A := B;
      end loop;
   end Test_Logical;

   procedure Test_Parentheses is
      I : Integer := 1;   -- OK
      J : Integer := (1); -- Should Trigger
      B : Boolean;
      procedure P (X : Integer) is
      begin
         null;
      end P;
   begin
      -- Statements

      I := (1); -- Should Trigger
      I := 1;   -- OK

      if (B) then     -- Should Trigger
         null;
      elsif (B) then  -- Should Trigger
         null;
      end if;
      if B then       -- OK
         null;
      elsif B then    -- OK
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

      P ((1)); -- Should Trigger
      P (1);   -- OK

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
      B := I = 1 or (I = 2 or  I = 3);  -- OK (removing parentheses changes associativity)
      B := I = 1 or (I = 2 and I = 3);  -- OK
      B := not (B);                     -- Should Trigger
      B := not (B and B);               -- OK
      B := (not B) and B;               -- Should Trigger

      -- Short circuits
      B := I = 1 or (I = 2 and then I = 3);        -- OK
      B := I = 1 or else (I = 2 and then I = 3);   -- OK
      B := I = 1 and then (I = 2 and then I = 3);  -- Should Trigger
      B := I = 1 and then (I = 2);                 -- Should Trigger
      B := I = 1 and then (B or B);                -- OK
      B := (I = 1) and then B;                     -- Should Trigger
      B := I = 1 and then (B);                     -- Should Trigger
      B := I = 1 and then ("or" (B, B));           -- Should Trigger
   end Test_Parentheses;
begin
   Test_Range;
   Test_Logical;
   Test_Parentheses;
end T_simplifiable_expressions;
