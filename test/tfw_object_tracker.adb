procedure Tfw_Object_Tracker is
   C_One : constant Integer := 1;
   I     : Integer          := 0;
   X     : Integer;
   Const : constant Integer := I;

   function I_Plus_1 return Integer is (I + 1);

   procedure Pin    (Param : in     Integer) is null;
   procedure Pinout (Param : in out Integer) is null;
   function  Fin    (Param : in     Integer) return Integer is
   begin
      return 0;
   end Fin;
   function  Finout (Param : in out Integer) return Integer is
   begin
      Param := 1;
      return 0;
   end Finout;

   procedure Check_Label is
      CLI : Integer;
      CLX : Integer;
   begin
      CLI := 1;
      if CLI = 1 then                 -- True
         CLX := 0;
      end if;
      <<Label>>
      if CLI = 1 then                 -- Unknown (previous labeled statement)
         CLX := 0;
      end if;
      <<Label_2>>
   end Check_Label;

begin

   -- Simple cases
   if I = 0 then                      -- True
      X := 1;
   end if;

   if I = 10 then                     -- False
      X := 1;
   end if;

   I := 9;

   I := I + 1;
   if I = 10 then                     -- True
      X := 1;
   end if;

   I := I_Plus_1;
   if I = 11 then                     -- True
      X := 1;
   end if;

   if Const = 0 then                  -- True
      X := 1;
   end if;

   I := I + C_One;
   if I in 1 | 21 then                -- False
      X := 1;
   end if;

   if I in 1 | 12 | 21 then           -- True
      X := 1;
   end if;

   -- Check modular
   declare
      type Byte is mod 256;
      subtype Sb1 is Byte range 11 .. 21;
      B : Byte range 1 .. 10 := 3;
      subtype Sb2 is Byte range B .. B + 5;
   begin
      if B = 3 then                   -- True
         X := 1;
      end if;

      if B < 0 then                   -- False
         X := 1;
      end if;

      if Sb2'(B) in 2 .. 9 then       -- True
         X := 1;
      end if;
   end;

   -- Check enumerated
   declare
      type Enum is (One, Two, Three, Four, Five);
      E1 : Enum := Two;
      E2 : Enum range Three .. Four;
      subtype Es is Enum range Enum'Succ (E1) .. Four;
   begin
      if E1 = Two then                -- True
         X := 1;
      end if;

      E1 := One;
      if E2 not in Es then            -- False
         X := 1;
      end if;
   end;

   -- Check pointers
   declare
      type Ptr is access Integer;
      type Null_Excl is not null access all Integer;

      V1 : Ptr;
      V2 : Ptr := new Integer;
      V3 : not null Ptr := V2;
      V4 : Null_Excl := Null_Excl (V2);
      V5 : access Integer := new Integer;
      V6 : not null access Integer := new Integer;
   begin
      if V1 = null then               -- True
         V1 := new Integer;
         if V1 = null then            -- False
            X := 1;
         end if;
      end if;
      if V1 = null then               -- Unknown
         if V1 = null then            -- True
            X := 1;
         end if;
      end if;

      V1 := V2;
      if V1 = null then               -- False
         X := 1;
      end if;
      if V1 = V2 then                 -- True
         X := 1;
      end if;
      V1 := new Integer;
      if V1 = V2 then                 -- False
         X := 1;
      end if;
      if V5 = null then               -- False
         X := 1;
      end if;
      if V6 = null then               -- False
         X := 1;
      end if;

      V1 := null;
      V3 := V2;
      V2 := new Integer'(0);
      while V1 = null loop
         if V1 = null then            -- True
            X  := 1;
            V1 := V2;
         end if;
         if V1 = null then            -- Unknown
            X  := 1;
         end if;
         if V2 /= null then           -- Unknown
            X := 1;
         end if;
         if V3 = null then            -- False
            X := 1;
         end if;
         if V4 = null then            -- False
            X := 1;
         end if;
         if V5 = null then            -- Unknown
            X := 1;
         end if;
         if V6 = null then            -- False
            X := 1;
         end if;
      end loop;
   end;

   -- Check blocks
   I := 0;
   if X = 1 then
      begin
         I := 1;
      end;
   end if;
   if I = 0 then                      -- Unknown
      X := 1;
   end if;

   declare
      J : Integer := 0;
   begin
      if X = 1 then
         begin
            J := 1;
         end;
      end if;
      if J = 0 then                   -- Unknown
         X := 1;
      end if;
   end;

   -- Check "if" paths
   if X = I then
      I := 1;
      if I in 1 .. 3 then             -- True
         X := 1;
      end if;
   end if;
   if I in 1 .. 3 then                -- Unknown
      X := 1;
   end if;
   I := 1;
   if I in 1 .. 3 then                -- True
      X := 1;
   end if;

   -- Check "case" paths
   I := 1;
   case I is
      when 1 =>
         if I in 1 .. 3 then          -- True
            X := 1;
         end if;
      when 2 .. 20 =>
         if I not in 0 .. 30 then     -- False
            X := 1;
         end if;
      when others =>
         I := 2;
   end case;
   if I in 1 .. 3 then                -- Unknown
      X := 1;
   end if;
   I := 1;
   if I not in 1 .. 3 then            -- False
      X := 1;
   end if;

   -- Check loops
   while X < 10 loop                  -- Unknown
      if I > 3 then                   -- Unknown
         X := 1;
      end if;
      I := 5;
      if I > 3 then                   -- True
         X := 1;
      end if;
   end loop;
   if I > 3 then                      -- Unknown
      X := 1;
   end if;
   I := 1;
   while I < 2 loop                   -- Unknown
      X := 1;
   end loop;

   -- check parameters
   I := 1;
   Pin (I);
   if I = 1 then                      -- True
      X := 2;
   end if;
   Pinout (I);
   if I = 1 then                      -- Unknown
      X := 2;
   end if;

   I := 1;
   X := Integer'Succ (Fin (I)) + 5;
   if I = 1 then                      -- True
      X := 2;
   end if;
   I := 1;
   X := Integer'Succ (Finout (I)) + 5;
   if I = 1 then                      -- Unknown
      X := 2;
   end if;

   -- Check for loops
   for Control in Integer range 1 .. 1 loop
      if Control = 1 then             -- True
         X := 1;
      end if;
      if Control in 0 .. 2 then       -- True
         X := 1;
      end if;
   end loop;

   for Control in 1 .. 10 loop
      if Control >= 1 then            -- True
         X := 1;
      end if;
      if Control > 1 then             -- Unknown
         X := 1;
      end if;
      if Control in 1 .. 11 then      -- True
         X := 1;
      end if;
      if Control not in 20 .. 25 then -- True
         X := 1;
      end if;
      if Control < C_One then         -- False
         X := 1;
      end if;
   end loop;

   -- check complicated interval arithmetic
   for A in 1 .. 3 loop
      for B in -4 .. 2 loop
         if A * B in -12 .. 6 then    -- True
            X := 1;
         end if;
         if A * B in -11 .. 7 then    -- Unknown
            X := 1;
         end if;
         if A * B in 7 .. 10 then     -- False
            X := 1;
         end if;

         if B / A in -10 .. 3 then    -- True
            X := 1;
         end if;
      end loop;
   end loop;

   -- Check propagation
   declare
      function Not_Static return Integer is
      -- not expression function!
      begin
         return 5;
      end Not_Static;

      V1 : Integer range 1 .. 10 := Not_Static;
      V2 : Integer range 3 .. 12 := Not_Static;
   begin
      V1 := V2;
      if V1 not in 3 .. 10 then       -- False
         X := 1;
      end if;
   end;

   -- Check bounds from subtypes
   declare
      Max : Integer := 10;
      subtype Int is Integer range 1 .. Max;
      V1  : Int;
      V2  : Int range 2 .. Max / 2;
   begin
      Max := 20;
      if V1 in 1 .. 10 then           -- True
         X := 1;
         if V1 in 5 .. 15 then        -- Unknown
            if V1 in 5 .. 10 then     -- True
               X := 2;
            end if;
         end if;
      end if;
      if V2 not in 2 .. 6 then        -- False
         X := 1;
      end if;
   end;

   declare
      Low  : Integer range 1  .. 10;
      High : Integer range 21 .. 30;
   begin
      for V in Integer range Low .. High loop
         if V in 1 .. 30 then        -- True
            X := 1;
         end if;
         if V in 10 .. 21 then       -- Unknown
            X := 1;
         end if;
      end loop;
   end;

   -- Check conditions
   if I <= 10 then
      if I < 11 then                  -- True
         X := 1;
      end if;
   end if;

   while I <= 10 loop
      if I < 11 then                  -- True
         X := 1;
      end if;
   end loop;

   while I > 10 loop
      if I >= 5 then                  -- True
         X := 1;
      end if;
      I := 3;
      if I >= 5 then                  -- False
         X := 1;
      end if;
   end loop;

   -- Check access from inner SP
   declare
      Inner   : Integer;
      Not_Set : Integer := 1;

      procedure Proc is
      begin
         Inner := Not_Set;
      end Proc;
   begin
      Inner := 1;
      if Inner = 1 then               -- Unknown
         X := 1;
      end if;
      if Not_Set = 1 then             -- True
         X := 1;
      end if;
   end;

   -- Check generic and instantiation
   declare
      Inner1  : Integer;
      Inner2  : Integer;
      Not_Set : Integer := 1;

      generic
         V1 : in out Integer;
         V2 : in     Integer;
      procedure Gen;
      procedure Gen is
      begin
         Inner2 := 0;
      end Gen;

      procedure Inst is new Gen (Inner1, Not_Set);
   begin
      Inner1 := 1;
      if Inner1 = 1 then              -- Unknown (actual for in out formal object)
         X := 1;
      end if;
      Inner2 := 1;
      if Inner2 = 1 then              -- Unknown
         X := 1;
      end if;
      if Not_Set = 1 then             -- True
         X := 1;
      end if;
   end;

   I := 10; -- This assignment to (possibly) fool the exception handler
exception
      -- check handlers
   when Constraint_Error =>
      if I > 3 then                   -- Unknown
         X := 1;
      end if;
      I := 5;
      if I > 3 then                   -- True
         X := 1;
      end if;

end Tfw_Object_Tracker;
