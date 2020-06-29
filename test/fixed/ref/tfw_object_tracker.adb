-- Objective: test proper behaviour of object tracker

procedure Tfw_Object_Tracker is
   C_One : constant Integer := 1;
   I     : Integer          := 0;
   X     : Integer;
   Const : constant Integer := I;
   I_Bis : Integer renames I;

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
      CLX := 0;
      <<Label>>
      if CLI = 1 then                 -- Unknown (previous labeled statement)
         CLX := 0;
      end if;
      <<Label_2>>
   end Check_Label;

begin

   -- Simple cases
   X := 1;

   

   I := 9;

   I := I + 1;
   X := 1;

   I := I_Plus_1;
   X := 1;

   X := 1;

   I := I + C_One;
   

   X := 1;

   -- Check modular
   declare
      type Byte is mod 256;
      subtype Sb1 is Byte range 11 .. 21;
      B : Byte range 1 .. 10 := 3;
      subtype Sb2 is Byte range B .. B + 5;
   begin
      X := 1;

      

      X := 1;
   end;

   -- Check enumerated
   declare
      type Enum is (One, Two, Three, Four, Five);
      E1 : Enum := Two;
      E2 : Enum range Three .. Four;
      subtype Es is Enum range Enum'Succ (E1) .. Four;
   begin
      X := 1;

      E1 := One;
      
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
      V1 := new Integer;
         if V1 = null then            -- False
            X := 1;
         end if;
      if V1 = null then               -- Unknown
         X := 1;
      end if;

      V1 := V2;
      
      X := 1;
      V1 := new Integer;
      
      
      

      V1 := null;
      V3 := V2;
      V2 := new Integer'(0);
      while V1 = null loop
         X  := 1;
            V1 := V2;
         if V1 = null then            -- Unknown
            X  := 1;
         end if;
         if V2 /= null then           -- Unknown
            X := 1;
         end if;
         
         
         if V5 = null then            -- Unknown
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
      X := 1;
   end if;
   if I in 1 .. 3 then                -- Unknown
      X := 1;
   end if;
   I := 1;
   X := 1;

   -- Check "case" paths
   case I is
      when 1 =>
         X := 1;
      when 2 .. 20 =>                 -- Choice covers no value
         
      when others =>
         I := 2;
   end case;
   if I in 1 .. 3 then                -- Unknown
      X := 1;
   end if;
   I := 1;
   

   -- Check loops
   while X < 10 loop                  -- Unknown
      if I > 3 then                   -- Unknown
         X := 1;
      end if;
      I := 5;
      X := 1;
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
   X := 2;
   Pinout (I);
   if I = 1 then                      -- Unknown
      X := 2;
   end if;

   I := 1;
   X := Integer'Succ (Fin (I)) + 5;
   X := 2;
   I := 1;
   X := Integer'Succ (Finout (I)) + 5;
   if I = 1 then                      -- Unknown
      X := 2;
   end if;

   -- Check for loops
   for Control in Integer range 1 .. 1 loop
      X := 1;
      X := 1;
   end loop;

   for Control in 1 .. 10 loop
      X := 1;
      if Control > 1 then             -- Unknown
         X := 1;
      end if;
      X := 1;
      X := 1;
      
   end loop;

   -- check complicated interval arithmetic
   for A in 1 .. 3 loop
      for B in -4 .. 2 loop
         X := 1;
         if A * B in -11 .. 7 then    -- Unknown
            X := 1;
         end if;
         

         X := 1;
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
      
   end;

   -- Check bounds from subtypes
   declare
      Max : Integer := 10;
      subtype Int is Integer range 1 .. Max;
      V1  : Int;
      V2  : Int range 2 .. Max / 2;
   begin
      Max := 20;
      X := 1;
         if V1 in 5 .. 15 then        -- Unknown
            if V1 in 5 .. 10 then     -- True
               X := 2;
            end if;
         end if;
      
   end;

   declare
      Low  : Integer range 1  .. 10;
      High : Integer range 21 .. 30;
   begin
      for V in Integer range Low .. High loop
         X := 1;
         if V in 10 .. 21 then       -- Unknown
            X := 1;
         end if;
      end loop;
   end;

   -- Check conditions
   if I <= 10 then
      X := 1;
   end if;

   while I <= 10 loop
      X := 1;
   end loop;

   while I > 10 loop
      X := 1;
      I := 3;
      
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
      X := 1;
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
      X := 1;
   end;

   -- Check discriminants
   Regular_Discriminants: declare
      type Enum is (E1, E2, E3);
      type Rec (D1 : Integer := 0; D2, D3 : Enum := E1) is
         record
            Val : Integer;
         end record;
      R1 : Rec (1, E1, E2);         -- Discriminants: Initialization by explicit value
      R2 : Rec;                     -- Discriminants: Initialization by default value
      R3 : Rec := (10, E3, E2, 0);  -- Discriminants: Initialization by initial value
   begin
      I := 1;

      R1 := (I, E1, E2, I);
      if R1.D1 in 3 .. 5 then
         I := 1;
      end if;

      R2 := (4, E3, E2, 0);
      
      I := 0;

      R1 := R2;
      

      I := 0;
   end Regular_Discriminants;

   Access_Discriminants  : declare
      type Acc is access all Integer;
      type Acc_Rec1 (Ptr : Acc) is null record;
      type Acc_Rec2 (Ptr : not null access Integer) is null record;
      A11 : Acc_Rec1 (new Integer'(2));
      A12 : Acc_Rec1 (null);
      A21 : Acc_Rec2 (new Integer);
      A22 : Acc_Rec2 := (Ptr => new Integer);
      A23 : Acc_Rec2 := A22;
   begin
      -- check initialization
      
      if A12.Ptr = null then          -- True
         I := 0;
      elsif A21.Ptr = null then          -- False
         I := 0;
      elsif A22.Ptr = null then          -- False;
         I := 0;
      elsif A23.Ptr = null then          -- False;
         I := 0;
      elsif A23.Ptr = A22.Ptr then       -- True;
         I := 0;
      end if;
   end Access_Discriminants;

   -- Check Unknown bounds, obtained from type
   declare
      type Int is range 1 .. 10;
      type Rec (D : Int) is null record;

      function F return Int is
      begin
         return 5;
      end F;
      V1 : Int := 2;
      V2 : Int := 2;
      V3 : Integer;  -- Predefined type
      D1 : Rec := (D => 5);
      D2 : Rec := (D => F);

      type Reca (D : access Integer) is null record;
      Da : Reca (new Integer'(1));
      Db : Reca (new Integer'(1));

      procedure P is
      begin
         V2 := 3;
         D2 := (D => 1);
         Da := Da;
      end P;
   begin
      null;
      if V2 = 2 then            -- Unknown
         null;
      end if;
      null;
      
      null;
      null;
      if Da.D = null then       -- Unknown
         null;
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
      X := 1;

end Tfw_Object_Tracker;
