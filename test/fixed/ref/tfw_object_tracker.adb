-- Objective: test proper behaviour of object tracker
with System;
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

   procedure Check_Expression_Function is
      J : Integer := 1;
      function J_Plus_1 return Integer is (J + 1);
   begin
      null;
      if I_Plus_1 = 1 then            -- Unkown (non local expression function)
         null;
      end if;
   end;

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
      subtype With_Predicate is Enum with Static_Predicate => With_Predicate /= Three;
   begin
      X := 1;

      E1 := One;
      
      if E1 in With_Predicate then    -- Unknown (predicate)
         null;
      end if;
   end;

   -- Check strings
   declare
      function F return String is
      begin
         return "abc";
      end F;
      C : constant String := "xyz";
      V : String := "abc";
   begin
      if F & C = "abc" then
         null;
      elsif V & C = "abc" then
         null;
      end if;

      null;

      null;

      
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

   I := 1;
   X := 2;
   Pinout (Natural (I));
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
   declare                            -- No exceptions raised here
      B : array (1 .. 10) of Boolean;
      I : Integer := 0;
      function F return Boolean is (B (I));
      function G return Boolean is
      begin
         return (B (I));
      end G;
   begin
      null;
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


   -- Check declarations from nested (generic) package specs
   declare
      package Pack1 is
         type T (D : Integer) is null record;
         V1 : T (1);
         V11 : Integer := 1;
      end Pack1;
      use Pack1;

      generic package Gen is
         type T (D : Integer) is null record;
         V2 : T (1);
      end Gen;
      package Pack2 is new Gen;
      use Pack2;

      V01 : Pack1.T (1);
      V02 : Pack2.T (1);

   begin
      null;

      null;

      null;

      null;

      null;
   end;

   -- Combinations of formal parameters and discriminants
   declare
      type Rec1 (D : Character := 'a') is
         record
            case D is
               when 'a'              => A : Integer;
               when 'b' .. 'e' | 'z' => B : Integer;
               when others => null;
            end case;
         end record;
      subtype Rec1_A     is Rec1 ('a');
      type Int_Ptr is access Integer;
      type Rec (D1 : Int_Ptr := null; D2 : Integer := 1) is null record;

      type Short is range 1 .. 10;
      procedure P1 (Y : in out Short) is
      begin
         

         if Y = 1 then    -- Unknown
            null;
         end if;

         Y := 1;
         null;
      end P1;

      procedure P2 (X : in Rec; Y : in out Rec) is
      begin
         X.D1.all := 1;   -- Unknown
         Y.D1.all := 1;   -- Unknown
         if Y.D2 = 1 then -- Unknown
            null;
         end if;

         Y := (new Integer, 2);
         Y.D1.all := 1;   -- Ok

         Y := (null, 1);
         Y.D1.all := 1;   -- Null dereference
         null;
      end P2;

      A : Short;
   begin
      

      if A = 1 then -- Unknown
         null;
      end if;

      A := 1;
      null;
   end;

   -- Several discriminants, complicated case
   declare
      type Enum is (A, B, C);
      type Rec (Bool : Boolean := True; D : Enum := A) is
         record
            case Bool is
               when True =>
                  case D is
                     when A =>
                        I : Integer;
                     when B | C =>
                        F : Float;
                  end case;
               when False =>
                  J : Integer;
            end case;
         end record;
      procedure Proc (V : out Rec) is
      begin
         V := (True, B, 1.0);
      end Proc;
      V : Rec;
      X : Integer;
   begin
      case X is
         when 1 =>
            V.F := V.F;               -- Constraint_Error x2
            V.J := V.J;               -- Constraint_Error x2
         when others =>
            null;
      end case;

      case X is
         when 1 =>
            Proc (V);
         when 2 =>
            null;
         when others =>
            null;
      end case;

      case X is
         when 1 =>
            V.F := V.F;               -- Unknown
            V.J := V.J;               -- Unknown
         when others =>
            null;
      end case;
   end;

   -- Private type with discriminants
   declare
      package Pack is
         type T (D1 : access Integer; D2 : Integer) is private;
      private
         type T (D1 : access Integer; D2 : Integer) is null record;
      end Pack;
      subtype St1 is Pack.T;
      subtype St2 is Pack.T (null, 1);

      procedure P (X : St1; Y : St2) is
         J : Integer;
      begin
         J := X.D1.all;
         if X.D2 = 1 then
            null;
         end if;

         J := Y.D1.all;     -- Null dereference
         null;
      end P;
   begin
      null;
   end;

   -- Task and protected types with discriminants
   declare
      protected type Prot (D1 : access Integer; D2 : Integer) is
      end Prot;

      protected body Prot is
      end Prot;

      subtype Spt1 is Prot;
      subtype Spt2 is Prot (null, 1);

      procedure P (X : Spt1; Y : Spt2) is
         J : Integer;
      begin
         J := X.D1.all;
         if X.D2 = 1 then
            null;
         end if;

         J := Y.D1.all;     -- Null dereference
         null;
      end P;

      task type TT (D1 : access Integer; D2 : Integer) is end TT;

      task body TT is
      begin
         null;
      end TT;

      subtype Stt1 is TT;
      subtype Stt2 is TT (null, 1);

      procedure P (X : Stt1; Y : Stt2) is
         J : Integer;
      begin
         J := X.D1.all;
         if X.D2 = 1 then
            null;
         end if;

         J := Y.D1.all;     -- Null dereference
         null;
      end P;
   begin
      null;
   end;

   --  Aliased and volatile variables, and variables with address clause (or aspect) are not tracked
   declare
      Al : aliased Integer := 0;
      Vol : access Integer with Volatile;
      type Ptr is access Integer;
      Addr : System.Address;
      V1  : Ptr with Address => Addr, Import, Convention => Ada;
      V2  : Ptr;
      for V2'Address use Addr;
      V3  : Ptr;
      for V3 use at Addr;
   begin
      Vol.all := Al;
      if Al = 0 then
         null;
      end if;
      if V1 /= null then
         null;
      end if;
      if V2 /= null then
         null;
      end if;
      if V3 /= null then
         null;
      end if;
   end;


   -- Blocks with/without exception handlers
   declare
      Ok : Boolean;
   begin
      begin
         Ok := True;
      end;

      null;

      begin
         Ok := True;
      exception
         when Constraint_Error =>
            Ok := False;
      end;
      if Ok then       -- Unknown
         null;
      end if;

      begin
         Ok := True;
         -- Assume exception raised here
         Ok := False;
      exception
         when Constraint_Error =>
            null;
      end;
      if Ok then       -- Unknown
         null;
      end if;

      begin
         Ok := True;
         begin
            Ok := True;
            -- Assume exception raised here
            Ok := False;
         exception
            when Constraint_Error =>
               null;
         end;
         Ok := False;
      end;

      
   end;

   I := 10;         -- This assignment to (possibly) fool the exception handler
exception
      -- check handlers
   when Constraint_Error =>
      if I > 3 then -- Unknown
         X := 1;
      end if;
      I := 5;
      X := 1;

end Tfw_Object_Tracker;
