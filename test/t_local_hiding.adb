with Text_IO;
procedure T_local_hiding is
   X : Integer;     -- OK
   Text_IO : Float; -- Hiding
   type T is
      record
         X : Integer; -- OK
      end record;
   type Acc_Proc is access procedure  -- OK
     (X : Integer);                   -- OK

   procedure P     -- OK
     (X : Integer) -- Hiding
   is
      procedure P is begin null; end; -- Overloading
      procedure P                     -- Overloading, Hiding
        (X : Integer)                 -- Hiding (x2)
      is begin null; end;
      type Enum                       -- OK
         is (T);                      -- Hiding
   begin
      null;
   end;

   procedure P   -- Overloading
     (D : Duration) -- OK
   is
   begin
      null;
   end;
   procedure PP    -- OK
     (X : Integer) -- OK
     renames P;

   procedure Q is     -- OK
      X: Integer;     -- Hiding
      procedure T     -- Hiding
        (A : Integer) -- OK
        renames P;
   begin
      null;
   end;
   T_Local_Hiding : Integer;  -- Hiding

   package Pack is           -- OK
      type Priv is private;  -- OK
      Z : Integer;           -- OK
      procedure Proc;        -- OK
      procedure P            -- Overloading, Hiding
        (Param : Integer);   -- OK
      package Internal is    -- OK
         protected Protec is -- OK
            procedure P;     -- Overloading (x3)
            procedure P      -- Overloading (x2), Hiding (x2)
              (X : Integer); -- Hiding
         end Protec;
      end Internal;
   private
      type Priv is new Integer;     -- OK
      type Deferred;                -- OK
      type Deferred is new Integer; -- OK
   end Pack;

   procedure Proc is begin null; end; -- OK

   package body Pack is                     -- OK
      procedure P                           -- Overloading, Hiding
        (Param : Integer)                   -- OK
        is begin null; end;
      procedure Proc (X : Float) is begin null; end; -- Overloading (x2), Hiding (No explicit spec)
      procedure Proc is begin null; end;    -- Overloading, Hiding
      package body Internal is              -- OK
         Z : Integer;                       -- Hiding
         protected body Protec is           -- OK
            procedure P is begin null; end; -- Overloading (x4)
            procedure P                     -- Overloading (x2), Hiding (x2)
              (X : Integer)                 -- Hiding
            is begin null; end;
         end Protec;
      end Internal;
      procedure P_Int     -- OK
        (X : Integer)     -- Hiding
      is begin null; end;
   begin
      declare
         Z : Float; -- Hiding
      begin
         null;
      end;
   end Pack;

   package Sep is
      procedure Sep_Proc;
   end Sep;

   package body Sep is separate;

   procedure Sep_Proc (A, B : Integer) is separate;

begin
   declare
      X : Integer; -- Hiding
   begin
      null;
   end;

   begin
      null;
   exception
      when Y : Constraint_Error => -- OK
         declare
            Y : Integer;           -- Hiding
         begin
            null;
         end;
      when X : others =>           -- Hiding
         null;
   end;

   declare
      function "+" (L, R : Integer) return Integer is
      begin
         return 1;
      end "+";
      function "+" (L, R : Float) return Integer is  -- Operator overloading
      begin
         return 1;
      end "+";
   begin
      null;
   end;

   declare
      function A return Float is
      begin
         return 1.0;
      end;

      type T1 is (A, B, C);           -- Overloading (function)
      type T2 is (A, W, Z);           -- Overloading x1 (function, not T1'(A)
      function A return Integer is    -- Overloading x3
      begin
         return 1;
      end;
   begin
      null;
   end;

   -- Pattern exceptions
   declare
      Instance  : Integer;
      Instance1 : Integer;
      Name      : Integer;
      Name1     : Integer;
      function Get (I : Integer) return Integer is
      begin
         return 1;
      end Get;

      procedure Inner is
         Instance  : Integer;                        -- OK (exception to hiding)
         Instance1 : Integer;                        -- Hiding (not full name)
         Name      : Integer;                        -- OK (exception to hiding)
         Name1     : Integer;                        -- OK (exception to hiding)
         function Get (F : Float) return Integer is  -- OK (exception to overloading)
         begin
            return 1;
         end Get;
      begin
         null;
      end Inner;
   begin
      null;
   end;
end T_local_hiding;
