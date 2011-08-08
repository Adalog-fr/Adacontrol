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
      procedure P is begin null; end; -- OK
      procedure P                     -- Hiding
        (X : Integer)                 -- Hiding
      is begin null; end;
      type Enum                       -- OK
         is (T);                      -- Hiding
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
      procedure P            -- Hiding
        (Param : Integer);   -- OK
      package Internal is    -- OK
         protected Protec is -- OK
            procedure P;     -- OK
            procedure P      -- Hiding
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
      procedure P                           -- Hiding
        (Param : Integer)                   -- OK
        is begin null; end;
      procedure Proc is begin null; end;    -- Hiding
      package body Internal is              -- OK
         Z : Integer;                       -- Hiding
         protected body Protec is           -- OK
            procedure P is begin null; end; -- OK
            procedure P                     -- Hiding
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
end T_local_hiding;
