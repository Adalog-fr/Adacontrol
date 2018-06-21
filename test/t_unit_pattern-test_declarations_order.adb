separate (T_Unit_Pattern)
procedure Test_Declarations_Order is
-- All the declarations directly in the procedure are OK
-- Incorrect order is in the nested units

   Max : constant := 5;
   type Int is range 1 .. Max;
   subtype Less is Int range 1 .. 3;
   type Another is range 1 .. 20;

   procedure P (X : Int) is
   begin
      null;
   end P;

   procedure Q;
   procedure Q is
   begin
      null;
   end;

   package Incorrect is
      EE1 : exception;           -- OK for package spec
      type T is range 1 .. 10;
      Pi : constant := 3.14159;  -- Out of order
      type Priv is private;      -- Out of order

      package Nested1 is
      end Nested1;

      function Inc (X : Integer) return Integer is (X + 1); -- Out of order

      EE2 : exception;            -- Out of order

      package Nested2 is
      end Nested2;

   private
      EE3 : exception;
      type Priv is new T;         -- Out of order
   end Incorrect;

   E : exception;

   use Incorrect;
   use type Incorrect.T;
   use all type Incorrect.Priv;
   use Incorrect.Nested1;
begin
      null;
end Test_Declarations_Order;
