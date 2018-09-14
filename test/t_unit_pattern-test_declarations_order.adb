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
         procedure P;
      end Nested1;

      function Inc (X : Integer) return Integer is (X + 1); -- Out of order

      EE2 : exception;            -- Out of order

      package Nested2 is
         procedure P;
      end Nested2;

      package Nested3 is
         generic procedure In_Public;
      private
         generic procedure In_Private;
      end Nested3;

      package Nested4 is
         generic procedure In_Public;
      private
         generic procedure In_Private;
      end Nested4;

   private
      EE3 : exception;
      type Priv is new T;         -- Out of order
   end Incorrect;

   package Tasks_OK is
      task T1 is
         entry E;
      end T1;
   private
      task T2;
   end Tasks_OK;

   package body Incorrect is
      package body Nested1 is     -- OK package body
         procedure Q is
         begin
            null;
         end;

         procedure P is
         begin
            null;
         end;
      end Nested1;

      package body Nested2 is
         procedure P is
         begin
            null;
         end;

         procedure Q is           -- Out of order
         begin
            null;
         end;
      end Nested2;

      package body Nested3 is     -- OK package body
         procedure In_Private is
         begin
            null;
         end;
         procedure In_Public is
         begin
            null;
         end;
      end Nested3;

      package body Nested4 is
         procedure In_Public is
         begin
            null;
         end;
         procedure In_Private is   -- Out of order
         begin
            null;
         end;
      end Nested4;

   end Incorrect;

   package body Tasks_OK is
      task body T1 is
      begin
         accept E;
      end T1;

      task body T2 is
      begin
         null;
      end T2;
   end Tasks_OK;

   package Tasks_Error is          -- Out of order
      task T1 is
         entry E;
      end T1;
   private
      task T2;
   end Tasks_Error;

   package body Tasks_Error is
      task body T2 is
      begin
         null;
      end T2;

      task body T1 is              -- Out of order
      begin
         accept E;
      end T1;
   end Tasks_Error;


   E : exception;

   use Incorrect;
   use type Incorrect.T;
   use all type Incorrect.Priv;
   use Incorrect.Nested1;
begin
   null;
end Test_Declarations_Order;
