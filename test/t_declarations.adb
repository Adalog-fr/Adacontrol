with Ada.Numerics.Generic_Elementary_Functions;
with X_Declarations.Child;
procedure T_declarations is
   type I1 is range 1..10;      -- signed_type, integer_type
   type I2 is mod 128;

   type Fl is digits 5;
   type Fx1 is delta 0.1 range 0.0 .. 1.0;
   type Fx2 is delta 0.1 digits 5;

   task T1 is
     entry E (I : Integer := 1);
   end T1;
   task body T1 is
   begin
      null;
   exception
      when others =>
         null;
   end T1;

   task type T2 (X : Integer) is
     entry E;
   end T2;
   task body T2 is
   begin
      null;
   end T2;

   protected P1 is
      entry E (I : out Integer; J : in out Integer);   -- Callable_Out, Callable_In_Out
   end P1;
   protected body P1 is
      entry E (I : out Integer; J : in out Integer) when True  is
      begin
         null;
      end E;
   end P1;

   protected type P2 (X : Integer := 0) is
      entry E;
   private
      I : Integer;      -- Uninitialized_Protected_Field
      J : Integer := 0; -- Initialized_Protected_Field
   end P2;
   protected body P2 is
      entry E when True is
      begin
         null;
      end E;
   end P2;

   E : exception;
   NN1 : constant := 1;   -- Named_Number
   NN2 : constant := 1.0; -- Named_Number

   type Acc1 is access Integer;
   type Acc2 is access procedure;
   type Acc3 is access T2;
   type Acc4 is access P2;

   type Der_Task is new T2;
   type Acc5 is access Der_Task;

   I,J,K : aliased Integer;
   C : aliased constant Character := ' ';

   type Rec1 is tagged null record;
   type Rec2 (X : Integer) is tagged limited null record;
   type Rec3 is null record;
   type Rec4 (X : Integer := 0) is
      record
         case X is
            when 0 =>
               I : Integer;      -- Uninitialized_Record_Field
            when others =>
               J : Integer := 0; -- Initialized_Record_Field
         end case;
      end record;
   type Rec5 is null record;
   type Rec6 is record
      null;
   end record;
   type Rec7 is
      record
         I : Integer;      -- Uninitialized_Record_Field
         J : Integer := 0; -- Initialized_Record_Field
      end record;
   type Arr1 is array (1..10) of Character;
   type Arr2 is array (Positive range <>) of Integer;
   VArr1 : array (1..10) of Character;

   type Der1 is new Rec1 with null record;
   type Der2 (Y : Integer) is new Rec1 with null record;
   type Der3 (Y : Integer) is new Rec2 (Y) with null record;
   type Der4 is new Rec3;

   type T_Float is digits 5;
   type T_Fixed1 is delta 0.01 range 0.0 .. 1.0;
   type T_Fixed2 is delta 0.01 digits 7;

   generic                                                          -- Nested_Generic_Procedure
      I : Integer := 1;
   procedure P (J : Integer := 1; K : in out Float; L : out Float); -- Defaulted_Parameter, Out_Parameter, In_Out_Parameter
   procedure P (J : Integer := 1; K : in out Float; L : out Float) is begin null; end;

   package Pack1 is end Pack1;
   package body Pack1 is
   end Pack1;

   package Pack2 is
      type Priv1 is private;                      -- Non_Limited_Private_Type
      type Priv2 is limited private;              -- Limited_Private_Type
      type Ext1 is new Rec1 with private;         -- Non_Limited_Private_Extension
   private
      type Priv1 is new Integer;                  -- Derived_Type
      type Priv2 is new Integer;                  -- Derived_Type
      type Ext1 is new Rec1 with null record;     -- Null_Extension, Record_Type
   end Pack2;
   package body Pack2 is
   begin
      null;
   end Pack2;

   package Pack3 renames Pack2;                          -- renames, non_identical_renaming
   generic package Generic_Elementary_Functions
      renames Ada.Numerics.Generic_Elementary_Functions; --Renames, Not_Operator_Renaming

   procedure Sep is separate;

   Tab : array (1..10) of Integer;

   function "+" (X, Y : Integer) return Integer is
   begin
      return 1;
   end "+";

   function "-" (X, Y : Integer) return Integer;
   function "-" (X, Y : Integer) return Integer is
   begin
      return 1;
   end "-";

   function F1  (X, Y : Integer) return Integer renames "+";            -- renames, non_identical_renaming
   function F2  (X, Y : Integer) return Integer renames Standard."+";   -- renames, non_identical_renaming, multiple_names
   function "*" (X, Y : Integer) return Integer renames Standard."*";   -- renames

   generic                                 -- Nested_Generic_Function
      Global : in out Integer;
      with procedure Formal_P;
      with function Formal_F return Integer;
      with package EF is new Ada.Numerics.Generic_Elementary_Functions (<>);
   function Test_Formals return Integer;
   function Test_Formals return Integer is
   begin
      return 0;
   end Test_Formals;

   subtype Int is Integer range 1..10;

   Arr : Integer renames X_Declarations.Arr (1);                        -- renames, non_identical_renaming, not_operator_renaming
   function Succ (X : Integer) return Integer renames Integer'Succ;     -- renames, non_identical_renaming, not_operator_renaming
begin
   null;
end T_declarations;
