separate (T_Style)
procedure No_Default_In is
   procedure Proc_A (Param1 : in Integer := 1) is        -- No_Default_In
   begin
      return ;
   end Proc_A;
   procedure Proc_B (Param2 : in Integer) is
   begin
      return;
   end Proc_B;

   generic
      Limit, Reset_Value : in Integer;                    -- No_Default_In
      Variable : in out Integer;
   procedure Reset_Integer_Template (Var : in Integer);   -- No_Default_In

   procedure Reset_Integer_Template (Var : in Integer) is
   begin
      if Variable > Limit then
         Variable := Reset_Value;
      end if;
   end Reset_Integer_Template;

   generic
      Max : in Positive;
      Min : in Positive;                                -- No_Default_In
      with procedure P (X : in Integer);                -- No_Default_In
   package Foo is
      procedure Bar (X : in Integer);
      function Foo_Bar (Y : in Integer) return Integer;   -- No_Default_In
   end Foo;
   package body Foo is
      procedure Bar (X : in Integer) is                   -- No_Default_In
      begin
         null;
      end Bar;
      function Foo_Bar (Y : in Integer) return Integer is -- No_Default_In
      begin
         return 1;
      end Foo_Bar;
   end Foo;

   procedure Access_Param (X : access Integer) is
   begin
      null;
   end Access_Param;
begin
   Proc_A;
end No_Default_In;
