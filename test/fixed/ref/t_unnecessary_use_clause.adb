with Ada.Text_IO;
with System;    -- Unused (case of declaration outside unit)
with Ada.Strings;
with Ada.Strings.Fixed;
with T_unnecessary_use_clause.Gen;
with T_Unnecessary_Use_Clause.Child;
package body T_unnecessary_use_clause is
   use Ada;            -- OK, used next line
           -- Unused
   package Pack1 is
       -- Used in body, in scope of outer clause
      procedure Proc;
   end Pack1;

   use Ada.Strings.Fixed; -- Used in Sep
   procedure Sep is separate;
    -- Already given

   package body Pack1 is
      procedure Proc is begin null; end;
   begin
      Put_Line ("Hello world");
   end Pack1;

   use Pack1;  -- Use needed for instantiation
   package Inst is new Gen;

   package Pack2 is
      procedure Proc2;
   end Pack2;
   package body Pack2 is
      procedure Proc2 is begin null; end;
   end Pack2;

   use Pack2; --Use needed for formal package below
   generic
     with package Formal is new Gen (Proc2);
   package Gen2 is
   end Gen2;

   package Pack3 is
      type Int is new Integer;
   end Pack3;

   use Pack3;
   V : Pack3.Int;

   package Pack4 is
      X : Integer;
   end Pack4;
       -- Only qualified usage

   package Pack5 is
      type Int is range 1 .. 10;
   end Pack5;
   generic             -- Check use clause in generic formal part
             -- Unnecessary
   procedure P (X : Integer);
   pragma Import (C, P); -- Even when completed by import!
begin
   declare
       -- Unused
   begin
      V := V + 1;
      Pack4.X := Pack4.X + 1;
   end;
end T_unnecessary_use_clause;
