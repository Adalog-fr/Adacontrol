with Ada.Text_IO;
with System; use System;   -- Declaration outside unit
with Ada.Strings;
with Ada.Strings.Fixed;
with T_unnecessary_use_clause.Gen;
package body T_unnecessary_use_clause is
   use Ada; -- OK, used next line
   use Text_IO; -- Unused
   package Pack1 is
      use Ada.Text_Io; -- Used in body, in scope of outer clause
      procedure Proc;
   end Pack1;

   use Ada.Strings.Fixed; -- Used in Sep
   procedure Sep is separate;

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

begin
   declare
      use Ada.Strings; -- Unused
   begin
      null;
   end;
end T_unnecessary_use_clause;
