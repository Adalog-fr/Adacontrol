with Ada.Text_IO;
with System; use System;   -- Declaration outside unit
with Ada.Strings;
with Ada.Strings.Fixed;
with Test_Case_108.Gen;
package body Test_Case_108 is
   use Ada; -- OK, used next line
   use Text_IO; -- Unused
   package Pack is
      use Ada.Text_Io; -- Used in body, in scope of outer clause
      procedure Proc;
   end Pack;

   use Ada.Strings.Fixed; -- Used in Sep
   procedure Sep is separate;

   package body Pack is
      procedure Proc is
      begin
         null;
      end Proc;
   begin
      Put_Line ("Hello world");
   end Pack;

   use Pack;  -- Use needed for instantiation
   package Inst is new Gen;
begin
   declare
      use Ada.Strings; -- Unused
   begin
      null;
   end;
end Test_Case_108;
