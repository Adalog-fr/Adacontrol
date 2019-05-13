with Ada.Text_IO;
with System;          -- Unused: (case of declaration outside unit)
with Ada.Strings;
with Ada.Strings.Fixed;
with T_unnecessary_use_clause.Gen;
with T_Unnecessary_Use_Clause.Child;
package body T_unnecessary_use_clause is
   use Ada;                      -- OK, used next line
   use Text_IO;                  -- Only used  for operators (in separate body)
   package Pack1 is
                 -- Nested: In scope of outer clause
      procedure Proc;
   end Pack1;

   use Ada.Strings.Fixed;        -- Used in Sep
   procedure Sep is separate;
           -- Nested: In scope of outer clause

   package body Pack1 is
      procedure Proc is begin null; end;
   begin
      Put_Line ("Hello world");
   end Pack1;

   use Pack1;                    -- OK, use needed for instantiation
   package Inst is new Gen;

   package Pack2 is
      procedure Proc2;
   end Pack2;
   package body Pack2 is
      procedure Proc2 is
                       -- Nested: Used inside nested unit
      begin
         null;
      end;
   end Pack2;

   use Pack2;                    -- OK, use needed for formal package below
   generic
      with package Formal is new Gen (Proc2);
   package Gen2 is
   end Gen2;

   package Pack3 is
      type Int is new Integer;
   end Pack3;

   
   use type T_unnecessary_use_clause.Pack3.Int;                    -- Operator: Only used for operators
   V : Pack3.Int;

   package Pack4 is
      X : Integer;
   end Pack4;
                       -- Qualified: Only qualified usage

   -- Check use clause in generic formal part
   package Pack5 is
      type Int is range 1 .. 10;
   end Pack5;

   generic
                       -- Unused: unused
   procedure P (X : Integer);
   pragma Import (C, P);         -- Even when completed by import!

    -- Nested: Same package

   package Use_Type is
   end Use_Type;
   package body Use_Type is separate;

begin
   declare
                 -- Unused: unused
   begin
      V := V + 1;
      Pack4.X := Pack4.X + 1;
   end;
end T_unnecessary_use_clause;
