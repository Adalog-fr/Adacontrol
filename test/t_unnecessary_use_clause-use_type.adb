with X_Unnecessary_Use_Clause;
with Ada.Assertions;
separate (T_Unnecessary_Use_Clause)
package body Use_Type is
   use type Ada.Text_IO.Count;                         -- Nested: Use type in scope of use package clause
   use type X_Unnecessary_Use_Clause.Int_1;            -- Qualified: All uses qualified

   use type X_Unnecessary_Use_Clause.Int_1;         -- Nested: In scope of use type clause

   type Local_Int1 is range 1 .. 10;
   use type Local_Int1;                             -- Nested: useless (type not in package spec)

   type Local_Int2 is range 1 .. 10;
   use all type Local_Int2;                         -- Nested: useless (type not in package spec)

   package Pack1 is
      subtype Local_Sub is Local_Int1 range 1 .. 2;

      type Int1 is range 1 .. 10;
      procedure Prim (X : Int1) is null;

      type Int2 is range 1 .. 10;

      type Tag1 is tagged null record;
      procedure Prim (X : access Tag1);             -- (Primitive by access parameter)

      type Flt is digits 5;
   end Pack1;

   package body Pack1 is
      use type Local_Sub;                           -- Nested: useless (type not in package spec)
      procedure Prim (X : access Tag1) is null;
   end Pack1;

   package Pack2 is
      type Der1 is new X_Unnecessary_Use_Clause.Int_1;
      subtype Sub_Der1 is Der1;
   end Pack2;

   type Der2 is new X_Unnecessary_Use_Clause.Int_1; -- Derived type not in package spec (still has primitive ops)

   package Pack3 is
      use type Pack1.Int1;                          -- Movable: can be moved to body
   end Pack3;

   package body Pack3 is
      V : Pack1.Int1;
   begin
      V := V + 1;
   end Pack3;

begin
   declare
      use type Pack2.Der1;                          -- OK
      use type Der2;                                -- Nested: type not in package spec

      A : Text_IO.Count := 1;
      B : X_Unnecessary_Use_Clause.Int_1 := 1;
      C : Pack2.Der1;
      D : Der2;
   begin
      A := A + 1;
      B := X_Unnecessary_Use_Clause."+" (B, 1);
      C := C + 1;
      D := D + 1;
   end;

   declare
      use type Pack2.Der1;                          -- Qualified: All uses qualified (operator case)
      V : Pack2.Der1;
   begin
      V := Pack2."+" (V, 1);
   end;

   declare
      use type Pack2.Der1;                          -- OK
      V : Pack2.Der1;
   begin
      V := V + 1;
   end;

   declare
      use type Pack2.Sub_Der1;                      -- Unused: unused (not primitive op)

      V : Pack2.Der1;

      function "-" (L, R : Pack2.Der1) return Pack2.Der1 is (L);
   begin
      V := V - 1;
   end;

   declare
      use Pack1;                                    -- Primitive: used for primitive operation (on access parameter)
      V : aliased Pack1.Tag1;
   begin
      Prim (V'Access);
   end;

   declare
      use all type Pack1.Int1, Pack1.Int2;          -- Operator: used for operators x2
      use type Pack1.Flt;                           -- OK
      V1 : Pack1.Int1;
      V2 : Pack1.Int2;
      V3 : Pack1.Flt;
   begin
      V1 := V1 + 1;
      V2 := V2 + 1;
      V3 := 0.5 * V3;
   end;

   declare
      use all type Pack1.Int1;                      -- Operator: used for operators
      use all type Pack1.Int2;                      -- Operator: used for operators
      V1 : Pack1.Int1;
      V2 : Pack1.Int2;
   begin
      V1 := V1 + 1;
      V2 := V2 + 1;
   end;

   declare
      use Pack1;                                    -- Operator: used for operators x2
      V1 : Pack1.Int1;
      V2 : Pack1.Int2;
   begin
      V1 := V1 + 1;
      V2 := V2 + 1;
   end;

   declare                                          -- Primitive declared in instantiation
      generic
      package Gen is
         type T is new Integer;
         function "and" (L, R : Integer) return T;
      end Gen;
      package body Gen is
         function "and" (L, R : Integer) return T is
         begin
            return T (L + R);
         end "and";
      end Gen;
      package Pack2 is new Gen;

      use all type Pack2.T;                         -- Operator:  used for operators
      V : Pack2.T;
      I : Integer;
      B : Boolean;
   begin
      V := I and I;
   end;

   declare                                          -- case of use package within scope of use type
      use type Pack1.Int1;
      V1 : Pack1.Int1;
      use Pack1;                                    -- Primitive: used for primitive operation
   begin
      V1 := V1 + 1;
      Prim (V1);
   end;

   declare                                          -- Use type of class-wide type
      V1     : Pack1.Tag1;
      V2, V3 : Pack1.Tag1'Class := V1;

      use all type PACK1.TAG1'CLASS;
      use all type PAck1.Tag1;
   begin
      if V3 = V2 then
         null;
      end if;
      if Pack1.Tag1 (V3) = (null record) then
         null;
      end if;
   end;

   ----------------- Special cases that created false positives in the past

   declare     -- SF 000055
      package My_Math is
         type Real is digits 15 range -1.79E308 .. 1.79E308;
      end My_Math;

      package Lem is
         subtype Real is My_Math.Real;
         use type My_Math.Real;

         Max_Speed : constant := 10_000.0;
         type Speed_Range is new Real range -Max_Speed .. +Max_Speed;
      end Lem;
   begin
      null;
   end;

   ----------------- These are to check fixes, insertion of use [all] type
   declare
      use PACK1;                 -- Primitive: "use" clause for Pack1 used for operators
      P : Pack1.Int1;
   begin
      P := P + 1;
   end;

   declare
      use pack1;                 -- Primitive: "use" clause for Pack1 used for primitive operations
      P : Pack1.Int1;
   begin
      P := -1;
      Prim (P);
   end;

   declare
      use Pack1;                 -- Primitive: "use" clause for Pack1 used for primitive operations x2
      P : Pack1.Int1;
      V : aliased Pack1.Tag1;
   begin
      Prim (P);
      Prim (V'Access);
   end;

   declare
      use Ada.Assertions, Pack1; -- Primitive: "use" clause for Pack1 used for primitive operations x2
      P : Pack1.Int1;
      V : aliased Pack1.Tag1;
   begin
      Prim (P);
      Prim (V'Access);
      Assert (True);
   end;

   declare
      use Pack1;                 -- Operator: "use" clause for Pack1 used for operators
      P : Pack1.Int1;
   begin
      if P >= 1 then
         null;
      end if;
   end;

   declare
      use Pack1;                 -- Operator: "use" clause for Pack1 used for operators x2
      P : Pack1.Int1;
      Q : Pack1.Int2;
   begin
      P := -1;
      if Q >= 1 then
         null;
      end if;
   end;

   declare
      use Pack1;                 -- Operator: "use" clause for Pack1 used for operators, Primitive: "use" clause for Pack1 used for primitive operations
      P : Pack1.Int1;
      Q : Pack1.Int2;
   begin
      Prim (P);
      if Q >= 1 then
         null;
      end if;
   end;

   declare
      use Pack1, Pack2, Pack3;   -- Unused: unused (x3) (for multiple fixes)
      use Pack1, Pack2;          -- Nested: in scope of use clause for same package (x2) (for multiple fixes)
   begin
      null;
   end;

end Use_Type;
