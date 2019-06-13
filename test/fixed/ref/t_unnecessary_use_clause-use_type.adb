with X_Unnecessary_Use_Clause;
with Ada.Assertions;
separate (T_Unnecessary_Use_Clause)
package body Use_Type is
                            -- Nested: Use type in scope of use package clause
               -- Qualified: All uses qualified

            -- Nested: In scope of use type clause

   type Local_Int1 is range 1 .. 10;
                                -- Nested: useless (type not in package spec)

   type Local_Int2 is range 1 .. 10;
                            -- Nested: useless (type not in package spec)

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
                                 -- Nested: useless (type not in package spec)
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
                                      -- Nested: type not in package spec

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
                                -- Qualified: All uses qualified (operator case)
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
                            -- Unused: unused (not primitive op)

      V : Pack2.Der1;

      function "-" (L, R : Pack2.Der1) return Pack2.Der1 is (L);
   begin
      V := V - 1;
   end;

   declare
      
      use all type T_unnecessary_use_clause.Use_Type.Pack1.Tag1;                                    -- Primitive: used for primitive operation (on access parameter)
      V : aliased Pack1.Tag1;
   begin
      Prim (V'Access);
   end;

   declare
      
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int2;
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int1;          -- Operator: used for operators x2
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
      
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int1;                      -- Operator: used for operators
      
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int2;                      -- Operator: used for operators
      V1 : Pack1.Int1;
      V2 : Pack1.Int2;
   begin
      V1 := V1 + 1;
      V2 := V2 + 1;
   end;

   declare
      
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int1;
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int2;                                    -- Operator: used for operators x2
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

      
      use type T_unnecessary_use_clause.Use_Type._anonymous_.Pack2.T;                         -- Operator:  used for operators
      V : Pack2.T;
      I : Integer;
      B : Boolean;
   begin
      V := I and I;
   end;

   declare                                          -- case of use package within scope of use type
      use type Pack1.Int1;
      V1 : Pack1.Int1;
      
      use all type T_unnecessary_use_clause.Use_Type.Pack1.Int1;                                    -- Primitive: used for primitive operation
   begin
      V1 := V1 + 1;
      Prim (V1);
   end;

   ----------------- These are to check fixes, insertion of use [all] type
   declare
      
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int1;                 -- Primitive: "use" clause for Pack1 used for operators
      P : Pack1.Int1;
   begin
      P := P + 1;
   end;

   declare
      
      use all type T_unnecessary_use_clause.Use_Type.Pack1.Int1;                 -- Primitive: "use" clause for Pack1 used for primitive operations
      P : Pack1.Int1;
   begin
      P := -1;
      Prim (P);
   end;

   declare
      
      use all type T_unnecessary_use_clause.Use_Type.Pack1.Int1;
      use all type T_unnecessary_use_clause.Use_Type.Pack1.Tag1;                 -- Primitive: "use" clause for Pack1 used for primitive operations x2
      P : Pack1.Int1;
      V : aliased Pack1.Tag1;
   begin
      Prim (P);
      Prim (V'Access);
   end;

   declare
      use Ada.Assertions;
      use all type T_unnecessary_use_clause.Use_Type.Pack1.Int1;
      use all type T_unnecessary_use_clause.Use_Type.Pack1.Tag1; -- Primitive: "use" clause for Pack1 used for primitive operations x2
      P : Pack1.Int1;
      V : aliased Pack1.Tag1;
   begin
      Prim (P);
      Prim (V'Access);
      Assert (True);
   end;

   declare
      
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int1;                 -- Operator: "use" clause for Pack1 used for operators
      P : Pack1.Int1;
   begin
      if P >= 1 then
         null;
      end if;
   end;

   declare
      
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int1;
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int2;                 -- Operator: "use" clause for Pack1 used for operators x2
      P : Pack1.Int1;
      Q : Pack1.Int2;
   begin
      P := -1;
      if Q >= 1 then
         null;
      end if;
   end;

   declare
      
      use type T_unnecessary_use_clause.Use_Type.Pack1.Int2;
      use all type T_unnecessary_use_clause.Use_Type.Pack1.Int1;                 -- Operator: "use" clause for Pack1 used for operators, Primitive: "use" clause for Pack1 used for primitive operations
      P : Pack1.Int1;
      Q : Pack1.Int2;
   begin
      Prim (P);
      if Q >= 1 then
         null;
      end if;
   end;
end Use_Type;
