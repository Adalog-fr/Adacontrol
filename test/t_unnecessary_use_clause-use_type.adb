with X_Unnecessary_Use_Clause;
separate (T_Unnecessary_Use_Clause)
package body Use_Type is
   use type Ada.Text_IO.Count;                         -- Nested: Use type in scope of use package clause
   use type X_Unnecessary_Use_Clause.Int_1;            -- Qualified: All uses qualified

   procedure Proc is
      use type X_Unnecessary_Use_Clause.Int_1;         -- Nested: In scope of use type clause, Qualified: all uses qualified

      type Local_Int1 is range 1 .. 10;
      use type Local_Int1;                             -- Nested: useless (type not in package spec)

      type Local_Int2 is range 1 .. 10;
      use all type Local_Int2;                         -- Nested: useless (type not in package spec)

      package Pack1 is
         subtype Local_Sub is Local_Int1 range 1 .. 2;

         type Int1 is range 1 .. 10;
         type Int2 is range 1 .. 10;

         type Tag1 is tagged null record;
         procedure Prim (X : access Tag1);             -- (Primitive by access parameter)
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
         use type Pack1.Int1;                           -- Movable: can be moved to body
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
         use type Pack2.Sub_Der1;                          -- Unused: unused (not primitive op)

         V : Pack2.Der1;

         function "-" (L, R : Pack2.Der1) return Pack2.Der1 is (L);
      begin
         V := V - 1;
      end;

      declare
         use Pack1;                                    -- Primitive: only used for primitive operation (on access parameter)
         V : aliased Pack1.Tag1;
      begin
         Prim (V'Access);
      end;

      declare
         use all type Pack1.Int1, Pack1.Int2;          -- Operator: only used for operators x2
         V1 : Pack1.Int1;
         V2 : Pack1.Int2;
      begin
         V1 := V1 + 1;
         V2 := V2 + 1;
      end;

      declare
         use all type Pack1.Int1;                      -- Operator: only used for operators
         use all type Pack1.Int2;                      -- Operator: only used for operators
         V1 : Pack1.Int1;
         V2 : Pack1.Int2;
      begin
         V1 := V1 + 1;
         V2 := V2 + 1;
      end;

      declare
         use Pack1;                                    -- Operator: only used for operators
         V1 : Pack1.Int1;
         V2 : Pack1.Int2;
      begin
         V1 := V1 + 1;
         V2 := V2 + 1;
      end;
   end Proc;

end Use_Type;
