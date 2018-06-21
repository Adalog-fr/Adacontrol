procedure T_Max_Call_Depth is
   procedure Dont_Count is  -- Recursive, but forced
   begin
      Dont_Count;
   end Dont_Count;

   -- Simple cases
   procedure P1 is
   begin
      null;
   end P1;

   procedure P2 is
   begin
      P1;
   end P2;

   -- Check packages
   procedure P3 is
      package Pack is
      end Pack;
      package body Pack is
      begin
         P1;
      end Pack;
   begin
      null;
   end P3;

   procedure P4 is
      package Pack is
         procedure Proc;
      end Pack;
      package body Pack is
         procedure Proc is
         begin
            P1;
         end Proc;
      end Pack;
   begin
      null;
   end P4;

   -- Check recursivity
   procedure Recur1;
   procedure Recur2 is
   begin
      Recur1;         -- Recursive
   end Recur2;
   procedure Recur1 is
   begin
      Recur2;         -- Recursive
   end Recur1;

   -- Check functions and parameters
   function F1 return Integer is
   begin
      return 1;
   end F1;

   function F2 (X : Integer := F1) return Integer is
   begin
      return 1;
   end F2;

   function F3 return Integer is
   begin
      P2;                                 -- Depth = 2
      return 1;
   end F3;

   -- Check tasks
   task T1 is
      entry E;
   end T1;
   task body T1 is
   begin
      accept E do
         P2;                              -- Depth = 2
      end E;
   end T1;

   task type TT is
      entry E;
   end TT;
   task body TT is
   begin
      accept E do
         P2;                              -- Depth = 2
      end E;
   end TT;
   T2 : TT;

   -- Check interface SP
   procedure In_C;
   pragma Import (C, In_C);

   -- Check null procedure
   procedure Null_Proc is null;

   -- Check renamings
   function F4 return Integer renames F3;
   function F5 return Integer  renames F4;
   function F6 (X : Integer) return Integer renames Integer'Succ;
   function F7 return Integer; -- Renaming as body
   function F7 return Integer renames F5;
   type Enum is (Enum1, Enum2, Enum3);
   function Z return Enum renames Enum1;

   type Proc_Access is access procedure;
   Indirect : Proc_Access := P1'Access;
   procedure Ren_Indirect renames Indirect.all;

   -- Check use in declarations
   I : Integer;
   J : Integer := F3;                     -- Depth = 3
   K : constant Integer := F3;            -- Depth = 3
   subtype Int is Integer range 1 .. F3;  -- Depth = 3
   E : Enum;

   -- Check renamings, dynamic, renaming, and unknown cases
   package Obj is
      type T is tagged null record;
      procedure Prim (X : T);
   end Obj;
   package body Obj is
      procedure Prim (X : T) is
      begin
         null;
      end Prim;
   end Obj;
   use Obj;

   procedure Check_Unknown is
      O : Obj.T;
   begin
      In_C;
      Prim (T'Class (O));
      Indirect.all;
      Ren_Indirect;
   end Check_Unknown;

   -- Check expression_Functions
   function F8  return Integer is begin P1; return 1; end; -- Depth = 1
   function F9  return Integer is (F8);                    -- Depth = 2
   function F10 return Integer is (F9);                    -- Depth = 2 (since Count_Expr_Fun_Calls = Off)

begin
   -- Check use in statements
   Dont_Count;            -- Depth = 1
   P2;                    -- Depth = 2
   P3;                    -- Depth = 2
   P4;                    -- OK
   Recur1;                -- Recursive
   I := F2 (I);           -- OK
   I := F2 (F2(I));       -- OK
   I := F2;               -- OK
   I := F3;               -- Depth = 3
   I := F2(F3);           -- Depth = 3
   T1.E;                  -- OK
   T2.E;                  -- OK
   In_C;                  -- OK
   Null_Proc;             -- OK
   I := Integer'Succ (1); -- OK (Attribute)
   I := F4;               -- Depth = 3
   I := F5;               -- Depth = 3
   I := F6 (1);           -- OK (attribute)
   I := F7;               -- Depth = 3
   E := Enum1;            -- OK
   E := Z;                -- OK (enumeration literal)

   Check_Unknown;         -- Depth >= 2

   I := F10;              -- Depth = 2 (since Count_Expr_Fun_Calls = Off)
end T_Max_Call_Depth;
