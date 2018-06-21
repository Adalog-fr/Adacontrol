separate (T_Expressions)
procedure Test_Dereference is
   type As is access String;
   V_As : As;

   type Rec is
      record
         I : Integer;
      end record;
   type Ar is access Rec;
   V_Ar : Ar;

   type Ap is access procedure (Ptr : access String);
   V_Ap : Ap;

   type Af is access function (X : Integer) return Integer;
   V_Af : Af;

   procedure P (Ptr : access String) is
   begin
      Ptr     (1) := 'a';        -- Implicit dereference
      Ptr.all (1) := 'a';        -- Explicit dereference
   end P;

   package Obj is
      type T is tagged null record;
      function F (X : T) return Integer;
   end Obj;
   package body Obj is
      function F (X : T) return Integer is begin return 0; end;
   end Obj;
   use Obj;

   I : Integer;
   V : aliased Obj.T;
   Ptr : access Obj.T := V'Access;

   function Object_Method return Integer renames Ptr.F; -- Implicit dereference

begin
   V_As     (1) := 'a';          -- Implicit dereference
   V_As.all (1) := 'a';          -- Explicit dereference
   V_As     (1..2) := "ab";      -- Implicit dereference, slice, universal_range
   V_As.all (1..2) := "ab";      -- Explicit dereference, slice, universal_range
   V_Ar.    I := V_Af (1);       -- Implicit dereference x2, range Function_Call, Function_Call, Dynamic_Function_Call
   V_Ar.all.I := V_Af.all (1);   -- Explicit dereference x2, range Function_Call, Function_Call, Dynamic_Function_Call
   V_Ap     (V_As);              -- Implicit dereference
   V_Ap.all (V_As);              -- Explicit dereference

   I := V_As    'First;          -- Implicit dereference
   I := V_As.all'First;          -- Explicit dereference
   I := V_As    'Component_Size; -- Implicit dereference
   I := V_As.all'Component_Size; -- Explicit dereference

   I := Ptr.F;                   -- Implicit dereference,, range Function_Call, Function_Call
end Test_Dereference;
