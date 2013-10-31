separate (T_Object_Declarations)
procedure Test_Type is
   type Int is new Integer;
   subtype Nat is Int range 0 .. Int'Last;

   package Pack1 is
      type Ttt is range 1 .. 10;
   end Pack1;
   package Pack2 is
      type Ttt is range 11 .. 20;
   end Pack2;
   use Pack2;
   type Tag is tagged null record;

   V1 : Pack1.Ttt;                 -- All_All Ttt
   V2 : Ttt;                       -- All_All Ttt
   C1 : constant Pack1.Ttt := 1;   -- Const_Pack1.Ttt
   C2 : constant Pack2.Ttt := 11;  -- All_All Ttt
   V3 : Int;                       -- Var_Int
   V4 : Int'Base;                  -- Var_Int
   V5 : Nat;                       -- Var_Int
   C4 : constant Nat := 0;         -- Const_Nat
   V6 : Nat;                       -- Var_Int
   V7 : Tag;                       -- All_Tag
   V8 : Tag'Class := Tag'Class (V7);
begin
   null;
end Test_Type;
