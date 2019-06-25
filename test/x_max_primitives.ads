package X_Max_Primitives is

   type Foo is tagged null record;
   type Bar is tagged null record;
   
   procedure P1 (P : Foo) is null;
   procedure P2 (P : Bar) is null;
   procedure P3 (F : Foo; B : Bar'Class) is null;
   
   function F1 (F : Foo; B : Bar'Class) return Boolean is (True);

end X_Max_Primitives;
