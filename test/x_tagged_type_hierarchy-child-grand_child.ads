package X_Tagged_Type_Hierarchy.Child.Grand_Child is
      type Child3 is new Child2  with null record;   -- OK
      type Child4 is new Parent1 with null record;   -- Single_Tagged_Type, Tagged_Type_Hierarchy
      type Child5 is new Parent2 with null record;   -- Single_Tagged_Type
end X_Tagged_Type_Hierarchy.Child.Grand_Child;
