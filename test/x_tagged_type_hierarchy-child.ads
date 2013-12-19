package X_Tagged_Type_Hierarchy.Child is
   type Child2 is new Parent1 with null record;
   type Parent2 is tagged null record;            -- Single_Tagged_Type
end X_Tagged_Type_Hierarchy.Child;
