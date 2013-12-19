package X_Tagged_Type_Hierarchy is
   type Parent1 is tagged null record;
   type Child1 is new Parent1 with null record;   -- Single_Tagged_Type, Tagged_Type_Hierarchy
end X_Tagged_Type_Hierarchy;
