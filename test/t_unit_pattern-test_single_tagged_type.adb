separate (T_Unit_Pattern)
procedure Test_Single_Tagged_Type is
   type Tag1 is tagged null record;
   type Tag2 is tagged null record;   -- OK, not in package

   package Pack1 is
      type Tag3 is tagged null record; -- OK
   end Pack1;

   package Pack2 is
      type Tag4 is tagged null record;
      type Tag5 is tagged null record;         -- Single_Tagged_Type
      type Tag6 is new Tag4 with null record;  -- Single_Tagged_Type, Tagged_Type_Hierarchy
      type Tag7 is new Tag5 with null record;  -- Single_Tagged_Type, Tagged_Type_Hierarchy
   end Pack2;

   package Pack3 is
      type Tag10 is new Pack2.Tag4 with null record;  -- Tagged_Type_Hierarchy
      type Tag11 is new Pack2.Tag4 with null record;  -- Single_Tagged_Type, Tagged_Type_Hierarchy
      type Tag12 is new Tag10 with null record;       -- Single_Tagged_Type, Tagged_Type_Hierarchy
   end Pack3;

   package body Pack2 is
      type Tag8 is tagged null record;         -- Single_Tagged_Type
      type Tag9 is new Tag4 with null record;  -- Single_Tagged_Type, Tagged_Type_Hierarchy
   end Pack2;

begin
   null;
end;
