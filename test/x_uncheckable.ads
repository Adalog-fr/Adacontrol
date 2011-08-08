-- This package provides typical dynamic constructs useful for other rules
-- to check their uncheckable conditions;
--## rule off all ## We don't want this package to pollute the reports of other rules
package X_Uncheckable is
   -- Dynamic objects
   type Ptr_Int is access Integer;
   Tab : array (1..10) of Integer;
   Dyn_Index : Positive := 5;

   Dyn_Var   : Integer renames Tab (Dyn_Index);
   Dyn_Ptr   : Ptr_Int;

   -- Dynamic subprograms
   type Ptr_Proc is access procedure;
   Dyn_Proc : Ptr_Proc;
   procedure Dyn_Ren_Proc renames Dyn_Proc.all;

   -- Dispatching calls
   type T_Tagged is tagged null record;
   procedure Dispatch (X : T_Tagged);

   Stat_Tagged : T_Tagged;
   Dyn_Tagged  : T_Tagged'Class := T_Tagged'Class (Stat_Tagged);
end X_Uncheckable;
