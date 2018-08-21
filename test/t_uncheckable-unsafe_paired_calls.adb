separate (T_Uncheckable)
procedure unsafe_paired_calls is
   procedure P is null;
   procedure V is null;

   package Object is
      type T is tagged null record;
      procedure P (X : T) is null;
      procedure V (X : T) is null;
   end Object;
   type D is new Object.T with null record;

   O :  D;
begin
   P (D'Class (O));  -- Dispatching call
   null;
   V (D'Class (O));  -- Dispatching call
exception
   when Constraint_Error =>
      V (D'Class (O));  -- Dispatching call
   when others =>
      P (D'Class (O));  -- Dispatching call
      V (D'Class (O));  -- Dispatching call
end unsafe_paired_calls;
