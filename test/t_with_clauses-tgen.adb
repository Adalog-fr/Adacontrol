with X_With_Clauses_Gen;
with X_With_Clauses_1;     -- Unnecessary (not necessary for default of instantiation)
with X_With_Clauses_2;     -- OK (used for <> default of instantiation)
separate (T_With_Clauses)
procedure Tgen is
   use X_With_Clauses_2;
   package Inst is new X_With_Clauses_Gen (1);
begin
   null;
end TGen;
