with X_With_Clauses_1;      -- Regular
with X_With_Clauses_2;      -- Regular
generic
   V1 : Integer;
   V2 : Integer := X_With_Clauses_2.I;
   with procedure Proc1 is X_With_Clauses_1.Proc;
   with function F return Integer is <>;
package X_With_Clauses_Gen is
end X_With_Clauses_Gen;
