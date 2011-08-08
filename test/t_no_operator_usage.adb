with X_No_Operator_Usage;
procedure T_No_Operator_Usage is
   type T1 is mod 10;                     -- logical operators
   type T2 is new T1;
   V1 : T1;
   V2 : T2;
   VX2 : X_No_Operator_Usage.Internal.X2;
   VX4 : X_No_Operator_Usage.X4;

   use type X_No_Operator_Usage.X4;
begin
   declare
      type T11 is range 1 .. 10;          -- no operators
      type T12 is new T1;                 -- no operators

      V11 : T11;
      V12 : T12;
   begin
      null;
   end;

   V1 := V1 and 1;
   V2 := V2 + 1;

   VX2 := X_No_Operator_Usage.Internal."+" (VX2, 1);
   VX4 := abs VX4;
end T_No_Operator_Usage;
