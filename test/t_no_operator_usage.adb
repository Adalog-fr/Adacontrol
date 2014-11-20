with X_No_Operator_Usage;
procedure T_No_Operator_Usage is
   type T1 is mod 10;                     -- Unsigned all info, Only ops, rel+log+inx
   type T2 is new T1;
   V1 : T1;
   V2 : T2;
   VX2 : X_No_Operator_Usage.Internal.X2;
   VX4 : X_No_Operator_Usage.X4;

   use type X_No_Operator_Usage.X4;
begin
   declare
      type T11 is range 1 .. 10;          -- Signed all info, Only ops, norel+nolog+noinx
      type T12 is new T1;                 -- Unsigned all info, Only ops, rel+nolog+noinx

      V11 : T11;
      V12 : T12;
   begin
      if V1 > 1 then
         null;
      end if;
      if V12 in 5..7 then
         null;
      end if;
   end;

   V1 := V1 and 1;
   V2 := V2 + 1;

   VX2 := X_No_Operator_Usage.Internal."+" (VX2, 1);
   VX4 := abs VX4;

   declare
      type T13 is range 1 .. 10;
      type T14 is new T13;                 -- Signed all info, Only ops
      subtype ST13 is T13 range 1 .. 5;
      function "+" (L, R : ST13) return  ST13 is
      begin
         return L;
      end "+";

      type Arr_1_13 is array (T1) of Character;
      V13 : array (T14) of T2;

      VST13 : ST13;
   begin
      VST13 := VST13 + 1;
   end;
end T_No_Operator_Usage;
