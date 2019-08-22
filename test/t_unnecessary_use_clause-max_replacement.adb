separate (T_Unnecessary_Use_Clause)
procedure Max_Replacement is
   package Pack1 is

      type P1_T1 is new Integer;
      type P1_T2 is new Integer;
      type P1_T3 is new Integer;
      type P1_T4 is new Integer;
      type P1_T5 is new Integer;
   end Pack1;
   
   use Pack1;

   V1_1 : Pack1.P1_T1;
   V1_2 : Pack1.P1_T2;
   V1_3 : Pack1.P1_T3;
   V1_4 : Pack1.P1_T4;
   V1_5 : Pack1.P1_T5;

   package Pack2 is

      type P2_T1 is new Integer;
      type P2_T2 is new Integer;
      type P2_T3 is new Integer;
      type P2_T4 is new Integer;
      type P2_T5 is new Integer;
      type P2_T6 is new Integer;
      
      type Tag1 is tagged null record;
      procedure Prim (X : access Tag1) is null;             -- (Primitive by access parameter)
   end Pack2;
   
   use Pack2;
   
   V2_1 : Pack2.P2_T1;
   V2_2 : Pack2.P2_T2;
   V2_3 : Pack2.P2_T3;
   V2_4 : Pack2.P2_T4;
   V2_5 : Pack2.P2_T5;
   
   package Pack3 is
      type Int is new Integer;
   end Pack3;

   use Pack3;                    -- Operator: Only used for operators
   V : Pack3.Int;
begin
   declare
      -- Unused: unused
   begin
      V := V + 1;
      
      V1_1 := V1_1 + 1;
      V1_2 := V1_2 + 1;
      V1_3 := V1_3 + 1;
      V1_4 := V1_4 + 1;
      V1_5 := V1_5 + 1;

      V2_1 := V2_1 + 1;
      V2_2 := V2_2 + 1;
      V2_3 := V2_3 + 1;
      V2_4 := V2_4 + 1;
      V2_5 := V2_5 + 1;
   end;
   
   declare
      use Pack2;                                    -- Primitive: used for primitive operation (on access parameter)
      V : aliased Pack2.Tag1;
   begin
      Prim (V'Access);
   end;
end Max_Replacement;
