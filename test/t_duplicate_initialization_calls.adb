procedure T_Duplicate_Initialization_Calls is
   procedure In_Proc (P1 : Integer; P2 : Character) is
   begin
      null;
   end In_Proc;

   procedure Out_Proc (P1 : out Integer; P2 : Character) is
   begin
      null;
   end Out_Proc;

   procedure Other_Out_Proc (P1 : out Integer; P2 : Character) renames Out_Proc;
   I1, I2 : Integer;
   C : Character;

   T1 : array (1..10) of Integer;
begin
   In_Proc (1, 'a');
   In_Proc (1, 'b');
   In_Proc (2, 'a');
   In_Proc (1, 'a');          -- duplicate
   In_Proc (I1, C);           -- non static (x 2)

   Out_Proc (I1, 'a');
   Out_Proc (I2, 'a');
   Out_Proc (I1, 'b');        -- duplicate
   Other_Out_Proc (I2, 'c');  -- duplicate

   Out_Proc (T1 (1), 'a');
   Out_Proc (T1 (2), 'b');
   Out_Proc (T1 (3 - 2), 'c');  -- duplicate
end T_Duplicate_Initialization_Calls;
