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

   T1 : array (1 .. 10) of Integer;

   type Rec is
      record
         I,J :Integer;
      end record;
   Cr1 : constant Rec := (I => 3, J => 4);
   Cr2 : constant Rec := (I => 3, J => 4);
   Ren1 : Integer renames Cr1.I;
   Ren2 : Integer renames Cr1.J;
   Ren3 : Integer renames Cr1.I;
   Ren4 : Integer renames Cr2.I;

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

   -- Case of renamings
   In_Proc (Ren1, 'a');         -- non static
   In_Proc (Ren2, 'a');         -- non static
   In_Proc (Ren3, 'a');         -- duplicate, non static
   In_Proc (Ren4, 'a');         -- non static
end T_Duplicate_Initialization_Calls;
