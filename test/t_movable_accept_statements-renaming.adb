separate (T_Movable_Accept_Statements)
procedure Renaming is

   -- A_Task_Type_Declaration
   -- A dummy task type with renamings
   -- This one explores simple object renaming
   task type Task_With_Simple_Renamed_Entities is
      entry No_Parameter;
      entry One_Parameter (P : in out Digit);
   end Task_With_Simple_Renamed_Entities;
   -- A_Task_Body_Declaration
   task body Task_With_Simple_Renamed_Entities is
      D1, D2: Digit;
      D : Digit renames D1;
   begin
      -- An_Accept_Statement (without body)
      accept No_Parameter;

      -- An_Accept_Statement (with a body)
      accept No_Parameter do
         D1 := 1;                                               -- should trigger (no parameter, no dependency)
      end No_Parameter;

      -- An_Accept_Statement (with a body)
      accept One_Parameter (P : in out Digit) do
         D2 := 2;                                               -- should trigger
         D  := 1;                                               -- should not trigger (implicit dependency with renaming)
         P  := D1;                                              -- should not trigger (explicit dependency)
         D1 := 1;                                               -- should trigger (below last parameter reference)
      end One_Parameter;
   end Task_With_Simple_Renamed_Entities;


   -- A_Task_Type_Declaration
   -- A simple task type with renamings
   -- This one explores record components renamings
   task Task_With_Renamed_Record_Components is
      entry One_Parameter (P : in out Digit);
   end Task_With_Renamed_Record_Components;
   -- A_Task_Body_Declaration
   task body Task_With_Renamed_Record_Components is
      -- A simple record declaration
      type Simple_Record is
         record
            B : Boolean;
            D : Digit;
         end record;

      R1, R2, R3 : Simple_Record;
      R1_B       : Boolean renames R1.B;
      R2_D       : Digit renames R2.D;
      B          : Boolean;
   begin
      -- An_Accept_Statement (with a body)
      accept One_Parameter (P : in out Digit) do
         R1.D := Digit'First;                                   -- should not trigger (implicit dependency)
         R1_B := True;                                          -- should not trigger (implicit dependency with renaming)

         R2 := Simple_Record'(B => True, D => Digit'First);     -- should trigger (no dependency)

         if not R1.B then                                       -- should not trigger (explicit reference)
            P := 2;
         end if;

         B := False;                                            -- should trigger (below last parameter reference)
         R3 := Simple_Record'(B => B, D => R2_D);               -- should trigger (below last parameter reference)
      end One_Parameter;
   end Task_With_Renamed_Record_Components;


   -- A_Task_Type_Declaration
   -- A tricky task type with renamings
   -- This one explores multiple renaming
   --   (f.e. multiple renamings through simple objects,
   --         multiple renamings through record components)
   task type Task_With_Multiple_Renamed_Entities is
      entry Check;
      entry Synchronize;
      entry Init_With_Default_Value (P : in out Digit);
   end Task_With_Multiple_Renamed_Entities;
   -- A_Task_Body_Declaration
   task body Task_With_Multiple_Renamed_Entities is
      -- record types used for renamings
      type Dummy_Record is
         record
            D : Digit;
         end record;
      type Complex_Record is
         record
            D  : Digit;
            DR : Dummy_Record;
         end record;
      type Tricky_Record is
         record
            D  : Digit;
            CR : Complex_Record;
            DR : Dummy_Record;
         end record;

      D       : Digit;
      DR      : Dummy_Record;
      CR      : Complex_Record;
      TR      : Tricky_Record;
      TR_D    : Digit renames TR.D;                     -- renaming
      TR_CR   : Complex_Record renames TR.CR;           -- renaming
      TR_CR_D : Digit renames TR_CR.D;                  -- renaming a renames
   begin
      -- An_Accept_Statement (without body)
      accept Check;

      -- An_Accept_Statement (with a body)
      accept Init_With_Default_Value (P : in out Digit) do
         TR.DR.D := P;                                  -- should not trigger (explicit dependency)
         TR_CR_D := Digit'First;                        -- should not trigger (implicit dependency with renaming)

         D := Digit'First;                              -- should trigger (no dependency)

         DR   := Dummy_Record'(D => Digit'Last);        -- should not trigger (implicit dependency)
         DR.D := Digit'Last;                            -- should not trigger (implicit dependency)

         CR   := Complex_Record'(D  => Digit'First,     -- should not trigger (implicit dependency)
                                 DR => DR);
         CR.D := P;                                     -- should not trigger (explicit dependency)

         P := Digit'Last;                               -- should not trigger (explicit dependency)

         D := Digit'First;                              -- should trigger (below last parameter reference)
         TR_D := D;                                     -- should trigger (below last parameter reference)
      end Init_With_Default_Value;

      -- An_Accept_Statement (with a body)
      accept Check do
         D := Digit'Last;                               -- should trigger (no parameter, no dependency)
         accept Synchronize;                            -- should not trigger (unmovable statement, synchronization)
         D := Digit'First;                              -- should trigger (no parameter, no dependency)
      end Check;

      -- An_Accept_Statement (with a body)
      accept Check do
         TR_CR := Complex_Record'(D => D, DR => DR);    -- should trigger (no parameter, no dependency)
         requeue Synchronize;                           -- should not trigger (unmovable statement, synchronization)
         D := Digit'First;                              -- should trigger (below exclusive statement)
      end Check;

      -- An_Accept_Statement (with a body)
      accept Check do
         return;                                        -- should not trigger (unmovable statement)
         D := Digit'First;                              -- should trigger (below exclusive statement)
      end Check;
   end Task_With_Multiple_Renamed_Entities;

begin
   null;
end Renaming;
