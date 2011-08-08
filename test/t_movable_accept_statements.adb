procedure T_Movable_Accept_Statements is

   type Digit is range 0..9;

   task type Simple_Task_Type is
      entry Inner_Return_Entry;
      entry Declare_Entry (P : in out Digit);
      entry User_Defined_Dependent_Function_Entry;
      entry User_Defined_Dependent_Procedure_Entry;
   end Simple_Task_Type;
   task body Simple_Task_Type is
      B : Boolean;
      D : Digit;
      N : Natural;
      I : Integer;

      function Integer_Five return Integer is
      begin
         return 5;
      end Integer_Five;

      procedure Integer_Five (I : out Integer) is
      begin
         I := 5;
      end Integer_Five;

   begin
      accept Inner_Return_Entry do
         B := True;                                     -- should not trigger (reference follows in unmovable `if')
         D := Digit'First;                              -- should trigger (no dependency)
         if B then                                      -- should not trigger
            return;                                     --    ('inner exclusive')
         end if;
         B := False;                                    -- should trigger (below 'inner exclusive')
      end Inner_Return_Entry;

      accept Declare_Entry (P : in out Digit) do
         N := Natural'Last;                             -- should trigger (no dependency)
         D := Digit'First;                              -- should not trigger (referenced follows in unmovable `declare')
         B := False;                                    -- should not trigger (referenced follows in unmovable `declare')
         declare                                        -- should not trigger (explicit reference)
            procedure Assign (Left : out Digit;
                              Right : in Digit)
            is
            begin
               Left := Right;
            end Assign;
         begin
            Assign (P, D);                              -- explicit reference is here
            B := True;
         end;
         N := Natural'First;                            -- should trigger (below last parameter reference)
      end Declare_Entry;

      accept User_Defined_Dependent_Function_Entry do
         D := Digit'Last;                               -- should trigger (no dependency)
         I := Integer_Five;                             -- should not trigger (user-defined dependency)
         N := Natural (I) + 1;                          -- should trigger (below user-defined dependency)
      end User_Defined_Dependent_Function_Entry;

      accept User_Defined_Dependent_Procedure_Entry do
         D := Digit'Last;                               -- should trigger (no dependency)
         Integer_Five (I);                              -- should not trigger (user-defined dependency)
         N := Natural (I) + 1;                          -- should trigger (below user-defined dependency)
      end User_Defined_Dependent_Procedure_Entry;
   end Simple_Task_Type;

   task Separate_Task is
      entry Out_Parameter (X : out Boolean);
   end Separate_Task;
   task body Separate_Task is separate;

   -- A_Procedure_Body_Stub (for renamings)
   procedure Renaming is separate;

begin
   null;
end T_Movable_Accept_Statements;
