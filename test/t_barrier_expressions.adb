with Ada.Text_IO;

procedure T_Barrier_Expressions is

   -- Some type definitions necessary for tests
   subtype Index is Integer range 1 .. 26;
   GlobI     : Index;
   Ren_GlobI : Index renames GlobI;

   type Pointer is access Integer;
   type Enumeration is (Enum1, Enum2, Enum3);

   type Record_Type is tagged
      record
         I : Integer := 0;
      end record;
   type Extended_Record_Type is new Record_Type with
      record
         J : Integer := 0;
      end record;

   task type Task_Type;
   task body Task_Type is
   begin
      null;
   end Task_Type;
   T : Task_Type;

   type Function_Access is access function (I : in Integer) return Boolean;
   Pfunc : Function_Access;

   function Global return Boolean is
      B : Boolean;
      protected P is
         entry E;
      end P;
      protected body P is
         entry E when Global.B is            -- variable (not function call)
         begin
            null;
         end E;
      end P;
   begin
      return True;
   end Global;

   function Ren_Global return Boolean renames Global;

   protected Other_Prot is
      function F return Boolean;
      entry E;
   end Other_Prot;
   protected body Other_Prot is
      function F return Boolean is
      begin
         return True;
      end F;

      entry E when Other_Prot.F is         -- variable, local_function
      begin
         null;
      end E;
   end Other_Prot;

   protected type Check (Discr : Standard.Integer'Base) is
      entry Simple_Boolean;
      entry Errors;
      function Local (I : in Integer) return Boolean;

   private
      B1, B2, B3 : Boolean := False;
      I, J, K : Integer    := 0;
      F : Float            := 0.0;
      C : Character        := ' ';
      S : String (Index)   := (others => ' ');
      P : Pointer          := null;
      E : Enumeration      := Enum1;
      R : Record_Type;
      ER : Extended_Record_Type;
   end Check;

   External : Check (1);

   protected body Check is
      entry Simple_Boolean when B1 is                  -- Always OK
      begin
         null;
      end;

      entry Errors when
        (((B1)))                                      -- Parenthesized, OK
        or                                            -- logical_operator
          (I in 1 .. Natural'Last                     -- logical_operator, any_component, value_attribute
           and then                                   -- logical_operator
             (I + J ** K) mod I                       -- any_component x4, arithmetic_operator x3
           > 3                                        -- comparison_operator
          )
        or                                            -- logical_operator
          I = Integer (30.0)                          -- any_component, comparison_operator, conversion
        or                                            -- logical_operator
          I = Integer'(10)                            -- any_component, comparison_operator, conversion
        or                                            -- logical_operator
          Discr = 10                                  -- any_component, comparison_operator
        or                                            -- logical_operator
          P = new Integer'(10)                        -- any_component, comparison_operator, allocation, conversion
        or                                            -- logical_operator
          R.I > 0                                     -- any_component, comparison_operator
          or                                          -- logical_operator
            S (S'First) = '1'                         -- indexing, any_component, comparison_operator, value_attribute
          or                                          -- logical_operator
            S (S'First .. 3) = "abc"                  -- indexing, any_component, value_attribute, comparison_operator
          or                                          -- logical_operator
            Local (I)                                 -- local_function, any_component
          or                                          -- logical_operator
            Check.Local (I)                           -- local_function, any_component
          or                                          -- logical_operator
            External.Local (I)                        -- non_local_function_call, variable, any_component
          or                                          -- logical_operator
            Other_Prot.F                              -- non_local_function_call, variable
          or                                          -- logical_operator
            P.all /= 1                                -- dereference, any_component, comparison_operator
          or                                          -- logical_operator
            Pfunc.all (I)                             -- dereference, variable, any_component
          or                                          -- logical_operator
            Pfunc (J)                                 -- dereference, variable, any_component
          or                                          -- logical_operator
            Global                                    -- non_local_function_call
          or                                          -- logical_operator
            Ren_Global                                -- non_local_function_call
          or                                          -- logical_operator
            T'Terminated                              -- value_attribute
          or                                          -- logical_operator
            Integer'Succ (I) = 2                      -- function_attribute, any_component, comparison_operator
          or                                          -- logical_operator
            R  = Record_Type'(I => 10)                -- any_component, comparison_operator, conversion, record_aggregate
          or                                          -- logical_operator
            ER = Extended_Record_Type'(R with 10)     -- any_component x2, comparison_operator, conversion, record_extension
          or                                          -- logical_operator
            S = String'( 'a', 'b', 'c' )              -- any_component, comparison_operator, conversion, array_aggregate
          or                                          -- logical_operator
            S = String'(1 => 'a', 2 => 'b', 3 => 'c') -- any_component, comparison_operator, converson, array_aggregate
          or                                          -- logical_operator
            S = String'(1 .. Globi => ' ')            -- any_component, operator_comparison, conversion, array_aggregate, variable
          or                                          -- logical_operator
            S = String'(1 .. Ren_Globi => ' ')        -- any_component, operator_comparison, conversion, array_aggregate, variable
      is
      begin
         null;
      end;

      function Local (I : in Integer) return Boolean is
      begin
         return I = 0;
      end Local;
   end Check;

begin
   null;
end T_Barrier_Expressions;
