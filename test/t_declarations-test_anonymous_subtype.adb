separate (T_Declarations)
procedure Test_Anonymous_Subtype is            -- not library procedure, local_procedure
   subtype Int is Integer range 1 .. 10;       -- subtype
   subtype IntOK is Integer;                   -- subtype, unconstrained_subtype
   subtype Ws is Wide_String (1 .. 200);       -- subtype, anonymous_subtype_declaration
   subtype WsOK is Wide_String (Int);          -- subtype
   I : constant Int range 1 .. 5 := 3;         -- constant, anonymous_subtype_declaration
   IOK : constant Int := 3;                    -- constant
   V1 : Wide_String (1 .. 200);                -- variable, constrained_array_variable, array, uninitialized_variable, anonymous_subtype_declaration (x2)
   V2 : Wide_String (Natural range 1 .. 200);  -- variable, constrained_array_variable, array, uninitialized_variable, anonymous_subtype_declaration (x2)
   V3 : Wide_String (V2'Range);                -- variable, constrained_array_variable, array, uninitialized_variable, anonymous_subtype_declaration (x2)
   V4  : Wide_String (Int);                    -- variable, constrained_array_variable, array, uninitialized_variable, anonymous_subtype_declaration
   VOK : WS;                                   -- variable, constrained_array_variable, array, uninitialized_variable
   type T is access Integer range 1 .. 10;     -- access_type, anonymous_subtype_declaration
   type Int2 is new Integer range 1..10;       -- derived_type, anonymous_subtype_declaration
   type Rec (D : Integer) is                   -- ordinary_record_type, record_type, discriminant
      record
         C : Integer range 0 .. 100;           -- uninitialized_record_component, anonymous_subtype_declaration
      end record;
   type Acc_Rec is access Rec;                 -- access_nondef_discriminated_type, access_type
   Acc : Acc_Rec;                              -- variable, uninitialized_variable

   J : Integer;                                -- variable, scalar_variable, uninitialized_variable

   --
   -- Anonymous access
   --

   type T_Proc is access procedure (B : access Integer);          -- access_subprogram_type, access_type, anonymous_access_parameter

   AA_A : access Integer;                                         -- Variable, Uninitialized_Variable, Anonymous_access_variable
   AA_B : constant access Integer := new Integer'(1);             -- Constant, Anonymous_access_constant
   AA_C : array (1..10) of access procedure;                      -- Variable, Single_Array, Constrained_Array_Variable, Array, Uninitiaized_Variable, Anonymous_access_component, Anonymous_Subtype_Declaration

   type AA_T is array (1..10) of access function return Integer;  -- Constrained_Array_Type, Array, Anonymous_access_component, Anonymous_Subtype_Declaration
   type AA_U (D : access Integer) is                              -- Ordinary_Record_Type, Record_Type, Discriminant, Anonymous_Access_Discriminant
      record
         F : access Integer;                                      -- Uninitialized_Record_Component, Anonymous_access_component
      end record;

   procedure AA_P (X : access Integer) is                         -- Not Library Procedure, Local_Procedure, no_spec_procedure, Anonymous_access_parameter
   begin
      null;                                                       -- null_procedure_body, Null_Procedure
   end AA_P;

   generic                                        -- not library generic_procedure, generic
      C : access Integer;                         -- anonymous_access_constant
      V : in out access Integer;                  -- in_out_generic_parameter, anonymous_access_variable
      F : access function return Integer;         -- anonymous_access_constant
      G : access procedure (X : access Integer);  -- anonymous_access_constant, anonymous_access_parameter
   procedure P;
   procedure P is begin null; end;             -- null_procedure_body, null_procedure

   function F (X : access Integer) return access Integer;    -- anonymous_access_parameter
   function F (X : access Integer) return access Integer is  -- OK (message on spec)
   begin
      return X;
   end F;

   function G(X : access Integer) return access Integer renames F;  -- anonymous_access_parameter

   generic                                                   -- not library generic_function, generic
   function H (X : access Integer) return access Integer;    -- anonymous access_parameter
   function H (X : access Integer) return access Integer is  -- OK (message on spec)
   begin
      return new Integer'(1);
   end H;

begin
   for I in Natural range 1 .. 10 loop         -- anonymous_subtype_for
      null;
   end loop;
   for I in 1 .. 10 loop                       -- anonymous_subtype_for
      null;
   end loop;
   for I in Int loop
      null;
   end loop;

   case J is
      when Int =>
         null;
      when 11 .. 20 =>                         -- anonymous_subtype_case
         null;
      when Integer range 21 .. 30 =>           -- anonymous_subtype_case
         null;
      when others =>
         null;
   end case;

   J := (case J is
            when Int =>
               0,
            when 11 .. 20 =>                   -- anonymous_subtype_case
               1,
            when Integer range 21 .. 30 =>     -- anonymous_subtype_case
               2,
            when others =>
               J
         );

   V1 (1..10) := (others => 'a');              -- anonymous_subtype_indexing
   V1 (Int)   := (others => 'a');              --
   V2 (Natural range 1..10) := (1..10 => 'a'); -- anonymous_subtype_indexing (x2)

   Acc := new Rec (2);                         -- anonymous_subtype_allocator
end Test_Anonymous_Subtype;
