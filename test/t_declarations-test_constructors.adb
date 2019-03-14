separate (T_declarations)
procedure Test_Constructors is                            -- not_library_procedure, local_procedure
   package X_Declarations_Constructors is                 -- not_library_package
      type T is tagged record                             -- tagged_type, record_type
         I : Integer;                                     -- uninitialized_record_component
      end record;

      function Fun1 (I : T) return T;
      function Fun2 (I : T) return Integer;
      function Fun3 (I : Integer) return T;               -- constructor
      function Fun4 return T;                             -- constructor
      function Bar  (J : Integer) return T renames Fun3;  -- renaming_as_declaration, renaming, constructor, not_operator_renaming, not_identical_renaming, synonym_renaming
      function Foo  (K : Integer) return T is ((I => K)); -- expression_function, constructor

      type D is new T with null record;                   -- null_extension, extension, tagged_type, record_type
      overriding function Fun3 (I : Integer) return D;    -- constructor
   end X_Declarations_Constructors;

   package body X_Declarations_Constructors is
      function Fun1 (I : T) return T is
      begin
         return (I);
      end Fun1;

      function Fun2 (I : T) return Integer is
      begin
         return I.I;
      end Fun2;

      function Fun3 (I : Integer) return T is
      begin
         return (I => I);
      end Fun3;

      function Fun4 return T is
      begin
         return (I => 1);
      end Fun4;

      overriding function Fun3 (I : Integer) return D is
      begin
         return (I => I);
      end Fun3;

   end X_Declarations_Constructors;

begin
   null;                                                  -- null_procedure_body, null_procedure
end Test_Constructors;
