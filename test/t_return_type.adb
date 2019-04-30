with T_Return_Type.Anonymous_Access; -- To force inclusion into analyzed units
package body T_Return_Type is

   ------------------------------------------------------------
   -- Checking functions specification and body declarations --
   ------------------------------------------------------------

   subtype Another_String is String;
   subtype A_Constrained_String1 is Another_String (1..10);
   subtype A_Constrained_String2 is A_Constrained_String1;

   type Yet_Another_String is new String;

   -- A_Function_Declaration
   -- returning a statically constrained type
   function To_Integer (S : String) return Integer;                             -- OK
   -- A_Function_Body_Declaration (with defined specification)
   function To_Integer (S : String) return Integer is
   begin
      return Integer'Value (S);
   end To_Integer;

   -- A_Function_Declaration
   -- returning an unconstrained subtype
   function To_String (I : Integer) return Another_String;                      -- unconstrained_array
   -- A_Function_Body_Declaration (with defined specification)
   function To_String (I : Integer) return Another_String is                    -- OK (explicit specification)
   begin
      return Integer'Image (I);
   end To_String;

   -- A_Function_Body_Declaration (without defined specification)
   -- returning a statically constrained subtype
   function To_Spaces_Fill_String return A_Constrained_String2 is               -- constrained_array
   begin
      return (others => ' ');
   end To_Spaces_Fill_String;

   -- A_Function_Body_Declaration (without defined specification)
   -- returning an unconstrained type
   function Hello_World return Yet_Another_String is                            -- unconstrained_array
   begin
      return "Hello world!";
   end Hello_World;

   N : Integer;
   type Constrained_Array is array (Integer range 1..N) of Integer;
   -- A_Function_Body_Declaration (without defined specification)
   -- returning a non-statically constrained type
   function Constrained_Function (Length : Integer) return Constrained_Array is -- constrained_array
      Tab : Constrained_Array;
   begin
      for I in 1..N loop
         Tab(I) := I;
      end loop;
      return Tab;
   end Constrained_Function;


   ---------------------------------
   -- Checking separate functions --
   ---------------------------------

   -- A_Function_Body_Stub (specification with separate body)
   -- returning a non-statically constrained type
   function Empty_String return String is separate;                             -- unconstrained_array


   --------------------------
   -- Checking named types --
   --------------------------

   procedure Named_Types is separate;

   --------------------------------
   -- Checking generic functions --
   --------------------------------

   -- A_Generic_Function_Declaration
   -- returning a discrete type (constrained)
   generic
      type Generic_Type_1 is range <>;
   function Generic_Function_1 (V : Generic_Type_1) return Generic_Type_1;      -- OK
   -- A_Generic_Function_Body_Declaration
   function Generic_Function_1 (V : Generic_Type_1) return Generic_Type_1 is    -- OK (explicit specification)
   begin
      return V;
   end Generic_Function_1;

   -- A_Generic_Function_Declaration
   generic
      type Generic_Type_2 is array (Positive range <>) of Character;
   function Generic_Function_2 (V : Generic_Type_2) return Generic_Type_2;      -- unconstrained_array
   -- A_Generic_Function_Body_Declaration
   function Generic_Function_2 (V : Generic_Type_2) return Generic_Type_2 is    -- OK (explicit specification)
   begin
      return V;
   end Generic_Function_2;


   -- dummy stuff
   type Digit is new Integer range 0..9;


   -- A_Function_Instantiation
   -- returning a discrete type
   function Digit_Function is new Generic_Function_1 (Digit);                   -- OK
   -- A_Function_Instantiation
   -- returning a unconstrained array type
   function String_Function is new Generic_Function_2 (String);                 -- unconstrained_array


   -- A_Generic_Package
   generic
      type Generic_Type_3 is range <>;
   package Generic_Package is
      type Integer_Array is array (Generic_Type_3) of Integer;
      -- A_Function_Declaration (within A_Generic_Package)
      -- returning a non-statically constrained type
      function Dup_Function (V : Integer_Array) return Integer_Array;           -- constrained_array
   end Generic_Package;

   -- A_Generic_Package_Body
   package body Generic_Package is
      -- A_Function_Body_Declaration
      function Dup_Function (V : Integer_Array) return Integer_Array is         -- OK (explicit specification)
      begin
         return V;
      end Dup_Function;
   end Generic_Package;


   ---------------------------------
   -- Checking functions renaming --
   ---------------------------------

   -- A_Function_Renaming_Declaration
   -- returning a non-statically constrained type
   function To_String_Renaming (I : Integer) return String renames To_String;   -- unconstrained_array


   ---------------------------------------------------------
   -- Checking functions returning An_Attribute_Reference --
   ---------------------------------------------------------

   -- A_Function_Body_Declaration (without defined specification)
   -- returning An_Attribute_Reference . A_Base_Attribute
   function Base_Attribute_Reference_Function return Digit'Base is              -- OK (Prefix is constrained)
   begin
      return 5;
   end Base_Attribute_Reference_Function;

   -- Defining a tagged record type
   type Tagged_Type is tagged
      record
         I : Integer;
      end record;
   -- A_Function_Body_Declaration (without defined specification)
   -- returning An_Attribute_Reference . A_Class_Attribute
   function Class_Attribute_Reference_Function return Tagged_Type'Class is      -- class_wide
   begin
      return Tagged_Type'(I => 2);
   end Class_Attribute_Reference_Function;

   ------------------------------------------------------
   -- Checking functions returning discriminated types --
   ------------------------------------------------------
   -- Defining a discriminated type
   type Discriminated_Record (D : Boolean) is
      record
         Common : Float;
         case D is
            when True =>
               B : Boolean;
            when False =>
               I : Integer;
         end case;
      end record;

   -- A_Function_Body_Declaration (without defined specification)
   -- returning a discriminated type whose discriminant is known
   function Returning_Discriminated_Type return Discriminated_Record is         -- unconstrained_discriminated
      Rec : Discriminated_Record (D => True);
   begin
      Rec.Common := 5.0;
      Rec.B := False;
      return Rec;
   end Returning_Discriminated_Type;


   -- Defining a constrained discriminated subtype
   subtype Constrained_Discriminated_Record is Discriminated_Record (D => False);
   -- A_Function_Body_Declaration (without defined specification)
   -- returning a constrained discriminated subtype (known record structure)
   function Returning_Constrained_Discriminated_Subtype                         -- OK
     return Constrained_Discriminated_Record
   is
      Rec : Constrained_Discriminated_Record;
   begin
      Rec.Common := 5.0;
      Rec.I := 0;
      return Rec;
   end Returning_Constrained_Discriminated_Subtype;


   -- A_Generic_Function_Declaration
   -- returning a discriminated type whose discriminant is unknown
   generic
      type Generic_Discriminated_Type (<>) is private;
   function Returning_Generic_Discriminated_Type                                -- OK
     (GDT : Generic_Discriminated_Type)
     return Generic_Discriminated_Type;
   -- A_Generic_Function_Body_Declaration
   function Returning_Generic_Discriminated_Type                                -- OK (explicit specification)
     (GDT : Generic_Discriminated_Type)
     return Generic_Discriminated_Type
   is
   begin
      return GDT;
   end Returning_Generic_Discriminated_Type;

   -- A_Function_Instantiation
   -- returning a discriminated type with known discriminant
   function Returning_Generic_Discriminated_Record is                           -- unconstrained_discriminated
      new Returning_Generic_Discriminated_Type (Discriminated_Record);



   -------------------------------------------------------
   -- Checking functions returning formal derived types --
   -------------------------------------------------------
   type Simple_Record is
      record
         I : Integer;
      end record;
   -- A_Generic_Function_Declaration
   -- returning a derived type from record type
   generic
      type Derived_Record is new Simple_Record;
   function Generic_Returning_Derived_Record return Derived_Record;             -- OK
   -- A_Generic_Function_Body
   -- returning a derived type from record type
   function Generic_Returning_Derived_Record return Derived_Record is           -- OK
   begin
      return Derived_Record'(I => 0);
   end Generic_Returning_Derived_Record;

   -- A_Generic_Function_Declaration
   -- returning a derived type from An_Attribute_Reference ('Base only)
   generic
      type Integer_Base is new Integer'Base;
   function Generic_Returning_Integer_Base return Integer_Base;                 -- OK
   -- A_Generic_Function_Body
   -- returning a derived type from An_Attribute_Reference ('Base only)
   function Generic_Returning_Integer_Base return Integer_Base is               -- OK
   begin
      return 5;
   end Generic_Returning_Integer_Base;

   ----------------------------------------------------
   -- Checking functions in procedure instantiations --
   ----------------------------------------------------
   generic
      type T is private;
      V : T;
   procedure Gen;
   procedure Gen is
      function F return T is
      begin
         return V;
      end F;
   begin
      null;
   end Gen;

   procedure Inst1 is new Gen (Integer, 1);            -- OK
   subtype Str10 is String (1..10);
   procedure Inst2 is new Gen (Str10, "0123456789");   -- Return_Constrained_Array

begin
   null;
end T_Return_Type;
