pragma Ada_95;
procedure T95_Return_Type_Limited_Return is

--------------------------------------------------------
-- Checking functions returning a tagged limited type --
--------------------------------------------------------
   type Limited_Tagged_Type is tagged limited
      record
         I : Integer;
      end record;
   -- A_Function_Body_Declaration (without defined specification)
   -- returning An_Attribute_Reference . A_Class_Attribute (limited)

   Limited_Tagged_Object : Limited_Tagged_Type;
   function Class_Attribute_Reference_Function return Limited_Tagged_Type'Class is      -- class_wide, limited_class_wide
   begin
      return Limited_Tagged_Object;
   end Class_Attribute_Reference_Function;

   ----------------------------------------------
   -- Checking functions returning a task type --
   ----------------------------------------------
   -- Defining a task type
   task type Dummy_Task_Type;
   task body Dummy_Task_Type is
   begin
      null;
   end Dummy_Task_Type;

   My_Dummy_Task_Object : Dummy_Task_Type;

   -- A_Function_Body_Declaration (without defined specification)
   -- returning a task type
   function Returning_Task_Type return Dummy_Task_Type is                       -- task
   begin
      return My_Dummy_Task_Object;
   end Returning_Task_Type;


   ---------------------------------------------------
   -- Checking functions returning a protected type --
   ---------------------------------------------------
   -- Defining a protected type
   protected type Dummy_Protected_Type is
      procedure Inner_Dummy_Proc;
   end Dummy_Protected_Type;
   protected body Dummy_Protected_Type is
      procedure Inner_Dummy_Proc is
      begin
         null;
      end Inner_Dummy_Proc;
   end Dummy_Protected_Type;

   My_Dummy_Protected_Object : Dummy_Protected_Type;

   -- A_Function_Body_Declaration (without defined specification)
   -- returning a protected type
   function Returning_Protected_Type return Dummy_Protected_Type is             -- protected
   begin
      return My_Dummy_Protected_Object;
   end Returning_Protected_Type;


   -----------------------------------------------------------
   -- Checking functions returning discriminated task types --
   -----------------------------------------------------------
   -- Defining a discriminated task type
   task type Discriminated_Task_Type (Length : Integer);
   task body Discriminated_Task_Type is
      S : String (1 .. Length) := (others => ' ');
   begin
      null;
   end;

   My_Discriminated_Task : Discriminated_Task_Type (Length => 80);

   -- A_Function_Body_Declaration (without defined specification)
   -- returning a discriminated task type (task + discriminant)
   function Return_Discriminated_Task_Type return Discriminated_Task_Type is    -- task, unconstrained_discriminated
   begin
      return My_Discriminated_Task;
   end Return_Discriminated_Task_Type;

   -- Defining a constrained discriminated task subtype
   subtype Constrained_Discriminated_Task_Subtype is Discriminated_Task_Type (Length => 80);

   My_Constrained_Discriminated_Task : Constrained_Discriminated_Task_Subtype;

   -- A_Function_Body_Declaration (without defined specification)
   -- returning a constrained discriminated task subtype
   function Returning_Constrained_Discriminated_Task_Subtype
     return Constrained_Discriminated_Task_Subtype                              -- task
   is
   begin
      return My_Constrained_Discriminated_Task;
   end Returning_Constrained_Discriminated_Task_Subtype;


   ----------------------------------------------------------------
   -- Checking functions returning discriminated protected types --
   ----------------------------------------------------------------
   -- Defining a discriminated protected type
   protected type Discriminated_Protected_Type (Length : Integer) is
      procedure P;
   private
      S : String (1 .. Length);
   end Discriminated_Protected_Type;
   protected body Discriminated_Protected_Type is
      procedure P is
      begin
         null;
      end P;
   end Discriminated_Protected_Type;

   My_Discriminated_Protected : Discriminated_Protected_Type (Length => 80);

   -- A_Function_Body_Declaration (without defined specification)
   -- returning a discriminated protected type (protected + discriminant)
   function Return_Discriminated_Protected_Type
     return Discriminated_Protected_Type                                        -- protected, unconstrained_discriminated
   is
   begin
      return My_Discriminated_Protected;
   end Return_Discriminated_Protected_Type;


   -- Defining a derived constrained discriminated protected type
   type Constrained_Discriminated_Protected_Subtype is new Discriminated_Protected_Type (Length => 80);

   My_Constrained_Discriminated_Protected : Constrained_Discriminated_Protected_Subtype;

   -- A_Function_Body_Declaration (without defined specification)
   -- returning a constrained discriminated protected type
   function Returning_Constrained_Discriminated_Protected_Subtype
     return Constrained_Discriminated_Protected_Subtype                         -- protected
   is
   begin
      return My_Constrained_Discriminated_Protected;
   end Returning_Constrained_Discriminated_Protected_Subtype;


   ---------------------------
   -- Checking tricky cases --
   ---------------------------

   -- A_Generic_Function_Declaration
   -- returning a limited type
   generic
      type Item (<>) is limited private;
   function Generic_Returning_Limited (X : access Item) return Item;            -- OK

   -- A_Generic_Function_Body (with defined specification)
   -- returning a limited type
   function Generic_Returning_Limited (X : access Item) return Item is          -- OK
   begin
      return X.all;
   end Generic_Returning_Limited;

   -- A_Function_Instantiation
   -- returning a task type
   function Generic_Instantiation_Returning_Task is                             -- task
     new Generic_Returning_Limited (Item => Dummy_Task_Type);
   -- A_Function_Instantiation
   -- returning a protected type
   function Generic_Instantiation_Returning_Protected is                        -- protected
     new Generic_Returning_Limited (Item => Dummy_Protected_Type);

   type Crazy (X : Integer) is tagged limited
      record
         T : Dummy_Task_Type;
         P : Dummy_Protected_Type;
      end record;

   function Generic_Instantiantion_Maxi is                                      -- class-wide, limited_class_wide, task, protected, unconstrained_discriminated
     new Generic_Returning_Limited (Item => Crazy'Class);


begin
   null;
end T95_Return_Type_Limited_Return;
