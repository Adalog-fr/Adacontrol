separate (T_Return_Type)
procedure Named_Types is
   package Return_Type_Check is
      type Enum    is (One, Two, Three);
      subtype Sub_Enum is Enum range One .. Two;
      function F1 return Enum;               -- type Enum
      function F1_Sub return Sub_Enum;       -- type Enum

      type Record1 is record
         F1 : Enum;
      end record;
      function F2 return Record1;            -- type Record1

      type Class1 is tagged record
         F1 : Enum;
      end record;
      subtype Class1_Class is Class1'Class;
      package Not_Primitive is
         function F4 return Class1;          -- type Class1
      end Not_Primitive;
      function F4_Class return Class1'Class; -- class-wide, Main.Return_Type_Check.Class1'Class
      function F5_Class return Class1_Class; -- class-wide, Main.Return_Type_Check.Class1

      type Class2 is new Class1 with
         record
            F2 : Enum;
         end record;
      function F6 return Class2;             -- OK (we don't follow derived types)
      function F7 return Class2'Class;       -- class-wide

   end Return_Type_Check;

   package body Return_Type_Check is

      function F1 return Enum is
      begin
         return One;
      end F1;

      function F1_Sub return Sub_Enum is
      begin
         return Two;
      end F1_Sub;

      function F2 return Record1 is
      begin
         return (F1 => F1);
      end F2;

      type Record2 is new Record1;
      function F3 return Record2 is          -- Record2
         Result : Record2;
      begin
         return Result;
      end F3;

      package body Not_Primitive is
         function F4 return Class1 is
         begin
            return (F1 => F1);
         end F4;
      end Not_Primitive;

      function F4_Class return Class1'Class is
         Result : Class1 := (F1 => F1);
      begin
         return Result;
      end F4_Class;

      function F5_Class return Class1_Class is
         Result : Class1 := (F1 => F1);
      begin
         return Result;
      end F5_Class;

      function F6 return Class2 is
      begin
         return
           Result : Class2 :=  (F1 => F1, F2 => Two)
         do
            null;
         end return;
      end F6;

      function F7 return Class2'Class is
      begin
         return Class2'(F1 => F1, F2 => Two);
      end F7;

   end Return_Type_Check;

   generic
      type T is (<>);
   function Gen return T;
   function Gen return T is
   begin
      return T'First;
   end Gen;

   function Inst is new Gen (Return_Type_Check.Enum);  -- Enum
   function Inst_Integer is new Gen (Natural);         -- Integer
begin
   null;
end Named_Types;
