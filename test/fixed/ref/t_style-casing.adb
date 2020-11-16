with X_Style_Casing.Child_1.Child_2;                    -- Casing_Identifier x3
with System;                                            -- Casing_Keyword
separate (T_STYLE)
procedure Casing is
   pragma PAGE;
   pragma PAGE;                                         -- Casing_Pragma
   Int : Integer;
   function Foo ( I : in Integer ) return Integer;
   pragma INLINE (Foo);                                 -- Casing_Identifier
   pragma CONVENTION (Convention => C, Entity => Foo);  -- Casing_Identifier
   function Foo ( I : in Integer ) return Integer is    -- Casing_Identifier
   begin
      return I + 1;                                     -- Casing_Identifier
   end Foo;                                             -- Casing_Identifier

   S   : String (1 .. 1);
   function "and" (L, R : in Integer) return Integer is -- Casing_Keyword
   begin
      return 1;
   end "and";                                           -- Casing_Keyword
   I : Integer;

   type T is (Aa, Bb, CC)
     with Size => System.Word_Size;              -- Casing_Aspect, Casing_Identifier
   subtype ST is T range Aa .. Bb;               -- Casing_Identifier x2 (Mantis 0000032)
   type Ptr is access all Integer;               -- Casing_Keyword x2

   package Pack is
      type TT is abstract tagged null record;    -- Casing_Keyword
      not overriding procedure Prim (X : in TT)  -- Casing_Keyword x2
         is abstract with Pre'Class => True;     -- Casing_Aspect, Casing_Keyword
   end Pack;

   Dim   : constant := 1;
   Expo1 : aliased constant Float := 1.0E10;    -- Casing_Keyword
   Expo2 : constant Float := 1.0E10;            -- Casing_Exponent
   Expo3 : constant := 1E5;                     -- Casing_Exponent
   Base1 : constant := 16#1AB#;
   Base2 : constant Float := 16#1AB.0C#E2;      -- Casing_Number
   Base3 : Integer := 14#1ABC#;                 -- Casing_Number

   type Sync is synchronized interface;               -- Casing_Keyword x2
   Str   : constant String := "abcd";                 -- Casing_Keyword
   B     : Boolean := (for some C of Str => C = 'a'); -- Casing_Keyword x2

begin
   Int := abs 1;                                -- Casing_Keyword
   Ada.Text_IO.Put_Line (Integer'Image (Int));  -- Casing_Identifier, Casing_Attribute
   Int := Foo (Int);                            -- Casing_Identifier x3
   Ada.Text_IO.Put_Line (Integer'Image (Int));  -- Casing_Identifier x2
   for I in S
     '
     Range 
   loop                                 -- Casing_Keyword, Multiple_Stmts
      null;                                     -- Casing_Keyword
   end loop;                                    -- Casing_Keyword x2
   for I in S'Range (Dim) loop                  -- Casing_Identifier
      null;
   end loop;
   if 'a'in Character then                      -- exposed_literal
     null;                                      -- Casing_Keyword
   end if;                                      -- Casing_Keyword x2

   I := I and 1;
   I := I and 1;                                -- Casing_Keyword
   I := "and" (L => I, R => 1);
   I := "and" (L => I, R => 1);                 -- Casing_Keyword
end Casing;
