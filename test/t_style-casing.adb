with X_STYLE_CASING.CHILD_1.CHILD_2;                    -- Casing_Identifier x3
WITH System;                                            -- Casing_Keyword
separate (T_STYLE)
procedure Casing is
   pragma PAGE;
   pragma Page;                                         -- Casing_Pragma
   Int : Integer;
   function Foo ( I : in Integer ) return Integer;
   pragma INLINE (FOO);                                 -- Casing_Identifier
   pragma CONVENTION (Convention => C, Entity => FOO);  -- Casing_Identifier
   function foo ( I : in Integer ) return Integer is    -- Casing_Identifier
   begin
      return i + 1;                                     -- Casing_Identifier
   end foo;                                             -- Casing_Identifier

   S   : String (1 .. 1);
   function "aNd" (L, R : in Integer) return Integer is -- Casing_Keyword
   begin
      return 1;
   end "AND";                                           -- Casing_Keyword
   I : Integer;

   type T is (Aa, Bb, CC)
     with SIZE => System.Word_SIZE;              -- Casing_Aspect, Casing_Identifier
   subtype ST is T range AA .. BB;               -- Casing_Identifier x2 (Mantis 0000032)
   type Ptr is Access All Integer;               -- Casing_Keyword x2

   package Pack is
      type TT is ABSTRACT tagged null record;    -- Casing_Keyword
      Not OVERRIDING procedure Prim (X : in TT)  -- Casing_Keyword x2
         IS abstract with PRE'CLASS => True;     -- Casing_Aspect, Casing_Keyword
   end Pack;

   Dim   : constant := 1;
   Expo1 : ALIASED constant Float := 1.0E10;    -- Casing_Keyword
   Expo2 : constant Float := 1.0e10;            -- Casing_Exponent
   Expo3 : constant := 1e5;                     -- Casing_Exponent
   Base1 : constant := 16#1AB#;
   Base2 : constant Float := 16#1ab.0c#E2;      -- Casing_Number
   Base3 : Integer := 14#1AbC#;                 -- Casing_Number

   type Sync is Synchronized Interface;               -- Casing_Keyword x2
   Str   : constanT String := "abcd";                 -- Casing_Keyword
   B     : Boolean := (For Some C of Str => C = 'a'); -- Casing_Keyword x2

begin
   Int := Abs 1;                                -- Casing_Keyword
   Ada.Text_IO.Put_Line (Integer'image (INT));  -- Casing_Identifier, Casing_Attribute
   INT := FOo (INt);                            -- Casing_Identifier x3
   Ada.TEXT_IO.Put_Line (Integer'Image (InT));  -- Casing_Identifier x2
   for I in S
     '
     Range LOOP                                 -- Casing_Keyword, Multiple_Stmts
      Null;                                     -- Casing_Keyword
   END LOOP;                                    -- Casing_Keyword x2
   for I in S'Range (DIM) loop                  -- Casing_Identifier
      null;
   end loop;
   if 'a'in Character then                      -- exposed_literal
     NULL;                                      -- Casing_Keyword
   eND If;                                      -- Casing_Keyword x2

   I := I and 1;
   I := I AND 1;                                -- Casing_Keyword
   I := "and" (L => I, R => 1);
   I := "AND" (L => I, R => 1);                 -- Casing_Keyword
end Casing;
