with X_STYLE_CASING.CHILD_1.CHILD_2;                    -- Casing_Identifier x3
separate (T_Style)
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

   type T is (Aa, Bb, CC);
   subtype ST is T range AA .. BB;               -- Casing_Identifier (Mantis 0000032)

begin
   Int := 1;
   Ada.Text_IO.Put_Line (Integer'image (INT));  -- Casing_Identifier, Casing_Attribute
   INT := FOo (INt);                            -- Casing_Identifier x3
   Ada.Text_Io.Put_Line (Integer'Image (InT));  -- Casing_Identifier x2
   for I in S
     '
     Range LOOP                                 -- Casing_Keyword
      Null;                                     -- Casing_Keyword
   END LOOP;                                    -- Casing_Keyword x2
   if 'a'in Character then                      -- exposed_literal
     NULL;                                      -- Casing_Keyword
   eND If;                                      -- Casing_Keyword x2

   I := I and 1;
   I := I AND 1;                                -- Casing_Keyword
   I := "and" (L => I, R => 1);
   I := "AND" (L => I, R => 1);                 -- Casing_Keyword
end Casing;
