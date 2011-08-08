separate (T_Style)
procedure Casing is
   pragma PAGE;
   pragma Page;                                 -- Casing_Pragma
   Int : Integer;
   function Foo ( I : in Integer ) return Integer is
   begin
      return i + 1;                             -- Casing_Identifier
   end Foo;
begin
   Int := 1;
   Ada.Text_IO.Put_Line (Integer'image (INT));  -- Casing_Identifier, Casing_Attribute
   INT := FOo (INt);                            -- Casing_Identifier x3
   Ada.Text_Io.Put_Line (Integer'Image (InT));  -- Casing_Identifier x2
end Casing;
