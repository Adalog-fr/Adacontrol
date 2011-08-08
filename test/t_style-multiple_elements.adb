with X_Style1, X_Style2;
separate (T_Style)
procedure Multiple_Elements is
   Decl1 : Integer := 0; Decl2 : Integer := 0;      -- Multiple_Elements
   Decl3        : Integer
     := 0; Decl4 : Integer := 0;                    -- Multiple_Elements

   use Ada.Text_IO; use Ada.Strings.Wide_Unbounded; -- Multiple_Elements
   type Enum is (Enum_1, Enum_2, Enum3);            -- OK


   E : Enum := Enum'First;
begin
   Decl1 := Decl2; Decl2 := Decl1;                  -- Multiple_Elements
   if Decl1 = Decl2 then No_Default_In; end if;     -- Multiple_Elements, Compound_Statement
   case E is
      when Enum_1 => null;                          -- Multiple_Elements
      when others => null;                          -- Multiple_Elements
   end case;
end Multiple_Elements;
