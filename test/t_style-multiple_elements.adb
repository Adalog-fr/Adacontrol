with X_Style1, X_Style2;
with Text_IO; use Text_IO;
with Text_IO; pragma ELABORATE (Text_IO); use Text_IO; -- Multiple (clause)
with System; use Text_IO;                              -- Multiple (clause)
with Ada.Text_IO; use Text_IO;                         -- Multiple (clause)
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed, Ada.Strings.Bounded; use Ada.Strings.Fixed, Ada.Strings.Unbounded, Text_IO;   -- Multiple (clause)
with Ada.Strings.Fixed, Ada.Strings.Bounded; use Ada.Strings.Fixed, Ada.Strings.Bounded; use Text_IO; -- Multiple (clause)

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
