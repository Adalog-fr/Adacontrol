with X_Style1, X_Style2;
with Text_IO; use Text_IO;
with Text_IO; pragma ELABORATE (Text_IO); use Text_IO; -- Multiple_Clause, Multiple_Pragma
with System; use Text_IO;                              -- Multiple_Clause
with Ada.Text_IO; use Text_IO;                         -- Multiple_Clause
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed, Ada.Strings.Bounded; use Ada.Strings.Fixed, Ada.Strings.Unbounded, Text_IO;   -- Multiple_Clause x2
with Ada.Strings.Fixed, Ada.Strings.Bounded; use Ada.Strings.Fixed, Ada.Strings.Bounded; use Text_IO; -- Multiple_Clause

separate (T_Style)
procedure Multiple_Elements is
   Decl1 : Integer := 0; Decl2 : Integer := 0;      -- Multiple_Decl
   Decl3        : Integer
     := 0; Decl4 : Integer := 0;                    -- Multiple_Decl

   use Ada.Text_IO; use Ada.Strings.Wide_Unbounded; -- Multiple_Clause
   type Enum is (Enum_1, Enum_2, Enum3);            -- OK

   pragma LIST (Off); pragma LIST (On);             -- Multiple_Pragma

   E : Enum := Enum'First;

   procedure P is begin                             -- Multiple_Stmts
      null; end P;                                  -- Multiple_Stmts

   procedure
   Q  is begin                                      -- Multiple_Stmts x2
      null;
   end                                              -- Multiple_Decl
     Q;

   function F return Integer
   is
   begin
      return X : Integer do
         X := 1; end return;                        -- Multiple_Stmts
      return
        X : Integer
      do
         X := 1;
      end                                           -- Multiple_Stmts
      return;
      return
        X : Integer do                              -- Multiple_Stmts
         X := 1;
      end return; exception                         -- Multiple_Stmts
      when Constraint_Error => null;                -- Multiple_Stmts
         null; when Tasking_Error =>                -- Multiple_Stmts
         null;  end F                               -- Multiple_Stmts x2
   ;
begin
   Decl1 := Decl2; Decl2 := Decl1;                  -- Multiple_Stmts
   if Decl1 = Decl2 then No_Default_In; end if;     -- Compound_Statement, Multiple_Stmts x2
   case E is
      when Enum_1 => null;                          -- Multiple_Stmts
      when Enum_2 =>
         null;  when others => null; end case;      -- Multiple_Stmts x3
   if Decl1 =
     Decl2 then                                     -- Multiple_Stmts
      Decl1 :=
        1 ; elsif Decl2 =                           -- Multiple_Stmts
                    Decl1 then                      -- Multiple_Stmts
         Decl1 := 1;
      elsif Decl2 = Decl1 then
        Decl1 :=
        1 ; else                                    -- Multiple_Stmts
      Decl1 := 1; end                               -- Multiple_Stmts x2
   if;

   begin begin                                      -- Multiple_Stmts
      null;
   end; end;                                        -- Multiple_Stmts

   begin declare begin null; end; end;              -- Compound_Statement x2, Multiple_Stmts x5

   <<L1>>
   if Decl1 = Decl2 then
      null;
   end if;

   L2 :
   while True loop
      null;
   end loop L2;

   <<L3>>
   L4 :
   loop
      null;
   end loop L4;
exception
   when Tasking_Error =>
      null;
end Multiple_Elements;
