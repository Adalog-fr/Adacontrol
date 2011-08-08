with Ada.Text_io;                                       -- Casing
with Ada.Strings.Wide_Unbounded;
procedure t_style is
   procedure No_Default_In is                           -- No_Closing_Name
      procedure Proc_A(param1 : Integer := 1) is        -- No_Default_In
      begin
         return ;
      end Proc_A;
      procedure Proc_B(param2 : in Integer) is
      begin
         return;
      end Proc_B;

      generic
         Variable : in out Integer;
         Limit, Reset_Value : Integer;                  -- No_Default_In
      procedure Reset_Integer_Template(Var: Integer);   -- No_Default_In

      procedure Reset_Integer_Template(Var : in Integer) is
      begin
         if Variable > Limit then
            Variable := Reset_Value;
         end if;
      end Reset_Integer_Template;

      generic
         Max: in Positive;
         Min: Positive;                                 -- No_Default_In
         with procedure P (X : Integer);                -- No_Default_In
      package Foo is
         procedure Bar(X: in Integer);
         function Foo_Bar(Y: Integer) return Integer;   -- No_Default_In
      end Foo;
      package body Foo is
         procedure Bar(X: Integer) is                   -- No_Default_In
         begin
            null;
         end Bar;
         function Foo_Bar(Y: Integer) return Integer is -- No_Default_In
         begin
            return 1;
         end Foo_Bar;
      end Foo;


   begin
      Proc_A;
   end ; -- no_default_in


   procedure Identifier is
      int : Integer;
      function foo( I: Integer ) return Integer is        -- No_Default_In
      begin
         return i + 1;                                    -- Casing
      end foo;
   begin
      Int := 1;                                           -- Casing
      ada.Text_IO.put_line("Var :"& Integer'Image(INT));  -- Casing, Exposed_Literal (String)
      iNT := fOo(INt);                                    -- Casing
      ada.text_io.put_line("Var :"& Integer'Image(inT));  -- Casing, Exposed_Literal (String)
   end Identifier;


   procedure Named_Assoc is
      type Arecord is
         record
            A : Integer;
            B : Integer;
            C : Integer;
         end record;
      type Trecord is tagged
         record
            D : Integer;
         end record;
      type ETrecord is new Trecord with
         record
            E : Integer;
         end record;
      type NTrecord is new Trecord with null record;


      function Nothing( X : Integer ) return Integer is   -- No_Default_In
      begin
         return X;
      end Nothing;
      procedure Nproc( Y: Integer ; Z : Integer) is       -- No_Default_In
      begin
         null;
      end Nproc;
      Variable : Integer;
      RecordI : Arecord;
      RecordIT : Trecord;
      RecordITE : ETrecord;
      Max : constant := 5;
      tab : array (1..Max) of Integer;

      generic
         Elem : Integer;                                 -- No_Default_In
      procedure Pgen;

      procedure Pgen is
      begin
         null;
      end pgen;

      generic
         Elem: Integer;                                  -- No_Default_In
      function Fgen return Integer;

      function Fgen return Integer is
      begin
         return Elem;
      end Fgen;

      generic
         Elem: Integer;                                  -- No_Default_In
      package Pac_Gen is
      end Pac_Gen;

      package body Pac_Gen is
      begin
         null;
      end Pac_Gen;


      procedure ppgen is new pgen(Elem => 1) ;           -- Casing
      function ffgen is new fgen(Elem => 1);             -- Casing
      procedure pppgen is new pgen(1);                   -- Casing, Positional_Association
      function fffgen is new fgen(1);                    -- Casing, Positional_Association
      package Ppac_Gen is new Pac_Gen(Elem => 1);
      package PPpac_Gen is new Pac_Gen(1);               -- Positional_Association

      task Taske is
         entry EntryCall( I : Integer );                 -- No_Default_In
      end Taske;
      task body Taske is
      begin
         accept EntryCall( I : Integer ) do
            null;
         end EntryCall;
      end Taske;

   begin
      Variable := Nothing(X => 1);
      Variable := Nothing(1);                            -- Positional_Association
      Nproc( Y => Variable, Z => 1);
      Nproc(0, 1);                                       -- Positional_Association
      RecordI := (A=>1,B=>0,C=>1);
      RecordI := (1,0,1);                                -- Positional_Association
      RecordIT := Trecord'(D => 1);
      RecordITE := (RecordIT with E => 1);
      RecordITE := (RecordIT with 1);                    -- Positional_Association
      tab := (1,0,1,0,1);                                -- Positional_Association x5
      tab := (1=>0, 2=>1, 3=>0, 4=>1, 5=>0);             -- Exposed_Literal (Integer) x4
      Taske.EntryCall(1);                                -- Positional_Association
      Taske.EntryCall(I => 1);

   end Named_Assoc;

   procedure No_Closing_Name is
      procedure P (X : Integer) is                       -- No_Closing_Name
      begin
         null;
         null;
         null;
      end;

      package Pack is                                   -- No_Closing_Name
         Z : Integer;
      end;

      package body Pack is                              -- No_Closing_Name
      begin
         null;
      end;

      generic                                           -- No_Closing_Name
      package Gen is
         I : Integer;
      end;
   begin
      null;
   end No_closing_name;

   procedure Renamed_Entity is
      I : Integer;
      function F return Integer is
      begin
         return 1;
      end F;

      package Pack is
         V : Integer;
      end Pack;

   begin
      I := 1;        --OK
      Pack.V := F;  -- OK

      declare
         function RenF return Integer renames F;
         package RenPack renames Pack;
         RenI : Integer renames I;
         function Succ (I : in Integer) return Integer renames Integer'Succ;   -- Uncheckable
         function "-" (L, R : in Integer) return Integer renames Standard."+"; -- Uncheckable
         J : Integer;
      begin
         I := 1;                -- Renamed_Entity
         Pack.V := F;           -- Renamed_Entity (x2)

         RenI := 1;             -- OK
         RenPack.V := RenF;     -- OK

         J := Integer'Succ (J); -- Not checked
         J := J + 1;            -- Not checked
      end;

      I := 1;        --OK
      Pack.V := F;  -- OK
   end Renamed_Entity;

   -- Integer literals
   I1   : constant Integer := 0;
   I2   : constant Integer := 10_000;
   I3   : constant Integer := 1_0000;                       -- Numeric_Literal

   I4   : constant Integer := 1_2_3_4_5_6;                  -- Numeric_Literal

   IE1  : constant Integer := 1e9;
   IE2  : constant Integer := 1_000e6;
   IE3  : constant Integer := 1_0000e5;                     -- Numeric_Literal


   -- Real literals
   R1   : constant Float   := 0.0;
   R2   : constant Float   := 3.141_592;
   R3   : constant Float   := 1.61_80_33;                   -- Numeric_Literal

   RE1  : constant Float   := 1.0e-9;
   RE2  : constant Float   := 0.314_159e1;
   RE3  : constant Float   := 16_18.03_39E-3;               -- Numeric_Literal


   -- Based literals
   BI1  : constant Integer :=  2#1111_1111#;
   BI2  : constant Integer :=  8#0000_0177#;                -- OK (not specified)
   BI3  : constant Integer := 16#0000_00FF#;

   BI4  : constant Integer :=  7#0064_3502#;                -- Numeric_Literal (not allowed)
   BI5  : constant Integer := 13#003A_5BC8#;                -- Numeric_Literal (not allowed)

   BIE1 : constant Integer :=  2#0000_1010#E6;
   BIE2 : constant Integer := 16#0000_000A#E6;

   BR1  : constant Float   :=  2#1111_1111.1111_1111#;
   BR2  : constant Float   := 16#0000_00FF.FF00_0000#;
   BR3  : constant Float   := 16#41_89_AF_3B.FF_01_00_00#;  -- Numeric_Literal

   BR4  : constant Float   :=  7#0064_3502.2053_4600#;      -- Numeric_Literal (not allowed)

   BRE1 : constant Float   :=  2#0100_0001.1100_0000#E6;
   BRE2 : constant Float   := 16#C0_AF_E3.70_06#E1;         -- Numeric_Literal

   procedure Exposed_Literal is
      II : constant Integer   := 4;                         -- OK
      FX : constant Float     := 4.5;                       -- OK
      NI : constant           := 5;                         -- OK
      NF : constant           := 4.5;                       -- OK
      SS : constant String    := "AdaControl";              -- OK
      CC : constant Character := 'a';                       -- OK
      I  : Integer := 5;                                    -- OK
      F  : Float;
      S  : String (1..NI);
      C  : Character;
      type Enum is (A);
      for Enum use (A => 5);                                -- OK
      type Rec is
         record
            A,B,C : Integer;
         end record;
      for Rec use
         record
            A at 0 range 0..31;                             -- OK
            B at 4 range 0..31;                             -- OK
            C at 8 range 0..31;                             -- OK
         end record;
   begin
      I := 0;                                               -- OK
      I := 2;                                               -- Exposed_Literal (integer)
      F := 1.0;                                             -- OK
      F := 2.0;                                             -- Exposed_Literal (real)
      S := "rosen";                                         -- OK
      if S = "" then                                        -- OK
         S := "12345";                                      -- Exposed_Literal (string)
      elsif S = "12345" then                                -- Exposed_Literal (string)
         C := 'A';                                          -- Exposed_Literal (character)
      end if;
   end Exposed_Literal;

   Decl1 : Integer := 0; Decl2 : Integer := 0;              -- Multiple_Elements
   Decl3        : Integer
     := 0; Decl4 : Integer := 0;                            -- Multiple_Elements

   use Ada.Text_IO; use Ada.Strings.Wide_Unbounded;         -- Multiple_Elements
   type Enum is (Enum_1, Enum_2, Enum3);                    -- OK

   B : Boolean;                                             -- OK
   E : Enum := Enum'First;                                  -- OK
begin
   if not B then                                            -- OK
      null;
   end if;

   if not B then                                            -- OK
      null;
   elsif B then
      null;
   end if;

   if not B then                                            -- Negative_Condition
      null;
   else
      null;
   end if;

   if ((not B)) then                                    -- Negative_Condition
      null;
   else
      null;
   end if;

   Decl1 := Decl2; Decl2 := Decl1;                      -- Multiple_Elements
   if Decl1 = Decl2 then Identifier; end if;            -- Multiple_Elements
   case E is
      when Enum_1 => null;                              -- Multiple_Elements
      when others => null;                              -- Multiple_Elements
   end case;

end t_style;




