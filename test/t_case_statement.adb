procedure T_Case_Statement is
  V : Integer;
begin

   -- Simple cases
   declare
      subtype Integer_Subtype is Integer range 10 .. 20;
      subtype Small_Integer_Subtype is Integer range 1..2;
      type Derived_Integer_Type is new Integer range 10 .. 200;

      Integer_Case : Integer := 15;
      Derived_Integer_Case : Derived_Integer_Type := 15;

   begin
      case Integer_Subtype (Integer_Case) is          -- Max Values check, Max Values_Others check, Min_Paths OK
         when 10 .. 11 => null;
         when 12 .. 15 => null;
         when 16 .. 20 => null;                       -- Range_Span search
         when others => null;                         -- Min_Others covers 0, check, count
      end case;

      case Integer_Subtype (Integer_Case) is          -- Max Values check, Max_Values_Others OK, Min_Paths check
         when 10 .. 11 => null;
         when 12 .. 20 => null;                       -- Range_Span check
      end case;

      case Integer_Subtype (Integer_Case) is          -- Max Values check, Max Values_Others check, Min_Paths check, count
         when 10 .. 15 => null;                       -- Range_Span Check
         when others => null;                         -- Min_Others covers 5, search
      end case;

      case Integer_Subtype (Derived_Integer_Case) is  -- Max Values check, Max Values_Others check, Min_Paths check, count
         when 10 .. 15 => null;                       -- Range_Span check
         when others => null;                         -- Min_Others covers 5, search
      end case;

      case Small_Integer_Subtype (Integer_Case) is    -- Min Values check, Min_Paths check
         when 1 => null;
         when 2 => null;
      end case;

      case Derived_Integer_Case is                    -- Max Values check, Max Values_Others check, count, Min_Paths search, count
         when 10 .. 99 => null;                       -- Range_Span check, count
         when 100 | 102 | 104 | 106 => null;
         when others => null;                         -- Min_Others covers 97, search
      end case;
   end;

   -- Mixed enumeration
   declare
      type Mixed_Enumeration_Type is ('A', 'B', 'C', 'D', 'E', MF, MG, MH, MI, MJ);
      subtype Mixed_Enumeration_Non_Characters is Mixed_Enumeration_Type range MF .. MJ;
      subtype Mixed_Enumeration_Both_Types is Mixed_Enumeration_Type range 'D' .. MG;

      Mixed_Enumeration_Case : Mixed_Enumeration_Type := 'A';
   begin
      case Mixed_Enumeration_Case is                    -- Max Values search, Min_Paths search, count
         when 'A' .. 'E' => null;                       -- Enumeration_Range_Span check
         when Mixed_Enumeration_Non_Characters => null; -- Enumeration_Range_Span check
         when others => null;                           -- Min_Others covers 0, check, count
      end case;

      case Mixed_Enumeration_Case is                  -- Max Values search, Min_Paths check, count
         when Mixed_Enumeration_Both_Types => null;   -- Enumeration_Range_Span found
         when others => null;                         -- Min_Others covers 6, search
      end case;
   end;

   -- Generics
   declare
      generic
         type G_Enum is (<>);
         type G_Int is range <>;
         type G_Mod is mod <>;
      package G_Inner is
      end G_Inner;

      package body G_Inner is
         G_Enum_V : G_Enum := G_Enum'LAST;
         G_Int_V  : G_Int  := G_Int'LAST;
         G_Mod_V  : G_Mod  := G_Mod'LAST;
      begin
         case G_Enum_V is                             -- Min_Paths check, count
            when others => null;                      -- Min_Others covers unknown
         end case;

         case G_Int_V is                              -- Min_Paths check, count
            when 10 .. 15 => null;                    -- Range_Span check
            when others => null;                      -- Min_Others covers unknown
         end case;

         case G_Mod_V is                              -- Min_Paths check, count
            when 200 .. 255 |                         -- Range_Span check, count
              0 .. 198 => null;                       -- Range_Span check, count
            when others => null;                      -- Min_Others covers unknown
         end case;
      end G_Inner;
   begin
      null;
   end;

   -- Dynamic subtype:
   -- Must consider the range of the type
   if True then
      V := 10;  -- Make V not statically evaluable
   end if;
   declare

      type Parent is range 1..20;
      Max : Parent := 10;

      subtype Int1 is Parent range 1 .. Max;
      I1 : Int1;
      subtype Int2 is Parent range 1 .. Parent (V);
      I2 : Int2;
   begin
      case I1 is                                      -- Min_Paths check, count
         when 1..3 => null;
         when others => null;                         -- Min_Others covers 7, search
      end case;
      case I2 is                                      -- Min_Paths check, count
         when 1 .. 3 => null;
         when others => null;                         -- Min_Others covers 17, search
      end case;
   end;

   -- Static expressions in case ranges:
   declare
      I : Integer;
      A : constant := 20;
      B : constant Integer := 30;
   begin
      case I is                                             -- Max Values check, Max Values_Others check, count
         when Integer'First .. Integer'Pred (-10) => null;  -- Range_Span check, count
         when -9 .. 0 => null;                              -- Range_Span check
         when 1+2 .. 5+3 => null;                           -- Range_Span check
         when A .. B => null;
         when 40 .. Integer'Last => null;                   -- Range_Span check, count
         when others => null;                               -- Min_Others covers 23, search
      end case;
   end;

 end T_Case_Statement;
