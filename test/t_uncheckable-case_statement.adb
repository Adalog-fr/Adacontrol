separate (T_Uncheckable)
procedure Case_Statement is
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
         case G_Enum_V is                             -- uncheckable
            when others => null;
         end case;

         case G_Int_V is                              -- uncheckable
            when 10 .. 15 => null;
            when others => null;
         end case;

         case G_Mod_V is                              -- Uncheckable
            when 200 .. 255 |
              0 .. 198 => null;
            when others => null;
         end case;
      end G_Inner;
   type T1 is range 1..10 with Static_Predicate => T1 not in 2 | 4 | 6;
   V1 : T1 := 1;

   type T2 is range 1..10;
   subtype S2 is T2 with  Static_Predicate => S2 not in 2 | 4 | 6;
   V2 : T2 := 1;
begin
   case V1 is                                        -- Uncheckable x2
      when 1 | 3 | 5 => null;
      when 7 ..10    => null;
      when others    => null;
   end case;

   case V2 is
      when S2        => null;                        -- Uncheckable x2
      when 2 | 4 | 6 => null;
      when others    => null;
   end case;
end Case_Statement;
