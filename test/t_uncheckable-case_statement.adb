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
begin
   null;
end Case_Statement;
