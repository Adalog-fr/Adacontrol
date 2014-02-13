separate (t_naming_convention)
procedure T_Cat is
-- Check categories

   type T_Enum is (Enum_A, Enum_B, Enum_C);
   type T_Signed is range 1 .. 10;
   type T_Mod is mod 10;

   procedure Proc (Vvv1 : out T_Enum;     -- Enum_Var
                   E_V1 : out T_Enum;     -- OK
                   Vvv2 : out T_Signed;   -- Int_Var
                   I_V2 : out T_Signed;   -- OK
                   Vvv3 : out T_Mod;      -- Int_Var
                   I_V3 : out T_Mod)      -- OK
   is
   begin
      null;
   end Proc;

begin
   null;
end T_Cat;
