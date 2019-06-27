package Xfw_Fixes.Child2 is

   type Mod_Type1 is mod 10;
   type Mod_Type2 is mod 10;

   procedure Dummy is null;
   procedure Op1 (Value : Mod_Type1) is null;
   procedure Op2 (Value : Mod_Type2) is null;

end Xfw_Fixes.Child2;
