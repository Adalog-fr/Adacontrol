separate (T_Expressions)
procedure Test_Real_Equality is

   type A is digits 10 range -1.0 .. 1.0;
   type B is digits 10;
   subtype SB is B range -2.0 .. 10.0;

   type C is delta 0.125 range -1.0 .. 1.0;
   type D is delta 0.1 digits 15;
   subtype SD is D digits 10;

   VA1, VA2 : A := 0.0;
   VB1, VB2 : B := 0.0;
   VSB1, VSB2 : SB := 0.0;
   VC1, VC2 : C := 0.0;
   VD1, VD2 : D := 0.0;
   VSD1, VSD2 : SD := 0.0;
   F : Float := 0.0;

   function X return A is
   begin
      return A (0.0);     -- type_conversion
   end X;

   function X return SB is
   begin
      return SB (0.0);     -- type_conversion
   end X;

   function X return C is
   begin
      return C (0.0);     -- type_conversion
   end X;

   function X return Float is
   begin
      return Float (0.0);     -- type_conversion
   end X;

begin

   if VA1 = VA2 then               -- real_equality
      null;
   elsif VB1 /= 0.0 then           -- real_equality
      null;
   elsif 1.0 /= VB1 then           -- real_equality
      null;
   elsif VB1 /= X then             -- real_equality
      null;
   elsif VSB1 = VSB2 then          -- real_equality
      null;
   elsif VSB1 = X then             -- real_equality
      null;
   elsif VC1 /= 0.0 then           -- real_equality
      null;
   elsif 1.0 /= VC1 then           -- real_equality
      null;
   elsif VC1 /= C (0.0) then       -- real_equality, type_conversion
      null;
   elsif VC1 = X then              -- real_equality
      null;
   elsif VD1 = VD2 then            -- real_equality
      null;
   elsif VSD1 /= 0.0 then          -- real_equality
      null;
   elsif 1.0 /= VSD1 then          -- real_equality
      null;
   elsif VSD1 /= SD (0.0) then     -- real_equality, type_conversion
      null;
   elsif F /= X then               -- real_equality
      null;
   elsif 0.0 = 1.0 then            -- real_equality
      null;
   end if;
end Test_Real_Equality;
