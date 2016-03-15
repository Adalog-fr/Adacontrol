pragma Ada_2012;
procedure Essai is
   package Wrapper is
      type By_Copy is private;     -- A by copy type that is not subject to Ada 2012 aliasing checks
   private
      type By_Copy is new Integer;
   end Wrapper;
   use Wrapper;

   function F (A, B : in out By_Copy) return Boolean is
   begin
      return True;
   end F;

   procedure P (A, B : in out By_Copy) is
   begin
      null;
   end P;

   V1, V2 : By_Copy;
   Z : Boolean renames F (V1, V1);
   I : Integer;
begin
   P (V1, V1);
   if F (V1, V2) then
      null;
   end if;
   if F (V1, V1) then
      null;
   end if;
end Essai;
