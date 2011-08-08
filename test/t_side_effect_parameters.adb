procedure T_side_effect_parameters is
   generic
   function FG return Integer;
   function FG return Integer is begin return 1; end FG;

   function F1 return Integer is begin return 1; end F1;
   function F2 return Integer is begin return 1; end F2;
   function F3 return Integer is begin return 1; end F3;
   function F4 return Integer renames F3;
   function F5 is new FG;

   procedure P (X, Y : Integer; Z : Integer := F3) is
   begin
      null;
   end P;

   generic
      type T is private;
      X, Y : Integer;
   package Gen is
      function G1 return Integer;
   end Gen;

   package body Gen is
      function G1 return Integer is begin return 1; end G1;
   end Gen;

   generic
      with package P is new Gen (Integer, F1, F2);
   package Gen_Gen is end Gen_Gen;

   package Inst is new Gen (Integer, F1, F2);

   function Ren_Value (X : String) return Integer renames Integer'Value;
begin
   P (F1, F2, F3);
   P (F1, F1);
   P (X => F2, Y => F3, Z => F3);
   P (F2, F4);
   P (F1, F3);   -- No conflict here
   P (F2, Inst.G1);
   P (F5, F2);
   P (Integer'Value("1"), Integer'Value ("3"));
   P (Integer'Value("1"), Ren_Value ("3"));
end T_side_effect_parameters;
