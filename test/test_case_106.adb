with Ada.Text_IO; use Ada.Text_IO;
with System;
procedure Test_Case_106 is
   type A is access all Integer;
   I : aliased Integer;
   B : A := I'Unchecked_Access;
   C : A := I'Unrestricted_Access;
   D : System.Address := B'Address;
begin
   null;
end Test_Case_106;

