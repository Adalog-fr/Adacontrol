with Ada.Text_IO; use Ada.Text_IO;
package body Test_Case_113 is
   procedure P is
      package Inner_P is
      end Inner_P;

      package body Inner_P is
      begin
         Put_Line ("Hello"); -- Not elaboration
      end Inner_P;
      Z : Count := Line (F); -- Not elaboration
   begin
      Put_Line ("Hello"); -- Not elaboration
   end P;

   package Inner is
   end Inner;

   package body Inner is
      procedure Q is
         Z : Count := Line (F); -- Not elaboration
      begin
         Put_Line ("Hello"); -- Not elaboration
      end Q;
   begin
      Put_Line ("Hello"); --OK
   end Inner;

   Y : Count := Line (F); --OK
begin
   begin
      Put_Line ("Hello"); --OK
   end;
end Test_Case_113;
