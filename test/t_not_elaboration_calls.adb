with Ada.Text_IO; use Ada.Text_IO;
package body T_not_elaboration_calls is
   type Int_Ptr is access Integer;
   Ptr : Int_Ptr;

   procedure P is
      package Inner_P is
         type T is tagged null record;
         procedure Dispatch (X : T);
      end Inner_P;

      package body Inner_P is
         procedure Dispatch (X : T) is
         begin
            null;
         end Dispatch;
         procedure Print (S: String) renames Put_Line;
      begin
         Put_Line ("Hello"); -- Not elaboration
         Print    ("Hello"); -- Not elaboration
         Ptr := new Integer; -- Not elaboration
      end Inner_P;
      use Inner_P;

      Z : Count := Line (F); -- Not elaboration
      V : T;
   begin
      Put_Line ("Hello");         -- Not elaboration
      Put_Line (Count'Image (Z)); -- Not elaboration x2
      Dispatch (V);               -- Not elaboration, non dispatching
      declare
         procedure Inner is
         begin
            Dispatch (T'Class (V));     -- Not elaboration, dispatching
            Ptr := new Integer'(32);    -- Not elaboration
         end Inner;
      begin
         Inner;
      end;
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
      Ptr := new Integer; --OK
   end;
end T_not_elaboration_calls;
