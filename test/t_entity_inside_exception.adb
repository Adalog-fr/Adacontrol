with Ada.Text_IO; use Ada.Text_IO;
procedure T_entity_inside_exception is
   F : File_Type;
   procedure Put_Line (I : Integer) is
   begin
      null;
   end;
   procedure Put_Line (S : string) is
   begin
      null;
   end;

   procedure P1 is
   begin
      null;
   end P1;

   procedure P2 is
   begin
      null;
   end P2;

   function Func return Integer is
   begin
      return 1;
   end Func;

   X : Integer;
   Y : Float;
begin
   X := Integer'Value (Integer'Image (X));
exception
   when others =>
      Ada.Text_IO.Put_Line ("");              -- not_all_overloaded
      Put_Line (F, "");                       -- not_all_not_overloaded
      Put_Line ("");                          -- all_overloaded
      Put_Line (3);                           -- all_not_overloaded
      X := Integer'Value (Integer'Image (X)); -- attributes x2
      Y := Float'Value (Float'Image (Y));
      P1;                                     -- calls
      P2;
      X := Func;                              -- calls
      X := Integer'Succ (X);                  -- calls
end T_entity_inside_exception;



