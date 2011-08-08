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
   
   task T is
      entry E;
   end T;
   task body T is
   begin
      accept E;
   end T;

   X : Integer;
   Y : Float;
   Ren : Integer renames X;
   procedure My_PL (S : String) renames Ada.Text_IO.Put_Line;
begin
   X := Integer'Value (Integer'Image (X));
exception
   when others =>
      Ada.Text_IO.Put_Line ("");              -- not_all_overloaded
      My_PL ("");                             -- not_all_overloaded
      Put_Line (F, "");                       -- not_all_not_overloaded
      Put_Line ("");                          -- all_overloaded
      Put_Line (3);                           -- all_not_overloaded
      X := Integer'Value (Integer'Image (X)); -- variable x2, attributes x2
      Y := Float'Value (Float'Image (Y));
      P1;                                     -- calls
      P2;
      X := Func;                              -- variable, calls
      X := Integer'Succ (X) + 2;              -- variable x2, calls, no_arithmetic
      Ren := Integer(Y);                      -- variable
      T.E;                                    -- entry_call
end T_entity_inside_exception;



