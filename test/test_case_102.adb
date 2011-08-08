with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions;
procedure Test_Case_102 is

   procedure P is
      use Ada.Exceptions;
   begin
      null;
   exception
      when others =>
         Raise_Exception (Constraint_Error'Identity);
   end P;
   I : Integer;

begin
   Put_Line ("Body");

exception
   when Name_Error =>
      raise;

   when End_Error =>
      Put_Line ("End_Error");

   when Use_Error =>
      if False then
         raise;
      end if;

   when Data_Error =>
      Put_Line ("Data_error");
      for I in 1 .. 10 loop
         P;
      end loop;

  when Constraint_Error =>
      if False then
         raise;
      else
         P;
      end if;

   when Program_Error =>
      null;
end Test_Case_102;
