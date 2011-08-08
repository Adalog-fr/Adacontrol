with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions;
procedure T_silent_exceptions is

   procedure P is
      use Ada.Exceptions;
   begin
      null;
   exception
      when others => --OK
         Raise_Exception (Constraint_Error'Identity);
   end P;
   I : Integer;

begin
   Put_Line ("Body");

exception
   when Name_Error => --OK
      raise;

   when End_Error => --OK
      Put_Line ("End_Error");

   when Use_Error =>
      if False then
         raise;
      end if;

   when Data_Error => --OK
      Put_Line ("Data_error");
      for I in 1 .. 10 loop
         P;
      end loop;

  when Constraint_Error => --OK
      if False then
         raise;
      else
         P;
      end if;

   when Program_Error =>
      null;
end T_silent_exceptions;
