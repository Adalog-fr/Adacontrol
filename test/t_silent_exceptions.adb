with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions;
procedure T_silent_exceptions is
   Static_Str : String (1..10);

   procedure P is
      use Ada.Exceptions;
   begin
      null;
   exception
      when others => --OK, Counted
         Raise_Exception (Constraint_Error'Identity);
   end P;
   I : Integer;

   function F return String is
   begin
      return "1234567890";
   end F;

   function G (X : String) return String is
   begin
      return "";
   exception
      when others => -- Found
         if True then
            I := Integer'Value (F);
         else
            return "";
         end if;
   end G;

   generic
   procedure Genproc;
   procedure Genproc is
   begin
      null;
   end;

   generic
   function Genfunc return String;
   function Genfunc return String is
   begin
      return "1234567890";
   end;

   generic
      Param : String;
   package Genpack1 is
      X : String := F;
   end Genpack1;

   generic
   package Genpack2 is
   end Genpack2;
begin
   begin -- Check procedure calls
      Put_Line ("Body");

   exception
      when Name_Error => -- Found, Counted
         raise;

      when End_Error =>  -- OK
         P;

      when Mode_Error => -- Error, Counted
         if False then
            raise;
         end if;

      when Use_Error =>  -- Error, Counted
          if I=0 then
            P;
          elsif I=1 then
             P;
          else
            null;
          end if;

     when Data_Error => -- OK (static loop)
         Put_Line ("Data_error");
         for C of Static_Str loop
            P;
         end loop;

     when Tasking_Error => -- Error, counted (statically not executed)
         Put_Line ("Tasking_error");
         for I in 10 .. 1 loop
            P;
         end loop;

     when Device_Error => -- Error, Counted (non static loop)
         Put_Line ("Data_error");
         for J in I .. 10 loop
            P;
         end loop;

      when Constraint_Error => -- Found (in Standard, but constraint_error), Counted
         if False then
            raise;
         else
            P;
         end if;

      when Program_Error => -- Error, Counted
         if I = 0 then
            P;
         elsif I = 1 then
            P;
         else
            null;
         end if;
   end;

   declare  -- Check function calls
      X : String (1..10);
   begin
      null;
   exception
      when Mode_Error =>         -- Found
         Put_Line (F);

      when Name_Error =>         -- Found
         declare
            X : constant String := F;
         begin
            Put_Line (X);
         end;

      when Data_Error =>         -- Error, Counted
         X := G ("");

      when Tasking_Error =>      -- Error, Counted
         delay Duration'Value ("1.0");

      when Program_Error | End_Error =>     -- Found (not all uncontrolled)
         delay Duration'Value (F);

      when Storage_Error =>     -- Error, Counted
         declare
            function H return String is
            begin
               return F;
            end H;
         begin
            X := H;
         end;
      when Use_Error =>         -- OK (Found but Use_Error)
         X := G(F);
   end;

   begin  -- Check local declarations
      null;
   exception
      when Constraint_Error =>  -- Error, Counted
         declare
            package Pack is new Genpack1 ("");
         begin
            null;
         end;
      when End_Error =>          -- Found
         declare
            package Pack is new Genpack1 (F);
         begin
            null;
         end;
      when Data_Error =>         -- Found
         declare
            package Pack is
               X : String := F;
            end Pack;
         begin
            null;
         end;
      when Storage_Error =>      -- OK
         declare
            package Pack is
            end Pack;
            package body Pack is
            begin
               P;
            end Pack;
         begin
            null;
         end;
   end;

   declare   -- Check instantiations
      procedure PP is new Genproc;
      function FF is new Genfunc;
   begin
      null;
   exception
      when Name_Error =>        -- Found
         declare
            package Pack is new Genpack2;
         begin
            null;
         end;
      when Data_Error =>        -- Found
         I := FF'length;
      when Storage_Error =>     -- OK
         PP;
   end;

   begin  -- Check exit
      null;
   exception
      when Constraint_error =>   -- Error, Counted
         loop
         if True then
            exit;
         end if;
         P;
         end loop;
      when Tasking_Error =>  -- Error, Counted
      L11:
         loop
      L12:
         loop
            if True then
               exit L11;
            end if;
         end loop L12;
         P;
         end loop L11;
      when Program_Error => -- OK
      L21:
         loop
      L22:
         loop
            if True then
               P;
               exit L21;
            end if;
         end loop L22;
         P;
         end loop L21;
   end;

   declare -- Check return, requeue
      protected Prot is
         entry E1;
         entry E2;
      end Prot;

      protected body Prot is
         entry E1 when True is
         begin
            null;
         exception
            when others => -- Found, Counted
               requeue E2;
         end;
         entry E2 when True is
         begin
            null;
         end;
      end Prot;

   begin
      null;
   exception
      when others => -- Found, Counted
         return;
   end;

   declare  -- Check extended return
      function F return String is
      begin
         return "abcd";
      exception
         when Data_Error =>
            return S : String := "AAAA" do -- Found, Counted
               null;
            end return;
         when Constraint_Error =>          -- Found, Counted
            return S : String := F do
               null;
            end return;
         when Tasking_Error =>
            return S : String (1 .. 3) do
               S := "ABC";
               P;
               return;
            end return;
         when others =>                   -- Error, Counted
            return S : String (1 .. 3) do
               S := "ABC";
               if S(2) = 'A' then
                  goto Next;
               else
                  P;
               end if;
            end return;
            <<Next>> null;
      end F;
   begin
      null;
   end;
end T_silent_exceptions;
