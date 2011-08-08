with Ada.Exceptions; use Ada.Exceptions;
separate (T_Exception_Propagation)
procedure X_Local_Exception is
   E11, E12 : exception;   -- no handler

   procedure P1 is
      E : exception;
   begin
      raise E;
   exception
      when others =>       -- can propagate
         raise;
   end P1;

   procedure P2 is
      E : exception;       -- no handler
   begin
      null;
   end P2;

begin
   declare
      E21, E22 : exception; -- no handler
   begin
      null;
   exception
      when E22 =>
         null;
   end;

   declare
      E3 : exception;
   begin
      null;
   exception
      when E11 =>          -- can propagate
         raise E3;
      when Occur: E3 =>    -- can propagate
        Reraise_Occurrence (Occur);
      when Occur: E12 =>
        Reraise_Occurrence (Occur);
   end;

   declare
      E41 : exception;
      E42 : exception renames E41;
   begin
      null;
   exception
      when E41 =>         -- can propagate
         Raise_Exception (E42'Identity);
   end;
exception
   when E12 =>
      null;
end X_Local_Exception;
