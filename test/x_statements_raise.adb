package body X_Statements_Raise is
   Internal : exception;

   procedure P is
   begin
      raise Exported;    -- raise
      raise Internal;    -- raise, raise_nonpublic
      raise Private_Exc; -- raise, raise_nonpublic
   end P;

begin
   raise Exported;    -- raise
   raise Internal;    -- raise, raise_nonpublic
   raise Private_Exc; -- raise, raise_nonpublic
end X_Statements_Raise;
