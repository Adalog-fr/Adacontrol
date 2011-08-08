procedure T_no_closing_name is
   procedure P (X : Integer) is
   begin
      null;
      null;
      null;
   end;

   package Pack is
      Z : Integer;
   end;

   package body Pack is
   begin
      null;
   end;

   generic
   package Gen is
      I : Integer;
   end;
begin
   null;
end T_no_closing_name;

