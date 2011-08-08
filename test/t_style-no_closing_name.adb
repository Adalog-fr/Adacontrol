separate (T_Style)
procedure No_Closing_Name is
   procedure P (X : in Integer) is   -- No_Closing_Name
   begin
      null;
      null;
      null;
   end;

   package Pack is                -- No_Closing_Name
      Z : Integer;
   end;

   package body Pack is           -- No_Closing_Name
   begin
      null;
   end;

   generic                        -- No_Closing_Name
   package Gen is
      I : Integer;
   end;
begin
   null;
end No_Closing_Name;
