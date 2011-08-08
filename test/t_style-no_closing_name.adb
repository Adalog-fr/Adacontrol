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

   task T1 is                     -- No_Closing_Name
   end;
   task body T1 is                -- No_Closing_Name
   begin
      null;
   end;

   task T2;
   task body T2 is
   begin
      null;
   end T2;

   task type T3 is                -- No_Closing_Name
   end;
   task body T3 is                -- No_Closing_Name
   begin
      null;
   end;

   task type T4;
   task body T4 is
   begin
      null;
   end T4;
begin
   null;
end No_Closing_Name;
