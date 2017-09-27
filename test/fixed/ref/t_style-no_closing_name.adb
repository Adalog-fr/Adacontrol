separate (T_Style)
procedure No_Closing_Name is
   procedure P (X : in Integer) is   -- No_Closing_Name
   begin
      null;
      null;
      null;
   end P;

   package Pack is                -- No_Closing_Name
      Z : Integer;
   end Pack;

   package body Pack is           -- No_Closing_Name
   begin
      null;
   end Pack;

   generic                        -- No_Closing_Name
   package Gen is
      I : Integer;
   end Gen;

   task T1 is                     -- No_Closing_Name
   end T1;
   task body T1 is                -- No_Closing_Name
   begin
      null;
   end T1;

   task T2;
   task body T2 is
   begin
      null;
   end T2;

   task type T3 is                -- No_Closing_Name
   end T3;
   task body T3 is                -- No_Closing_Name
   begin
      null;
   end T3;

   task type T4;
   task body T4 is
   begin
      null;
   end T4;
begin
   null;
end No_Closing_Name;
