-- Objective : Check derogation (rule_file_off from rules files)
procedure Tfw_Rule_File_Off is
   A : Integer;
   B : Float;
   X : exception;

   --## rule on rule1
   C : Integer;
   D : Float;      -- Error
   Y : exception;

   --## rule on entities
   E : Integer;    -- Error
   F : Float;      -- Error
   Z : exception;
begin
   null;
end Tfw_Rule_File_Off;
