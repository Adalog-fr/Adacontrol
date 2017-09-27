-- WARNING!!
-- This file contains TAB characters
-- If you edit this file, make sure your editor does not remove TABs!
procedure T_Characters  is
   -- Following line contains only a TAB:
   
   -- Following line starts with a TAB:
   X : Integer;
   -- Following line ends with a TAB:
   Y : Integer;    
   -- Following line has a TAB in the middle:
   Z : Integer;
   -- not ISO_646:
   S1 : constant String := "é à µ";
   -- wide:
   S2 : constant Wide_String := "["1041"] ["1042"]";
begin
   -- Following line has 3 trailing spaces:
   null;
   -- Following line contains 4 spaces

end T_Characters;
