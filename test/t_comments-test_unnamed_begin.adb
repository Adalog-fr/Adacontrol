separate (T_Comments)
procedure Test_Unnamed_Begin is

   procedure P1 is begin null; end P1; --P1

   procedure P2 is
   begin      --            P2
      null;
   end;

   package Pack1 is end Pack1;

   package body Pack1 is
      procedure P3 is
      begin      --            P3;
         null;
      end;

      function F1 return Integer is
      begin
         return 0;
      end F1;

      function F2 return Integer is
         Result : constant Integer := 0;
      begin
         return Result;
      end F2;

      function F3 return Integer is
         Result : constant Integer := 0;
      begin  -- F3
         return Result;
      end F3;

   begin
      null;
   end Pack1;

   package Pack2 is end Pack2;

   package body Pack2 is
      I : Integer;
   begin
      null;
   end Pack2;
begin
   null;
end Test_Unnamed_Begin;
