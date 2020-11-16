separate (T_Comments)
procedure Test_Unnamed_Begin is

   -- Error: Terminating, begin OK
   procedure P1 is begin null; end P1; --P1

   -- OK (with comma separated comment)
   procedure P2 is
   begin      --            P2, with comment
      null;
      null;
      null;
   end;

   -- OK (less than 3 lines)
   procedure P3 is
   begin
      null;
   end;

   -- Error (2 statements on 3 lines)
   procedure P4 is
   begin
      null
      ;
      null;
   end;

   package Pack1 is end Pack1;

   package body Pack1 is
      -- Error: extra ';'
      procedure P3 is
      begin      --            P3;
         null;
      end;

      -- OK (no declaration)
      function F1 return Integer is
      begin
         null;
         null;
         return 0;
      end F1;

      -- Error (non empty declaration part)
      function F2 return Integer is
         Result : constant Integer := 0;
      begin
         return Result;
      end F2;

      -- OK (with space separated comment)
      function F3 return Integer is
         Result : constant Integer := 0;
      begin  -- F3 returning Integer
         null;
         null;
         return Result;
      end F3;

      -- Error: bad comment (even though less than 3 lines)
      function F4 return Integer is
      begin    -- FF
         return 0;
      end F4;

   -- Error (contains program units)
   begin
      null;
   end Pack1;

   package Pack2 is end Pack2;

   -- OK (no program unit)
   package body Pack2 is
      I : Integer;
   begin
      null;
   end Pack2;

-- Error: no comment
begin
   null;
   null;
   null;
end Test_Unnamed_Begin;
