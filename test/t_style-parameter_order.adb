with Ada.Numerics.Generic_Elementary_Functions;
separate (T_Style)
procedure Parameter_Order is
   procedure P1 (X   : Integer;              -- no_default_in
                 Y   : access Integer;
                 Z   : out Integer;
                 T   : in out Integer;
                 Def : in Integer := 1)
   is
   begin
      null;
   end P1;

   procedure P2 (X   : Integer;              -- no_default_in
                 Def : in Integer := 1)
   is
   begin
      null;
   end P2;

   procedure P3 (X   : in  Integer;
                 Z   : out Integer;
                 T   : in out Integer;
                 Def : in Integer := 0;
                 Y   : access Integer);      -- out_of_order
   procedure P3 (X   : in  Integer;
                 Z   : out Integer;
                 T   : in out Integer;
                 Def : in Integer := 0;
                 Y   : access Integer)
   is
   begin
      null;
   end P3;

   -- only one out of order
   procedure P4 (X   : in Integer;
                 Z   : out Integer;
                 Y   : access Integer;       -- out_of_order
                 T   : in out Integer;
                 Def : in Integer := 0)
   is
   begin
      null;
   end P4;

   procedure P5 (X : in out Integer) is
   begin
      null;
   end P5;

   generic
      X : in Integer;
      Y : in Integer := 1;
      Z : in out Integer;
      with procedure P;
      with function F return Integer is <>;
      with package Pack1 is new Ada.Numerics.Generic_Elementary_Functions (Float);
      with package Pack2 is new Ada.Numerics.Generic_Elementary_Functions (<>);
   procedure Gen1;
   procedure Gen1 is
   begin
      null;
   end Gen1;

   generic
      with package Pack is new Ada.Numerics.Generic_Elementary_Functions (Float);
      with function F return Integer is <>;              -- out_of_order
      Z : in out Integer;                                -- out_of_order
      X : in Integer;                                    -- out_of_order
   procedure Gen2 (A : in Integer := 0; B : Integer);    -- out_of_order, default_in
   procedure Gen2 (A : in Integer := 0; B : Integer) is  -- default_in
   begin
      null;
   end Gen2;
begin
   null;
end Parameter_Order;
