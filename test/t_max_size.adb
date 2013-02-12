procedure T_Max_Size is
   V : Integer;

   package Pack1 is
      A : Integer;
   end Pack1;                        -- OK

   package Pack2 is
      B : Integer;
      procedure P;
      function F return Integer;
   end Pack2;                        -- Package_Spec

   package body Pack2 is
      procedure P is
      begin
         null;
      end P;                         -- OK
      function F return Integer is begin return 1;  end F; -- OK
   begin
      null;
   end Pack2;                        -- Package_Body

   function F return Integer is
   begin
      return 1;
   end F;                            -- Function_Body

   task T is
      entry E;
   end T;                            -- OK
   task body T is
   begin
      accept E;
      accept E do
         null;
      end E;                         -- OK
      accept E do
         null;
         null;
         null;
      end E;                         -- Accept
   end T;                            -- Task_Body

   task type TT is
      entry E1;
      entry E2;
   end TT;                           -- Task_Spec

   task body TT is
   begin
      null;
   end TT;                           -- OK

   protected type PT is
      procedure E1;
      entry E2;
   end PT;                           -- Protected_Spec

   protected body PT is
      procedure E1 is
      begin
         null;
      end E1;                        -- OK
      entry E2 when True is
      begin
         null;
      end E2;                        -- Entry_Body
   end PT;                           -- Protected_Body

   protected type P is
      procedure E1;
   end P;                            -- OK

   protected body P is
      procedure E1 is
      begin
         null;
      end E1;                        -- OK
   end P;                            -- OK
begin
   if V = 1 then
      if V = 2 then
         null;
      end if;                        -- If_Branch
   elsif V = 2 then
      null;
      null;
      null;                          -- If_Branch
   else
      null;
      null;
   end if;                           -- If

   case V is
      when 1 =>
         null;
         null;
      when 2 =>
         null;
         null;
         null;                       -- Case_Branch
      when others =>
         case V is
            when others =>
               null;
         end case;                   -- Case_Branch
   end case;                         -- Case

   while V /= 0 loop                 --OK
      null;
   end loop;
   loop
      null;
      null;
   end loop;                         -- Unnamed_Loop
L1: for I in 1 .. 10 loop
      null;
      null;
   end loop L1;
L2: for I in 1 .. 10 loop
      null;
      null;
      null;
      null;
   end loop L2;                      -- Loop

   begin
      declare
         VVV : Integer;
      begin
         null;
      end;
   exception
      when others =>
         null;
   end;                              -- Block, Unnamed_Block, Simple_Block

B1: begin
      declare
         VVV : Integer;
      begin
         null;
      end;
   exception
      when others =>
         null;
   end B1;                           -- Block, Simple_Block

   declare
      VV : Integer;
   begin
      declare
         VVV : Integer;
      begin
         null;
      end;
   end;                              -- Block, Unnamed_Block

   B2: declare
      VV : Integer;
   begin
      declare
         VVV : Integer;
      begin
         null;
      end;
   end B2;                           -- Block
end T_Max_Size;                      -- Procedure_Body, Unit
