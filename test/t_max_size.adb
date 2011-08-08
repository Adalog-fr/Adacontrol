procedure T_Max_Size is
   V : Integer;

   task T is
      entry E;
   end T;
   task body T is
   begin
      accept E;
      accept E do
         null;
      end E;
      accept E do  -- accept
         null;
         null;
         null;
      end E;
   end T;
begin
   if V = 1 then       -- if
      if V = 2 then    -- if branch
         null;
      end if;
   elsif V = 2 then
      null;           -- if branch
      null;
      null;
   else
      null;
      null;
   end if;

   case V is          -- case
      when 1 =>
         null;
         null;
      when 2 =>
         null;        -- case_branch
         null;
         null;
      when others =>
         case V is    -- case branch
            when others =>
               null;
         end case;
   end case;

   while V /= 0 loop  --OK
      null;
   end loop;
   loop               -- Unnamed_Loop
      null;
      null;
   end loop;
L1: for I in 1 .. 10 loop
      null;
      null;
   end loop L1;
L2: for I in 1 .. 10 loop -- Loop
      null;
      null;
      null;
      null;
   end loop L2;

   begin                 -- Block, Unnamed_Block, Simple_Block
      declare
         VVV : Integer;
      begin
         null;
      end;
   exception
      when others =>
         null;
   end;

B1: begin                 -- Block, Simple_Block
      declare
         VVV : Integer;
      begin
         null;
      end;
   exception
      when others =>
         null;
   end B1;

   declare                -- Block, Unnamed_Block
      VV : Integer;
   begin
      declare
         VVV : Integer;
      begin
         null;
      end;
   end;

   B2: declare            -- Block
      VV : Integer;
   begin
      declare
         VVV : Integer;
      begin
         null;
      end;
   end B2;
end T_Max_Size;
