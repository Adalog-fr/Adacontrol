separate (T_Unsafe_Paired_Calls)
procedure Breaks is
   procedure P1 is null;
   procedure V1 is null;
   procedure P2 is null;
   procedure V2 is null;
   Expression : constant Boolean := True;
   X          : Integer := 3;

begin
   -- Breaking statements, simple case
   begin
      P1;

      if X = 2 then
         V1;
         goto Bottom1;   -- OK exits block
      end if;

      if X = 2 then
         V1;  -- Error, has closing block and does not exit
         goto Inside1;
      end if;

      <<Inside1>>
      V1;
   exception
      when others =>
         V1;
   end;
   <<Bottom1>>

   -- Nested breaking statements
   begin
      P1;

      begin
         if X = 2 then
            V1;
            goto Bottom2;   -- OK exits block
         end if;

         if X = 2 then
            V1;  -- Error, has closing block and does not exit
            goto Inside2;
         end if;
      end;

      <<Inside2>>
      V1;
   exception
      when others =>
         V1;
   end;
   <<Bottom2>>

   -- Breaking statements, bloc case
   begin
      if Expression then
         P1;
      else
         P2;
      end if;

      if X = 2 then
         if Expression then
            V1;
         else
            V2;
         end if;
         null;
         goto Bottom3;   -- OK exits block
      end if;

      if X = 2 then
         if Expression then
            V1;  -- Error, has closing block and does not exit
         else
            V2;  -- Error, has closing block and does not exit
         end if;
         goto Inside3;
      end if;

      <<Inside3>>
      if Expression then
         V1;
      else
         V2;
      end if;
   exception
      when others =>
         if Expression then
            V1;
         else
            V2;
         end if;
   end;
   <<Bottom3>>

   begin
      P1;

      if X = 2 then
         if Expression then
            V1;                  -- Error, nested call
         else
            V2;                  -- Error, no opening call
         end if;
         return;                 -- Error, no closing block
      end if;

      L1 : loop
         begin
            P2;
            goto Next;  -- OK, no closing block, does not exit scope
            <<Next>>
            if X = 2 then
               V2;
               exit L1;       -- OK, closing block, exits scope
            end if;
            L2: while Expression loop
               V2;            -- Error, closing block, does not exit scope
               exit L2;
            end loop L2;
            L3 : while Expression loop
               V2;            -- OK, closing block, exits scope
               exit L1;
            end loop L3;
            exit L1;             -- Error, no closing block, exits scope
         exception
            when others => V2;
         end;
      end loop L1;

      -- Check return that does not exit a block
      declare
         procedure P is
         begin
            P2;
            if X = 2 then
               V2;
               return;   -- OK (P1 not closed here)
            end if;

            V2;
         exception
            when others =>
               V2;
         end P;
      begin
         P;
      end;

      V1;
      return;
   exception
      when others =>
         V1;
   end;

   -- Check nested blocks
   begin
      P1;

      begin
         P2;

         if X = 2 then
            V2;
            return;  -- Error, Missing V1
         else
            V1;
            return;  -- Error, Missing V2
         end if;

         V2;
      exception
         when others =>
            V2;
      end;

      V1;
   exception
      when others =>
         V1;
   end;

end Breaks;
