separate (T_Simplifiable_Statements)
procedure Test_Loop is
   I : Integer := 1;
begin
   loop  -- loop x2
      null;
   end loop;
   loop -- loop x2
      null;
   end loop;
   
   while I < 2 loop -- while_for_for x2
      I := I + 1;
   end loop;
   while I < 5 loop -- while_for_for x1
      I := I + 1;
      if I = 5 then
         exit;
      end if;
   end loop;
   loop             -- loop_for_while x2
      exit when I = 1;
   end loop;
   L1 : loop
      loop
         exit L1;
      end loop;
   end loop L1;
   L2 : loop        -- loop_for_while x2
      exit when I = 1;
      loop
         if I > 2 then
            exit;
         end if;
      end loop;
   end loop L2;
   L3 : loop        -- loop_for_while x1
      exit when I = 1;
      loop
         if I > 2 then
            exit L3;
         end if;
      end loop;
   end loop L3;
   L4 : loop        -- loop_for_while x2
      exit when I = 1;
      loop
         case I is
            when 2 =>
               null;
            when others =>
               exit;
         end case;
      end loop;
   end loop L4;
   L5 : loop        -- loop_for_while x1
      exit when I = 1;
      loop
         case I is
            when 2 =>
               null;
            when others =>
               exit L5;
         end case;
      end loop;
   end loop L5;
end Test_Loop;
