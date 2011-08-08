procedure T_Max_Statement_Nesting is

   V, V1, V2, V3 : Integer := 0;

   procedure Z is
   begin
      for X in 0..1 loop                                  -- loop 1, all 1
         loop                                             -- loop 2, all 2
            while True loop                               -- loop 3, all 3
               case V3 is                                 -- case 1, all 4
                  when 3 =>
                     loop                                 -- loop 4, all 5
                        null;
                     end loop;
                  when others =>
                     null;
               end case;
            end loop;
         end loop;
      end loop;
   end Z;

   begin
   if True then                                           -- if 1,   all 1
      if True then                                        -- if 2,   all 2
         if True then                                     -- if 3,   all 3
            if True then                                  -- if 4,   all 4
               null;
            else
               case V is                                  -- case 1, all 5
                  when 0 =>
                     null;
                  when others =>
                     null;
               end case;
            end if;
         end if;
      end if;
   end if;

   case V1 is                                             -- case 1, all 1
      when 1 =>
         case V1 is                                       -- case 2, all 2
            when 1 =>
               case V2 is                                 -- case 3, all 3
                  when 2 =>
                     null;
                  when others =>
                     case V3 is                           -- case 4, all 4
                        when 3 =>
                           loop                           -- loop 1, all 5
                              null;
                           end loop;
                        when others =>
                           null;
                     end case;
               end case;
            when others =>
               null;
         end case;
      when others =>
         null;
   end case;

   if True then                                           -- if 1,    all 1
      if True then                                        -- if 2,    all 2
         declare                                          -- block 1, all 3
            procedure X is
               task Y is
                  entry E;
               end Y;
               task body Y is
               begin
                  accept E do
                     if True then                         -- if 1,   all 1
                        if True then                      -- if 2,   all 2
                           if True then                   -- if 3,   all 3
                              for X in 0..1 loop          -- loop 1, all 4
                                 loop                     -- loop 2, all 5
                                    while True loop       -- loop 3, all 6
                                       case V3 is         -- case 1, all 7
                                          when 3 =>
                                             loop         -- loop 4, all 8
                                                null;
                                             end loop;
                                          when others =>
                                             null;
                                       end case;
                                    end loop;
                                 end loop;
                              end loop;
                           end if;
                        end if;
                     end if;
                  end E;
               end Y;
            begin -- Procedure X
               if True then                               -- if 1,   all 1
                  if True then                            -- if 2,   all 2
                     if True then                         -- if 3,   all 3
                        loop                              -- loop 1, all 4
                           while True loop                -- loop 2, all 5
                              case V3 is                  -- case 1, all 6
                                 when 3 =>
                                    loop                  -- loop 3, all 7
                                       Y.E;
                                    end loop;
                                 when others =>
                                    null;
                              end case;
                           end loop;
                        end loop;
                     end if;
                  end if;
               end if;
            end X;
         begin
            if True then                                  -- if 3,    all 4
               begin                                      -- block 2, all 5
                  if True then                            -- if 4,    all 6
                     null;
                  end if;
               end;
            end if;
         end;
      end if;
   end if;

end T_Max_Statement_Nesting;
