with Ada.Text_IO;                                   -- Casing
with Ada.Strings.Wide_Unbounded;
procedure T_Style is
   procedure No_Default_In     is separate;
   procedure Casing            is separate;
   procedure No_Closing_Name   is separate;
   procedure Renamed_Entity    is separate;
   procedure Literals          is separate;
   procedure Exposed_Literal   is separate;
   procedure Multiple_Elements is separate;
   procedure Parameter_Order   is separate;

   B : Boolean;
begin
   if not B then                                    -- OK
      null;
   end if;

   if not B then                                    -- OK
      null;
   elsif B then
      null;
   end if;

   if not B then                                    -- Negative_Condition
      null;
   else
      null;
   end if;

   if ((not B)) then                                -- Negative_Condition
      null;
   else
      null;
   end if;

   case B is  
   when others =>                        -- Compound_Statement, Multiple_Stmts
         null; 
   end case;                            -- Multiple_Stmts

   for I in 0 .. 1 loop                             -- Compound_Statement
      null; 
   end loop;                               -- Multiple_Stmts
   -- An acceptable comment
end T_Style;




