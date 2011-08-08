with Ada.Text_io;                                   -- Casing
with Ada.Strings.Wide_Unbounded;
procedure T_Style is
   procedure No_Default_In          is separate;
   procedure Casing                 is separate;
   procedure Positional_Association is separate;
   procedure No_Closing_Name        is separate;
   procedure Renamed_Entity         is separate;
   procedure Literals               is separate;
   procedure Exposed_Literal        is separate;
   procedure Multiple_Elements      is separate;

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

   case B is  when others =>                            -- Compound_Statement
         null; end case;

   for I in 0 .. 1 loop                             -- Compound_Statement
      null; end loop;
end T_Style;




