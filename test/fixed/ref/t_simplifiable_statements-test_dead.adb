with Ada.Exceptions; use Ada.Exceptions;
separate (T_Simplifiable_Statements)
procedure Test_Dead is
   I    : Integer;
   RenI : Integer renames I;
   J    : Integer;
   RenJ : Integer renames J;
   C    : constant Integer := 1;
   Lim  : Integer := C;

   subtype ST1 is Integer range 5 .. 4;
   subtype ST2 is Integer range 2 * Lim .. 5 * Lim; -- 2..5

   S : String (1 .. 10);
   type S_Ptr is access String;
   V : S_Ptr;
   W : String renames V.all;
begin
   if I = 1 then
      null;
   
   else
      I := 1;
      Raise_Exception (Constraint_Error'Identity); -- Unreachable
      
   end if;
   
   if I = 1 then
      I := 2;
   end if;

   case I is
      
      
      
      when 8 .. 7 | 2 =>
         I := 3;
      when others =>
         I := 0;
   end case;

   case ST2 (I) is
      when 0 .. 1 =>                               -- choices cover no value
         null;
      when 2 | 3.. 5 =>                            -- OK (bounds deducted from subtype conversion)
         I := 1;
      when others =>
         null;
   end case;

   I := 1;
   case I is
      when 1 =>
         null;
      when 2 =>                                    -- choices cover no value
         null;
      when 3 .. 10 =>                              -- choices cover no value
         null;
      when others =>
         null;
   end case;

   

   if V'Length = 0 then                            -- OK (dynamic, implicit dereference)
      I := 1;
   end if;

   if V.all'Length = 0 then                        -- OK (dynamic, explicit dereference)
      I := 1;
   end if;

   if W'Length = 0 then                            -- OK (dynamic, renaming of dereference)
      I := 1;
   end if;

   while I = 1 loop
      exit when I /= 1;
      exit when C = 1;                             -- unreachable
      
   end loop;
   

   
   for X in 1 .. C loop
      I := 3;
   end loop;
   for X in 1 .. C + 1 loop
      I := 3;
   end loop;

   <<L1>> goto L1;                                 -- OK (next statement is labelled)
   <<L2>> goto L2;                                 -- Unreachable
   
   <<L3>>                                          -- no more unreachable from here

   -- Check equivalent branches
   if I = 1 then                                  -- (1)
      null;
   
   elsif I + J - 2 = 0 then                       -- (2)
      null;
   
   
   end if;

   if I + 2 > Integer'(J) + 3 then                -- (3)
      null;
   
   
   elsif "+" (Right => I, Left => 2) > J + 3 then
      null;
   end if;

   if Integer'Pos (I) > Integer'Pos (1) then      --  (4)
      null;
   elsif Positive'Pos (I) > Integer'Pos (J) then
      null;
   elsif I'Image /= J'Image then                  --  (5)
      null;
   
   
   end if;

   declare -- case of aggregates
      type Rec1 is tagged
         record
            I, J : Integer;
         end record;
      V1 : Rec1;

      type Rec2 is new Rec1 with
         record
            K : Integer;
         end record;
      V2 : Rec2;

      type Arr1 is array (1 .. 5) of Integer;
      A1 : Arr1;
   begin
      if V1 = (0, 1) then                         --  (6)
         null;
      
      end if;

      if V2 = (J => 1, I => 0, K=> 2) then        --  (7)
         null;
      
      end if;

      if A1 = (1, 2, 3, 4, 5) then                                      --  (8)
         null;
      
      end if;

      if A1 = Arr1'(1, 2, 3, others => 0) then                          --  (9)
         null;
      
      end if;

      if A1 = Arr1'(1, 2, 3, 4, others => 0) then
         null;
      elsif A1 = Arr1'(1, 2, 3, 4, 0) then                              --  OK (others vs. value)
         null;
      end if;
   end;

end Test_Dead;
