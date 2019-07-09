with Ada.Calendar;
use  Ada.Calendar;
separate (T_Simplifiable_Statements)
procedure Test_While_For_For is
   V   : Integer;
   Ren : Integer renames V;
   Y, Z : Integer;

   type Enum is (A, B, C);
   E   : Enum := A;

   procedure P (I : in Integer; O : out Integer; IO : in out Integer) is
   begin
      null;
   end P;

   function F (I : Integer) return Integer is (I);
begin
   V := 1;
   while V <= 10 loop                        -- While_for_for direct x2
      V := V + 1;
      P (V, Y, Z);
   end loop;

   while 10 <= V loop                        -- While_for_for reverse x2
      V := V - 1;
   end loop;

   while V > 10 loop                         -- While_for_for reverse x2
      V := V - 1;
   end loop;

   while 10 > V loop                         -- While_for_for direct x2
      V := V - (-1);
   end loop;

   while V < 10 loop                         -- While_for_for direct x2
      V := Integer'Succ (V);
   end loop;

   while V > 10 loop                         -- While_for_for reverse x2
      V := Integer'Pred (V);
   end loop;

   while V /= 10 loop                        -- While_for_for direct x2
      V := Integer'Succ (V);
   end loop;

   while V /= 1 loop                         -- While_for_for reverse x2
      V := V - 1;
   end loop;

   while V <= 10 loop                        -- While_for_for direct (with renaming) x2
      Ren := V + 1;
   end loop;

   while V <= 10 loop                        -- While_for_for direct (with renaming) x2
      V := Ren + 1;
   end loop;

   while V <= 10 loop                        -- While_for_for direct (with renaming) x2
      Ren := Ren + 1;
   end loop;

   while Ren <= 10 loop                      -- While_for_for direct (with renaming) x2
      V := V + 1;
   end loop;

   while E <= B loop                         -- While_for_for direct x2
      E := Enum'Succ (E);
   end loop;

   while A < E loop                          -- While_for_for reverse x2
      E := Enum'Pred (E);
   end loop;

   L1 : while V <= 10 loop                   -- While_for_for direct (not same loop exited) x2
      L2 : for I in 1 .. 10 loop
         if I = 2 then
            exit L2;
         end if;
      end loop L2;
      V := V + 1;
   end loop L1;

   -- Inapplicable cases
   while V <= 10 loop                        -- OK (bad direction)
      V := V - 1;
   end loop;

   while V <= 10 loop                        -- OK (out parameter)
      V := V + 1;
      P (Z, V, Z);
   end loop;

   while V <= 10 loop                        -- OK (in out parameter, nested)
      V := V + 1;
      if Z = 0 then
         for I in 1 .. 10 loop
            P (Z, Z, V);
         end loop;
      end if;
   end loop;

   while E > A loop                          -- OK (bad direction)
      E := Enum'Succ (E);
   end loop;

   while V <= 10 loop                        -- OK (not increment by 1)
      V := V + 2;
   end loop;

   while V <= 10 loop                        -- OK (nested increment)
      if V /= 3 then
         V := V + 1;
      end if;
   end loop;

   while V <= Integer (Seconds (Clock)) loop -- OK (no assignment);
      null;
   end loop;

   while V <= 10 loop                        -- OK (more than one increment)
      V := V + 1;
      V := V + 1;
   end loop;

   while V <= 10 loop                        -- OK (deep assignment)
      if V < 0 then
         for I in 1 .. 10 loop
            V := V + 1;
         end loop;
      end if;
   end loop;

   while V <= 10 loop                        -- OK (increment + deep assignment)
      V := V + 1;
      if V < 0 then
         for I in 1 .. 10 loop
            V := 2 * V;
         end loop;
      end if;
   end loop;

   while V <= 10 loop                        -- While_for_for direct x1 (exit statement)
      exit when V = 3;
      V := V + 1;
   end loop;

   L3 : while V <= 10 loop                   -- While_for_for direct x1 (exit statement, nested)
      L4 : for I in 1 .. 10 loop
         if I = 2 then
            exit L3;
         end if;
      end loop L4;
      V := V + 1;
   end loop L3;

   V := 1;
   while V <= F(10) loop                     -- While_for_for direct x1 (function call)
      V := V + 1;
      P (V, Y, Z);
   end loop;

end Test_While_For_For;
