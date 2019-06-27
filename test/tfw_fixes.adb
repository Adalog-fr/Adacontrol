with Ada.Text_IO, Ada.Strings.Unbounded, Xfw_Fixes.Child2;
with Ada.Calendar, Ada.Wide_Text_IO, Ada.Strings.Wide_Unbounded;

package body Tfw_Fixes is

   procedure Console_Log is
      use Ada.Wide_Text_IO, Ada.Strings.Wide_Unbounded;
   begin
      Put_Line (To_Wide_String (To_Unbounded_Wide_String ("É")));
   end Console_Log;

   -----------
   -- Hello --
   -----------

   procedure Hello is
      use Ada.Text_IO;
      procedure World is
         X : constant Boolean := False;
         Y : constant Boolean := True;
      begin
         if X = False and then Y = (True) then
            Put ("World");
         else
            null;
         end if;

         if Y = False then
            Put_Line (".");
         else
            Put_Line ("!");
         end if;
      end World;

      C : constant Integer := 1;
      I : Integer := 1;
   begin
      Put ("Hello ");

      while I = 1 loop
         I := 0;
         exit when I /= 1;
         exit when C = 1;                             -- unreachable
         I := 3;
         exit;                                        -- unreachable
         I := 5;
         exit;
      end loop;
      while (C + 3) / 4 /= 1 loop                     -- never executed
         I := I + 1;
      end loop;

      New_Line;
      World;
   end Hello;

   use all type Xfw_Fixes.Child2.Mod_Type1;
   use all type Xfw_Fixes.Child2.Mod_Type2;
   V1 : constant Xfw_Fixes.Child2.Mod_Type1 := 1;
   V2 : constant Xfw_Fixes.Child2.Mod_Type1 := 2;
   V3 : constant Xfw_Fixes.Child2.Mod_Type1 := V1 + V2;
   V4 : constant Xfw_Fixes.Child2.Mod_Type2 := 3;
   V5 : constant Xfw_Fixes.Child2.Mod_Type2 := 4;
   V6 : constant Xfw_Fixes.Child2.Mod_Type2 := V4 + V5;
begin
   Hello;
end Tfw_Fixes;
