with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Xfw_Fixes.Child2;
with Ada.Calendar;
with Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded;

package body Tfw_Fixes is

   procedure Console_Log is
      use Ada.Wide_Text_IO;
      use all type Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   begin  -- Console_Log
      Put_Line (To_Wide_String (To_Unbounded_Wide_String ("É")));
   end Console_Log;

   -----------
   -- Hello --
   -----------

   procedure Hello is
      
      procedure World is
         X : constant Boolean := False;
         Y : constant Boolean := True;
      begin  -- World
         if X = False and then Y = (True) then
            Put ("World");
         
         end if;

         Put_Line ("!");
      end World;

      C : constant Integer := 1;
      I : Integer := 1;
      procedure Not_Static (I : in out Integer) is
      begin  -- Not_Static
         I := 1;
      end Not_Static;
   begin  -- Hello
      Put ("Hello ");

      while I = 1 loop
         Not_Static (I);
         exit when I /= I+1;
         exit when C = 1;                             -- unreachable
         
      end loop;
      

      New_Line;
      World;
   end Hello;

   
   use type Xfw_Fixes.Child2.Mod_Type1;
   
   use type Xfw_Fixes.Child2.Mod_Type2;
   V1 : constant Xfw_Fixes.Child2.Mod_Type1 := 1;
   V2 : constant Xfw_Fixes.Child2.Mod_Type1 := 2;
   V3 : constant Xfw_Fixes.Child2.Mod_Type1 := V1 + V2;
   V4 : constant Xfw_Fixes.Child2.Mod_Type2 := 3;
   V5 : constant Xfw_Fixes.Child2.Mod_Type2 := 4;
   V6 : constant Xfw_Fixes.Child2.Mod_Type2 := V4 + V5;
begin
   Hello;
end Tfw_Fixes;
