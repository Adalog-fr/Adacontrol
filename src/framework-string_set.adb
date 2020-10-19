----------------------------------------------------------------------
--  Framework.String_Set - Package body                             --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
--  The Ada Controller is  free software; you can  redistribute  it --
--  and/or modify it under  terms of the GNU General Public License --
--  as published by the Free Software Foundation; either version 2, --
--  or (at your option) any later version. This unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from the units  of this program, or if you  link this unit with --
--  other files  to produce  an executable, this  unit does  not by --
--  itself cause the resulting executable  to be covered by the GNU --
--  General  Public  License.   This  exception  does  not  however --
--  invalidate any  other reasons why the executable  file might be --
--  covered by the GNU Public License.                              --
----------------------------------------------------------------------
package body Framework.String_Set is
   Dummy : constant Null_Record := (null record);

   ---------
   -- Add --
   ---------

   procedure Add (To : in out Set; Key : in Wide_String) is
   begin
      Add (To, To_Unbounded_Wide_String (Key), Dummy);
   end Add;

   --------------
   -- Cardinal --
   --------------

   function Cardinal (The_Set : in Set) return Natural is
      use Null_Map;

      Temp : Map := Map (The_Set);
      -- must have a variable for Count_Elements

      Counter : Natural := 0;
      procedure Inc (Key : Unbounded_Wide_String; Val : in out Null_Record) is
         pragma Unreferenced (Key, Val);
      begin
         Counter := Counter + 1;
      end Inc;
      procedure Count_Elements is new Iterate (Inc);

   begin  -- Cardinal
      Count_Elements (Temp);
      return Counter;
   end Cardinal;

   ------------
   -- Delete --
   ------------

   procedure Delete (From : in out Set; Key : in Wide_String) is
   begin
      Delete (From, To_Unbounded_Wide_String (Key));
   end Delete;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (The_Set : in Set) return Boolean is
   begin
      return Null_Map.Is_Empty (Null_Map.Map (The_Set));
   end Is_Empty;

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present (Within : in Set; Key : in Wide_String) return Boolean is
   begin
      return Is_Present (Within, To_Unbounded_Wide_String (Key));
   end Is_Present;

   -------------
   -- Balance --
   -------------

   procedure Balance (The_Set : in out Set) is
   begin
      Null_Map.Balance (Null_Map.Map (The_Set));
   end Balance;

   -----------
   -- Clear --
   -----------

   procedure Clear (The_Set : in out Set) is
   begin
      Null_Map.Clear (Null_Map.Map (The_Set));
   end Clear;

end Framework.String_Set;
