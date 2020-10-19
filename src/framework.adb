----------------------------------------------------------------------
--  Framework - Package body                                        --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2008.           --
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

-- Adalog
with
  Units_List,
  Utilities;

-- AdaControl
with
  Framework.Control_Manager;

package body Framework is

   -----------
   -- Image --
   -----------

   function Image (Entity : in Entity_Specification) return Wide_String is
      use Utilities;
   begin
      case Entity.Kind is
         when Box =>
            return "<>";
         when Equal =>
            return "=";
         when Regular_Id =>
            return To_Title (To_Wide_String (Entity.Specification));
         when All_Id =>
            return "all " & To_Title (To_Wide_String (Entity.Specification));
      end case;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Name : in Wide_String) return Entity_Specification is
      use Utilities;
   begin
      return (Kind          => Regular_Id,
              Specification => To_Unbounded_Wide_String (To_Upper (Name)));
   end Value;

   -------------------------------
   -- Entity_Specification_Kind --
   -------------------------------

   function Entity_Specification_Kind (Entity : in Entity_Specification) return Entity_Specification_Kinds is
   begin
      return Entity.Kind;
   end Entity_Specification_Kind;

   -------------
   -- Matches --
   -------------

   function Matches (Entity    : in Entity_Specification;
                     Name      : in Asis.Element;
                     Extend_To : in Extension_Set := No_Extension) return Boolean
   is
   -- This implementation of Matches is a bit violent, but it ensures consistency with Matching_Context
      use Framework.Control_Manager;

      Junk_Store : Context_Store;
      Result     : Boolean;
   begin
      if Entity.Kind = Box then
         return True;
      end if;

      Associate (Junk_Store, Entity, Root_Context'(null record));
      Result := Matching_Context (Junk_Store, Name, Extend_To) /= No_Matching_Context;
      Clear (Junk_Store);
      return Result;
   end Matches;

begin  -- Framework
   Units_List.Initialize (Adactl_Context'Access);
end Framework;
