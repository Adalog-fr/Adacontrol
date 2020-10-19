----------------------------------------------------------------------
--  Framework.Variables.Shared_Types - Package body                 --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2012.           --
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

package body Framework.Variables.Shared_Types is
   package body String_Type is
      procedure Set (Variable : in out String_Type.Object; To : Wide_String) is
      begin
         Variable.Value := To_Unbounded_Wide_String (To);
      end Set;

      function Value_Image  (Variable : in String_Type.Object) return Wide_String is
      begin
         return '"' & To_Wide_String (Variable.Value) & '"';
      end Value_Image;

      function All_Values (Variable : in String_Type.Object) return Wide_String is
         pragma Unreferenced (Variable);
      begin
         return "String";
      end All_Values;
   end String_Type;

end Framework.Variables.Shared_Types;
