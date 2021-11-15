----------------------------------------------------------------------
--  Adactl_Version - Function body                                  --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2020.           --
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

-- Adacontrol
with
   Framework.Specific_Plugs;
function Adactl_Version return Wide_String is

   Version : constant Wide_String := "1.22r16b";

begin
   -- Gnat (and AdaControl) warn that the following condition is always false/true, but
   -- this is intended
   pragma Warnings (Off);
   if Framework.Specific_Plugs.Specific_Version = "" then  --## Rule Line Off Simplifiable_Statements
      return Version;
   else                                                    --## Rule Line Off Simplifiable_Statements
      return Version & '-' & Framework.Specific_Plugs.Specific_Version;
   end if;
end Adactl_Version;
