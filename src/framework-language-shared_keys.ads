----------------------------------------------------------------------
--  Framework.Language.Shared_Keys - Package specification          --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Controller  is  free software;  you can redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
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
--                                                                  --
--  This  software is  distributed  in  the hope  that  it will  be --
--  useful,  but WITHOUT  ANY  WARRANTY; without  even the  implied --
--  warranty  of  MERCHANTABILITY   or  FITNESS  FOR  A  PARTICULAR --
--  PURPOSE.                                                        --
----------------------------------------------------------------------

-- Adacontrol
with
   Framework.Language;
pragma Elaborate (Framework.Language);
-- We must "with" our parent, because the pragma Elaborate is required
-- here, as for any unit that instantiates a generic from Framework.Language
package Framework.Language.Shared_Keys is

   -----------------------------------------------------------------------------------
   -- Scope_Places
   -----------------------------------------------------------------------------------

   type Scope_Places is (S_All, S_Block, S_Local, S_Nested, S_Own, S_Private, S_Public, S_Task_Body);
   package Scope_Places_Utilities is new Modifier_Utilities (Scope_Places, "S_");

   subtype Places_Set is Scope_Places_Utilities.Modifier_Set;
   Everywhere : constant Places_Set := (S_All => True, others => False);
   function Get_Places_Set_Modifiers return Places_Set;
   function Is_Applicable (Expected_Places : Places_Set) return Boolean;
   -- Checks if Current_Scope matches all Scope_Places in Expected_Places

end Framework.Language.Shared_Keys;
