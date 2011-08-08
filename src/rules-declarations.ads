----------------------------------------------------------------------
--  Rules.Declarations - Package specification                      --
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

-- Asis
with
  Asis;

package Rules.Declarations is

   -- This rule can be used to check/search for the occurrence of some Ada declarations
   -- Parameter(1): Declaration name
   -- Parameter(2): Declaration name
   -- ...
   -- Parameter(N): Declaration name
   --
   -- Declaration names: access, access_protected, access_subprogram, access_task, aliased,
   --                    exception, generic, package_statements, protected, protected_entry,
   --                    protected_type, renames, separate, single_array, single_protected,
   --                    single_task, tagged, task, task_entry, task_type, entry, task_entry,
   --                    protected_entry, discriminant, defaulted_discriminant, operator,
   --                    child_unit

   Rule_Id : constant Wide_String := "DECLARATIONS";

   procedure Process_Declaration (Element : in Asis.Declaration);

   procedure Process_Statement   (Element : in Asis.Declaration);
   -- For handlers in accept and block statements.

   procedure Process_Unit (Unit : in Asis.Compilation_Unit);
end Rules.Declarations;
