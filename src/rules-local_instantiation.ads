----------------------------------------------------------------------
--  Rules.Local_Instantiation - Package specification               --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Code Cheker  is free software;  you can redistribute  it and/or --
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
with Asis;

package Rules.Local_Instantiation  is

   -- This rule can be used to check/search for the instantiations that are
   -- done in a local scope
   -- Parameters (1) => generic name
   -- Parameters (2) => generic name
   -- ...
   -- Parameters (N) => generic name

   Rule_Id : constant Wide_String := "LOCAL_INSTANTIATION";

   procedure Process_Instantiation (Instantiation : in Asis.Declaration);
   -- Applies the rule
   --
   -- Expected Element_Kinds:
   --      A_Declaration
   --
   -- Expected Declartation_Kinds:
   --      A_Package_Instantiation
   --      A_Procedure_Instantiation
   --      A_Function_Instantiation

end Rules.Local_Instantiation;
