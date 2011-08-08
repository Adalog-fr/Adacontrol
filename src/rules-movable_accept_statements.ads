----------------------------------------------------------------------
--  Rules.Movable_Accept_Statements - Package specification         --
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
with Asis;

package Rules.Movable_Accept_Statements  is

   -- This rule can be used to check/search statements that could be moved
   -- from an accept scope to the directly outer scope, i.e. statements not
   -- depending on the use of any formal parameter of the accept structure
   -- might be moved, according to certain circumstances, to the directly
   -- outer scope.
   -- Note: circumstances are explained in the documentation.
   -- Parameters: None

   --                               OR

   -- This rule can be used to check/search statements that could be moved
   -- from the `accept' scope to an outer scope, i.e. every statements not
   -- depending on any parameter of the `accept' structure should be moved
   -- to the direct outer scope.
   --
   -- Parameters: None
   --

   Rule_Id : constant Wide_String := "MOVABLE_ACCEPT_STATEMENTS";

   procedure Process_Accept_Statement (Statement : in Asis.Statement);

end Rules.Movable_Accept_Statements;
