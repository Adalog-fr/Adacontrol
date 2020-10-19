----------------------------------------------------------------------
--  Rules.Unnecessary_Use_Clause - Package specification            --
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

-- Asis
with Asis;

package Rules.Unnecessary_Use_Clause  is

   Rule_Id : constant Wide_String := "UNNECESSARY_USE_CLAUSE";

   procedure Process_Use_Clause (Clause : in Asis.Clause);
   -- Called whenever a use clause is encountered
   -- Must be called as a post_procedure, to ensure that the names contained in it
   -- are checked against other use clauses, but not this one.
   --
   -- Expected Clause_Kinds:
   --    A_Use_Package_Clause

   procedure Process_Identifier (Name : in Asis.Name);
   -- Called whenever an identifier is encountered
   -- Check whether it is declared inside one of the packages that are mentionned
   -- in a (currently active) use clause.

   procedure Process_Instantiation (Instantiation : Asis.Declaration);
   -- Called for generic instantiations
   -- We need to process instantiations specially, because we need the visibility
   -- on defaulted parameters, and these won't be traversed by the ruler.
   --
   -- Expected Declaration_Kinds:
   --    A_Generic_Instantiation

   procedure Process_Scope_Exit (Scope : in Asis.Element);

end Rules.Unnecessary_Use_Clause;
