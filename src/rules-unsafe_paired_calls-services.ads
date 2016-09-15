----------------------------------------------------------------------
--  Rules.Unsafe_Paired_Calls.Signatures - Package specification    --
--                                                                  --
--  This module is  (c) BelgoControl and Adalog  2004-2005. The Ada --
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

-- ASIS
with
   Asis;

private package Rules.Unsafe_Paired_Calls.Services is

   function Effective_Last_Statement (Stats : Asis.Statement_List) return Asis.Statement;
   -- Returns the last statement of Stats that is not an exit, return, or null statement


   ------------------ Signature of a lock call possibly nested
   type Nesting_Signature is new Asis.Element_List;
   -- Describes the nesting of if statements down to the initial call
   -- A list containing:
   --   For an if: the if_statement (can provide the condition_expression)
   --   For a path: the path
   --   For a call: the call
   -- The call is always the last element of the list

   Invalid_Nesting : constant Nesting_Signature := Nesting_Signature (Asis.Nil_Element_List);
   -- Returned by Signature if for some reason the structure does not obey the required model, including:
   -- An if statement is not the only statement of its path, except for terminating return, exit, null
   -- A condition_expression is not a simple boolean constant
   -- Another kind of statement is encountered

   function Signature (Stmt : Asis.Statement) return Nesting_Signature;
   -- Computes the signature by going up enclosing elements from Stmt
   -- Stops when anything else than if's (and corresponding paths) is encountered,
   -- or when an if has a condition which is not a simple reference to a boolean constant

   function Matching_Call (Stat : Asis.Statement; Signature : Nesting_Signature) return Asis.Statement;
   -- Returns the matching call burried into nested if statements, if the structure matches Signature,
   -- including that every condition expression of if statements matches the one from the signature
   -- Returns Nil_Element otherwise

end Rules.Unsafe_Paired_Calls.Services;
