----------------------------------------------------------------------
--  Rules.Known_Exceptions - Package specification                  --
--                                                                  --
--  This software is (c) Adalog 2004-2020.                          --
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
with
  Asis;

package Rules.Known_Exceptions is
   pragma Elaborate_Body;

   Rule_Id : constant Wide_String := "KNOWN_EXCEPTIONS";

   procedure Process_Assignment         (Stmt : Asis.Statement);
   procedure Process_Dereference        (Expr : Asis.Expression);
   procedure Process_Function_Call      (Expr : Asis.Expression);
   procedure Process_Index_Expression   (Expr : Asis.Expression);
   procedure Process_Object_Declaration (Decl : Asis.Declaration);
   procedure Process_Raise_Expression   (Expr : Asis.Expression);
   procedure Process_Selected_Component (Expr : Asis.Expression);
end Rules.Known_Exceptions;
