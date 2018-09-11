----------------------------------------------------------------------
--  Rules.Unsafe_Paired_Calls - Package specification               --
--                                                                  --
--  This module is (c) Adalog 2004-2016.                            --
--  The Ada Controller is  free software; you can  redistribute  it --
--  and/or modify it under terms of the GNU General Public  License --
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

-- AdaControl
private with
   Framework.Variables.Shared_Types;

package Rules.Unsafe_Paired_Calls is

   Rule_Id : constant Wide_String := "UNSAFE_PAIRED_CALLS";

   procedure Process_Call (Call : in Asis.Element);

   procedure Process_Breaking_Statement (Stmt : Asis.Statement);

private
   -- Declarations for child units

   -- Rule variables
   use Framework.Variables.Shared_Types;
   Conditionals_Allowed : aliased Framework.Variables.Shared_Types.Switch_Type.Object := (Value => On);
   Name_As_Given        : aliased Framework.Variables.Shared_Types.Switch_Type.Object := (Value => Off);

end Rules.Unsafe_Paired_Calls;
