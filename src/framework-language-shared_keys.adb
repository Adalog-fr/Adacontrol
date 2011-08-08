----------------------------------------------------------------------
--  Framework.Language.Shared_Keys - Package body                   --
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

-- ASIS
with
  Asis.Elements;

-- Adacontrol
with
  Framework.Scope_Manager,
  Framework.Language.Scanner;

package body Framework.Language.Shared_Keys is

   -------------------
   -- Is_Applicable --
   -------------------

   function Is_Applicable (Expected_Places : Places_Set) return Boolean is
      use Framework.Scope_Manager, Scope_Places_Utilities;
      use Asis, Asis.Elements;
      Scope_Kind : constant Declaration_Kinds := Declaration_Kind (Current_Scope);

      Locations  : constant Places_Set := (S_All       => False,
                                           S_Block     => Statement_Kind (Current_Scope) = A_Block_Statement,
                                           S_Local     => not Is_Current_Scope_Global,
                                           S_Nested    => Current_Depth /= 0,
                                           S_Own       => Scope_Kind = A_Package_Body_Declaration,
                                           S_Private   => In_Private_Part,
                                           S_Public    => (Scope_Kind = A_Package_Declaration
                                                           or Scope_Kind = A_Generic_Package_Declaration)
                                                          and not In_Private_Part,
                                           S_Task_Body => Scope_Kind = A_Task_Body_Declaration);
   begin
      if Expected_Places (S_All) then
         return True;
      end if;

      return (Expected_Places and Locations) = Expected_Places;
   end Is_Applicable;

   ------------------------------
   -- Get_Places_Set_Parameter --
   ------------------------------

   function Get_Places_Set_Modifiers return Places_Set is
      use Scope_Places_Utilities, Framework.Language.Scanner;
      Loc : constant Places_Set := Get_Modifier_Set;
   begin
      if Loc = Empty_Set then
         return Everywhere;
      elsif Loc (S_All) and Loc /= Everywhere then
         Syntax_Error ("""all"" cannot be specified with other locations", Current_Token.Position);
      else
         return Loc;
      end if;
   end Get_Places_Set_Modifiers;

end Framework.Language.Shared_Keys;
