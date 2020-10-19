----------------------------------------------------------------------
--  Framework.Plugs - Package specification                         --
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

with Framework.Locations;

package Framework.Plugs is
   use Framework.Locations;

   procedure Enter_Unit           (Unit    : in Asis.Compilation_Unit);
   procedure Exit_Context_Clauses (Unit    : in Asis.Compilation_Unit);
   procedure Exit_Unit            (Unit    : in Asis.Compilation_Unit);
   procedure Enter_Scope          (Element : in Asis.Element);
   procedure Enter_Private_Part   (Element : in Asis.Element);
   procedure Exit_Scope           (Element : in Asis.Element);
   procedure Enter_Statement_List (Element : in Asis.Element);

   procedure True_Identifier (Element : in Asis.Expression);

   procedure Pre_Procedure  (Element : in     Asis.Element);
   procedure Post_Procedure (Element : in     Asis.Element);

   procedure Text_Enter_Unit (Unit : in Asis.Compilation_Unit);
   procedure Text_Analysis   (Line : Asis.Program_Text; Loc : Location);
end Framework.Plugs;
