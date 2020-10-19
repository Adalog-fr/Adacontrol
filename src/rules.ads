----------------------------------------------------------------------
--  Rules - Package specification                                   --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.         --
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
private with  -- ASIS
  Asis;

-- All rules necessarily need these units. We put the with clauses
-- here so they apply to all child units. This way, rules need to
-- put in their with clauses only the ones they need for some special
-- purpose.
-- Of course, we have to disable the warnings that these units are
-- not referenced here

pragma Warnings (Off);
--## RULE OFF With_Clauses
-- Adactl
with
  Framework,
  Framework.Control_Manager,
  Framework.Language,
  Framework.Locations,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Warnings (On);
--## RULE ON With_Clauses

package Rules is
   pragma Elaborate_Body (Rules);

private
   -- Declarations to make writing rules easier

   -- For simple instantiations of Traverse_Element:
   type Null_State is null record;

   procedure Null_State_Procedure (Element : in     Asis.Element;
                                   Control : in out Asis.Traverse_Control;
                                   State   : in out Null_State);
end Rules;
