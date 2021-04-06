----------------------------------------------------------------------
--  Adactl_Constants - Package specification                        --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2008.           --
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

package Adactl_Constants is

   -------------------------------------------------------------------
   -- General dimensioning constants                                --
   -------------------------------------------------------------------

   -- These constants define limits about the rules themselves
   -- These limits are arbitrary and can be changed at will, no other change is needed.

   Max_Rule_Name_Length : constant := 50;
   -- Maximum number of characters in a rule's name

   Max_Rules : constant := 100; -- Max number of rules in AdaControl
   type Rules_Count is range 0 .. Max_Rules;


   -- These constants define limits about what "reasonable" programs may contain.
   -- They can be used by rules to limit some capabilities.
   -- These limits are arbitrary and can be changed at will, no other change is needed.

   Max_ID_Length : constant := 250;
   -- Max length of an Ada identifier (and of a keyword by the same token)
   type ID_Count is range 0 .. Max_ID_Length;

   Max_Controls_For_Rule : constant := 100;
   -- For rules that need an upper bound to the number of times they can
   -- be specified in a control

   Max_Controls_Set : constant := 50;
   -- As previous, but for rules that need a set (array of boolean) of controls

   Max_Parameters : constant := 30;
   -- Maximum number of parameters declared by a subprogram or an entry

   Max_Loop_Nesting : constant := 20;
   -- Maximum depth of nested loops

end Adactl_Constants;
