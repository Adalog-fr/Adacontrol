----------------------------------------------------------------------
--  Framework.Interrupt_Std - Package body                          --
--                                                                  --
--  This software is (c) Adalog 2004-2012.                          --
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

package body Framework.Interrupt_Std is

-- This is a dummy body for Framework.Interrupt that does NOT handle
-- interrupts. It avoids dragging in the full tasking run-time, which has
-- a small, but not neglectible effect on efficiency
--
-- For debugging purposes in the case of an endless loop, replace this body
-- by the one in file framework-interrupt-dbg.adb. See programmer manual for
-- details.

   -----------------------
   -- Run_Interruptable --
   -----------------------

   procedure Run_Interruptable (Proc : access procedure) is
   begin
      Proc.all;
   end Run_Interruptable;

end Framework.Interrupt_Std;
