----------------------------------------------------------------------
--  Framework.Interrupt_Dbg - Package body                          --
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
--                                                                  --
--  This  software is  distributed  in  the hope  that  it will  be --
--  useful,  but WITHOUT  ANY  WARRANTY; without  even the  implied --
--  warranty  of  MERCHANTABILITY   or  FITNESS  FOR  A  PARTICULAR --
--  PURPOSE.                                                        --
----------------------------------------------------------------------

-- Ada
with
   Ada.Interrupts,
   Ada.Interrupts.Names;

package body Framework.Interrupt_Dbg is

-- This version of the body of Framework.Interrupt implements
-- Run_Interruptable for real, i.e. the provided procedure can be interrupted
-- by Ctrl-C.
--
-- Use of this implementation is intended for debugging purposes, since the
-- "Interrupted" exception is handled in Framework.Language.Commands.Go_Command
-- to provide (some) information about where AdaControl was interrupted, a starting
-- point when some rule enters an infinite loop.
--
-- Use of this implementation is not recommended for regular use, since the mere
-- presence of a protected type drags in the tasking run-time, slowing down AdaControl
-- by ca 18%. See programmer manual for details.

   pragma Unreserve_All_Interrupts;

   protected IT is
      procedure Set_Active (State : Boolean);
      entry Received;
   private
      procedure Handler;
      pragma Interrupt_Handler (Handler);

      Signaled : Boolean := False;
   end IT;

   protected body IT is
      procedure Set_Active (State : Boolean) is
         use Ada.Interrupts, Ada.Interrupts.Names;
      begin
         if State then
            Attach_Handler (IT.Handler'Access, SIGINT);
            Attach_Handler (IT.Handler'Access, SIGTERM);
         else
            Detach_Handler (SIGINT);
            Detach_Handler (SIGTERM);
         end if;
      end Set_Active;

      entry Received when Signaled is
      begin
         Signaled := False;
      end Received;

      procedure Handler is
      begin
         Signaled := True;
      end Handler;
   end IT;

   -----------------------
   -- Run_Interruptable --
   -----------------------

   procedure Run_Interruptable (Proc : access procedure) is
   begin
      IT.Set_Active (True);
      select
         IT.Received;
         IT.Set_Active (False);
         raise Interrupted;
      then abort
         Proc.all;
      end select;
      IT.Set_Active (False);
   end Run_Interruptable;

end Framework.Interrupt_Dbg;
