----------------------------------------------------------------------
--  Framework.Interrupt - Package body                              --
--                                                                  --
--  This software  is (c) Adalog  2004-2012. The Ada  Controller is --
--  free software;  you can redistribute it and/or  modify it under --
--  terms of  the GNU  General Public License  as published  by the --
--  Free Software Foundation; either version 2, or (at your option) --
--  any later version.   This unit is distributed in  the hope that --
--  it will be  useful, but WITHOUT ANY WARRANTY;  without even the --
--  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
--  PURPOSE.  See the GNU  General Public License for more details. --
--  You  should have  received a  copy  of the  GNU General  Public --
--  License distributed  with this  program; see file  COPYING.  If --
--  not, write to  the Free Software Foundation, 59  Temple Place - --
--  Suite 330, Boston, MA 02111-1307, USA.                          --
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
   Ada.Interrupts.Names,
     Ada.Task_Identification;

-- ASIS
with
   ASIS.Implementation;

-- AdaControl
with
   Adactl_Version,
   Framework.Rules_Manager,
   Units_List,
   Utilities;

package body Framework.Interrupt is
   use Ada.Task_Identification;

   -- It's the main task that elaborates this package:
   Main_Task : constant Task_Id := Current_Task;

   -- This procedure is called from the interrupt handler for Ctrl-C, which is evil
   -- since it is furiously potentially blocking.
   -- Oh well, we are not running on a bare board, so it works, and this is just
   -- for debugging purposes...
   -- BTW, we make it a separate procedure to avoid complaints from Gnat (about being
   -- potentially blocking) .
   procedure Report_State is
      use ASIS.Implementation;
      use Framework.Rules_Manager, Utilities;
      Unit_Name : constant Wide_String := Units_List.Current_Unit;
   begin
      User_Message ("=== Interrupt");
      User_Message ("============= Phase: "
                    & To_Title (Control_Phases'Wide_Image (Current_Phase))
                    & " =============");
      User_Message ("AdaCtl version: " & Adactl_Version
                    & " with " & ASIS_Implementor_Version);
      User_Message ("   In rule: " & Last_Rule);
      if Unit_Name /= "" then
         User_Message ("   For unit: " & Unit_Name);
      end if;
      Abort_Task (Main_Task);
   end Report_State;

   protected IT is
      procedure Handler;
      pragma Interrupt_Handler (Handler);
   end IT;

   protected body IT is
      procedure Handler is
      begin
         Report_State;
      end Handler;
   end IT;

   --------------
   -- Activate --
   --------------

   procedure Activate is
      use Ada.Interrupts, Ada.Interrupts.Names;
   begin
      Attach_Handler (IT.Handler'Access, SIGINT);
      Attach_Handler (IT.Handler'Access, SIGTERM);
   end Activate;

end Framework.Interrupt;
