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
   pragma Unreserve_All_Interrupts;

   protected body IT is
      procedure Activate is
         use Ada.Interrupts, Ada.Interrupts.Names;
      begin
         Attach_Handler (IT.Handler'Access, SIGINT);
      end Activate;

      entry Received when Signaled is
      begin
         Signaled := False;
      end Received;

      procedure Handler is
      begin
         Signaled := True;
      end Handler;
   end IT;

end Framework.Interrupt;
