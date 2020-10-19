----------------------------------------------------------------------
--  Framework.Language.Commands - Package specification             --
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

private package Framework.Language.Commands is
   procedure Go_Command;
   procedure Help_Command       (On          : in Wide_String);
   procedure Inhibit_Command    (Rule_Name   : in Wide_String);
   -- Inhibit given rule for all units passed as parameters
   -- Unit names to be parsed using normal parameter parsing procedures
   procedure Message_Command    (Message     : in Wide_String; With_Pause : Boolean);
   procedure Set_Output_Command (Output_File : in Wide_String; Force_Overwrite : Boolean);
   procedure Set_Trace_Command  (Trace_File  : in Wide_String);
   procedure Source_Command     (Name        : in Wide_String; Success : out Boolean);

end Framework.Language.Commands;
