----------------------------------------------------------------------
--  Adactl_Options - Package specification                          --
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

-- Adactl
with
  Framework.Variables.Shared_Types;

package Adactl_Options is
   pragma Elaborate_Body;  -- For registration of options from the body
   use Framework.Variables.Shared_Types;

   type Action_Kinds is (Help, Check, Process, Interactive_Process, Dependents);
   subtype Need_Asis_Actions is Action_Kinds range Process .. Action_Kinds'Last;

   Action : Action_Kinds;

   -- Options are initialized to avoid uninitialized values when Help or Check
   -- Implemented as rule variables:
   Debug_Option     : aliased Switch_Type.Object          := (Value => Off);
   Exit_Option      : aliased Switch_Type.Object          := (Value => Off);
   Ignore_Option    : aliased Extended_Switch_Type.Object := (Value => Off);
   Verbose_Option   : aliased Switch_Type.Object          := (Value => Off);

   -- Implemented as Ada variables (not settable with the set command):
   Recursive_Option : Switch :=  Off;
   Overwrite_Option : Switch :=  Off;
   Spec_Option      : Switch :=  Off;
   Unit_Option      : Switch :=  Off;

   function Command_Line return Wide_String;
   -- Returns the raw command line

   procedure Analyse_Options;
   -- Analyses and sets program options

   procedure Help_On_Options;
   -- Help on command line options

   function Asis_Options return Wide_String;
   -- Returns ASIS options passed by command line

   function Command_Line_Commands return Wide_String;
   -- Return the commands stated as options on the command line

   function Ada_Units_List return Wide_String;
   -- Returns list of Ada units to process

   function Initialize_String return Wide_String;
   -- Returns a initialize string

   Options_Error : exception;

end Adactl_Options;
