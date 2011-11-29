----------------------------------------------------------------------
--  Framework.Rules_Manager - Package specification                 --
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

package Framework.Rules_Manager is
   pragma Elaborate_Body;

   Max_Rules : constant := 100; -- Max number of rules in AdaControl
   type Rules_Count is range 0 .. Max_Rules;

   procedure Initialize;

   type Help_Procedure        is access procedure;
   type Add_Control_Procedure is access procedure (Ctl_Label : in Wide_String;
                                                   Ctl_Kind  : in Control_Kinds);
   type Rule_Action is (Clear, Suspend, Resume);
   type Command_Procedure  is access procedure (Action : Rule_Action);
   type Prepare_Procedure  is access procedure;
   type Finalize_Procedure is access procedure;

   type Extended_Rule_Kind is (Semantic, Textual, Semantic_Textual);
   subtype Rule_Kind is Extended_Rule_Kind range Semantic .. Textual;
   procedure Register (Rule           : Wide_String;
                       R_Kind         : Extended_Rule_Kind;
                       Help_CB        : Help_Procedure;
                       Add_Control_CB : Add_Control_Procedure;
                       Command_CB     : Command_Procedure;
                       Prepare_CB     : Prepare_Procedure  := null;
                       Finalize_CB    : Finalize_Procedure := null);

   procedure Enter (Rule : Wide_String);

   ---------------------------------------------------------------------
   --
   --  Declarations below this line are for the use of the framework
   --

   Timing_Option : Boolean := False;

   function Has_Active_Rules (R_Kind : Rule_Kind) return Boolean;

   function Number_Of_Rules return Rules_Count;

   function Is_Rule_Name (Rule : Wide_String) return Boolean;

   function Last_Rule return Wide_String;
   -- Name of last rule entered

   procedure Help_On_Rules (Pattern : Wide_String);
   -- Displays help for rules matching Pattern

   procedure Help_On_Names (Pretty : Boolean);
   -- Displays all rule names

   procedure Prepare_All;
   -- Calls the Prepare procedure for each rule, which is intended to do some actions
   -- at the beginning of each "Go" command

   procedure Finalize_All;
   -- Calls the Finalize procedure for each rule, which is intended to do some actions
   -- at the end of each "Go" command

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds;
                          Rule_Name : in Wide_String);
   -- Adds a new control for a rule.

   procedure Command_All (Action : Rule_Action);
   procedure Command (Rule_Id : in Wide_String; Action : Rule_Action);

   procedure Report_Timings;

   -------------------------------------------------------------------
   --  Inhibition management                                        --
   -------------------------------------------------------------------

   procedure Inhibit (Rule_Name : Wide_String; Entity : Entity_Specification; Is_All : Boolean);
   procedure Process_Inhibition (Unit : Asis.Compilation_Unit; State : Framework.Rules_Manager.Rule_Action);
   function Is_Banned (Element : in Asis.Element; For_Rule : in Wide_String) return Boolean;
   -- Returns True if Element is declared within a banned unit for rule For_Rule.
   -- A banned unit is one which is the target of an inhibit all command.

end Framework.Rules_Manager;
