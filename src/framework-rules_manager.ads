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

   type Help_Procedure     is access procedure;
   type Add_Use_Procedure  is access procedure (Label     : in Wide_String;
                                                Rule_Type : in Rule_Types);
   type Rule_Action is (Clear, Suspend, Resume);
   type Command_Procedure  is access procedure (Action : Rule_Action);
   type Prepare_Procedure  is access procedure;
   type Finalize_Procedure is access procedure;

   procedure Register_Semantic (Rule     : Wide_String;
                                Help     : Help_Procedure;
                                Add_Use  : Add_Use_Procedure;
                                Command  : Command_Procedure;
                                Prepare  : Prepare_Procedure  := null;
                                Finalize : Finalize_Procedure := null);

   procedure Register_Textual (Rule     : Wide_String;
                               Help     : Help_Procedure;
                               Add_Use  : Add_Use_Procedure;
                               Command  : Command_Procedure;
                               Prepare  : Prepare_Procedure  := null;
                               Finalize : Finalize_Procedure := null);

   procedure Enter (Rule : Wide_String);

   --
   --  Declarations below this line are for the use of the framework
   --

   type Rule_Kind is (Semantic, Textual);
   function Has_Active_Rules (Kind : Rule_Kind) return Boolean;

   function Number_Of_Rules return Rules_Count;

   function Is_Rule_Name (Rule : Wide_String) return Boolean;

   function Last_Rule return Wide_String;
   -- Name of last rule entered

   procedure Help_All;
   -- Displays all rules help
   procedure Help (Rule_Id : in Wide_String);
   -- Displays specific rule help
   procedure Help_Names;
   -- Displays all rule names

   procedure Prepare_All;
   -- Calls the Prepare procedure for each rule, which is intended to do some actions
   -- at the beginning of each "Go" command

   procedure Finalize_All;
   -- Calls the Finalize procedure for each rule, which is intended to do some actions
   -- at the end of each "Go" command

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types;
                      Rule_Name : in Wide_String);
   -- Adds a new use of a rule.

   procedure Command_All (Action : Rule_Action);
   procedure Command (Rule_Id : in Wide_String; Action : Rule_Action);
end Framework.Rules_Manager;
