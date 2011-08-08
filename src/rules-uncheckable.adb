----------------------------------------------------------------------
--  Rules.Ucheckable - Package body                                 --
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

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);
package body Rules.Uncheckable is
   use Framework, Framework.Reports;

   package Uncheckable_Flag_Utilities is new Framework.Language.Flag_Utilities (Uncheckable_Kinds);

   type Flags_Array is array (Uncheckable_Kinds) of Boolean;
   Rule_Used : Flags_Array := (others => False);
   Save_Used : Flags_Array;

   Missing_Unit_Context : Basic_Rule_Context;
   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Uncheckable_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);

      Help_On_Flags ("Parameter(s):", Footer => "(optional, default = all)");
      User_Message ("Control occurrences of uncheckable constructs in other rules");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language, Uncheckable_Flag_Utilities;
      Flag : Uncheckable_Kinds;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Flag := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Flag) then
               Parameter_Error (Rule_Id, "value already given");
            end if;
            Rule_Used (Flag) := True;
            if Flag in Uncheckable_Consequence then
               Set_Uncheckable (Flag, Rule_Type, Label);
            else
               Missing_Unit_Context := Basic.New_Context (Rule_Type, Label);
            end if;
         end loop;
      else
         if Rule_Used (Missing_Unit) then
            Parameter_Error (Rule_Id, "value already given");
         end if;
         for F in Uncheckable_Consequence loop
            if Rule_Used (F) then
               Parameter_Error (Rule_Id, "value already given");
            end if;
            Set_Uncheckable (F, Rule_Type, Label);
         end loop;
         Rule_Used := (others => True);
      end if;

   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => False);
            Reset_Uncheckable;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   --------------------------
   -- Process_Missing_Unit --
   --------------------------

   procedure Process_Missing_Unit (Message : Wide_String) is
   begin
      if not Rule_Used (Missing_Unit) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Report (Rule_Id, Missing_Unit_Context, Null_Location, Message);
   end Process_Missing_Unit;
begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.Uncheckable;
