----------------------------------------------------------------------
--  Rules.Uncheckable - Package body                                --
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

-- Adalog
with
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Uncheckable is
   use Framework, Framework.Control_Manager, Framework.Reports;

   subtype Subrules is Uncheckable_Kinds;
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Flags_Array is array (Uncheckable_Kinds) of Boolean;
   Not_Used : constant Flags_Array := (others => False);
   Rule_Used : Flags_Array := Not_Used;
   Save_Used : Flags_Array;

   Missing_Unit_Context : Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of uncheckable constructs in other rules");
      User_Message;
      Help_On_Flags ("Parameter(s):", Footer => "(optional, default = all)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities;
      Subrule : Subrules;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Subrule := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Subrule) then
               Parameter_Error (Rule_Id, "value already given");
            end if;
            Rule_Used (Subrule) := True;
            if Subrule in Uncheckable_Consequence then
               Set_Uncheckable (Subrule, Ctl_Kind, Ctl_Label);
            else
               Missing_Unit_Context := Basic.New_Context (Ctl_Kind, Ctl_Label);
            end if;
         end loop;
      else
         if Rule_Used (Missing_Unit) then
            Parameter_Error (Rule_Id, "value already given");
         end if;
         Missing_Unit_Context := Basic.New_Context (Ctl_Kind, Ctl_Label);

         for F in Uncheckable_Consequence loop
            if Rule_Used (F) then
               Parameter_Error (Rule_Id, "value already given");
            end if;
            Set_Uncheckable (F, Ctl_Kind, Ctl_Label);
         end loop;
         Rule_Used := (others => True);
      end if;

   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := Not_Used;
            Reset_Uncheckable;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Not_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   --------------------------
   -- Process_Missing_Unit --
   --------------------------

   procedure Process_Missing_Unit (Message : Wide_String) is
      use Framework.Locations;
   begin
      if not Rule_Used (Missing_Unit) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Report (Rule_Id, Missing_Unit_Context, Null_Location, Message);
   end Process_Missing_Unit;
begin  -- Rules.Uncheckable
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Uncheckable;
