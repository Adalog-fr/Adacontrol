----------------------------------------------------------------------
--  Rules.Characters - Package body                                 --
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

-- Ada
with
  Ada.Characters.Handling,
  Ada.Characters.Wide_Latin_1,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis.Text;

-- Adalog
with
  Utilities;

-- AdaControl
with
  Framework.Reports.Fixes,
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Characters is
   use Framework, Framework.Control_Manager;

   type Subrule is (Control, Not_Iso_646, Trailing_Space, Wide);
   package Subrule_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrule);

   type Subrule_Set is array (Subrule) of Boolean;
   Rule_Used : Subrule_Set := (others => False);
   Save_Used : Subrule_Set;

   Contexts : array (Subrule) of Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrule_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls source lines that contain specified kind of characters");
      User_Message;
      Help_On_Flags ("Parameter(s):",
                     Footer => "(optional, default = all)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrule_Flag_Utilities, Utilities;
      Sr : Subrule;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Sr := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Sr) then
               if not Basic.Merge_Context (Contexts (Sr), Ctl_Kind, Ctl_Label) then
                  Parameter_Error (Rule_Id, "rule already specified for " & Image (Sr, Lower_Case));
               end if;
            else
               Contexts  (Sr) := Basic.New_Context (Ctl_Kind, Ctl_Label);
               Rule_Used (Sr) := True;
            end if;
         end loop;
      else
         for S in Subrule loop
            if Rule_Used (S) then
               Parameter_Error (Rule_Id, "rule already specified for " & Image (S, Lower_Case));
            end if;
            Contexts  (S) := Basic.New_Context (Ctl_Kind, Ctl_Label);
            Rule_Used (S) := True;
         end loop;
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
            Rule_Used := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Locations.Location) is
      use Ada.Strings.Wide_Maps;
      use Framework.Locations, Framework.Reports, Asis.Text;

      procedure Check_Control is
         use Ada.Strings.Wide_Fixed;
         use Utilities;

         Pos_Tab      : Natural;
         First_Before : Natural;
         Last_Before  : Natural;
         First_After  : Natural;
         Last_After   : Natural;
      begin
         Pos_Tab := Index (Line, Tab_Chars);
         if Pos_Tab /= 0 then
            Last_Before := Pos_Tab - 1;
            while Last_Before >= Line'First and then Is_In (Line (Last_Before), Separators) loop
               Last_Before := Last_Before - 1;
            end loop;
            if Last_Before >= Line'First then
               First_Before := Last_Before;
               while First_Before > Line'First and then not Is_In (Line (First_Before - 1), Separators) loop
                  First_Before := First_Before - 1;
               end loop;
            end if;

            First_After := Pos_Tab + 1;
            while First_After <= Line'Last and then Is_In (Line (First_After), Separators) loop
               First_After := First_After + 1;
            end loop;
            if First_After <= Line'Last then
               Last_After := First_After;
               while Last_After < Line'Last and then not Is_In (Line (Last_After + 1), Separators) loop
                  Last_After := Last_After + 1;
               end loop;
            end if;

            if Last_Before < Line'First and First_After > Line'Last then
               Report (Rule_Id,
                       Contexts (Control),
                       Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Character_Position (Pos_Tab)),
                       "Control character found");
            elsif Last_Before < Line'First and First_After <= Line'Last then
               Report (Rule_Id,
                       Contexts (Control),
                       Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Character_Position (Pos_Tab)),
                       "Control character found before """  & Line (First_After .. Last_After) & '"');
            elsif Last_Before >= Line'First and First_After > Line'Last then
               Report (Rule_Id,
                       Contexts (Control),
                       Loc + (Pos_Tab - 1),
                       "Control character found after """  & Line (First_Before .. Last_Before) & '"');
            else
               Report (Rule_Id,
                       Contexts (Control),
                       Loc + (Pos_Tab - 1),
                       "Control character found between """  & Line (First_Before .. Last_Before)
                       & """ and """ & Line (First_After .. Last_After) & '"');
            end if;

            case Line (Pos_Tab) is
               when Ada.Characters.Wide_Latin_1.HT =>
                   -- Replace by spaces up to multiple of 4
                  Fixes.Replace (From => Loc + (Pos_Tab - 1), Length => 1, By => (4 - Pos_Tab rem 4) * " ");
               when Ada.Characters.Wide_Latin_1.VT | Ada.Characters.Wide_Latin_1.FF =>
                   -- Replace by single space
                  Fixes.Replace (From => Loc + (Pos_Tab - 1), Length => 1, By => " ");
               when others =>
                  Failure ("Check_Control: character code " & Integer_Img (Wide_Character'Pos (Line (Pos_Tab))));
            end case;
         end if;
      end Check_Control;

      procedure Check_Trailing is
         use Utilities;
         Last_Separator : Positive;
      begin
         if Line = "" then
            return;
         end if;

         if Is_In (Line (Line'Last), Separators) then
            Last_Separator := Line'First;
            for I in reverse Line'Range loop
               if not Is_In (Line (I), Separators) then
                  Last_Separator := I + 1;
                  exit;
               end if;
            end loop;
            Report (Rule_Id,
                    Contexts (Trailing_Space),
                    Loc + (Last_Separator - 1),
                    Integer_Img (Line'Last - Last_Separator + 1) & " trailing space(s)");
            Fixes.Delete (From => Loc + (Last_Separator - 1), To => Loc + Line'Last);
         end if;
      end Check_Trailing;

      procedure Check_Character_Set is
         use Ada.Characters.Handling;
         Wide_First        : constant Wide_Character := Wide_Character'Val (Character'Pos (Character'Last) + 1);
         Not_ISO_646_First : constant Wide_Character := Wide_Character'Val (Character'Pos (ISO_646'Last)   + 1);
      begin
         for I in Line'Range loop
            if Rule_Used (Not_Iso_646) and then Line (I)>= Not_ISO_646_First then
               Report (Rule_Id,
                       Contexts (Not_Iso_646),
                       Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Character_Position (I)),
                       "character not in Iso_646");
            end if;
            if Rule_Used (Wide) and then Line (I) >= Wide_First then
               Report (Rule_Id,
                       Contexts (Wide),
                       Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Character_Position (I)),
                       "wide character");
            end if;
         end loop;
      end Check_Character_Set;

   begin  -- Process_Line
      if Rule_Used = (Subrule => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (Control) then
         Check_Control;
      end if;

      if Rule_Used (Trailing_Space) then
         Check_Trailing;
      end if;

      if Rule_Used (Not_Iso_646) or Rule_Used (Wide) then
         Check_Character_Set;
      end if;
  end Process_Line;

begin  -- Rules.Characters
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Textual,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Characters;
