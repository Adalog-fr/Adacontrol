----------------------------------------------------------------------
--  Rules.Control_Characters - Package body                         --
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

-- Ada
with
  Ada.Characters.Handling,
  Ada.Characters.Latin_1,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Control_Characters is
   use Ada.Strings.Wide_Maps, Framework;

   Tab_Chars  : constant  Wide_Character_Set := To_Set (Ada.Characters.Handling.To_Wide_String
                                                          (Ada.Characters.Latin_1.HT
                                                         & Ada.Characters.Latin_1.VT
                                                         & Ada.Characters.Latin_1.FF));
   Separators : constant Wide_Character_Set := Tab_Chars or To_Set (' ');

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Active_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Active_Type  : Rule_Types;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter: None");
      User_Message ("Controls source lines that contain the control characters allowed by the language");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if Rule_Used then
         Parameter_Error (Rule_Id, "rule already specified");
      end if;

      if Parameter_Exists then
         Parameter_Error (Rule_Id, "no parameter allowed");
      end if;

      Active_Label := To_Unbounded_Wide_String (Label);
      Active_Type  := Rule_Type;
      Rule_Used    := True;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Location) is
      use Framework.Reports, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded;
      Pos_Tab     : Natural;
      First_Before,
      Last_Before : Natural;
      First_After,
      Last_After : Natural;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Pos_Tab := Index (Line, Tab_Chars);
      if Pos_Tab /= 0 then
         Last_Before := Pos_Tab-1;
         while Last_Before >= Line'First and then Is_In (Line (Last_Before), Separators) loop
            Last_Before := Last_Before - 1;
         end loop;
         if Last_Before >= Line'First then
            First_Before := Last_Before;
            while First_Before > Line'First and then not Is_In (Line (First_Before-1), Separators) loop
               First_Before := First_Before -1;
            end loop;
         end if;

         First_After := Pos_Tab+1;
         while First_After <= Line'Last and then Is_In (Line (First_After), Separators) loop
            First_After := First_After + 1;
         end loop;
         if First_After <= Line'Last then
            Last_After := First_After;
            while Last_After < Line'Last and then not Is_In (Line (Last_After+1), Separators) loop
               Last_After := Last_After + 1;
            end loop;
         end if;

         if Last_Before < Line'First and First_After > Line'Last then
           Report (Rule_Id,
                   To_Wide_String (Active_Label),
                   Active_Type,
                   Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Pos_Tab),
                   "Control character found");
         elsif Last_Before < Line'First and First_After <= Line'Last then
            Report (Rule_Id,
                    To_Wide_String (Active_Label),
                    Active_Type,
                    Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Pos_Tab),
                    "Control character found before """  & Line (First_After .. Last_After) & '"');
         elsif Last_Before >= Line'First and First_After > Line'Last then
            Report (Rule_Id,
                    To_Wide_String (Active_Label),
                    Active_Type,
                    Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Pos_Tab),
                    "Control character found after """  & Line (First_Before .. Last_Before) & '"');
         else
            Report (Rule_Id,
                    To_Wide_String (Active_Label),
                    Active_Type,
                    Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Pos_Tab),
                    "Control character found between """  & Line (First_Before .. Last_Before)
                    & """ and """ & Line (First_After .. Last_After) & '"');
         end if;
      end if;
  end Process_Line;

begin
   Framework.Rules_Manager.Register_Textual (Rule_Id,
                                             Help    => Help'Access,
                                             Add_Use => Add_Use'Access,
                                             Command => Command'Access);
end Rules.Control_Characters;
