----------------------------------------------------------------------
--  Rules.Special_Comments - Package body                           --
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
  Ada.Unchecked_Deallocation,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Fixed;

-- Adalog
with
  String_Matching,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Special_Comments is
   use Framework;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   type Usage_Rec;
   type Usage_Rec_Access is access Usage_Rec;
   type Pattern_Access is access String_Matching.Compiled_Pattern;
   type Usage_Rec is new Basic_Rule_Context with
      record
         Pattern : Pattern_Access;
         Next    : Usage_Rec_Access;
      end record;

   Usage : Usage_Rec_Access;

   -----------
   -- Clear --
   -----------

   procedure Clear (Rec : in out Usage_Rec_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Usage_Rec, Usage_Rec_Access);
      procedure Free is new Ada.Unchecked_Deallocation (String_Matching.Compiled_Pattern, Pattern_Access);
      Temp : Usage_Rec_Access := Rec;
   begin
      while Rec /= null loop
         Temp := Rec.Next;
         Free (Rec.Pattern);
         Free (Rec);
         Rec := Temp;
      end loop;
   end Clear;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): Special comment pattern");
      User_Message ("Control comments that match the specified pattern");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use String_Matching, Framework.Language;

   begin
      if not Parameter_Exists then
         Parameter_Error ("At least one parameter required for rule " & Rule_Id);
      end if;

      while Parameter_Exists loop
         declare
            Pattern : constant Wide_String := Get_String_Parameter;
         begin
            Usage := new Usage_Rec'(Basic.New_Context (Rule_Type, Label) with
                                    Pattern => new Compiled_Pattern'(Compile (Pattern, Ignore_Case => True)),
                                    Next    => Usage);
         exception
            when Pattern_Error =>
               Parameter_Error ("Incorrect pattern: " & Pattern);
         end;
      end loop;

      Rule_Used := True;
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
            Clear (Usage);
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

   Separators : constant Ada.Strings.Wide_Maps.Wide_Character_Set
     := Ada.Strings.Wide_Maps.To_Set (Ada.Characters.Handling.To_Wide_String (' ' & Ada.Characters.Latin_1.HT));

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Location) is
      use String_Matching, Framework.Reports, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Maps;
      Current : Usage_Rec_Access;
      Start   : Natural;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Start := Index (Line, "--");
      if Start = 0 then
         return;
      end if;

      Start := Start + 2;
      while Start <= Line'Last and then Is_In (Line (Start), Separators) loop
         Start := Start + 1;
      end loop;
      if Start > Line'Last then
         return;
      end if;

      Current := Usage;
      while Current /= null loop
         if Match (Line (Start .. Line'Last), Current.Pattern.all) then
            Report (Rule_Id,
                    Current.all,
                    Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Start),
                    '"' & Line (Start .. Line'Last) & '"');
            end if;
         Current := Current.Next;
      end loop;
   end Process_Line;

begin
   Framework.Rules_Manager.Register_Textual (Rule_Id,
                                             Help    => Help'Access,
                                             Add_Use => Add_Use'Access,
                                             Command => Command'Access);
end Rules.Special_Comments;
