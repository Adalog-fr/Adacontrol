----------------------------------------------------------------------
--  Rules.Max_Blank_Lines - Package body                            --
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
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Max_Blank_Lines is
   use Framework;

   -- Algorithm
   --
   -- The algorithm is quite straightforward (just count blank lines)
   -- The only difficulty is that we delay the report until the first non-blank line, to
   -- avoid having both a check and search message if both have been specified with different
   -- maximum values

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Rule_Label : array (Rule_Types) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Maximum    : array (Rule_Types) of Natural := (others => Natural'Last);

   Blank_Lines_Count : Natural;
   Fail_Type : Rule_Types;
   Fail_Loc  : Location;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter: <Max allowed consecutive blank lines>");
      User_Message ("Control that there is no more than the indicated number of consecutive blank lines");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if not Parameter_Exists then
         Parameter_Error ("Maximum value required for rule " & Rule_Id);
      end if;

      if Maximum (Rule_Type) = Natural'Last then
         Maximum    (Rule_Type) := Get_Integer_Parameter (Min => 0);
         Rule_Label (Rule_Type) := To_Unbounded_Wide_String (Label);
      else
         Parameter_Error ("Rule already specified");
      end if;
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
            Maximum := (others => Natural'Last);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ----------------
   -- Enter_Unit --
   ----------------

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      pragma Unreferenced (Unit);
   begin
      Blank_Lines_Count := 0;
      Fail_Loc          := Null_Location;
   end Enter_Unit;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Location) is
      use Framework.Reports, Ada.Strings.Wide_Unbounded;
      use Utilities;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Trim_All (Line) /= "" then
         if Fail_Loc /= Null_Location then
            Report (Rule_Id, To_Wide_String (Rule_Label (Fail_Type)), Fail_Type, Fail_Loc,
                    "Too many consecutive blank lines ("
                    & Integer_Img (Blank_Lines_Count)
                    & ')');
            Fail_Loc := Null_Location;
         end if;
         Blank_Lines_Count := 0;
         return;
      end if;

      -- Here we have a blank line
      Blank_Lines_Count := Blank_Lines_Count + 1;

      -- Compare to Maximum (xx) + 1 to issue only one message if there is much more
      -- than the allowed number of consecutive blank lines
      if Blank_Lines_Count = Maximum (Check)+1 then
         Fail_Loc  := Loc;
         Fail_Type := Check;
      elsif Blank_Lines_Count = Maximum (Search)+1 then
         Fail_Loc  := Loc;
         Fail_Type := Search;
      end if;

      if Blank_Lines_Count = Maximum (Count)+1 then
         Report (Rule_Id, To_Wide_String (Rule_Label (Count)), Count, Loc, "");
      end if;
  end Process_Line;

begin
   Framework.Rules_Manager.Register_Textual (Rule_Id,
                                             Help    => Help'Access,
                                             Add_Use => Add_Use'Access,
                                             Command => Command'Access);
end Rules.Max_Blank_Lines;
