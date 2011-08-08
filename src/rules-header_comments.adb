----------------------------------------------------------------------
--  Rules.Header_Comments - Package body                            --
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
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Header_Comments is
   use Framework;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Rule_Label   : array (Rule_Types) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Comments     : array (Rule_Types) of Natural := (others => 0);
   Max_Comments : Natural;
   Search_Check_Reported : Boolean;
   Count_Reported        : Boolean;

   Wide_HT : constant Wide_Character := Ada.Characters.Handling.To_Wide_Character (ASCII.HT);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter: <Required number of comment lines>");
      User_Message ("Control that  each unit starts with (at least indicated number of comment lines");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if not Parameter_Exists then
         Parameter_Error ("Number of comment lines required for rule " & Rule_Id);
      end if;

      if Comments (Rule_Type) = 0 then
         Comments (Rule_Type) := Get_Integer_Parameter;
         if Comments (Rule_Type) <= 0 then
            Parameter_Error ("Comments value must be at least 1");
         end if;
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
            Rule_Used    := False;
            Comments     := (others => 0);
            Max_Comments := 0;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Max_Comments := 0;
      for R in Rule_Types loop
         Max_Comments := Natural'Max (Max_Comments, Comments (R));
      end loop;
   end Prepare;

  ----------------
  -- Enter_Unit --
  ----------------

  procedure Enter_Unit is
  begin
     Search_Check_Reported := False;
     Count_Reported        := False;
  end Enter_Unit;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Location) is
      use Framework.Reports, Ada.Strings.Wide_Unbounded;
      Line_Num : Natural;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Line_Num := Get_First_Line (Loc);
      if Line_Num > Max_Comments then
         return;
      end if;

      for Inx in Natural range Line'First .. Line'Last - 1 loop
         if Line (Inx) /= ' ' and Line (Inx) /= Wide_HT then
            if Line (Inx) = '-' and Line (Inx+1) = '-' then
               -- OK, comment line
               return;
            end if;
         end if;
      end loop;

      -- Here we have a non-comment line in the range where a check is required
      if not Search_Check_Reported then
         if Line_Num <= Comments (Check) then
            Report (Rule_Id, To_Wide_String (Rule_Label (Check)), Check, Loc,
                    "not enough header comment lines");
            Search_Check_Reported := True;
         elsif Line_Num <= Comments (Search) then
            Report (Rule_Id, To_Wide_String (Rule_Label (Search)), Search, Loc,
                    "not enough header comment lines");
            Search_Check_Reported := True;
         end if;
      end if;

      if not Count_Reported and Line_Num <= Comments (Count) then
         Report (Rule_Id, To_Wide_String (Rule_Label (Count)), Count, Loc, "");
         Count_Reported := True;
      end if;
  end Process_Line;

begin
   Framework.Rules_Manager.Register_Textual (Rule_Id,
                                             Help    => Help'Access,
                                             Add_Use => Add_Use'Access,
                                             Command => Command'Access,
                                             Prepare => Prepare'Access);
end Rules.Header_Comments;
