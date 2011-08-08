----------------------------------------------------------------------
--  Framework.Reports - Package body                                --
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
  Ada.Strings.Wide_Fixed,
  Ada.Characters.Wide_Latin_1,
  Ada.Strings.Wide_Maps,
  Ada.Wide_Text_Io;

-- Adalog
with
  Utilities;

-- Adactl
with
  Implementation_Options,
  Adactl_Options;

package body Framework.Reports is
   use Ada.Strings.Wide_Unbounded;

   Not_Found   : constant             := 0;
   Adactl_Mark : constant Wide_String := "##";
   Adactl_Tag  : constant Wide_String := "--" & Adactl_Mark;

   Error_Found : Boolean := False;


   package Counters is new Binary_Map (Unbounded_Wide_String, Natural);
   Rule_Counter : Counters.Map;

   subtype Word is Unbounded_Wide_String;
   type Words_List is array (Natural range <>) of Word;
   -- Provides a multipurpose list of words

   --------------------------
   -- To_Upper_Wide_String --
   --------------------------

   function To_Upper_Wide_String (W : in Word) return Wide_String is
      -- Returns a converted upper case wide string from a word
      use Utilities;
   begin
      return To_Upper (To_Wide_String (W));
   end To_Upper_Wide_String;

   -----------
   -- Split --
   -----------

   function Split (S : in Wide_String) return Words_List is
      -- Returns the Parameters list split from a line.
      -- Separators are space and horizontal tabulation.
      -- Lower bound of result is always 1.
      use Ada.Characters.Wide_Latin_1;
      use Ada.Strings;
      use Ada.Strings.Wide_Maps;
      use Ada.Strings.Wide_Fixed;
      use Ada.Strings.Wide_Unbounded;

      Separator_Set : constant Wide_Character_Set := To_Set (Wide_Space & Ht);
      Dummy_Length : constant Natural             := Count (S, Separator_Set) + 1;

      S_First : Natural := S'First;
      S_Last  : Natural;
      First   : Natural := 0;
      Last    : Natural := 0;
      Count   : Natural := 0;
      Dummy   : Words_List (1..Dummy_Length)
        := (others => Null_Unbounded_Wide_String);
   begin
      S_Last := Index (S, Adactl_Mark);
      if S_Last = Not_Found then
         S_Last := S'Last;
      end if;

      loop
         Find_Token (S (S_First..S_Last), Separator_Set, Outside, First, Last);

         if First = S_First and Last = 0 then
            exit;
         end if;

         Count := Count + 1;
         Dummy (Count) := To_Unbounded_Wide_String (S (First..Last));
         S_First := Last + 1;
      end loop;

      return (Dummy (1 .. Count));
   end Split;

   -----------
   -- Is_In --
   -----------

   function Is_In (Rule_Id : in Wide_String;
                   Words   : in Words_List)
                  return Boolean is
      use Utilities;
   begin
      for I in Words'Range loop
         if To_Upper_Wide_String (Words (I)) = To_Upper (Rule_Id) then
            return True;
         end if;
      end loop;

      -- end of words is reached, rule is not in words
      return False;
   end Is_In;

   ------------
   -- Update --
   ------------

   procedure Update (Rule_Id    : in     Wide_String;
                     Rule_Label : in     Wide_String;
                     Line       : in     Wide_String;
                     Active     : in out Boolean) is
      use Ada.Strings.Wide_Fixed;

      Pos : constant Natural := Index (Line, Adactl_Tag);
   begin

      if Pos /= Not_Found then
         declare
            Words : Words_List := Split (Line (Pos + Adactl_Tag'Length .. Line'Last));
         begin
            if Words'Length >= 3 then
               -- Words (Words'First) = Adactl_Tag
               if To_Upper_Wide_String (Words (1)) = "RULE" then
                  declare
                     S : constant Wide_String := To_Upper_Wide_String (Words (2));
                  begin
                     if Is_In (Rule_Id,    Words (3 .. Words'Last)) or else
                        Is_In (Rule_Label, Words (3 .. Words'Last)) or else
                        Is_In ("ALL",      Words (3 .. Words'Last))
                     then
                        if S = "ON" then
                           Active := True;
                        elsif S = "OFF" then
                           Active := False;
                        end if;
                     end if;
                  end;
               end if;
            end if;
         end;
      end if;
      -- in others cases (i.e. no deactivation, bad syntax or a single line
      -- deactivation)
      -- Active does not change
   end Update;

   ------------------------
   -- Single_Line_Update --
   ------------------------

   procedure Single_Line_Update (Rule_Id    : in     Wide_String;
                                 Rule_Label : in     Wide_String;
                                 Line       : in     Wide_String;
                                 Active     : in out Boolean) is
      use Ada.Strings.Wide_Fixed;
      Pos : constant Natural := Index (Line, Adactl_Tag);
   begin

      if Pos /= Not_Found then
         declare
            Words : Words_List := Split (Line (Pos + Adactl_Tag'Length .. Line'Last));
         begin
            if Words'Length >= 4 then
               if To_Upper_Wide_String (Words (1)) = "RULE" and
                  To_Upper_Wide_String (Words (2)) = "LINE"
               then
                  declare
                     S : constant Wide_String := To_Upper_Wide_String (Words (3));
                  begin
                     if Is_In (Rule_Id,    Words (4 .. Words'Last)) or else
                        Is_In (Rule_Label, Words (4 .. Words'Last)) or else
                        Is_In ("ALL",      Words (4 .. Words'Last))
                     then
                        if S = "ON" then
                           Active := True;
                        elsif S = "OFF" then
                           Active := False;
                        end if;
                     end if;
                  end;
               end if;
            end if;
         end;
      end if;
      -- in others cases (i.e. no deactivation or bad syntax)
      -- Active does not change
   end Single_Line_Update;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active (Rule_Id    : in Wide_String;
                       Rule_Label : in Wide_String;
                       Loc        : in Framework.Location)
                      return Boolean is
      use Ada.Characters.Handling;
      use Ada.Wide_Text_Io;
      use Framework;
      File      : File_Type;
      Line      : Wide_String (1..1024);
      Line_Last : Natural               := 0;
      Active    : Boolean               := True;
   begin
      Open (File,
            In_File,
            To_String (Get_File_Name (Loc)),
            Form => Implementation_Options.Form_Parameters);

      for I in 1 .. Get_First_Line (Loc) - 1 loop
         Get_Line (File, Line, Line_Last);
         Update (Rule_Id, Rule_Label, Line (Line'First .. Line_Last), Active);
      end loop;

      Get_Line (File, Line, Line_Last);
      Single_Line_Update (Rule_Id, Rule_Label, Line (Line'First .. Line_Last), Active);

      Close (File);

      return Active;

   exception
      when Name_Error =>
         -- if file is not found ???,
         -- consider that rule is active
         return True;
   end Is_Active;

   ------------
   -- Report --
   ------------

   procedure Report (Rule_Id    : in Wide_String;
                     Rule_Label : in Wide_String;
                     Rule_Type  : in Rule_Types;
                     Loc        : in Location;
                     Msg        : in Wide_String) is
      use Utilities, Adactl_Options;

      Label : constant Wide_String := Choose (Rule_Label, Otherwise => Rule_Id);

      procedure Issue_Message (Title : Wide_String) is
         use Ada.Wide_Text_IO;
      begin
         Put (Image (Loc));
         Put (": ");
         Put (Title);
         Put (": ");
         Put (Label);
         Put (": ");
         Put (Msg);
         New_Line;
      end Issue_Message;

      use Counters;
   begin
      if Ignore_Option or else Is_Active (Rule_Id, Rule_Label, Loc) then
         case Rule_Type is
            when Check =>
               Error_Found := True;
               Issue_Message ("Error");
            when Search =>
               if Warning_As_Error_Option then
                  Error_Found := True;
               end if;
               Issue_Message ("Found");
            when Count =>
               Add (Rule_Counter,
                    To_Unbounded_Wide_String (Label),
                    Fetch (Rule_Counter, To_Unbounded_Wide_String (Label), Default_Value => 0) + 1);
         end case;
      end if;
   end Report;

   --------------------
   -- Error_Reported --
   --------------------

   function Error_Reported return Boolean is
   begin
      return Error_Found;
   end Error_Reported;

   -------------------
   -- Report_Counts --
   -------------------

   procedure Report_Counts is
      use Counters, Ada.Wide_Text_IO;

      procedure Report_One_Count (Key : in Unbounded_Wide_String; Value : in out Natural) is
      begin
         Put (To_Wide_String (Key));
         Put (":");
         Put (Natural'Wide_Image (Value));
         New_Line;
      end Report_One_Count;

      procedure Report_All_Counts is new Iterate (Report_One_Count);
   begin
      if Is_Empty (Rule_Counter) then
         return;
      end if;

      New_Line;
      Put_Line ("Counts summary:");
      Report_All_Counts (Rule_Counter);
   end Report_Counts;

   ------------------
   -- Clear_Counts --
   ------------------

   procedure Clear_Counts is
      use Counters;
   begin
      Clear (Rule_Counter);
   end Clear_Counts;

end Framework.Reports;
