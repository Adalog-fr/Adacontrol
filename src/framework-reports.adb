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
  Ada.Wide_Text_IO;

-- Adalog
with
  Utilities;

-- Adactl
with
  Implementation_Options,
  Adactl_Options;

package body Framework.Reports is

   CSV_Separator : constant array (Output_Format range CSV..CSVX) of Wide_Character := (',', ';');

   Not_Found   : constant             := 0;
   Adactl_Mark : constant Wide_String := "##";
   Adactl_Tag  : constant Wide_String := "--" & Adactl_Mark;

   Error_Count   : Natural := 0;
   Warning_Count : Natural := 0;

   Uncheckable_Used   : array (Uncheckable_Consequence) of Boolean := (others => False);
   Uncheckable_Types  : array (Uncheckable_Consequence) of Rule_Types;
   Uncheckable_Labels : array (Uncheckable_Consequence) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   package Counters is new Binary_Map (Unbounded_Wide_String, Natural);
   Rule_Counter   : Counters.Map;
   Stats_Counters : array (Rule_Types) of Counters.Map;

   -----------
   -- Reset --
   -----------

   procedure Reset_One (Key : Unbounded_Wide_String; Counter_Value : in out Natural) is
      pragma Unreferenced (Key);
   begin
      Counter_Value := 0;
   end Reset_One;
   procedure Reset is new Counters.Iterate (Reset_One);

   ------------
   -- Update --
   ------------

   procedure Update (Rule_Id     : in     Wide_String;
                     Rule_Label  : in     Wide_String;
                     Line        : in     Wide_String;
                     Single_Line : in     Boolean;
                     Active      : in out Boolean) is
      use Utilities, Ada.Strings.Wide_Fixed;

      Pos : Natural := Index (Line, Adactl_Tag);

      function Next_Word return Wide_String is
         Quoted : Boolean;
         Start  : Positive := Pos;
         Stop   : Positive;
      begin
         loop
            if Start > Line'Last then
               return "";
            elsif Line (Start) <= ' ' then
               Start := Start+1;
            elsif Start <= Line'Last - Adactl_Mark'Length + 1
              and then Line (Start .. Start + Adactl_Mark'Length -1) = Adactl_Mark
            then
               return "";
            else
               exit;
            end if;
         end loop;

         Quoted := Line (Start) = '"';
         if Quoted then
            Start := Start + 1;
         end if;
         Stop := Start;

         loop
            if Quoted then
               exit when Line (Stop) = '"';
               if Stop = Line'Last then
                  -- badly formed quoted syntax
                  return "";
               end if;

            else
               exit when Stop > Line'Last;
               exit when Line (Stop) <= ' ';
            end if;
            Stop := Stop + 1;
         end loop;

         if Quoted then
            Pos := Stop + 1;
            return To_Upper (Line (Start .. Stop-1));
         else
            Pos := Stop;
            return To_Upper (Line (Start .. Stop-1));
         end if;
      end Next_Word;

   begin
      if Pos = Not_Found then
         return;
      end if;
      Pos := Pos + Adactl_Tag'Length;

      if Next_Word /= "RULE" then
         return;
      end if;

      declare
         Switch   : constant Wide_String := Next_Word;
         Activity : Boolean;
      begin
         if Single_Line then
            if Switch /= "LINE" then
               return;
            end if;

            declare
               Switch2 : constant Wide_String := Next_Word;
            begin
               if Switch2 = "ON" then
                  Activity := True;
               elsif Switch2 = "OFF" then
                  Activity := False;
               else
                  -- Illegal syntax, ignore
                  return;
               end if;
            end;

         else
            if Switch = "ON" then
               Activity := True;
            elsif Switch = "OFF" then
               Activity := False;
            else
               -- Illegal syntax, ignore
               return;
            end if;
         end if;

         loop
            declare
               Current : constant Wide_String := Next_Word;
            begin
               exit when Current = "";
               if        Current = To_Upper (Rule_Id)
                 or else Current = To_Upper (Rule_Label)
                 or else Current = "ALL"
               then
                  Active := Activity;
                  return;
               end if;
            end;
         end loop;
      end;

      -- in others cases (i.e. no deactivation, bad syntax or a single line
      -- deactivation)
      -- Active does not change
   end Update;

   ------------
   -- Report --
   ------------

   procedure Report (Rule_Id    : in Wide_String;
                     Rule_Label : in Wide_String;
                     Rule_Type  : in Rule_Types;
                     Loc        : in Location;
                     Msg        : in Wide_String) is
      use Utilities, Adactl_Options;

      Label     : constant Wide_String := Choose (Rule_Label, Otherwise => Rule_Id);
      Line      : Wide_String (1..1024);
      Line_Last : Natural := 0;

      procedure Issue_Message (Title : Wide_String) is
         use Ada.Wide_Text_IO;

         function Quote (Item : Wide_String) return Wide_String is
            use Ada.Strings.Wide_Fixed;
            Result : Wide_String (Item'First .. Item'Last + Ada.Strings.Wide_Fixed.Count (Item, """") + 2);
            Index  : Positive;
         begin
            Index := Result'First;
            Result (Index) := '"';

            for I in Item'Range loop
               if Item (I) = '"' then
                  Index := Index + 1;
                  Result (Index) := '"';
               end if;
               Index := Index + 1;
               Result (Index) := Item (I);
            end loop;
            Result (Result'Last) := '"';

            return Result;
         end Quote;

         Current_Col : Ada.Wide_Text_IO.Count;
      begin
         case Format_Option is
            when Gnat =>
               if Loc /= Null_Location then
                  Put (Image (Loc));
                  Put (": ");
               end if;
               Put (Title);
               Put (": ");
               Put (Label);
               Put (": ");
               Put (Msg);
               New_Line;
            when CSV | CSVX =>
               if Loc = Null_Location then
                  Put ("""""");
               else
                  Put (Image (Loc));
               end if;
               Put (CSV_Separator (Format_Option));
               Put (Title);
               Put (CSV_Separator (Format_Option));
               Put (Choose (Rule_Label, Otherwise => """"""));
               Put (CSV_Separator (Format_Option));
               Put (Rule_Id);
               Put (CSV_Separator (Format_Option));
               Put (Quote (Msg));
               New_Line;
            when Source =>
               if Loc /= Null_Location then
                  Put (Image (Loc));
                  Put (": ");
               end if;
               Current_Col := Col (Current_Output);
               Put (Line (1 .. Line_Last));
               New_Line;
               Set_Col (Current_Col + Ada.Wide_Text_IO.Count (Get_First_Column (Loc)) - 1);
               Put ("! ");
               Put (Title);
               Put (": ");
               Put (Label);
               Put (": ");
               Put (Msg);
               New_Line;
         end case;
      end Issue_Message;

      use Counters;
      Active : Boolean := True;

   begin
      if not Ignore_Option or Format_Option = Source then
         declare
            use Ada.Characters.Handling, Ada.Wide_Text_IO;
            File : File_Type;
         begin
            Open (File,
                  In_File,
                  To_String (Get_File_Name (Loc)),
                  Form => Implementation_Options.Form_Parameters);

            for I in Natural range 1 .. Get_First_Line (Loc) - 1 loop
               Get_Line (File, Line, Line_Last);
               if not Ignore_Option then
                  Update (Rule_Id, Rule_Label, Line (Line'First .. Line_Last), Single_Line => False, Active => Active);
               end if;
            end loop;

            Get_Line (File, Line, Line_Last);
            if not Ignore_Option then
               Update (Rule_Id, Rule_Label, Line (Line'First .. Line_Last), Single_Line => True, Active => Active);
            end if;

            Close (File);
         exception
            when Name_Error =>
               -- if file is not found ???,
               -- consider that rule is active
               null;
            when others =>
               if Is_Open (File) then
                  Close (File);
               end if;
               raise;
         end;
      end if;

      -- Here, Line is the good source line

      if Active then
         case Rule_Type is
            when Check =>
               Error_Count := Error_Count + 1;
               Issue_Message ("Error");
            when Search =>
               if Warning_As_Error_Option then
                  Error_Count := Error_Count + 1;
               else
                  Warning_Count := Warning_Count + 1;
               end if;

               if Warning_As_Error_Option or else not Skip_Warning_Option then
                  Issue_Message ("Found");
               end if;
            when Count =>
               Add (Rule_Counter,
                    To_Unbounded_Wide_String (Label),
                    Fetch (Rule_Counter, To_Unbounded_Wide_String (Label)) + 1);
         end case;

         if Stats_Level >= Nulls_Only then
            declare
               Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Rule_Id
                                                                                   & Choose (Rule_Label = "",
                                                                                             "",
                                                                                             "." & Rule_Label));
            begin
               Add (Stats_Counters (Rule_Type), Key, Fetch (Stats_Counters (Rule_Type), Key) + 1);
            end;
         end if;
      end if;
   end Report;

   ------------
   -- Report --
   ------------

   procedure Report (Rule_Id    : in Wide_String;
                     Context    : in Root_Context'Class;
                     Loc        : in Location;
                     Msg        : in Wide_String;
                     Count_Only : in Boolean := False)
   is
   begin
      if Context = No_Matching_Context then
         return;
      end if;

      declare
         Basic_Context : Basic_Rule_Context renames Basic_Rule_Context (Context);
      begin
         if not Count_Only then
            Report (Rule_Id,
                    To_Wide_String (Basic_Context.Rule_Label),
                    Basic_Context.Rule_Type,
                    Loc,
                    Msg);
         end if;

         if Basic_Context.With_Count then
            Report (Rule_Id,
                    To_Wide_String (Basic_Context.Count_Label),
                    Count,
                    Loc,
                    Msg);
         end if;
      end;
   end Report;

   -----------------
   -- Uncheckable --
   -----------------

   Risk_Message : constant array (Uncheckable_Consequence) of Wide_String (1 .. 8)
     := ("positive", "negative");

   procedure Uncheckable (Rule_Id : in Wide_String;
                          Risk    : in Uncheckable_Consequence;
                          Loc     : in Location;
                          Msg     : in Wide_String)
   is
      use Utilities;
      Rule_Label : constant Wide_String := To_Wide_String (Uncheckable_Labels (Risk));
   begin
      if Uncheckable_Used (Risk) then
         Report (Rule_Id,
                 Rule_Label,
                 Uncheckable_Types  (Risk),
                 Loc,
                 Choose (Rule_Label /= "", "in rule " & Rule_Id & ": ", "")
                   & "Possible false " & Risk_Message (Risk) & ": " & Msg);
      end if;
   end Uncheckable;

   -----------------------
   -- Reset_Uncheckable --
   -----------------------

   procedure Reset_Uncheckable is
   begin
      Uncheckable_Used := (others => False);
   end Reset_Uncheckable;

   ---------------------
   -- Set_Uncheckable --
   ---------------------

   procedure Set_Uncheckable (Risk : Uncheckable_Consequence; Rule_Type : Rule_Types; Label : Wide_String) is
   begin
      Uncheckable_Used   (Risk) := True;
      Uncheckable_Types  (Risk) := Rule_Type;
      Uncheckable_Labels (Risk) := To_Unbounded_Wide_String (Label);
   end Set_Uncheckable;

   ---------------
   -- Nb_Errors --
   ---------------

   function Nb_Errors return Natural is
   begin
      return Error_Count;
   end Nb_Errors;

   -----------------
   -- Nb_Warnings --
   -----------------

   function Nb_Warnings return Natural is
   begin
      return Warning_Count;
   end Nb_Warnings;

   -----------------
   -- Init_Counts --
   -----------------

   procedure Init_Counts (Rule_Id : Wide_String; Rule_Label : Wide_String) is
      use Counters, Utilities;

      Label : constant Wide_String := Choose (Rule_Label, Otherwise => Rule_Id);
   begin
      Add (Rule_Counter, To_Unbounded_Wide_String (Label), 0);
  end Init_Counts;

   -------------------
   -- Report_Counts --
   -------------------

   procedure Report_Counts is
      use Counters, Utilities, Ada.Wide_Text_IO;

      procedure Report_One_Count (Key : in Unbounded_Wide_String; Counter_Value : in out Natural) is
      begin
         Put (To_Wide_String (Key));
         case Format_Option is
            when Gnat | Source=>
               Put (": ");
            when CSV | CSVX =>
               Put (CSV_Separator (Format_Option));
         end case;
         Put (Integer_Img (Counter_Value));
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

   ---------------
   -- Clear_All --
   ---------------

   procedure Clear_All is
      use Counters;
   begin
      Clear (Rule_Counter);
      for R in Rule_Types loop
         Clear (Stats_Counters (R));
      end loop;
   end Clear_All;


   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Warning_Count := 0;
      Error_Count   := 0;
      Reset (Rule_Counter);

      for R in Rule_Types loop
         Reset (Stats_Counters (R));
      end loop;
   end Reset;


   ----------------
   -- Init_Stats --
   ----------------

   procedure Init_Stats (Rule, Label : Wide_String) is
      use Counters, Utilities;
      Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Rule & Choose (Label = "", "", "." & Label));
   begin
      for R in Rule_Types loop
        Add (Stats_Counters (R), Key, 0);
      end loop;
   end Init_Stats;


   -----------
   -- Clear --
   -----------

   procedure Clear (Rule : Wide_String) is
      use Counters;

      procedure Check_Delete_One (Key : in Unbounded_Wide_String; Counter_Value : in out Integer) is
         pragma Unreferenced (Counter_Value);
      begin
         if Head (Key, Rule'Length) = Rule then
            raise Delete_Current;
         end if;
      end Check_Delete_One;

      procedure Check_Delete is new Iterate (Check_Delete_One);

   begin
      Check_Delete (Rule_Counter);
      -- TBSL: delete entries whose name is a label associated to the rule

      for R in Rule_Types loop
         Check_Delete (Stats_Counters (R));
      end loop;
   end Clear;


   ------------------
   -- Report_Stats --
   ------------------

   procedure Report_Stats is
      use Counters, Ada.Wide_Text_IO;

      procedure Report_One_Stat (Key : in Unbounded_Wide_String; Counter_Value : in out Natural) is
         use Utilities;
         Triggered_Count : Natural     := Counter_Value;
         Wide_Key        : Wide_String := To_Wide_String (Key);
         Dot_Found       : Boolean     := False;
      begin
         for R in Rule_Types range Rule_Types'Succ (Rule_Types'First) .. Rule_Types'Last loop
            Triggered_Count := Triggered_Count + Fetch (Stats_Counters (R), Key);
         end loop;

         if Triggered_Count = 0 or else Stats_Level = Full then
            case Format_Option is
               when Gnat | Source=>
                  Put (Wide_Key);
                  if Triggered_Count = 0 then
                     Put ("not triggered");
                  else
                     Put (To_Title (Rule_Types'Wide_Image (Rule_Types'First)));
                     Put (": ");
                     Put (Integer_Img (Counter_Value));

                     for R in Rule_Types range Rule_Types'Succ (Rule_Types'First) .. Rule_Types'Last loop
                        Put (", ");
                        Put (To_Title (Rule_Types'Wide_Image (R)));
                        Put(": ");
                        Put (Integer_Img (Fetch (Stats_Counters (R), Key)));
                     end loop;
                  end if;
               when CSV | CSVX =>
                  -- Add CSV separator in place of first '.' in wide key
                  -- (Separates rule name from label)
                  for I in Wide_Key'Range loop
                     if Wide_Key (I) = '.' then
                        Wide_Key (I) := CSV_Separator (Format_Option);
                        Dot_Found    := True;
                        exit;
                     end if;
                  end loop;
                  Put (Wide_Key);
                  if not Dot_Found then
                     Put (CSV_Separator (Format_Option));
                  end if;

                  for R in Rule_Types loop
                     Put (CSV_Separator (Format_Option));
                     Put (Integer_Img (Fetch (Stats_Counters (R), Key)));
                  end loop;
            end case;
            New_Line;
         end if;
      end Report_One_Stat;

      procedure Report_All_Stats is new Iterate (Report_One_Stat);
   begin
      if Stats_Level = None then
         return;
      end if;

      if Stats_Level >= Nulls_Only then
         New_Line;
         Put_Line ("Rules usage statistics:");
         case Format_Option is
            when Gnat | Source=>
               null;
            when CSV | CSVX =>
               Put_Line ("Rule"
                           & CSV_Separator (Format_Option) & "Label"
                           & CSV_Separator (Format_Option) & "Check"
                           & CSV_Separator (Format_Option) & "Search"
                           & CSV_Separator (Format_Option) & "Count");
         end case;
         Report_All_Stats (Stats_Counters (Rule_Types'First));
      end if;

      if Stats_Level >= General then
         New_Line;
         Put ("Issued messages: Errors =" & Natural'Wide_Image (Nb_Errors));
         Put (", Warnings =" & Natural'Wide_Image (Nb_Warnings));
         New_Line;
      end if;
   end Report_Stats;

   ---------------------
   -- Report_Counters --
   ---------------------

   procedure Report_Counters is
   begin
      Report_Counts;
      Report_Stats;
   end Report_Counters;
end Framework.Reports;
