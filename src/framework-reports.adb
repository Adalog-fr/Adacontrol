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
  Ada.Exceptions,
  Ada.Characters.Handling,
  Ada.Strings.Wide_Fixed,
  Ada.Wide_Text_IO;

-- Adalog
with
  Binary_Map,
  Linear_Queue,
  Utilities;

-- Adactl
with
  Framework.Variables,
  Implementation_Options,
  Adactl_Options;

package body Framework.Reports is

   --
   -- User settable variables
   --
   Active_Warning_Option   : Boolean      := True;
   Warning_As_Error_Option : Boolean      := False;
   Max_Errors              : Natural      := Natural'Last;
   Max_Messages            : Natural      := Natural'Last;
   Stats_Level             : Stats_Levels := None;

   Check_Message           : Unbounded_Wide_String := To_Unbounded_Wide_String ("Error");
   Search_Message          : Unbounded_Wide_String := To_Unbounded_Wide_String ("Found");
   Adactl_Tag1             : Unbounded_Wide_String := To_Unbounded_Wide_String ("##");
   Adactl_Tag2             : Unbounded_Wide_String := To_Unbounded_Wide_String ("##");

   pragma Warnings (Off, "package * is not referenced");

   procedure Set_Format (Format : in Wide_String);   -- To set Format_Option (declared in spec)
   package Register_Format_Option is
     new Framework.Variables.Register_Special_Variable (Set_Format,
                                                        Variable_Name => "FORMAT");

   package Register_Active_Warning_Option is
     new Framework.Variables.Register_Discrete_Variable (Boolean,
                                                         Active_Warning_Option,
                                                         Variable_Name => "WARNING",
                                                         Decode        => Framework.Variables.On_Off_To_Boolean);
   package Register_Warning_As_Error_Option is
     new Framework.Variables.Register_Discrete_Variable (Boolean,
                                                         Warning_As_Error_Option,
                                                         Variable_Name => "WARNING_AS_ERROR",
                                                         Decode        => Framework.Variables.On_Off_To_Boolean);
   package Register_Max_Errors is
     new Framework.Variables.Register_Discrete_Variable (Natural,
                                                         Max_Errors,
                                                         Variable_Name => "MAX_ERRORS");
   package Register_Max_Messages is
     new Framework.Variables.Register_Discrete_Variable (Natural,
                                                         Max_Messages,
                                                         Variable_Name => "MAX_MESSAGES");
   function Decode_Level (Level : Wide_String) return Stats_Levels;
   package Register_Stats_Level is
     new Framework.Variables.Register_Discrete_Variable (Stats_Levels,
                                                         Stats_Level,
                                                         Variable_Name => "STATISTICS",
                                                         Decode => Decode_Level);
   package Register_Check_Message is
     new Framework.Variables.Register_String_Variable (Check_Message,
                                                       Variable_Name => "CHECK_KEY");
   package Register_Search_Message is
     new Framework.Variables.Register_String_Variable (Search_Message,
                                                       Variable_Name => "SEARCH_KEY");
   package Register_Tag1 is
     new Framework.Variables.Register_String_Variable (Adactl_Tag1,
                                                       Variable_Name => "TAG1");
   package Register_Tag2 is
     new Framework.Variables.Register_String_Variable (Adactl_Tag2,
                                                       Variable_Name => "TAG2");

   pragma Warnings (On, "package * is not referenced");

   --
   -- Local variables
   --
   CSV_Separator : constant array (Output_Format range CSV .. None) of Wide_Character := (',', ';', ';');

   Not_Found : constant := 0;

   Error_Count   : Natural := 0;
   Warning_Count : Natural := 0;

   Uncheckable_Used   : array (Uncheckable_Consequence) of Boolean := (others => False);
   Uncheckable_Controls : array (Uncheckable_Consequence) of Control_Kinds;
   Uncheckable_Labels   : array (Uncheckable_Consequence) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   type False_Positive_Info (Len : Positive) is
      record
         Loc : Location;
         Msg : Wide_String (1 .. Len);
      end record;
   package False_Positive_List is new Linear_Queue (False_Positive_Info);
   package False_Positive_Map  is new Binary_Map (Unbounded_Wide_String, False_Positive_List.Queue);
   False_Positive_Messages : False_Positive_Map.Map;
   -- The above map maps Rule Id's to a list of delayed false positive messages
   -- See Uncheckable.

   package Counters is new Binary_Map (Unbounded_Wide_String, Natural);
   Rule_Counter   : Counters.Map;
   Stats_Counters : array (Control_Kinds) of Counters.Map;
   -- Note on the management of stat counters:
   -- An entry in each map of Stats_Counters is added by Rule_Manager (Init_Stats)
   -- when a new control is added. This is necessary to know about rules that were
   -- not triggered at all.
   -- However, this does not guarantee that every rule is present in the map, because
   -- of Uncheckable, which does not follow the normal naming scheme of rules.
   -- Therefore, a default value must still be provided when fetching from those maps.

   ------------------
   -- Decode_Level --
   ------------------

   function Decode_Level (Level : Wide_String) return Stats_Levels is
   begin
      return Stats_Levels'Val (Natural'Wide_Value (Level));
   end Decode_Level;

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

   procedure Update (Id          : in     Wide_String;
                     Label       : in     Wide_String;
                     Line        : in     Wide_String;
                     Single_Line : in     Boolean;
                     Active      : in out Boolean) is
      use Utilities, Ada.Strings.Wide_Fixed;

      Mark1 : constant Wide_String := "--" & To_Wide_String (Adactl_Tag1);
      Mark2 : constant Wide_String := To_Wide_String (Adactl_Tag2);
      Pos   : Natural := Index (Line, Mark1);

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
            elsif Start <= Line'Last - Mark2'Length + 1
              and then Line (Start .. Start + Mark2'Length -1) = Mark2
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
               exit when Line (Stop) = '#';
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

   begin  -- Update
      if Pos = Not_Found then
         return;
      end if;
      Pos := Pos + Mark1'Length;

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
               if        Current = To_Upper (Id)
                 or else Current = To_Upper (Label)
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

   ----------------
   -- Set_Format --
   ----------------

   procedure Set_Format (Format : Wide_String) is
      use Utilities;
      use Ada.Strings.Wide_Fixed;
      Sep_Pos : Natural := Index (Format, "_");
   begin
      if Sep_Pos = 0 then
         Sep_Pos            := Format'Last + 1;
         Default_Short_Name := False;
      elsif To_Upper (Format (Sep_Pos .. Format'Last)) = "_SHORT" then
        Default_Short_Name := True;
      else
         raise Constraint_Error;
      end if;

      Format_Option := Output_Format'Wide_Value (Format (Format'First .. Sep_Pos - 1)); -- May raise C_E
   end Set_Format;

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Wide_String) return Wide_String is
   begin
      case Format_Option is
         when Gnat | Source=>
            return Left & ": " & Right;
         when CSV | CSVX | None =>
            return Left & CSV_Separator (Format_Option) & Right;
      end case;
   end "and";

   ----------------
   -- Raw_Report --
   ----------------

   procedure Raw_Report (Message : Wide_String) is
      use Ada.Wide_Text_IO;
   begin
      Put_Line (Message);
   end Raw_Report;

   ------------
   -- Report --
   ------------

   -- This one is the "true" Report procedure, i.e. the other Report procedure is just
   -- a front-end to this one.
   procedure Report (Rule_Id   : in Wide_String;
                     Ctl_Label : in Wide_String;
                     Ctl_Kind  : in Control_Kinds;
                     Loc       : in Location;
                     Msg       : in Wide_String)
   is
      use Utilities, Adactl_Options;

      Label     : constant Wide_String := Choose (Ctl_Label, Otherwise => Rule_Id);
      Line      : Wide_String (1..1024);
      Line_Last : Natural := 0;

      procedure Issue_Message (Title : Wide_String) is
         use Ada.Strings.Wide_Fixed, Ada.Wide_Text_IO;

         function Quote (Item : Wide_String) return Wide_String is
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

      begin   -- Issue_Message
         if Just_Created then
            Just_Created := False;
            case Format_Option is
               when CSV | CSVX =>
                  Put ("File");
                  Put (CSV_Separator (Format_Option));
                  Put ("Line");
                  Put (CSV_Separator (Format_Option));
                  Put ("Col");
                  Put (CSV_Separator (Format_Option));
                  Put ("Type");
                  Put (CSV_Separator (Format_Option));
                  Put ("Label");
                  Put (CSV_Separator (Format_Option));
                  Put ("Rule");
                  Put (CSV_Separator (Format_Option));
                  Put ("Message");
                  New_Line;
               when others =>
                  null;
            end case;
         end if;

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
                  Put ("""""" and """""" and """""");
               else
                  Put (Image (Loc, Separator => CSV_Separator (Format_Option)));
               end if;
               Put (CSV_Separator (Format_Option));
               Put (Title);
               Put (CSV_Separator (Format_Option));
               Put (Choose (Ctl_Label, Otherwise => """"""));
               Put (CSV_Separator (Format_Option));
               Put (Rule_Id);
               Put (CSV_Separator (Format_Option));
               Put (Quote (Msg));
               New_Line;
            when Source =>
               if Loc = Null_Location then
                  Put (Line (1 .. Line_Last));
               else
                  Put (Image (Loc));
                  Put (": ");
                  Put (Line (1 .. Line_Last));
                  New_Line;
                  Put (Image (Loc));
                  Put (": ");
                  Put ((Get_First_Column (Loc) - 1) * ' ');
                  Put ("! ");
               end if;
               Put (Title);
               Put (": ");
               Put (Label);
               Put (": ");
               Put (Msg);
               New_Line;
            when None =>
               null;
         end case;
      end Issue_Message;

      use Ada.Exceptions;
      use Counters, False_Positive_Map;
      Active : Boolean := True;

   begin  -- Report
      if Error_Count = Max_Errors or Error_Count + Warning_Count = Max_Messages then
         -- This can happen for finalization messages after the run has been previously cancelled
         -- due to too many errors/messages
         return;
      end if;

      -- Output delayed false positive messages for this rule
      -- There can be such messages only if Uncheckable has been activated
      if Is_Present (False_Positive_Messages, To_Unbounded_Wide_String (Rule_Id)) then
         declare
            use False_Positive_List;
            Message_Queue : constant Queue  := Fetch (False_Positive_Messages, To_Unbounded_Wide_String (Rule_Id));
            Current       : Cursor := First (Message_Queue);
         begin
            -- Delete entry in map here to avoid infinite recursion
            -- There is no problem, since Queues are controlled, the queue will be released when
            -- exiting the block.
            Clear_Delayed_Uncheckable_Messages (Rule_Id);
            while Has_Element (Current) loop
               declare
                  Mess_Info : constant False_Positive_Info := Fetch (Current);
               begin
                  Report (Rule_Id,
                          To_Wide_String (Uncheckable_Labels (False_Positive)),
                          Uncheckable_Controls  (False_Positive),
                          Mess_Info.Loc,
                          Mess_Info.Msg);
               end;
               Current := Next (Current);
            end loop;
         end;
      end if;

      -- Retrieve source line, but only if necessary since it can be quite
      -- a long operation
      if Format_Option = Source
        or (Format_Option /= None and not Ignore_Option)
      then
         declare
            use Ada.Characters.Handling, Ada.Wide_Text_IO;
            Source_File : File_Type;
         begin
            Open (Source_File,
                  In_File,
                  To_String (Get_File_Name (Loc)),
                  Form => Implementation_Options.Form_Parameters);

            for I in Natural range 1 .. Get_First_Line (Loc) - 1 loop
               Get_Line (Source_File, Line, Line_Last);
               if not Ignore_Option then
                  Update (Rule_Id, Ctl_Label, Line (Line'First .. Line_Last), Single_Line => False, Active => Active);
               end if;
            end loop;

            Get_Line (Source_File, Line, Line_Last);
            if not Ignore_Option then
               Update (Rule_Id, Ctl_Label, Line (Line'First .. Line_Last), Single_Line => True, Active => Active);
            end if;

            Close (Source_File);
         exception
            when Name_Error =>
               -- if file is not found ???,
               -- consider that rule is active
               null;
            when Occur : others =>
               if Is_Open (Source_File) then
                  Close (Source_File);
               end if;
               Failure ("Report: "
                        & To_Wide_String (Exception_Name (Occur)) & " raised while searching "
                        & Get_File_Name (Loc) & ", line " & Integer_Img (Get_First_Line (Loc)));
         end;
      end if;

      -- Here, Line is the good source line

      if Active then
         case Ctl_Kind is
            when Check =>
               Error_Count := Error_Count + 1;

               Issue_Message (To_Wide_String(Check_Message));
            when Search =>
               if Warning_As_Error_Option then
                  Error_Count := Error_Count + 1;
               else
                  Warning_Count := Warning_Count + 1;
               end if;

               if Warning_As_Error_Option or else Active_Warning_Option then
                  Issue_Message (To_Wide_String (Search_Message));
               end if;
            when Count =>
               Add (Rule_Counter,
                    To_Unbounded_Wide_String (Label),
                    Fetch (Rule_Counter, To_Unbounded_Wide_String (Label)) + 1);
         end case;

         if Stats_Level >= Nulls_Only then
            declare
               Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Rule_Id
                                                                                   & Choose (Ctl_Label = "",
                                                                                             "",
                                                                                             "." & Ctl_Label));
            begin
               Add (Stats_Counters (Ctl_Kind), Key, Fetch (Stats_Counters (Ctl_Kind), Key, Default_Value => 0) + 1);
            end;
         end if;

         if Error_Count = Max_Errors then
            Raise_Exception (Cancellation'Identity, Message => "too many errors");
         elsif Error_Count + Warning_Count = Max_Messages then
            Raise_Exception (Cancellation'Identity, Message => "too many messages");
         end if;
      end if;
   end Report;

   ------------
   -- Report --
   ------------

   procedure Report (Rule_Id : in Wide_String;
                     Context : in Control_Manager.Root_Context'Class;
                     Loc     : in Location;
                     Msg     : in Wide_String)
   is
      use Control_Manager;
   begin
      if Context = No_Matching_Context then
         return;
      end if;

      declare
         Basic_Context : Basic_Rule_Context renames Basic_Rule_Context (Context);
      begin
         Report (Rule_Id,
                 To_Wide_String (Basic_Context.Ctl_Label),
                 Basic_Context.Ctl_Kind,
                 Loc,
                 Msg);

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

   procedure Uncheckable (Rule_Id : in Wide_String;
                          Risk    : in Uncheckable_Consequence;
                          Loc     : in Location;
                          Msg     : in Wide_String)
   is
      use Utilities;
      Label : constant Wide_String := To_Wide_String (Uncheckable_Labels (Risk));
   begin
      if Uncheckable_Used (Risk) then
         case Risk is
            when False_Negative =>
               Report (Rule_Id,
                       Label,
                       Uncheckable_Controls (Risk),
                       Loc,
                       Choose (Label /= "", "in rule " & Rule_Id & ": ", "")
                       & "Possible false negative: " & Msg);
            when False_Positive =>
               -- False positive messages are delayed until the next call to Report from the
               -- same rule. Therefore, false positive messages will not appear if there are
               -- no messages from the rule at all.
               declare
                  use False_Positive_List, False_Positive_Map;
                  Rule_Queue : Queue := Fetch (False_Positive_Messages,
                                               To_Unbounded_Wide_String (Rule_Id),
                                               Default_Value => Empty_Queue);
                  Full_Msg : constant Wide_String := Choose (Label /= "", "in rule " & Rule_Id & ": ", "")
                                                     & "Possible false positive: " & Msg;
               begin
                  Append (Rule_Queue, (Full_Msg'Length,
                                       Loc,
                                       Full_Msg));
                  Add (False_Positive_Messages, To_Unbounded_Wide_String (Rule_Id), Rule_Queue);
               end;
         end case;

         -- Add UNCHECKABLE to statistics, total under "UNCHECKABLE", subtotal under "UNCHECKABLE.<rule>"
         if Stats_Level >= Nulls_Only then
            declare
               use Counters;
               Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String ("UNCHECKABLE");
            begin
               Add (Stats_Counters (Uncheckable_Controls (Risk)),
                    Key,
                    Fetch (Stats_Counters (Uncheckable_Controls (Risk)), Key, Default_Value => 0) + 1);
            end;
            declare
               use Counters;
               Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String ("UNCHECKABLE." & Rule_Id);
            begin
               Add (Stats_Counters (Uncheckable_Controls (Risk)),
                    Key,
                    Fetch (Stats_Counters (Uncheckable_Controls (Risk)), Key, Default_Value => 0) + 1);
            end;
         end if;
      end if;
   end Uncheckable;

   -----------------------
   -- Reset_Uncheckable --
   -----------------------

   procedure Reset_Uncheckable is
      use False_Positive_Map;
   begin
      Uncheckable_Used := (others => False);
      Clear (False_Positive_Messages);
   end Reset_Uncheckable;

   ---------------------
   -- Set_Uncheckable --
   ---------------------

   procedure Set_Uncheckable (Risk     : Uncheckable_Consequence;
                              Ctl_Kind : Control_Kinds;
                              Label    : Wide_String)
   is
   begin
      Uncheckable_Used   (Risk) := True;
      Uncheckable_Controls  (Risk) := Ctl_Kind;
      Uncheckable_Labels (Risk) := To_Unbounded_Wide_String (Label);
   end Set_Uncheckable;

   ----------------------------------------
   -- Clear_Delayed_Uncheckable_Messages --
   ----------------------------------------

   procedure Clear_Delayed_Uncheckable_Messages (Rule_Id : in Wide_String) is
      use False_Positive_Map;

      Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Rule_Id);
   begin
      if Is_Present (False_Positive_Messages, Key) then
         Delete (False_Positive_Messages, Key);
      end if;
   end Clear_Delayed_Uncheckable_Messages;

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

   procedure Init_Counts (Rule : Wide_String; Label : Wide_String) is
      use Counters, Utilities;

      Good_Label : constant Wide_String := Choose (Label, Otherwise => Rule);
   begin
      Add (Rule_Counter, To_Unbounded_Wide_String (Good_Label), 0);
  end Init_Counts;

   -------------------
   -- Report_Counts --
   -------------------

   procedure Report_Counts is
      use Counters, Ada.Wide_Text_IO;

      procedure Report_One_Count (Key : in Unbounded_Wide_String; Counter_Value : in out Natural) is
         use Utilities;
      begin
         Raw_Report (To_Wide_String (Key) and Integer_Img (Counter_Value));
      end Report_One_Count;

      procedure Report_All_Counts is new Iterate (Report_One_Count);

   begin -- Report_Counts
      if Is_Empty (Rule_Counter) or Format_Option = None then
         return;
      end if;

      case Format_Option is
         when None =>
            return;
         when CSV | CSVX =>
            Raw_Report ("Rule" and "Counts");
         when Source | Gnat =>
            New_Line;
            Raw_Report ("Counts summary:");
      end case;
      Report_All_Counts (Rule_Counter);
   end Report_Counts;

   ---------------
   -- Clear_All --
   ---------------

   procedure Clear_All is
      use Counters;
   begin
      Clear (Rule_Counter);
      for R in Control_Kinds loop
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

      for R in Control_Kinds loop
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
      for R in Control_Kinds loop
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

   begin  -- Clear
      Check_Delete (Rule_Counter);
      -- TBSL: delete entries whose name is a label associated to the rule

      for R in Control_Kinds loop
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
         for R in Control_Kinds range Control_Kinds'Succ (Control_Kinds'First) .. Control_Kinds'Last loop
            Triggered_Count := Triggered_Count + Fetch (Stats_Counters (R), Key, Default_Value => 0);
         end loop;

         if Triggered_Count = 0 or else Stats_Level = Full then
            case Format_Option is
               when Gnat | Source =>
                  Put (Wide_Key);
                  Put (": ");
                  if Triggered_Count = 0 then
                     Put ("not triggered");
                  else
                     Put (To_Title (Control_Kinds'Wide_Image (Control_Kinds'First)));
                     Put (": ");
                     Put (Integer_Img (Counter_Value));

                     for R in Control_Kinds range Control_Kinds'Succ (Control_Kinds'First) .. Control_Kinds'Last loop
                        Put (", ");
                        Put (To_Title (Control_Kinds'Wide_Image (R)));
                        Put(": ");
                        Put (Integer_Img (Fetch (Stats_Counters (R), Key, Default_Value => 0)));
                     end loop;
                  end if;
               when CSV | CSVX | None =>
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

                  for R in Control_Kinds loop
                     Put (CSV_Separator (Format_Option));
                     Put (Integer_Img (Fetch (Stats_Counters (R), Key, Default_Value => 0)));
                  end loop;
            end case;
            New_Line;
         end if;
      end Report_One_Stat;

      procedure Report_All_Stats is new Iterate (Report_One_Stat);

      use Utilities;
   begin  -- Report_Stats
      if Stats_Level = None then
         return;
      end if;

      if Stats_Level >= Nulls_Only then
         if Format_Option /= None then
            -- if format_option = none, there were no messages, and the stats are output in CSVX
            -- we don't need the separator, it is better to have the header line first
            New_Line;
            Put_Line ("Rules usage statistics:");
         end if;
         case Format_Option is
            when Gnat | Source =>
               null;
            when CSV | CSVX  | None=>
               Put_Line ("Rule" and "Label" and "Check" and "Search" and "Count");
         end case;
         Report_All_Stats (Stats_Counters (Control_Kinds'First));
      end if;

      if Stats_Level >= General then
         New_Line;
         Put ("Issued messages: Errors = " & Integer_Img (Nb_Errors));
         Put (", Warnings = " & Integer_Img (Nb_Warnings));
         New_Line;
      end if;
   end Report_Stats;
end Framework.Reports;
