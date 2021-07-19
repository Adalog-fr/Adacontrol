----------------------------------------------------------------------
--  Framework.Reports - Package body                                --
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
  Ada.Exceptions,
  Ada.Calendar,
  Ada.Calendar.Formatting,
  Ada.Calendar.Time_Zones,
  Ada.Characters.Handling,
  Ada.Characters.Latin_1,
  Ada.Directories,
  Ada.Strings.Wide_Unbounded,
  Ada.Strings.Wide_Fixed,
  Ada.Wide_Characters.Handling,
  Ada.Wide_Text_IO;

-- ASIS
with
   Asis.Text;

-- Adalog
with
  Binary_Map,
  Linear_Queue,
  Units_List,
  Utilities;

-- Adactl
with
  Framework.Variables.Shared_Types,
  Framework.Rules_Manager,
  Implementation_Options,
  Adactl_Options;

package body Framework.Reports is
   use Framework.Variables, Framework.Variables.Shared_Types;

   Std_Message_Format_Headers : constant array (Output_Format) of Unbounded_Wide_String :=
     (Source => To_Unbounded_Wide_String (""),
      Gnat   => To_Unbounded_Wide_String (""),
      CSV    => To_Unbounded_Wide_String ("""file"",""line"",""column"",""key"",""label"",""rule"",""message"""),
      CSVX   => To_Unbounded_Wide_String ("""file"";""line"";""column"";""key"";""label"";""rule"";""message"""),
      None   => To_Unbounded_Wide_String (""));

   Std_Message_Formats : constant array (Output_Format) of Unbounded_Wide_String :=
     (Source => To_Unbounded_Wide_String ("%s%n%! %k: %d: %m"),
      Gnat   => To_Unbounded_Wide_String ("%[%f:%l:%c: %]%k: %d: %m"),
      CSV    => To_Unbounded_Wide_String ("""%""f"",""%""l"",""%""c"",""%""k"",""%""t"",""%""r"",""%""m"""),
      CSVX   => To_Unbounded_Wide_String ("""%""f"";""%""l"";""%""c"";""%""k"";""%""t"";""%""r"";""%""m"""),
      None   => To_Unbounded_Wide_String (""));

   Std_Count_Format_Headers : constant array (Output_Format) of Unbounded_Wide_String :=
     (Source => To_Unbounded_Wide_String ("Counts summary:"),
      Gnat   => To_Unbounded_Wide_String ("Counts summary:"),
      CSV    => To_Unbounded_Wide_String ("Rule,Counts"),
      CSVX   => To_Unbounded_Wide_String ("Rule;Counts"),
      None   => To_Unbounded_Wide_String (""));

   Std_Count_Formats        : constant array (Output_Format) of Unbounded_Wide_String :=
     (Source => To_Unbounded_Wide_String ("%d: %m"),
      Gnat   => To_Unbounded_Wide_String ("%d: %m"),
      CSV    => To_Unbounded_Wide_String ("""%""d"", ""%m"""),
      CSVX   => To_Unbounded_Wide_String ("""%""d""; ""%m"""),
      None   => To_Unbounded_Wide_String (""));

   Wide_LF : constant Wide_Character := Ada.Characters.Handling.To_Wide_Character (Ada.Characters.Latin_1.LF);

   package Output_Format_Type is
      type Object is new Variables.Object with
         record
            Needs_Source : Boolean := False;
            Value        : Unbounded_Wide_String;
         end record;
      overriding procedure Set         (Variable : in out Output_Format_Type.Object; To : Wide_String);
      overriding function  Value_Image (Variable : in     Output_Format_Type.Object) return Wide_String;
      overriding function  All_Values  (Variable : in     Output_Format_Type.Object) return Wide_String;
   end Output_Format_Type;

   --
   -- User settable variables
   --
   package Fixes_Level_Type is new Framework.Variables.Discrete_Type (Fixes_Kinds);

   Active_Warning_Option   : aliased Switch_Type.Object       := (Value => On);
   Warning_As_Error_Option : aliased Switch_Type.Object       := (Value => Off);
   Max_Errors              : aliased Natural_Type.Object      := (Value => Natural'Last);
   Max_Messages            : aliased Natural_Type.Object      := (Value => Natural'Last);
   Stats_Level             : aliased Stats_Levels_Type.Object := (Value => No_Stats);
   Fix_Level               : aliased Fixes_Level_Type.Object  := (Value => None);

   Check_Message           : aliased String_Type.Object := (Value => To_Unbounded_Wide_String ("Error"));
   Search_Message          : aliased String_Type.Object := (Value => To_Unbounded_Wide_String ("Found"));
   Adactl_Tag1             : aliased String_Type.Object := (Value => To_Unbounded_Wide_String ("##"));
   Adactl_Tag2             : aliased String_Type.Object := (Value => To_Unbounded_Wide_String ("##"));

   Message_Format          : aliased Output_Format_Type.Object := (Needs_Source => False,
                                                                   Value        => Std_Message_Formats (Gnat));
   Message_Format_Header   : aliased String_Type.Object := (Value => Std_Message_Format_Headers (Gnat));
   Count_Format            : aliased String_Type.Object := (Value => Std_Count_Formats          (Gnat));
   Count_Format_Header     : aliased String_Type.Object := (Value => Std_Count_Format_Headers   (Gnat));

   Sequence_Value          : aliased Natural_Type.Object := (Value => 0);
   --
   -- Local variables
   --

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


   -- State of Enabling/Disabling messages
   Report_Enabled : Boolean;
   Last_Control   : Control_Kinds;


   --
   -- Local utilities
   --

   -------------------
   -- Multiline_Put --
   -------------------

   procedure Multiline_Put (S : Wide_String) is
      use Ada.Wide_Text_IO;
   begin
      for Cur_Char : Wide_Character of S loop
         if Cur_Char = Wide_LF then
            New_Line;
         else
            Put (Cur_Char);
         end if;
      end loop;
   end Multiline_Put;

   -----------
   -- Image --
   -----------
   type Time_Part is (Day, Hour);

   function Image (T : Ada.Calendar.Time; Part : Time_Part) return Wide_String is
      use Ada.Calendar, Ada.Calendar.Time_Zones, Ada.Characters.Handling;
   -- format of the image of a date is (9.6.1 (82/2)):
   -- YYYY-MM-DD HH:MM:SS
   -- 0        1 1      1
   -- 1        0 2      9
   begin
      case Part is
         when Day =>
            return To_Wide_String (Formatting.Image (T, Time_Zone => UTC_Time_Offset (T))) (01 .. 10);
         when Hour =>
            return To_Wide_String (Formatting.Image (T, Time_Zone => UTC_Time_Offset (T))) (12 .. 19);
      end case;
   end Image;

   --------------------
   -- Expand_Pattern --
   --------------------

   generic
      with function Translate (Key : Wide_Character) return Wide_String;
   function Expand_Pattern (Pattern : in Wide_String) return Wide_String;

   function Expand_Pattern (Pattern : in Wide_String) return Wide_String is
      Index : Natural := Pattern'First;

      function Parse_It return Wide_String is
         use Utilities;
         type Format_States is (Normal, Percent, Percent_Quote, Percent_Back_Slash);
         State           : Format_States;
         Cur_Char        : Wide_Character;
         Result          : Unbounded_Wide_String;
         Non_Empty_Subst : Boolean := False;
      begin
         State := Normal;
         loop
            Cur_Char := Pattern (Index);
            case State is
               when Normal =>
                  if Cur_Char = '%' then
                     State := Percent;
                  else
                     Append (Result, Cur_Char);
                  end if;
               when Percent | Percent_Quote | Percent_Back_Slash =>
                  case Cur_Char is
                     when '%' => -- %
                        Append (Result, '%');
                        State := Normal;
                     when '"' =>
                        case State is
                           when Normal => -- impossible
                              Failure ("Expand pattern: normal state with %\");
                           when Percent =>
                              State := Percent_Quote;
                           when Percent_Quote => -- case of %""
                              Append (Result, "%""""");
                              State := Normal;
                           when Percent_Back_Slash => -- case of %\"
                              Append (Result, "%\""");
                              State := Normal;
                        end case;
                     when '\' =>
                        case State is
                           when Normal => -- impossible
                              Failure ("Expand pattern: normal state with %\");
                           when Percent =>
                              State := Percent_Back_Slash;
                           when Percent_Quote => -- case of %"\
                              Append (Result, "%""\");
                           when Percent_Back_Slash => -- case of %\\
                                Append (Result, "%\\");
                              State := Normal;
                        end case;
                     when '[' =>
                        Index := Index + 1;
                        declare
                           Temp : constant Wide_String := Parse_It;
                        begin
                           if Temp /= "" then
                              Non_Empty_Subst := True;
                              Append (Result, Temp);
                           end if;
                           State := Normal;
                        end;
                     when ']' =>
                        if Non_Empty_Subst then
                           return To_Wide_String (Result);
                        else
                           return "";
                        end if;
                     when others =>
                        declare
                           Temp : constant Wide_String := Translate (Cur_Char);
                        begin
                           Non_Empty_Subst := Non_Empty_Subst or else (Temp /= "" and then Temp /= (1 => Wide_LF));
                           if Temp /= "" then
                              case State is
                                 when Normal => -- impossible
                                    Failure ("Expand pattern: normal state after percent");
                                 when Percent =>
                                    Append (Result, Temp);
                                 when Percent_Quote =>
                                    Append (Result, Quote (Temp, Escape_With => '"', Add_Outer => False));
                                 when Percent_Back_Slash =>
                                    Append (Result, Quote (Temp, Escape_With => '\', Add_Outer => False));
                              end case;
                           end if;
                           State := Normal;
                        end;
                  end case;
            end case;

            exit when Index = Pattern'Last;
            Index := Index + 1;
         end loop;
         case State is
            when Normal =>
               null;
            when Percent => -- Last character of format is a %
               Append (Result, '%');
            when Percent_Quote => -- Last 2 characters of format are %"
               Append (Result, "%""");
            when Percent_Back_Slash => -- Last 2 characters of format are %\
               Append (Result, "%\");
         end case;

         return To_Wide_String (Result);
      end Parse_It;
   begin  -- Expand_Pattern
      return Parse_It;
   end Expand_Pattern;


   --
   -- Exported operations
   --

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
                     Active      : in out Boolean)
   is
      use Utilities, Ada.Strings.Wide_Fixed;

      Mark1 : constant Wide_String := "--" & To_Wide_String (Adactl_Tag1.Value);
      Mark2 : constant Wide_String := To_Wide_String (Adactl_Tag2.Value);
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

   -----------------------
   -- Set_Output_Format --
   -----------------------

   procedure Set_Output_Format (Value : Wide_String) is
      use Ada.Wide_Characters.Handling, Ada.Strings.Wide_Fixed;

      Sep_Pos : Natural := Index (Value, "_");
      Format  : Output_Format;
   begin
      -- Is a defined output format
      if Sep_Pos = 0 then
         Sep_Pos := Value'Last + 1;
      elsif To_Upper (Value (Sep_Pos .. Value'Last)) = "_SHORT" then
         Set_Variable ("LONG_FILE_NAME", "Off");
      elsif To_Upper (Value (Sep_Pos .. Value'Last)) = "_LONG" then
         Set_Variable ("LONG_FILE_NAME", "On");
      else
         raise Constraint_Error;
      end if;
      Format := Output_Format'Wide_Value (Value (Value'First .. Sep_Pos - 1));

      Set_Variable ("FORMAT",        Val => To_Wide_String (Std_Message_Formats        (Format)));
      Set_Variable ("FORMAT_HEADER", Val => To_Wide_String (Std_Message_Format_Headers (Format)));
   end Set_Output_Format;

   ------------------------
   -- Output_Format_Type --
   ------------------------

   package body Output_Format_Type is
      procedure Set (Variable : in out Output_Format_Type.Object; To : Wide_String) is
         use Ada.Strings.Wide_Fixed;
         use Ada.Wide_Characters.Handling;
      begin
         Variable := (Value => To_Unbounded_Wide_String (To), Needs_Source => Index (To_Upper (To), "%s") > 0);
      end Set;

      function Value_Image (Variable : in Output_Format_Type.Object) return Wide_String is
         use Utilities;
      begin
         for F in Std_Message_Formats'Range loop
            if Message_Format.Value = Std_Message_Formats (F) then
               return Output_Format'Wide_Image (F);
            end if;
         end loop;

         return Quote (To_Wide_String (Variable.Value));
      end Value_Image;

      function  All_Values  (Variable : in Output_Format_Type.Object) return Wide_String is
         pragma Unreferenced (Variable);
         use Utilities;
         Buffer : Unbounded_Wide_String;
      begin
         for V in Output_Format range Output_Format'First .. Output_Format'Pred (None) loop
            Append (Buffer, To_Title (Output_Format'Wide_Image (V)) & ", ");
            Append (Buffer, To_Title (Output_Format'Wide_Image (V)) & "_Short, ");
            Append (Buffer, To_Title (Output_Format'Wide_Image (V)) & "_Long, ");
         end loop;
         return '('
                & To_Wide_String (Buffer)
                & To_Title (Output_Format'Wide_Image (None))
                & ", or ""<custom format>"""
                & ')';
      end All_Values;
   end Output_Format_Type;

   ----------------
   -- Raw_Report --
   ----------------

   procedure Raw_Report (Message : Wide_String) is
      use Ada.Wide_Text_IO;
   begin
      Put_Line (Message);
   end Raw_Report;

   --------------------
   -- Generate_Fixes --
   --------------------

   function Generate_Fixes return Boolean is
   begin
      return Report_Enabled and Last_Control <= Fix_Level.Value;
   end Generate_Fixes;

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

      Defaulted_Label : constant Wide_String := Choose (Ctl_Label, Otherwise => Rule_Id);
      Line            : Wide_String (1..1024);
      Line_Last       : Natural := 0;

      procedure Issue_Message (Kind_Message : Wide_String) is
         use Ada.Wide_Text_IO;

         function Message_Translate (Key : Wide_Character) return Wide_String is
            use Ada.Calendar, Ada.Characters.Handling, Ada.Strings.Wide_Fixed, Ada.Directories;
         begin
            case Key is
               when 'c' => -- Column
                  if Loc = Null_Location then
                     return "";
                  else
                     return ASIS_Integer_Img (Get_First_Column (Loc));
                  end if;
               when 'd' => -- Tag (defaulted)
                  return Defaulted_Label;
               when 'D' =>
                  return Image (Clock, Day);
               when 'E' =>
                  return Integer_Img (Error_Count);
               when 'f' => -- File
                  if Loc = Null_Location then
                     return "";
                  elsif Is_Long_File_Name then
                     return Get_File_Name (Loc);
                  else
                     return To_Wide_String (Simple_Name (To_String (Get_File_Name (Loc))));
                  end if;
               when 'k' => -- Kind
                  return Kind_Message;
               when 'l' => -- Line
                  if Loc = Null_Location then
                     return "";
                  else
                     return ASIS_Integer_Img (Get_First_Line (Loc));
                  end if;
               when 'm' => -- Message
                  return Msg;
               when 'n' => -- New line
                  return (1 => Wide_LF);
               when 'r' => -- Rule
                  return Rule_Id;
               when 's' => -- Source
                  return (if Loc = Null_Location then "" else Image (Loc) & ": " )
                          & Line (1 .. Line_Last);
               when 'S' =>
                  Sequence_Value.Value := Sequence_Value.Value + 1;
                  return Integer_Img (Sequence_Value.Value);
               when 't' => -- Tag
                  return Ctl_Label;
               when 'T' =>
                  return Image (Clock, Hour);
               when 'W' =>
                  return Integer_Img (Warning_Count);
               when '!' => -- Indicator
                    return (if Loc = Null_Location then "" else Image (Loc) & ": " )
                            & (Get_First_Column (Loc) - 1) * ' ' & '!';
               when others =>
                  return (1 => Key);
            end case;
         end Message_Translate;

         function Expand_Message is new Expand_Pattern (Message_Translate);
      begin   -- Issue_Message
         if Message_Format.Value = "" then
            return;
         end if;

         -- Output header if any
         if Just_Created and then Message_Format_Header.Value /= "" then    -- Output a header
            Multiline_Put (Expand_Message (To_Wide_String (Message_Format_Header.Value)));
            New_Line;
         end if;

         -- Output message
         Multiline_Put (Expand_Message (To_Wide_String (Message_Format.Value)));
         New_Line;
         Just_Created := False;
      end Issue_Message;

      use Ada.Exceptions;
      use Counters, False_Positive_Map;
   begin  -- Report
      if Error_Count = Max_Errors.Value or Error_Count + Warning_Count = Max_Messages.Value then
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
      Report_Enabled := Rules_Manager.Initial_Disabling_State (Rule_Id, Get_File_Name (Loc));
      if Message_Format.Needs_Source or Ignore_Option.Value /= On then
         declare
            use Ada.Characters.Handling, Ada.Wide_Text_IO;
            Source_File : File_Type;
         begin
            Open (Source_File,
                  In_File,
                  To_String (Get_File_Name (Loc)),
                  Form => Implementation_Options.Form_Parameters);

            for I in Asis.Text.Line_Number range 1 .. Get_First_Line (Loc) - 1 loop
               Get_Line (Source_File, Line, Line_Last);
               if Ignore_Option.Value /= On then
                  Update (Rule_Id,
                          Ctl_Label,
                          Line (Line'First .. Line_Last),
                          Single_Line => False,
                          Active      => Report_Enabled);
               end if;
            end loop;

            Get_Line (Source_File, Line, Line_Last);
            if Ignore_Option.Value /= On then
               Update (Rule_Id,
                       Ctl_Label,
                       Line (Line'First .. Line_Last),
                       Single_Line => True,
                       Active      => Report_Enabled);
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
                        & Get_File_Name (Loc) & ", line " & ASIS_Integer_Img (Get_First_Line (Loc)));
         end;
      end if;

      -- Here, Line is the good source line

      if Report_Enabled xor Ignore_Option.Value = Inverted then
         case Ctl_Kind is
            when Check =>
               Error_Count := Error_Count + 1;

               Issue_Message (To_Wide_String (Check_Message.Value));
               Last_Control := Check;
            when Search =>
               if Warning_As_Error_Option.Value = On then
                  Error_Count := Error_Count + 1;
               else
                  Warning_Count := Warning_Count + 1;
               end if;

               if Warning_As_Error_Option.Value = On or else Active_Warning_Option.Value = On then
                  Issue_Message (To_Wide_String (Search_Message.Value));
                  Last_Control := Search;
               end if;
            when Count =>
               Add (Rule_Counter,
                    To_Unbounded_Wide_String (Defaulted_Label),
                    Fetch (Rule_Counter, To_Unbounded_Wide_String (Defaulted_Label)) + 1);
         end case;

         if Stats_Level.Value >= Nulls_Only then
            declare
               Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Rule_Id
                                                                                   & Choose (Ctl_Label = "",
                                                                                             "",
                                                                                             "." & Ctl_Label));
            begin
               Add (Stats_Counters (Ctl_Kind), Key, Fetch (Stats_Counters (Ctl_Kind), Key, Default_Value => 0) + 1);
            end;
         end if;

         if Error_Count = Max_Errors.Value then
            Raise_Exception (Cancellation'Identity, Message => "too many errors");
         elsif Error_Count + Warning_Count = Max_Messages.Value then
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
         -- The call to Count must be /before/ the call to check/search for the case where
         -- the call to Report is followed by fixes
         if Basic_Context.With_Count then
            Report (Rule_Id,
                    To_Wide_String (Basic_Context.Count_Label),
                    Count,
                    Loc,
                    Msg);
         end if;

         Report (Rule_Id,
                 To_Wide_String (Basic_Context.Ctl_Label),
                 Basic_Context.Ctl_Kind,
                 Loc,
                 Msg);

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
         if Stats_Level.Value >= Nulls_Only then
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
      Uncheckable_Used     (Risk) := True;
      Uncheckable_Controls (Risk) := Ctl_Kind;
      Uncheckable_Labels   (Risk) := To_Unbounded_Wide_String (Label);
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
      use Counters;

      First_Time : Boolean := True;

      procedure Report_One_Count (Tag : in Unbounded_Wide_String; Counter_Value : in out Natural) is
         use Ada.Wide_Text_IO;

         function Count_Translate (Key : Wide_Character) return Wide_String is
            use Ada.Calendar;
            use Utilities;
         begin
            case Key is
               when 'd' => -- Defaulted_Tag
                  return To_Wide_String (Tag);
               when 'D' =>
                  return Image (Clock, Day);
               when 'E' =>
                  return Integer_Img (Error_Count);
               when 'm' => -- Message
                  return Integer_Img (Counter_Value);
               when 'n' => -- New line
                  return (1 => Wide_LF);
               when 'S' =>
                  Sequence_Value.Value := Sequence_Value.Value + 1;
                  return Integer_Img (Sequence_Value.Value);
               when 'T' =>
                  return Image (Clock, Hour);
               when 'W' =>
                  return Integer_Img (Warning_Count);
               when others =>
                  return (1 => Key);
            end case;
         end Count_Translate;

         function Expand_Count is new Expand_Pattern (Count_Translate);

      begin  -- Report_One_Count
         if First_Time then
            if Count_Format_Header.Value /= "" then    -- Output a header
               New_Line;
               Multiline_Put (Expand_Count (To_Wide_String (Count_Format_Header.Value)));
               New_Line;
            end if;
            First_Time := False;
         end if;

         Multiline_Put (Expand_Count (To_Wide_String (Count_Format.Value)));
         New_Line;
      end Report_One_Count;

      procedure Report_All_Counts is new Iterate (Report_One_Count);

   begin -- Report_Counts
      if Is_Empty (Rule_Counter) or Count_Format.Value = "" then
         return;
      end if;

      Report_All_Counts (Rule_Counter);
   end Report_Counts;

   ---------------
   -- Clear_All --
   ---------------

   procedure Clear_All is
      use Counters;
   begin
      Clear (Rule_Counter);
      for Ctr : Counters.Map of Stats_Counters loop
         Clear (Ctr);
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

      for Ctr : Counters.Map of Stats_Counters loop
         Reset (Ctr);
      end loop;
   end Reset;


   ----------------
   -- Init_Stats --
   ----------------

   procedure Init_Stats (Rule, Label : Wide_String) is
      use Counters, Utilities;
      Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Rule & Choose (Label = "", "", "." & Label));
   begin
      for Ctr : Counters.Map of Stats_Counters loop
         Add (Ctr, Key, 0);
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

      for Ctr : Counters.Map of Stats_Counters loop
         Check_Delete (Ctr);
      end loop;
   end Clear;


   ------------------
   -- Report_Stats --
   ------------------

   procedure Report_Stats is
      use Counters, Ada.Wide_Text_IO;

      procedure Report_One_Stat (Key : in Unbounded_Wide_String; Counter_Value : in out Natural) is
         use Utilities;
         Wide_Key        : constant Wide_String := To_Wide_String (Key);
         Triggered_Count :          Natural     := Counter_Value;
      begin
         for Ctr : Counters.Map of Stats_Counters (Control_Kinds'Succ (Control_Kinds'First) .. Control_Kinds'Last) loop
            Triggered_Count := Triggered_Count + Fetch (Ctr, Key, Default_Value => 0);
         end loop;

         if Triggered_Count = 0 or else Stats_Level.Value = Full then
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
            New_Line;
         end if;
      end Report_One_Stat;

      procedure Report_All_Stats is new Iterate (Report_One_Stat);

      use Utilities;
   begin  -- Report_Stats
      if Stats_Level.Value = No_Stats then
         return;
      end if;

      if Stats_Level.Value >= Nulls_Only then
         if Message_Format.Value /= "" then
            -- if format_option = none, there were no messages, and the stats are output in CSVX
            -- we don't need the separator, it is better to have the header line first
            New_Line;
            Put_Line ("Rules usage statistics:");
         end if;

         Report_All_Stats (Stats_Counters (Control_Kinds'First));
      end if;

      New_Line;
      Put ("Units processed: " & Integer_Img (Units_List.Length));
      New_Line;
      Put ("Lines processed: " & Integer_Img (Total_Lines));
      New_Line;
      Put ("Issued messages: Errors = " & Integer_Img (Nb_Errors));
      Put (", Warnings = " & Integer_Img (Nb_Warnings));
      New_Line;
   end Report_Stats;

   --------------------
   -- Report_Timings --
   --------------------

   procedure Report_Timings (Rule, Duration, Percent_Duration : Wide_String) is
      use Utilities;
   begin
      User_Message (Rule & ": ", Stay_On_Line => True);
      User_Message (Duration,    Stay_On_Line => True);
      User_Message (" (" & Percent_Duration & "%)");
   end Report_Timings;

begin  -- Framework.Reports
   Register (Check_Message'Access,           Variable_Name => "CHECK_KEY");
   Register (Count_Format'Access,            Variable_Name => "COUNT_FORMAT");
   Register (Count_Format_Header'Access,     Variable_Name => "COUNT_FORMAT_HEADER");
   Register (Fix_Level'Access,               Variable_Name => "FIXES_GEN");
   Register (Message_Format'Access,          Variable_Name => "FORMAT");
   Register (Message_Format_Header'Access,   Variable_Name => "FORMAT_HEADER");
   Register (Max_Errors'Access,              Variable_Name => "MAX_ERRORS");
   Register (Max_Messages'Access,            Variable_Name => "MAX_MESSAGES");
   Register (Search_Message'Access,          Variable_Name => "SEARCH_KEY");
   Register (Sequence_Value'Access,          Variable_Name => "SEQUENCE_VALUE");
   Register (Stats_Level'Access,             Variable_Name => "STATISTICS");
   Register (Adactl_Tag1'Access,             Variable_Name => "TAG1");
   Register (Adactl_Tag2'Access,             Variable_Name => "TAG2");
   Register (Active_Warning_Option'Access,   Variable_Name => "WARNING");
   Register (Warning_As_Error_Option'Access, Variable_Name => "WARNING_AS_ERROR");
end Framework.Reports;
