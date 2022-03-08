----------------------------------------------------------------------
--  Framework.Language.Commands - Package body                      --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2021.           --
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
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed,
  Ada.Wide_Text_IO;

-- Asis
with
  Asis.Exceptions,
  Asis.Implementation;

-- Adalog
with
  String_Matching,
  Units_List,
  Utilities;

-- Adacontrol
with
  Adactl_Version,
  Adactl_Options,
  Framework.Control_Manager,
  Framework.Interrupt,
  Framework.Language.Scanner,
  Framework.Reports,
  Framework.Ruler,
  Framework.Rules_Manager,
  Scope_Manager,
  Framework.String_Set,
  Framework.Variables.Shared_Types,
  Implementation_Options;

package body Framework.Language.Commands is
   use Utilities;

   --  Messages
   Copyright_1 : constant Wide_String := "Copyright (C) 2004-2022 Eurocontrol/Adalog and others.";
   Copyright_2 : constant Wide_String := "This software is covered by the GNU Modified General Public License.";
   Support     : constant Wide_String := "Commercial support is available, see https://www.adacontrol.fr";

   --  Output management
   Console_Name : constant Wide_String := "CONSOLE";

   Adactl_Output : Ada.Wide_Text_IO.File_Type;

   Seen_Files : Framework.String_Set.Set;

   ----------------------
   -- Help_On_Commands --
   ----------------------

   procedure Help_On_Commands is
      use Adactl_Options, Framework.Reports;
   begin
      if Action = Check or Rule_Error_Occurred then
         return;
      end if;

      User_Message ("Commands:");
      User_Message ("   Clear all|<rule name> ;");
      User_Message ("   Go;");
      User_Message ("   Help all|commands|generator|license|list|options|rules|version" &
                        "|variables {""<pattern>""}|<rule name> {,...} ;");
      User_Message ("   Inhibit <rule name> (<unit>{,<unit>});");
      User_Message ("   Message ""<message>"" [pause];");
      User_Message ("   Quit;");
      User_Message ("   Rule_file_off ""<pattern>"" all | <rule name> {,<rule name>}");
      User_Message ("   Set check_key|search_key ""<key>""");
      User_Message ("   Set format gnat|gnat_long|gnat_short|csv|csv_long|csv_short|csvx|csvx_long|csvx_short" &
                        "|source|source_long|source_short|none;");
      User_Message ("   Set output <output file>;");
      User_Message ("   Set statistics <level: 0 .." & Stats_Levels'Wide_Image (Stats_Levels'Last) & ">;");
      User_Message ("   Set trace <trace file>;");
      User_Message ("   Set debug|exit_on_error|ignore|verbose|warning|warning_as_error on|off ;");
      User_Message ("   Set <rule>.<variable> <value>");
      User_Message ("   Source <input file> ;");
      User_Message ("   [<label>:] <control> {, <control>} ;");
      User_Message ("Control:");
      User_Message ("   Search|Check|Count <rule name> [ ( <parameters> ) ]");
   end Help_On_Commands;

   -----------------------
   -- Help_On_Variables --
   -----------------------

   procedure Help_On_Variables (Pattern : Wide_String) is
      use Framework.Variables, String_Matching;
   begin
      User_Message ("Variables: ");
      for V : Unbounded_Wide_String of All_Variables loop
         if Match (To_Wide_String(V), Pattern, Ignore_Case => True) then
            Help_On_Variable (To_Wide_String (V));
         end if;
      end loop;
   end Help_On_Variables;


   ----------------
   -- Go_Command --
   ----------------

   Go_Count : Natural := 0;

   procedure Go_Command is
      use Ada.Exceptions, Ada.Wide_Text_IO;
      use Adactl_Options, Framework.Rules_Manager, Framework.Variables.Shared_Types;

      procedure Handle_Exception (Occur : Ada.Exceptions.Exception_Occurrence := Null_Occurrence) is
         use Asis.Exceptions, Ada.Characters.Handling;
         use Asis.Implementation;
         Phase : constant Wide_String := To_Title (Control_Phases'Wide_Image (Current_Phase));
      begin
         if Failure_Occurred and then Exception_Identity (Occur) = Scope_Manager.Scope_Manager_Failure'Identity then
            -- Presumably, a consequence of previous failure
            User_Message ("Internal error: unit " & Units_List.Current_Unit & " abandoned due to previous failure");
            -- Exit_Option.Value cannot be On, since the first failure would have stopped execution
            return;
         end if;

         Failure_Occurred := True;

         -- Clean-up:
         begin
            Ruler.Reset;
            Scope_Manager.Reset (Deactivate => False);
            Rules_Manager.Reset_All;
         exception
            when Other_Occur : others =>
               -- Sounds really bad here... warn, but preserve the original exception
               -- which is likely to be more interesting
               -- => Handle and forget this one.
               User_Message ("============= Recovery: "
                             & To_Wide_String (Exception_Name (Other_Occur))
                             & " raised during Reset =============");
         end;

         User_Message ("============= Phase: " & Phase & " =============");
         User_Message ("AdaCtl version: " & Adactl_Version
                       & " with " & ASIS_Implementor_Version);
         Reraise_Occurrence (Occur);

         -- If we are here, Occur = Null_Occurrence
         User_Message ("   In rule: " & Framework.Rules_Manager.Last_Rule);
         if Framework.Rules_Manager.Current_Phase = Processing then
            User_Message ("   For unit: " & Units_List.Current_Unit);
         end if;


      exception
         when Local_Occur : ASIS_Failed
            | ASIS_Inappropriate_Context
            | ASIS_Inappropriate_Container
            | ASIS_Inappropriate_Compilation_Unit
            | ASIS_Inappropriate_Element
            | ASIS_Inappropriate_Line
            | ASIS_Inappropriate_Line_Number
            =>
            User_Message ("ASIS error: " & To_Wide_String (Exception_Name (Local_Occur)));
            User_Message ("   In rule: " & Framework.Rules_Manager.Last_Rule);
            if Framework.Rules_Manager.Current_Phase = Processing then
               User_Message ("  For unit: " & Units_List.Current_Unit);
            end if;
            Asis_Exception_Messages;

            -- Propagate the exception only if Exit_Option set
            if Adactl_Options.Exit_Option.Value = On then
               raise;
            end if;

         when Local_Occur : others =>
            User_Message ("Internal error: " & To_Wide_String (Exception_Name (Local_Occur)));
            User_Message ("       In rule: " & Framework.Rules_Manager.Last_Rule);
            if Framework.Rules_Manager.Current_Phase = Processing then
               User_Message ("      For unit: " & Units_List.Current_Unit);
            end if;
            User_Message ("       Message: " & To_Wide_String (Exception_Message (Local_Occur)));

            -- Propagate the exception only if Exit_Option is set
            if Adactl_Options.Exit_Option.Value = On then
               raise;
            end if;
      end Handle_Exception;

      procedure Do_It is
         use Ada.Characters.Handling;
      begin
         Go_Count := Go_Count + 1;
         begin
            Framework.Rules_Manager.Current_Phase := Preparation;
            Framework.Rules_Manager.Prepare_All;
         exception
            when Utilities.User_Error =>
               -- Call to Parameter_Error while preparing => propagate silently
               raise;
            when Occur : others =>
               Handle_Exception (Occur);
               return;
         end;

         Units_List.Reset;
         Framework.Reports.Reset;
         Framework.Rules_Manager.Current_Phase := Processing;

         for I in Natural range 1 .. Units_List.Length loop
            begin
               Ruler.Process (Unit_Name  => Units_List.Current_Unit,
                              Unit_Pos   => I,
                              Spec_Only  => Adactl_Options.Spec_Option = On,
                              Go_Count   => Go_Count);
            exception
               when Utilities.User_Error =>
                  -- Call to Parameter_Error while traversing => propagate silently
                  raise;
               when Occur : Framework.Reports.Cancellation =>
                  User_Message ("Execution cancelled due to " & To_Wide_String (Exception_Message (Occur)));
                  exit;
               when Occur : others =>
                  Handle_Exception (Occur);
            end;

            Units_List.Skip;
         end loop;

         begin
            Framework.Rules_Manager.Current_Phase := Finalize;
            -- If run has been cancelled, messages from finalization will be ignored by Report
            Framework.Rules_Manager.Finalize_All;
         exception
               -- There should be no call to Parameter_Error here...
            when Occur : others =>
               Handle_Exception (Occur);
               return;
         end;
      end Do_It;
   begin  -- Go_Command
      if Action = Check or Rule_Error_Occurred then
         return;
      end if;

      if Utilities.Debug_Option then
         begin
            Framework.Interrupt.Run_Interruptable (Do_It'Access);
         exception
            when Framework.Interrupt.Interrupted =>
               Handle_Exception;
               if Adactl_Options.Exit_Option.Value = On then
                  raise;
               end if;
         end;
      else
         Do_It;
      end if;
      Framework.Reports.Report_Counts;

      Framework.Reports.Report_Stats;
      Rules_Manager.Report_Timings (Global_Report => False);

      if Is_Open (Adactl_Output) then
         Flush (Adactl_Output);
      end if;

      Scope_Manager.Reset (Deactivate => True);
      Framework.Rules_Manager.Current_Phase := Not_Started;
   end Go_Command;

   ------------------
   -- Help_Command --
   ------------------

   procedure Help_Command (On : in Wide_String) is
      use Ada.Strings, Ada.Strings.Wide_Fixed;
      use Asis.Implementation;
      use Adactl_Options, Implementation_Options, Framework.Rules_Manager;

      Upper_On : constant Wide_String := To_Upper (On);
   begin
      if Upper_On = "ALL" then
         Help_On_Rules (".*");

      elsif Upper_On = "COMMANDS" then
         Help_On_Commands;

      elsif Upper_On = "GENERATOR" then
         User_Message (Tree_Generator);

      elsif Upper_On = "LICENSE" then
         User_Message (Copyright_1);
         User_Message (Copyright_2);
         User_Message (Support);

      elsif Upper_On = "LIST" then
         Help_On_Names (Pretty => False);

      elsif Upper_On = "OPTIONS" then
         Help_On_Options;

      elsif Upper_On = "RULES" then
         Help_On_Names (Pretty => True);

      elsif Upper_On = "VARIABLES" or else Starts_With (Upper_On, "VARIABLES ") then
         Help_On_Variables (Trim (On (On'First + 10 .. On'Last), Both));

      elsif Upper_On = "VERSION" then
         User_Message ("ADACTL v. "
                         & Adactl_Version
                         & " with " & ASIS_Implementor_Version);

      else   -- Assume it is a rule name
         Help_On_Rules (Upper_On);
      end if;
   end Help_Command;

   ---------------------
   -- Inhibit_Command --
   ---------------------

   procedure Inhibit_Command (Rule_Name : in Wide_String) is
      use Framework.Control_Manager, Framework.Rules_Manager;
   begin
      if not Parameter_Exists then
         Parameter_Error ("Inhibit", "Missing unit names");
      end if;

      while Parameter_Exists loop
         declare
            Is_All : constant Boolean := Get_Modifier ("ALL", Default => False);
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Inhibit (Rule_Name, Entity, Is_All);
         exception
            when Already_In_Store =>
               Parameter_Error ("Inhibit", "Rule " & Rule_Name & " already inhibited for " & Image (Entity));
         end;
      end loop;
   end Inhibit_Command;

   ---------------------
   -- Message_Command --
   ---------------------

   procedure Message_Command (Message : in Wide_String; With_Pause : Boolean) is
      use Ada.Wide_Text_IO;
      use Adactl_Options;
   begin
      if Action = Check or Rule_Error_Occurred then
         return;
      end if;

      User_Message (Message);

      if With_Pause then
         Skip_Line (Standard_Input);
      end if;
   end Message_Command;

   ------------------------
   -- Set_Output_Command --
   ------------------------

   procedure Set_Output_Command (Output_File : Wide_String; Force_Overwrite : Boolean) is
      use Ada.Characters.Handling, Ada.Wide_Text_IO;
      use Adactl_Options, Framework.Reports, Framework.String_Set, Framework.Variables.Shared_Types;
   begin
      if Action = Check or Rule_Error_Occurred then
         return;
      end if;

      -- Note that the following sequence ensures that Current_Output is
      -- never a closed file.
      Set_Output (Standard_Output);
      if Is_Open (Adactl_Output) then
         Close (Adactl_Output);
      end if;

      if To_Upper (Output_File) = Console_Name then
         Utilities.Error_Is_Out         := True;
      else
         Utilities.Error_Is_Out := False;
         begin
            Open (Adactl_Output, In_File, To_String (Output_File));

            -- File exists
            Close (Adactl_Output);
            if Force_Overwrite
              or else (Adactl_Options.Overwrite_Option = On and not Is_Present (Seen_Files, Output_File))
            then
               Create (Adactl_Output, Out_File, To_String (Output_File));
               Framework.Reports.Just_Created := True;
            else
               Open (Adactl_Output, Append_File, To_String (Output_File));
               Framework.Reports.Just_Created := False;
            end if;
         exception
            when Name_Error =>
               -- File does not exist
               Framework.Reports.Just_Created := True;
               Create (Adactl_Output, Out_File, To_String (Output_File));
         end;

         Add (Seen_Files, Output_File);
         Set_Output (Adactl_Output);

         if Output_File'Length >= 4
           and then To_Upper (Output_File (Output_File'Last - 3 .. Output_File'Last)) = ".CSV"
         then
            Set_Output_Format ("CSV");
         end if;
      end if;
   exception
      when Name_Error =>
         Parameter_Error ("set output", "unable to create output file " & Output_File);
   end Set_Output_Command;

   -----------------------
   -- Set_Trace_Command --
   -----------------------

   procedure Set_Trace_Command (Trace_File : Wide_String) is
      use Adactl_Options;
   begin
      if Action = Check or Rule_Error_Occurred then
         return;
      end if;

      Utilities.Set_Trace (Trace_File);
   end Set_Trace_Command;

   --------------------
   -- Source_Command --
   --------------------

   procedure Source_Command (Name : Wide_String; Success : out Boolean) is
      use Ada.Wide_Text_IO, Framework.Language.Scanner;

      -- Note that these procedures are passed the value of Current_Input.
      -- This allows to restore it at the end, although it is limited.
      procedure Compile_File (File_Name      : Wide_String;
                              Previous_Input : File_Type)
      is
         use Ada.Characters.Handling;
         File : File_Type;
      begin
         if File_Name = "-" then
            -- By convention: treat Standard_Input as file (not interactive)
            Set_Input (Standard_Input);
         else
            begin
               Open (File, In_File, To_String (File_Name), Form => Implementation_Options.Form_Parameters);
            exception
               when Name_Error =>
                  -- retry with .aru extension
                  Open (File, In_File, To_String (File_Name) & ".aru", Form => Implementation_Options.Form_Parameters);
            end;
            Set_Input (File);
         end if;
         Set_Prompt ("");
         Start_Scan (From_String => False, Source => File_Name);

         Compile;

         Set_Input (Previous_Input);
         if File_Name /= "-" then
            Close (File);
         end if;

      exception
         when others =>
            if Is_Open (File) then
               Set_Input (Standard_Input);
               Close (File);
            end if;
            raise;
      end Compile_File;

      procedure Compile_Console (Previous_Input : File_Type) is
      begin
         Set_Input (Standard_Input);
         Set_Prompt ("Command");
         Start_Scan (From_String => False, Source => "Console");

         Compile;

         Set_Input (Previous_Input);
      end Compile_Console;

      Current_State : Scanner_State;
   begin   -- Source_Command
      Save_State (Current_State);

      if To_Upper (Name) = Console_Name then
         Compile_Console (Current_Input);
      else
         Compile_File (Clean_File_Name (Name), Current_Input);
      end if;

      Restore_State (Current_State);
      Success := True;
   exception
      when Name_Error =>
         Success := False;
         Restore_State (Current_State);
      when others =>
         Restore_State (Current_State);
         raise;
   end Source_Command;

begin  -- Framework.Language.Commands
   Utilities.Error_Is_Out := True;
end Framework.Language.Commands;
