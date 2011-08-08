----------------------------------------------------------------------
--  Framework.Language.Commands - Package body                      --
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
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed,
  Ada.Wide_Text_IO;

-- Asis
with
  Asis.Exceptions,
  Asis.Implementation;

-- Adalog
with
  Units_List,
  Utilities;

-- Adacontrol
with
  Adactl_Version,
  Adactl_Options,
  Framework.Language.Scanner,
  Framework.Reports,
  Framework.Ruler,
  Framework.Rules_Manager,
  Framework.Scope_Manager,
  Framework.String_Set,
  Implementation_Options;

package body Framework.Language.Commands is
   use Utilities;

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
      User_Message ("   Help all|commands|license|list|options|rules|version|<rule name> {,...};");
      User_Message ("   Inhibit <rule name> (<unit>{,<unit>});");
      User_Message ("   Message ""<message>"" [pause];");
      User_Message ("   Quit;");
      User_Message ("   Set check_key|search_key ""<key>""");
      User_Message ("   Set format gnat|gnat_short|csv|csv_short|csvx|csvx_short|source|source_short|none ;");
      User_Message ("   Set output <output file>;");
      User_Message ("   Set statistics <level: 0 .. "
                      & Integer_Img (Stats_Levels'Pos (Stats_Levels'Last))
                      & ">;");
      User_Message ("   Set trace <trace file>;");
      User_Message ("   Set verbose|debug|ignore|warning|warning_as_error on|off ;");
      User_Message ("   Source <input file> ;");
      User_Message ("   [<label>:] <control> {, <control>} ;");
      User_Message ("Control:");
      User_Message ("   Search|Check|Count <rule name> [ ( <parameters> ) ]");
   end Help_On_Commands;

   ----------------
   -- Go_Command --
   ----------------

   Go_Count : Natural := 0;

   procedure Go_Command is

      procedure Handle_Exception (Phase     : Wide_String;
                                  Occur     : Ada.Exceptions.Exception_Occurrence;
                                  Unit_Name : Wide_String := "")
      is
         use Ada.Exceptions, Asis.Exceptions, Ada.Characters.Handling;
      begin
         Failure_Occurred := True;

         -- Clean-up:
         Ruler.Reset;
         Framework.Scope_Manager.Reset (Deactivate => False);

         User_Message ("============= Phase: " & Phase & " =============");
         Reraise_Occurrence (Occur);
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
            User_Message ("In rule: " & Framework.Rules_Manager.Last_Rule);
            if Unit_Name /= "" then
               User_Message ("For unit: " & Unit_Name);
            end if;
            Asis_Exception_Messages;

            -- Propagate the exception only if Exit_Option set
            if Adactl_Options.Exit_Option then
               raise;
            end if;

         when Local_Occur : others =>
            User_Message ("Internal error: " & To_Wide_String (Exception_Name (Local_Occur)));
            User_Message ("       In rule: " & Framework.Rules_Manager.Last_Rule);
            if Unit_Name /= "" then
               User_Message ("      For unit: " & Unit_Name);
            end if;
            User_Message ("       Message: " & To_Wide_String (Exception_Message (Local_Occur)));

            -- Propagate the exception only if Exit_Option set
            if Adactl_Options.Exit_Option then
               raise;
            end if;
      end Handle_Exception;

      use Ada.Exceptions, Ada.Characters.Handling, Ada.Wide_Text_IO, Adactl_Options;
   begin  -- Go_Command
      if Action = Check or Rule_Error_Occurred then
         return;
      end if;

      Go_Count := Go_Count + 1;
      begin
         Framework.Rules_Manager.Prepare_All;
      exception
         when Utilities.User_Error =>
            -- Call to Parameter_Error while preparing => propagate silently
            raise;
         when Occur : others =>
            Handle_Exception ("Preparation", Occur);
            return;
      end;

      Units_List.Reset;
      Framework.Reports.Reset;

      for I in Natural range 1 .. Units_List.Length loop
         begin
            Ruler.Process(Unit_Name  => Units_List.Current_Unit,
                          Unit_Pos   => I,
                          Spec_Only  => Adactl_Options.Spec_Option,
                          Go_Count   => Go_Count);
         exception
            when Utilities.User_Error =>
               -- Call to Parameter_Error while traversing => propagate silently
               raise;
            when Occur : Framework.Reports.Cancellation =>
               User_Message ("Execution cancelled due to " & To_Wide_String (Exception_Message (Occur)));
               exit;
            when Occur : others =>
               Handle_Exception ("Processing", Occur, Units_List.Current_Unit);
         end;

         Units_List.Skip;
      end loop;

      begin
         -- If run has been cancelled, messages from finalization will be ignored by Report
         Framework.Rules_Manager.Finalize_All;
      exception
         -- There should be no call to Parameter_Error here...
         when Occur : others =>
            Handle_Exception ("Finalize", Occur);
            return;
      end;

      Framework.Reports.Report_Counts;

      Framework.Reports.Report_Stats;
      Rules_Manager.Report_Timings;

      if Is_Open (Adactl_Output) then
         Flush (Adactl_Output);
      end if;

      Framework.Scope_Manager.Reset (Deactivate => True);
   end Go_Command;

   ------------------
   -- Help_Command --
   ------------------

   procedure Help_Command (On : in Wide_String) is
      use Asis.Implementation;
      use Adactl_Options, Framework.Rules_Manager;

      Upper_On : constant Wide_String := To_Upper (On);
   begin
      if Upper_On = "ALL" then
         Help_On_All_Rules;

      elsif Upper_On = "COMMANDS" then
         Help_On_Commands;

      elsif Upper_On = "LICENSE" then
         User_Message ("Copyright (C) 2004-2008 Eurocontrol/Adalog and others.");
         User_Message ("This software is covered by the GNU Modified General Public License.");

      elsif Upper_On = "LIST" then
         Help_On_Names (Pretty => False);

      elsif Upper_On = "OPTIONS" then
         Help_On_Options;

      elsif Upper_On = "RULES" then
         Help_On_Names (Pretty => True);

      elsif Upper_On = "VERSION" then
         User_Message ("ADACTL v. "
                         & Adactl_Version
                         & " with " & ASIS_Implementor_Version);
         pragma Warnings (On);

      else   -- Assume it is a rule name
         Help_On_Rule (Upper_On);
      end if;
   end Help_Command;

   ---------------------
   -- Inhibit_Command --
   ---------------------

   procedure Inhibit_Command (Rule_Name : in Wide_String) is
      use Framework, Framework.Language;
   begin
      if not Parameter_Exists then
         Parameter_Error ("Inhibit", "Missing unit names");
      end if;

      while Parameter_Exists loop
         declare
            Is_All : constant Boolean := Get_Modifier ("ALL", Default => False);
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            -- Check that inhibition is not already specified
            -- (otherwise, the suspend/resume mechanism won't work since
            -- suspensions are not stacked)
            if Rule_Name = "ALL" then
               -- Can be given only once for each unit, and is incompatible with a specific rule name
               --   => Associate in non additive mode
               --   => will raise Parameter_Error if already specified for any rule
               Associate (Inhibited,
                          Entity,
                          Inhibited_Rule'(Rule_Name =>To_Unbounded_Wide_String (Rule_Name), Is_Banned => Is_All),
                          Additive => False);
            else
               -- Check that it is not already specified for "ALL". If it is, it is the only association,
               -- per previous test
               declare
                  Cont : constant Root_Context'Class := Association (Inhibited, Entity);
               begin
                  if Cont /= No_Matching_Context and then Inhibited_Rule (Cont).Rule_Name = "ALL" then
                     raise Already_In_Store;
                  end if;
               end;

               Associate (Inhibited,
                          Entity,
                          Inhibited_Rule'(Rule_Name =>To_Unbounded_Wide_String (Rule_Name), Is_Banned => Is_All),
                          Additive => True);
            end if;
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
         Skip_Line;
      end if;
   end Message_Command;

   ------------------------
   -- Set_Format_Command --
   ------------------------

   procedure Set_Format_Command (Format : Wide_String) is
      use Framework.Reports, Ada.Strings.Wide_Fixed;
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

      Format_Option := Output_Format'Wide_Value (Format (Format'First .. Sep_Pos - 1));
   exception
      when Constraint_Error =>
         Parameter_Error ("Set format",
                          """Gnat"", ""Gnat_Short"", "
                          & """CSV"", ""CSV_Short"", "
                          & """CSVX"", ""CSVX_Short"", "
                          & """Source"", ""Source_Short"", "
                          & """none"" "
                          & "expected for format");
   end Set_Format_Command;

   ------------------------
   -- Set_Output_Command --
   ------------------------

  procedure Set_Output_Command (Output_File : Wide_String) is
      use Ada.Characters.Handling, Ada.Wide_Text_IO;
      use Adactl_Options, Framework.String_Set;
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
         Framework.Reports.Just_Created := False;
      else
         Utilities.Error_Is_Out := False;
         begin
            Open (Adactl_Output, In_File, To_String (Output_File));

            -- File exists
            Close (Adactl_Output);
            if Adactl_Options.Overwrite_Option
              and not Is_Present (Seen_Files, Output_File)
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

   procedure Source_Command (Name : Wide_String) is
      use Ada.Wide_Text_IO, Framework.Language.Scanner;

      -- Note that these procedures are passed the value of Current_Input.
      -- This allows to restore it at the end, although it is limited.
      procedure Compile_File (File_Name      : Wide_String;
                              Previous_Input : File_Type) is
         use Ada.Characters.Handling;
         File      : File_Type;
      begin
         if File_Name = "-" then
            -- By convention: treat Standard_Input as file (not interactive)
            Set_Input (Standard_Input);
         else
            Open (File, In_File, To_String (File_Name), Form => Implementation_Options.Form_Parameters);
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
         Compile_File (Name, Current_Input);
      end if;

      Restore_State (Current_State);
   exception
      when others =>
         Restore_State (Current_State);
         raise;
   end Source_Command;

begin  -- Framework.Language.Commands
   Utilities.Error_Is_Out := True;
end Framework.Language.Commands;
