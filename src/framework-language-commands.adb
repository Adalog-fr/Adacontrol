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
  Ada.Wide_Text_IO;

-- Asis
with
  Asis.Exceptions;

-- Adalog
with
  Binary_Map,
  Units_List,
  Utilities;

-- Adacontrol
with
  Adactl_Options,
  Framework.Language.Scanner,
  Framework.Reports,
  Framework.Rules_Manager,
  Framework.Scope_Manager,
  Implementation_Options,
  Ruler;

package body Framework.Language.Commands is
   use Utilities;

   --  Output management
   Console_Name : constant Wide_String := "CONSOLE";

   Adactl_Output : Ada.Wide_Text_Io.File_Type;

   type No_Data is null record;
   package Seen_Files_Map is new Binary_Map (Unbounded_Wide_String, No_Data);
   Seen_Files : Seen_Files_Map.Map;


   ------------------
   -- Help_Command --
   ------------------

   procedure Help_Command is
   begin
      User_Message ("   Quit;");
      User_Message ("   Go;");
      User_Message ("   Message ""<message>"";");
      User_Message ("   Help [ all|<rule name>{,<rule name>} ];");
      User_Message ("   Clear all|<rule name> ;");
      User_Message ("   Set output ""<output file>"" ;");
      User_Message ("   Set verbose|debug|ignore on|off ;");
      User_Message ("   Source <input file> ;");
      User_Message ("   Search|Check <rule name> [ ( <parameters> ) ];");
   end Help_Command;

   ----------------
   -- Go_Command --
   ----------------

   procedure Go_Command is

      procedure Handle_Exception (Phase     : Wide_String;
                                  Occur     : Ada.Exceptions.Exception_Occurrence;
                                  Unit_Name : Wide_String := "")
      is
         use Ada.Exceptions, Asis.Exceptions, Ada.Characters.Handling;
      begin
         Failure_Occured := True;

         -- Clean-up:
         Ruler.Reset;
         Framework.Scope_Manager.Reset;

         User_Message ("============= Phase: " & Phase & " =============");
         Reraise_Occurrence (Occur);
      exception
         when Occur : ASIS_Failed
           | ASIS_Inappropriate_Context
           | ASIS_Inappropriate_Container
           | ASIS_Inappropriate_Compilation_Unit
           | ASIS_Inappropriate_Element
           | ASIS_Inappropriate_Line
           | ASIS_Inappropriate_Line_Number
           =>
            User_Message ("ASIS error: " & To_Wide_String (Exception_Name (Occur)));
            User_Message ("In rule: " & Framework.Rules_Manager.Last_Rule);
            if Unit_Name /= "" then
               User_Message ("For unit: " & Unit_Name);
            end if;
            Asis_Exception_Messages;

            -- Propagate the exception only if Exit_Option set
            if Adactl_Options.Exit_Option then
               raise;
            end if;

         when Occur : others =>
            User_Message ("Internal error: " & To_Wide_String (Exception_Name (Occur)));
            User_Message ("       In rule: " & Framework.Rules_Manager.Last_Rule);
            if Unit_Name /= "" then
               User_Message ("      For unit: " & Unit_Name);
            end if;
            User_Message ("       Message: " & To_Wide_String (Exception_Message (Occur)));

            -- Propagate the exception only if Exit_Option set
            if Adactl_Options.Exit_Option then
               raise;
            end if;
      end Handle_Exception;

      use Ada.Wide_Text_IO;
   begin
      begin
         Framework.Rules_Manager.Prepare_All;
      exception
         when Occur : others => Handle_Exception ("Preparation", Occur);
      end;

      Units_List.Reset;
      Framework.Reports.Clear_Counts;

      while not Units_List.Is_Exhausted loop
         begin
            Ruler.Process(Unit_Name  => Units_List.Current_Unit,
                          Spec_Only  => Adactl_Options.Spec_Option);
         exception
            when Occur : others => Handle_Exception ("Processing", Occur, Units_List.Current_Unit);
         end;

         Units_List.Skip;
      end loop;

      begin
         Framework.Rules_Manager.Finalize_All;
      exception
         when Occur : others => Handle_Exception ("Finalize", Occur);
      end;

      Framework.Reports.Report_Counts;

      if Is_Open (Adactl_Output) then
         Flush (Adactl_Output);
      end if;
   end Go_Command;

   ------------------------
   -- Set_Output_Command --
   ------------------------

  procedure Set_Output_Command (Output_File : Wide_String) is
      use Ada.Characters.Handling, Ada.Wide_Text_IO, Seen_Files_Map;
   begin
      -- Note that the following sequence ensures that Current_Output is
      -- never a closed file.
      Set_Output (Standard_Output);
      if Is_Open (Adactl_Output) then
         Close (Adactl_Output);
      end if;

      if To_Upper (Output_File) = Console_Name then
         Utilities.Error_Is_Out := True;
      else
         Utilities.Error_Is_Out := False;
         Safe_Open (Adactl_Output,
                    To_String (Output_File),
                    Append,
                    Adactl_Options.Overwrite_Option and not Is_Present (Seen_Files,
                                                                        To_Unbounded_Wide_String (Output_File)));
         Add (Seen_Files, To_Unbounded_Wide_String (Output_File), (null record));
         Set_Output (Adactl_Output);
      end if;
   end Set_Output_Command;

   --------------------
   -- Source_Command --
   --------------------

   procedure Source_Command (Name : Wide_String) is
      use Ada.Wide_Text_IO, Framework.Language.Scanner;

      -- Note that these procedures are passed the value of Current_Input.
      -- This allows to restore it at the end, although it is limited.
      procedure Compile_File (Name           : Wide_String;
                              Previous_Input : File_Type) is
         use Ada.Characters.Handling;
         File      : File_Type;
      begin
         if Name = "-" then
            -- By convention: treat Standard_Input as file (not interactive)
            Set_Input (Standard_Input);
         else
            Open (File, In_File, To_String (Name), Form => Implementation_Options.Form_Parameters);
            Set_Input (File);
         end if;
         Set_Prompt ("");
         Start_Scan (From_String => False, Source => Name);

         Compile;

         Set_Input (Previous_Input);
         if Name /= "-" then
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
         use Ada.Characters.Handling;
      begin
         Set_Input (Standard_Input);
         loop
            begin
               Set_Prompt ("Command");
               Start_Scan (From_String => False, Source => "Console");

               Compile;

               exit;
            exception
               when Occur : Utilities.User_Error =>
                  User_Message ("Command error: " & To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));
            end;
         end loop;
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
   end Source_Command;

begin
   Utilities.Error_Is_Out := True;
end Framework.Language.Commands;
