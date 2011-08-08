----------------------------------------------------------------------
--  Adactl - Main program body                                      --
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
  Ada.Calendar,
  Ada.Command_Line,
  Ada.Exceptions,
  Ada.Wide_Text_IO;

-- ASIS
with
  Asis.Ada_Environments,
  Asis.Errors,
  Asis.Exceptions,
  Asis.Implementation;

-- Adalog
with
  Utilities,
  Units_List;

-- Adactl
with
  Adactl_Options,
  Framework.Language,
  Framework.Reports,
  Ruler;

procedure Adactl is
   use Ada.Characters.Handling, Ada.Exceptions, Ada.Calendar;
   use Asis.Exceptions, Asis.Implementation;
   use Utilities, Adactl_Options;

   -- Return codes:
   OK            : constant Ada.Command_Line.Exit_Status :=  0;
   Checks_Failed : constant Ada.Command_Line.Exit_Status :=  1;
   Bad_Command   : constant Ada.Command_Line.Exit_Status :=  2;
   Failure       : constant Ada.Command_Line.Exit_Status := 10;

   Start_Time : constant Time := Clock;

   use Framework.Language;
begin

   Analyse_Options;

   if Action /= Help then
      --
      -- Init
      --
      User_Log ("Loading units, please wait...");
      Asis.Implementation.Initialize (Initialize_String);
      Asis.Ada_Environments.Associate (Ruler.My_Context, "Adactl", Asis_Options);
      Asis.Ada_Environments.Open (Ruler.My_Context);
      Units_List.Register (Unit_Spec  => Ada_Units_List,
                           Recursive  => Recursive_Option,
                           Add_Stubs  => False,
                           My_Context => Ruler.My_Context);
   end if;

   case Action is
      when Help =>
         null;           -- Help message printed from Analyse_Options

      when Dependents =>
         Execute (Command_Line_Commands);  -- For a possible -o option
         Units_List.Reset;
         while not Units_List.Is_Exhausted loop
            Ada.Wide_Text_IO.Put_Line (Units_List.Current_Unit);
            Units_List.Skip;
         end loop;

      when Process =>
         Execute (Command_Line_Commands);
         if not Go_Command_Found then
            Execute ("Go;");
         end if;

      when Interactive_Process =>
         Execute (Command_Line_Commands);
         Execute ("source console;");

   end case;

   if Action /= Help then
      -- Close output file if any
      Execute ("set output console;");

      --
      -- Clean up ASIS
      --
      Asis.Ada_Environments.Close (Ruler.My_Context);
      Asis.Ada_Environments.Dissociate (Ruler.My_Context);
      Asis.Implementation.Finalize;

      --
      -- Finalize
      --
      if Framework.Language.Had_Failure then
         Ada.Command_Line.Set_Exit_Status (Failure);
      elsif Framework.Reports.Error_Reported then
         Ada.Command_Line.Set_Exit_Status (Checks_Failed);
      else
         Ada.Command_Line.Set_Exit_Status (OK);
      end if;

      declare
         Exec_Time_String : constant Wide_String
           := Integer'Wide_Image (Integer ((Clock - Start_Time)*10));
      begin
         User_Log ("Execution_Time: "
                   & Choose (Exec_Time_String (2 .. Exec_Time_String'Last - 1), "0")
                   & '.'
                   & Exec_Time_String (Exec_Time_String'Last)
                   & "s.");
      end;
   end if;

exception
   when Occur : Options_Error | Units_List.Specification_Error =>
      Ada.Command_Line.Set_Exit_Status (Bad_Command);
      User_Message ("Parameter or option error: " & To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));
      User_Message ("try -h for help");

   when Occur : Utilities.User_Error =>
      User_Message ("Error in rule: " & To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));

   when Asis.Exceptions.ASIS_Failed =>
      case Status is
         when Asis.Errors.Use_Error =>
            Ada.Command_Line.Set_Exit_Status (Bad_Command);
            User_Message (Diagnosis);
         when others =>
            Ada.Command_Line.Set_Exit_Status (Failure);
            raise; -- To get stack trace
      end case;
end Adactl;
