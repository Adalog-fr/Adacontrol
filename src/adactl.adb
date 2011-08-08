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
  Framework.Rules_Manager,
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


   My_Context   : Asis.Context;
   Start_Time   : constant Time := Clock;

   use Framework.Language;
begin

   Analyse_Options;

   if Action /= Help then
      --
      -- Init
      --
      Asis.Implementation.Initialize (Initialize_String);
      Asis.Ada_Environments.Associate (My_Context, "Adactl", Asis_Options);
      Asis.Ada_Environments.Open (My_Context);
      Units_List.Register (Unit_Spec  => Ada_Units_List,
                           Recursive  => Recursive_Option,
                           Add_Stubs  => False,
                           My_Context => My_Context);

      Framework.Rules_Manager.Prepare_All;
   end if;

   case Action is
      when Help =>
         null;           -- Help message printed from Analyse_Options

      when Dependents =>
         Units_List.Reset;
         while not Units_List.Is_Exhausted loop
            Ada.Wide_Text_IO.Put_Line (Units_List.Current_Unit);
            Units_List.Skip;
         end loop;

      when Process =>
         if Command_Line_Rules /= "" then
            Compile_String (Command_Line_Rules);
         end if;

         if Rules_File /= "" then
            Compile_File (Rules_File);
         end if;

         Units_List.Reset;
         while not Units_List.Is_Exhausted loop
            Ruler.Process(Unit_Name  => Units_List.Current_Unit,
                          Spec_Only  => Adactl_Options.Spec_Option,
                          My_Context => My_Context);
            Units_List.Skip;
         end loop;
   end case;

   if Action /= Help then
      --
      -- Clean up
      --
      Asis.Ada_Environments.Close (My_Context);
      Asis.Ada_Environments.Dissociate (My_Context);

      --
      -- Finalize
      --
      Asis.Implementation.Finalize;
      if Ruler.Had_Failure then
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

   Finalize_Adactl_Output;

exception
   when Occur : Options_Error | Units_List.Specification_Error =>
      Ada.Command_Line.Set_Exit_Status (Bad_Command);
      User_Message ("Parameter or option error: " & To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));
      User_Message ("try -h for help");
      Finalize_Adactl_Output;

   when Occur : Utilities.User_Error =>
      User_Message ("Error in rule: " & To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));

   when Occur : Asis.Exceptions.ASIS_Failed =>
      case Status is
         when Asis.Errors.Use_Error =>
            Ada.Command_Line.Set_Exit_Status (Bad_Command);
            User_Message (Diagnosis);
         when others =>
            Ada.Command_Line.Set_Exit_Status (Failure);
            Asis_Exception_Messages (Occur);
            raise; -- To get stack trace
      end case;
      Finalize_Adactl_Output;
end Adactl;
