-- Ada
with
  Ada.Characters.Handling,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Unbounded,
  Ada.Wide_Text_Io,
  Ada.Exceptions;

-- Adalog
with
  Options_Analyzer,
  Utilities,
  Implementation_Options;

-- Adactl
with
  Framework.Reports,
  Framework.Rules_Manager;

package body Adactl_Options is

   Version : constant Wide_String := "1.3r2";

   type String_Access is access String;

   package Analyzer is
      new Options_Analyzer (Binary_Options => "Ddehirsuvwx",
                            Valued_Options => "flop",
                            Tail_Separator => "--");

   Adactl_Output : Ada.Wide_Text_Io.File_Type;

   Unit_List    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Extra_Pathes : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Body_Found   : Boolean := False;

   Command_Line_Rules_String : String_Access;
   Rules_File_String         : String_Access;

   ------------------
   -- Option_Error --
   ------------------

   procedure Option_Error (Message : String) is
      use Ada.Exceptions;
   begin
      Raise_Exception (Options_Error'Identity, Message);
   end Option_Error;

   procedure Option_Error (Occur : Ada.Exceptions.Exception_Occurrence) is
      use Ada.Exceptions;
   begin
      Option_Error (Exception_Message (Occur));
   end Option_Error;

   pragma No_Return (Option_Error);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Framework.Rules_Manager;
   begin
      User_Message ("Usage: adactl -h [<rule id>... | all]");
      User_Message ("       adactl [-deirsuvw] [-f <rules file>] [-l <rules list>] [-o <output file>]");
      User_Message ("          [-p <project file>] <unit>[+|-<unit>]|[@]<file> ... [-- <ASIS options>]");
      User_Message ("       adactl -D [-rsw] [-o <output file>]");
      User_Message ("          [-p <project file>] <unit>[+|-<unit>]|[@]<file> ... [-- <ASIS options>]");
      User_Message ("");

      User_Message ("Options:");
      User_Message ("   -d       enable debug mode");
      User_Message ("   -e       treat warnnings (Search) as errors (Check)");
      User_Message ("   -f file  use a file for the specification of rules");
      User_Message ("   -h       prints this help message");
      User_Message ("   -h rule  prints rule help");
      User_Message ("   -h all   prints all rules help");
      User_Message ("   -i       ignore local deactivations");
      User_Message ("   -l rules process with this rules");
      User_Message ("   -o file  specify an output file");
      User_Message ("   -p file  specify an emacs ada-mode project file (.adp)");
      User_Message ("   -r       recursive");
      User_Message ("   -s       process specifications only");
      User_Message ("   -u       treat all parameters as Ada units");
      User_Message ("   -v       enable verbose mode");
      User_Message ("   -w       overwrite output file (works with -o)");
      User_Message ("   -x       exit when internal error");
      User_Message ("Rules:");
      Help_Names;
      User_Message ("");
      User_Message ("ADACTL v. " & Version & ", Copyright (C) 2004 Eurocontrol/Adalog is");
      User_Message ("covered by the GNU Modified General Public License.");
   end Help;

   ----------------------------
   -- Finalise_Adactl_Output --
   ----------------------------

   procedure Finalize_Adactl_Output is
      use Ada.Wide_Text_Io;
   begin
      if Is_Open (Adactl_Output) then
         Set_Output (Standard_Output);
         Close (Adactl_Output);
      end if;
   end Finalize_Adactl_Output;

   --------------------
   -- Gnat_Unit_Name --
   --------------------

   function Gnat_Unit_Name (S : in Wide_String) return Wide_String is
      use Ada.Characters.Handling;
      use Ada.Strings;
      use Ada.Strings.Wide_Maps;
      use Ada.Strings.Wide_Fixed;
      use Ada.Strings.Wide_Unbounded;

      Unit_Name_First : Natural := Index (S, Set => To_Set ("/\"), Going => Backward);
   begin
      if Unit_Name_First = 0 then
         -- There is no directory separator
         Unit_Name_First := S'First;
      else
         declare
            Extra_Path : constant Wide_String := S (S'First ..  Unit_Name_First);
         begin
            if Index (Extra_Pathes, Extra_Path) = 0 then
               Append (Extra_Pathes, " -I" & Extra_Path);
            end if;

            Unit_Name_First := Unit_Name_First + 1;
         end;
      end if;

      return Translate (S (Unit_Name_First .. S'Last - 4), To_Mapping ("-", "."));

   end Gnat_Unit_Name;

   --------------
   -- Add_Unit --
   --------------

   procedure Add_Unit (S : in Wide_String) is
      use Utilities, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded;

      Ext  : constant Wide_String := To_Upper (Tail (S, 4));
   begin
      if not Unit_Option and (Ext = ".ADS" or Ext = ".ADB") then
         -- Take it as a file name
         if Ext = ".ADB" then
            Body_Found := True;
         end if;

         if Unit_List = Null_Unbounded_Wide_String then
            Unit_List :=  To_Unbounded_Wide_String (Gnat_Unit_Name (S));
         else
            Append (Unit_List, "+" & Gnat_Unit_Name (S));
         end if;

      else
         -- Unit name(s)
         Body_Found := True;
         if Unit_List = Null_Unbounded_Wide_String then
            Unit_List :=  To_Unbounded_Wide_String (S);
         elsif S(1) = '+' or S(1) = '-' then
            Append (Unit_List, S);
         else
            Append (Unit_List, "+" & S);
         end if;
      end if;
   end Add_Unit;

   ---------------------
   -- Analyse_Options --
   ---------------------

   procedure Analyse_Options is
      use Ada.Wide_Text_Io, Ada.Characters.Handling;
      use Utilities, Analyzer, Framework.Rules_Manager;
   begin
      --
      -- Help
      --
      if Is_Present (Option => 'h') then
         Action := Help;

         if Parameter_Count = 0 then
            Help;
         else
            for I in 1.. Parameter_Count loop
               declare
                  Val : constant Wide_String := To_Wide_String (Parameter (I));
               begin
                  if To_Upper (Val) = "ALL" then
                     Framework.Rules_Manager.Help_All;
                  else
                     Framework.Rules_Manager.Help (Val);
                  end if;
               end;
            end loop;
         end if;

         return;
      elsif Is_Present (Option => 'D') then
         Action := Dependents;
      else
         Action := Process;
      end if;


      --
      -- Initialize options
      --

      -- Output options
      Utilities.Debug_Option     := Is_Present (Option => 'd');
      Utilities.Overwrite_Option := Is_Present (Option => 'w');
      Utilities.Verbose_Option   := Is_Present (Option => 'v');
      Utilities.Error_Is_Out     := not Is_Present (Option => 'o');

      if Is_Present (Option => 'o') then
         -- modify current output
         Safe_Open (Adactl_Output,
                    Value (Option            => 'o',
                           Explicit_Required => True),
                    Create);
         Set_Output (Adactl_Output);
      end if;

      Framework.Reports.Warning_As_Error_Option := Is_Present (Option => 'e');

      -- Process options
      Recursive_Option := Is_Present (Option => 'r');
      Ignore_Option    := Is_Present (Option => 'i');
      Unit_Option      := Is_Present (Option => 'u');
      Spec_Option      := Is_Present (Option => 's');
      Exit_Option      := Is_Present (Option => 'x');

      if Parameter_Count = 0 then
         Option_Error ("At least one unit/file required");
      end if;

      if not Is_Present (Option => 'l') and not Is_Present (Option => 'f') then
         Option_Error("No rules specified");
      end if;

      if Is_Present (Option => 'l') then
         -- add rules uses from command line
         Command_Line_Rules_String := new String'(Value (Option => 'l', Explicit_Required => True));
      end if;

      if Is_Present (Option => 'f') then
         -- add rules uses from file
         Rules_File_String := new String'(Value (Option => 'f', Explicit_Required => True));
      end if;

      for I in 1 .. Parameter_Count loop
         Add_Unit (To_Wide_String (Parameter (I)));
      end loop;
      Spec_Option := Spec_Option or not Body_Found;

   exception
      when Occur : Analyzer.Options_Error =>
         Option_Error (Occur);
      when Overwrite_Error =>
         Option_Error ("File " & Value (Option => 'o')
                       & " already exists, use ""-w"" to overwrite");

   end Analyse_Options;

   ------------------
   -- Asis_Options --
   ------------------

   function Asis_Options return Wide_String is
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded, Ada.Wide_Text_IO;
      use Implementation_Options, Analyzer;

      Project_File : constant String := Value (Option            => 'p',
                                               Explicit_Required => True);
   begin
      return Parameters_String (Project_File,
                                To_Wide_String (Tail_Value)
                                  & To_Wide_String (Extra_Pathes));
   exception
      when Occur : Analyzer.Options_Error =>
         Option_Error (Occur);
      when Name_Error =>
        Option_Error ("Project file """ & Project_File & """ not found");
  end Asis_Options;

   --------------------
   -- Ada_Units_List --
   --------------------

   function Ada_Units_List return Wide_String is
      use Ada.Strings.Wide_Unbounded;
   begin
      return To_Wide_String (Unit_List);
   end Ada_Units_List;

   -----------------------
   -- Initialize_String --
   -----------------------

   function Initialize_String return Wide_String is
   begin
      return Implementation_Options.Initialize_String;
   end Initialize_String;

   ----------------
   -- Rules_File --
   ----------------

   function Rules_File return String is
   begin
      if Rules_File_String = null then
         return "";
      else
         return Rules_File_String.all;
      end if;
   end Rules_File;

   ------------------------
   -- Command_Line_Rules --
   ------------------------

   function Command_Line_Rules return String is
   begin
      if Command_Line_Rules_String = null then
         return "";
      else
         return Command_Line_Rules_String.all;
      end if;
   end Command_Line_Rules;

end Adactl_Options;
