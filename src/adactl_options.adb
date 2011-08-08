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

   Version : constant Wide_String := "1.4r20";

   package Analyzer is
      new Options_Analyzer (Binary_Options => "DdehiIrsuvwx",
                            Valued_Options => "flop",
                            Tail_Separator => "--");

   Unit_List    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Extra_Pathes : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Body_Found   : Boolean := False;

   Options_Commands : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

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
      User_Message ("Usage: adactl [-deirsuvw] [-f <rules file>] [-l <rules list>] [-o <output file>]");
      User_Message ("          [-p <project file>] <unit>[+|-<unit>]|[@]<file> ... [-- <ASIS options>]");
      User_Message ("       adactl -h [<rule id>... | all]");
      User_Message ("       adactl -I [-deirsuvw] [-o <output file>]");
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
      User_Message ("ADACTL v. " & Version);
      User_Message ("Copyright (C) 2004-2005 Eurocontrol/Adalog.");
      User_Message ("This software is covered by the GNU Modified General Public License.");
   end Help;

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
      use Ada.Wide_Text_Io, Ada.Characters.Handling, Ada.Strings.Wide_Fixed,
        Ada.Strings, Ada.Strings.Wide_Unbounded;
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

      elsif Is_Present (Option => 'I') then
         Action := Interactive_Process;

      else
         Action := Process;
      end if;


      --
      -- Initialize options
      --

      -- Output options
      Utilities.Debug_Option   := Is_Present (Option => 'd');
      Utilities.Verbose_Option := Is_Present (Option => 'v');
      Overwrite_Option         := Is_Present (Option => 'w');

      if Is_Present (Option => 'o') then
         -- modify current output
         Options_Commands := Options_Commands
           & "set output " & To_Wide_String (Value (Option => 'o', Explicit_Required => True)) & ';';
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

      if Is_Present (Option => 'D') then
         if Is_Present (Option => 'l') or Is_Present (Option => 'f') then
            Option_Error ("No rule can be specified with -D option");
         elsif Is_Present (Option => 'I') then
            Option_Error ("-D and -I options cannot be specified together");
         end if;
      elsif    not Is_Present (Option => 'l')
        and not Is_Present (Option => 'f')
        and not Is_Present (Option => 'I')
      then
         Option_Error("No rules specified");
      end if;

      if Is_Present (Option => 'l') then
         -- add rules uses from command line
         Options_Commands := Options_Commands
           & Trim (To_Wide_String (Value (Option => 'l', Explicit_Required => True)), Both);

         if Element (Options_Commands, Length (Options_Commands)) /= ';' then
            -- As a courtesy, provide the missing final ';'
            Options_Commands := Options_Commands & ';';
         end if;
      end if;

      if Is_Present (Option => 'f') then
         -- add rules uses from file
         Options_Commands := Options_Commands
           & "source " & To_Wide_String (Value (Option => 'f', Explicit_Required => True)) & ';';
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

   ---------------------------
   -- Command_Line_Commands --
   ---------------------------

   function Command_Line_Commands return Wide_String is
      use Ada.Strings.Wide_Unbounded;
   begin
      return To_Wide_String (Options_Commands);
   end Command_Line_Commands;

end Adactl_Options;
