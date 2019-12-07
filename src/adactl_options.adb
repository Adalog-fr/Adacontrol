----------------------------------------------------------------------
--  Adactl_Options - Package body                                   --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005.         --
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
  Ada.Directories,
  Ada.Environment_Variables,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Unbounded,
  Ada.Exceptions;

-- Adalog
with
  Options_Analyzer,
  Utilities,
  Implementation_Options,
  Project_File,
  Project_File.Factory;

-- Adactl
with
  Framework.Reports;

package body Adactl_Options is
   use Framework.Variables;

   package Analyzer is
      new Options_Analyzer (Binary_Options => "CdDeEhiIjrsTuvwx",
                            Valued_Options => "fFGlomMpSt",
                            Tail_Separator => "--");

   Unit_List    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Extra_Pathes : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Body_Found   : Boolean := False;

   Options_Commands : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   Project : Project_File.Class_Access;

   ------------------
   -- Option_Error --
   ------------------

   procedure Option_Error (Message : String);
   procedure Option_Error (Occur : Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Option_Error);

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


   ----------------------
   -- Is_Relative_Name --
   ----------------------

   function Is_Relative_Name (Name : String) return Boolean is
   -- TBSL: to be replaced by Ada.Directories.Hierarchical_File_Names.Is_Relative_Name when available
      Start : Positive;
   begin
      if Name'Length >= 3 and then Name (Name'First + 1) = ':' then
         Start := Name'First + 2;
      else
         Start := Name'First;
      end if;
      return Name (Start) /= '/' and Name (Start) /= '\';
   end Is_Relative_Name;


   ---------------------
   -- Help_On_Options --
   ---------------------

   procedure Help_On_Options is
      use Utilities, Framework.Reports;
   begin
      User_Message ("Usage: adactl [-deEirsTuvwx]");
      User_Message ("              [-p <project file>]     [-f <rules file>]    [-l < rules list > ]");
      User_Message ("              [-o <output file> ]     [-t <trace file>]    [-F <format>]");
      User_Message ("              [-S <statistics level>] [-m <warning limit>] [-M <message limit>]");
      User_Message ("              [-G <level> ");
      User_Message ("              <unit>[+|-<unit>]|[@]<file> ... [-- <ASIS options>]");
      User_Message ("       adactl -h [<help item>...]");
      User_Message ("       adactl -I [-deEirsTuvwx]");
      User_Message ("              [-p <project file>]     [-f <rules file>]    [-l < rules list > ]");
      User_Message ("              [-o < output file > ]   [-t <trace file>]    [-F <format>]");
      User_Message ("              [-S <statistics level>] [-m <warning limit>] [-M <message limit>]");
      User_Message ("              <unit>[+|-<unit>]|[@]<file> ... [-- <ASIS options>]");
      User_Message ("       adactl -D [-rswx] [-p <project file>] [-o <output file>] ");
      User_Message ("              <unit>[+|-<unit>]|[@]<file> ... [-- <ASIS options>]");
      User_Message ("       adactl -C [-dvx] [-f <rules file>] [-l <rules list>]");
      User_Message ("");

      User_Message ("Special modes:");
      User_Message ("   -h           prints general help message (options, rules, version, license)");
      User_Message ("   -h <rule>... prints specific rule(s) help (<rule> can be a regexp)");
      User_Message ("   -h all       prints all rules help");
      User_Message ("   -h commands  prints commands help");
      User_Message ("   -h license   prints license information");
      User_Message ("   -h list      prints all rules names (GPS format)");
      User_Message ("   -h options   prints command-line options help");
      User_Message ("   -h rules     prints all rules names (normal format)");
      User_Message ("   -h variables prints values of all global and rules variables");
      User_Message ("   -h version   prints version information");
      User_Message ("   -I           interactive mode");
      User_Message ("   -D           generate dependencies");
      User_Message ("   -C           check rules syntax only");
      User_Message ("Options:");
      User_Message ("   -d        enable debug mode");
      User_Message ("   -e        treat warnings (Search) as errors (Check)");
      User_Message ("   -E        print only errors (Check)");
      User_Message ("   -f file   use a file for the specification of rules");
      User_Message ("   -F format choose output format (GNAT, GNAT_SHORT, CSV, CSV_SHORT, CSVX, CSVX_SHORT, NONE)");
      User_Message ("   -G level  level to generate fixes for (NONE, SEARCH, CHECK)");
      User_Message ("   -i        ignore local deactivations");
      User_Message ("   -j        invert local deactivations");
      User_Message ("   -l rules  process with these rules");
      User_Message ("   -o file   specify an output file");
      User_Message ("   -p file   specify a project file (.gpr or .adp)");
      User_Message ("   -r        recursive");
      User_Message ("   -s        process specifications only");
      User_Message ("   -S level  statistics level (0 .. "
                      & Integer_Img (Stats_Levels'Pos (Stats_Levels'Last))
                      & ')');
      User_Message ("   -t file   specify a trace file");
      User_Message ("   -T        Report execution time of rules");
      User_Message ("   -u        treat all parameters as Ada units");
      User_Message ("   -v        enable verbose mode");
      User_Message ("   -w        overwrite output file (works with -o)");
      User_Message ("   -x        exit when internal error");
   end Help_On_Options;

   --------------------
   -- Gnat_Unit_Name --
   --------------------

   function Gnat_Unit_Name (S : in Wide_String) return Wide_String is
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
               Append (Extra_Pathes, " -I""" & Extra_Path & '"');
            end if;

            Unit_Name_First := Unit_Name_First + 1;
         end;
      end if;

      return Translate (S (Unit_Name_First .. S'Last - 4), To_Mapping ("-~", ".."));

   end Gnat_Unit_Name;

   --------------
   -- Add_Unit --
   --------------

   procedure Add_Unit (S : in Wide_String) is
      use Utilities, Ada.Strings, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded;

      Ext  : constant Wide_String := To_Upper (Tail (S, 4));

      procedure Add_Parents (Unit : Wide_String) is
         -- Add parents of the given unit *before* the unit, in order to make sure
         -- that a parent unit is always processed before its children.
         use Ada.Characters.Handling;
         Point_Pos : constant Natural := Index (Unit, ".", Going => Backward);
      begin
         if Point_Pos = 0 then
            return;
         end if;

         if Point_Pos = Unit'First then
            -- Name starts with '.'
            -- Can happen if the user gives "../name" instead of "../name.adb"
            Option_Error ("Illegal syntax for a unit (not file) name: " & To_String (Unit));
         end if;

         Add_Parents (Unit (Unit'First .. Point_Pos - 1));
         Append (Unit_List, "+" & Unit (Unit'First .. Point_Pos - 1));
      end Add_Parents;

   begin  -- Add_Unit
      if S (S'First) = '@' then
         Body_Found := True;
         Append (Unit_List, S);
         return;
      end if;

      if Unit_Option = Off and Ext in ".ADS" | ".ADB" then
         -- Take it as a file name
         if Ext = ".ADB" then
            Body_Found := True;
         end if;
         declare
            Unit : constant Wide_String := Gnat_Unit_Name (Clean_File_Name (S));
         begin
            Add_Parents (Unit);
            Append (Unit_List, "+" & Unit);
         end;

      else
         -- Unit name(s)
         Body_Found := True;
         case S(S'First) is
            when '+' =>
               Add_Parents (S (S'First+1..S'Last));
               Append (Unit_List, S);
            when '-' =>
               Append (Unit_List, S);
            when others =>
               Add_Parents (S);
               Append (Unit_List, "+" & S);
         end case;
      end if;
   end Add_Unit;

   ---------------------
   -- Analyse_Options --
   ---------------------

   procedure Analyse_Options is
      use Ada.Characters.Handling, Ada.Environment_Variables, Ada.Strings.Wide_Unbounded;
      use Utilities, Analyzer;

      Rules_Specified : Boolean := False;

      procedure Flag_To_Command (Option : Character; Param : Wide_String; Inverted : Boolean := False) is
      begin
         if Is_Present (Option) or else Project.Tool_Switch_Present ("adacontrol", '-' & Option) then
            if Inverted then
               Append (Options_Commands, "set " & Param & " off;");
            else
               Append (Options_Commands, "set " & Param & " on;");
            end if;
         end if;
      end Flag_To_Command;

      procedure Value_To_Command (Option : Character;
                                  Param    : Wide_String;
                                  Required : Boolean := True;
                                  Default  : String  := "")
      is
      begin
         if Is_Present (Option) then
            Append (Options_Commands,
                    Param & ' '
                    & To_Wide_String (Value (Option, Explicit_Required => Required, Default => Default)) & ';');
         elsif Project.Tool_Switch ("adacontrol", '-' & Option) /= "" then
            Append (Options_Commands,
                    Param & ' '
                    & To_Wide_String (Project.Tool_Switch ("adacontrol", '-' & Option)) & ';');
         end if;
      end Value_To_Command;

      function Is_Present (Option : Character) return Switch is
      begin
         if Is_Present (Option) then
            return On;
         else
            return Off;
         end if;
      end Is_Present;

      use Project_File;
   begin -- Analyse_Options
      --
      -- Help
      --
      if Is_Present (Option => 'h') then
         -- Options that make sense even with "help"
         Exit_Option.Value      := Is_Present (Option => 'x');
         Utilities.Debug_Option := Is_Present (Option => 'd');

         Action := Help;

         if Parameter_Count = 0 then
            Options_Commands := To_Unbounded_Wide_String ("help OPTIONS, RULES;"
                                                          & "message """";"
                                                          & "help VERSION, LICENSE;");
         else
            -- Beware that "help variables" may be followed by an optional pattern
            -- There is a small ambiguity here; we consider that if a parameter follows "variables", it is a
            --    pattern, not a rule name.
            -- Due to this special case, we can't use a regular "for" loop
            declare
               I : Positive := 1;
            begin
               loop
                  Append (Options_Commands, "help " & To_Wide_String (Parameter (I)));
                  if I /= Parameter_Count
                    and then To_Upper (Parameter (I)) = "VARIABLES"
                  then
                     Append (Options_Commands, ' ' & To_Wide_String (Parameter (I + 1)));
                     I := I + 1;
                  end if;
                  Append (Options_Commands, ';');
                  exit when I = Parameter_Count;
                  I := I + 1;
               end loop;
            end;
         end if;
         return;

      elsif Is_Present (Option => 'C') then  -- Must be first for -C to override any other option (except help)
         Action := Check;

      elsif Is_Present (Option => 'D') then
         Action := Dependents;

      elsif Is_Present (Option => 'I') then
         Action := Interactive_Process;

      else
         Action := Process;
      end if;


      --
      -- Options that are not settable from the command language
      --
      Recursive_Option  := Is_Present (Option => 'r');
      Spec_Option       := Is_Present (Option => 's');
      Unit_Option       := Is_Present (Option => 'u');
      Overwrite_Option  := Is_Present (Option => 'w');
      Exit_Option.Value := Is_Present (Option => 'x');

      begin
         Project := Project_File.Factory.Corresponding_Project (Value (Option => 'p', Explicit_Required => True));
      exception
         when Occur: Project_Error =>
            Option_Error (Ada.Exceptions.Exception_Message (Occur));
      end;


      --
      -- Options that are translated into the command language
      --

      -- Defaults from the ADACTLINI environment variable
      if Exists ("ADACTLINI") then
         Append (Options_Commands, To_Wide_String (Value ("ADACTLINI")));
         if Length (Options_Commands) > 0
           and then  Element (Options_Commands, Length (Options_Commands)) /= ';'
         then
            -- As a courtesy, provide the missing final ';'
            Append (Options_Commands, ';');
         end if;
      end if;

      -- Special case for Debug_Option: we must initialize the flag directly here, because it
      -- is needed by the ASIS initialization string. Note that this means that the presence or
      -- absence of the bug box (-nbb option) depends only on the command line, and not on subsequent
      -- "set debug on/off"
      Utilities.Debug_Option := Is_Present ('d');

      -- Command line parameters come first, since they define the default behaviour
      -- for commands given in the file (-f) and -l options
      Flag_To_Command  ('d', "debug");
      Flag_To_Command  ('e', "warning_as_error");
      Flag_To_Command  ('E', "warning", Inverted => True);
      Value_To_Command ('F', "set format");
      Value_To_Command ('G', "set fixes_gen", Required => False, Default => "NONE");
      if Is_Present (Option => 'j') then
         Append (Options_Commands, "set ignore inverted;"); -- -i ignored if -j given
      else
         Flag_To_Command  ('i', "ignore");
      end if;
      Value_To_Command ('m', "set max_errors",   Required => False);
      Value_To_Command ('M', "set max_messages", Required => False);
      Value_To_Command ('o', "set output",       Required => False, Default => "CONSOLE");
      Value_To_Command ('S', "set statistics");
      Value_To_Command ('t', "set trace");
      Flag_To_Command  ('T', "timing");
      Flag_To_Command  ('v', "verbose");

      -- add commands from file
      if Is_Present (Option => 'f') then
         Value_To_Command ('f', "source", Required => False);
         Rules_Specified := Value ('f', Explicit_Required => False, Default => "") /= "";

      elsif Action /= Dependents then
         declare
            use Ada.Directories;
            Rule_File : constant String := Project.Tool_Switch ("adacontrol", After => "-f");
         begin
            if Rule_File /= "" then
               if Is_Relative_Name (Rule_File) then
                  -- Note: it is not possible to compose two Directories, since the 2nd parameter
                  --       to compose needs to be a simple name. But we know that just concatenating
                  --       '/' works on every OS...
                  Append (Options_Commands,
                          "source "
                          & To_Wide_String (Compose (Containing_Directory (Project.Path)
                                                     & '/' & Containing_Directory (Rule_File),
                                                     Simple_Name (Rule_File)))
                          & ';');
               else
                  Append (Options_Commands, "source " & To_Wide_String (Rule_File) & ';');
               end if;
               Rules_Specified := True;
            end if;
         end;
      end if;

      -- add commands from command line
      -- Must stay after file, to allow changing parameters defined in file
      -- (assuming there is no "go" command in the file)
      if Is_Present (Option => 'l') then
         Append (Options_Commands,
                 Trim_All (To_Wide_String (Value (Option => 'l', Explicit_Required => True))));

         if Element (Options_Commands, Length (Options_Commands)) /= ';' then
            -- As a courtesy, provide the missing final ';'
            Append (Options_Commands, ';');
         end if;
         Rules_Specified := True;
      end if;

      --
      -- Check options
      --
      if Action = Dependents then
         if Is_Present (Option => 'l') or Is_Present (Option => 'f') then
            Option_Error ("No rule can be specified with -D option");
         elsif Is_Present (Option => 'I') then
            Option_Error ("-D and -I options cannot be specified together");
         end if;
      elsif Action /= Interactive_Process and not Rules_Specified then
         Option_Error("No rules specified");
      end if;

      --
      -- Add units
      --
      if Action /= Check then
         if Parameter_Count = 0 then
            if Is_Present (Option => 'p') then
               declare
                  use Ada.Directories;
                  Indirect_File : constant String := Project.Tool_Switch ("adacontrol", After => "-@");
               begin
                  if Indirect_File = "" then
                     declare
                        Mains : constant Names_List := Project.Main_Files;
                     begin
                        if Mains'Length = 0 then
                           Option_Error ("No unit/file specified and no indirect file or mains in project");
                        end if;
                        for Unit : Unbounded_Wide_String of Mains loop
                           Add_Unit (To_Wide_String (Unit));
                        end loop;
                        if Project.Tool_Switch_Present ("adacontrol", Switch => "-r") then
                           Recursive_Option := On;
                        end if;
                     end;
                  else
                     if Is_Relative_Name (Indirect_File) then
                        Add_Unit ('@' & To_Wide_String (Compose (Compose (Containing_Directory (Project.Path),
                                                                          Containing_Directory (Indirect_File)),
                                                                Simple_Name (Indirect_File))));
                     else
                        Add_Unit ('@' & To_Wide_String (Indirect_File));
                     end if;
                  end if;
               end;
            else
               Option_Error ("At least one unit/file required");
            end if;
         else
            for I in Natural range 1 .. Parameter_Count loop
               Add_Unit (To_Wide_String (Parameter (I)));
            end loop;
         end if;

         if not Body_Found then
            Spec_Option := On;
         end if;
      end if;

   exception
      when Occur : Analyzer.Options_Error =>
         Option_Error (Occur);
   end Analyse_Options;

   ------------------
   -- Asis_Options --
   ------------------

   function Asis_Options return Wide_String is
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;
      use Implementation_Options, Analyzer;
   begin
      return Parameters_String (Project,
                                Other_Options => To_Wide_String (Tail_Value) & To_Wide_String (Extra_Pathes));
   exception
      when Occur : Analyzer.Options_Error | Project_File.Project_Error =>
         Option_Error (Occur);
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
      return Implementation_Options.Initialize_String (Utilities.Debug_Option);
   end Initialize_String;

   ------------------
   -- Command_Line --
   ------------------

   function Command_Line return Wide_String is
      use Ada.Characters.Handling;
      use Analyzer;
   begin
      return To_Wide_String (Option_String (With_Command => True));
   end Command_Line;

   ---------------------------
   -- Command_Line_Commands --
   ---------------------------

   function Command_Line_Commands return Wide_String is
      use Ada.Strings.Wide_Unbounded;
   begin
      return To_Wide_String (Options_Commands);
   end Command_Line_Commands;

begin  -- Adactl_Options
   --
   -- Register option variables
   --

   Register (Debug_Option'Access,   Variable_Name => "DEBUG");
   Register (Exit_Option'Access,    Variable_Name => "EXIT_ON_ERROR");
   Register (Ignore_Option'Access,  Variable_Name => "IGNORE");
   Register (Verbose_Option'Access, Variable_Name => "VERBOSE");
end Adactl_Options;
