----------------------------------------------------------------------
--  Adactl_Options - Package body                                   --
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
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Unbounded,
  Ada.Wide_Text_IO,
  Ada.Exceptions;

-- Adalog
with
  Options_Analyzer,
  Utilities,
  Implementation_Options;

-- Adactl
with
  Framework.Reports;

package body Adactl_Options is

   package Analyzer is
      new Options_Analyzer (Binary_Options => "CDdeEhiIrsuvwx",
                            Valued_Options => "fFlopSt",
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

   ------------------
   -- Help_Options --
   ------------------

   procedure Help_Options is
      use Utilities, Framework.Reports;
   begin
      User_Message ("Usage: adactl [-deirsuvw] [-f <rules file>] [-l <rules list>] [-o <output file>]");
      User_Message ("          [-p <project file>] <unit>[+|-<unit>]|[@]<file> ... [-- <ASIS options>]");
      User_Message ("       adactl -h [<rule id>... | all]");
      User_Message ("       adactl -I [-deirsuvw] [-f <rules file>] [-l <rules list>] [-o <output file>]");
      User_Message ("          [-p <project file>] <unit>[+|-<unit>]|[@]<file> ... [-- <ASIS options>]");
      User_Message ("       adactl -D [-rsw] [-o <output file>]");
      User_Message ("          [-p <project file>] <unit>[+|-<unit>]|[@]<file> ... [-- <ASIS options>]");
      User_Message ("       adactl -C [-dv] [-f <rules file>] [-l <rules list>]");
      User_Message ("");

      User_Message ("Special modes:");
      User_Message ("   -h        prints this help message");
      User_Message ("   -h rule   prints rule help");
      User_Message ("   -h all    prints all rules help");
      User_Message ("   -I        interactive mode");
      User_Message ("   -D        generate dependencies");
      User_Message ("   -C        check rules syntax only");
      User_Message ("Options:");
      User_Message ("   -d        enable debug mode");
      User_Message ("   -e        treat warnings (Search) as errors (Check)");
      User_Message ("   -E        print only errors (Check)");
      User_Message ("   -f file   use a file for the specification of rules");
      User_Message ("   -F format choose output format (GNAT, GNAT_SHORT, CSV, CSV_SHORT, CSVX, CSVX_SHORT)");
      User_Message ("   -i        ignore local deactivations");
      User_Message ("   -l rules  process with these rules");
      User_Message ("   -o file   specify an output file");
      User_Message ("   -p file   specify an emacs ada-mode project file (.adp)");
      User_Message ("   -r        recursive");
      User_Message ("   -s        process specifications only");
      User_Message ("   -S        statistics level (0 .."
                      & Integer'Wide_Image (Stats_Levels'Pos (Stats_Levels'Last))
                      & ')');
      User_Message ("   -t file   specify a trace file");
      User_Message ("   -u        treat all parameters as Ada units");
      User_Message ("   -v        enable verbose mode");
      User_Message ("   -w        overwrite output file (works with -o)");
      User_Message ("   -x        exit when internal error");
   end Help_Options;

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
      use Utilities, Ada.Strings, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded;

      Ext  : constant Wide_String := To_Upper (Tail (S, 4));

      procedure Add_Parents (Unit : Wide_String) is
         -- Add parents of the given unit *before* the unit, in order to make sure
         -- that a parent unit is always processed before its children.
         Point_Pos : constant Natural := Index (Unit, ".", Going => Backward);
      begin
         if Point_Pos = 0 then
            return;
         end if;

         if Point_Pos = Unit'First then
            -- Name starts with '.'
            -- Can happen if the user gives "../name" instead of "../name.adb"
            Option_Error ("Illegal syntax for a unit (not file) name");
         end if;

         Add_Parents (Unit (Unit'First .. Point_Pos - 1));
         Append (Unit_List, "+" & Unit (Unit'First .. Point_Pos - 1));
      end Add_Parents;
   begin
      if S(S'First) = '@' then
         Body_Found := True;
         Append (Unit_List, S);
         return;
      end if;

      if not Unit_Option and (Ext = ".ADS" or Ext = ".ADB") then
         -- Take it as a file name
         if Ext = ".ADB" then
            Body_Found := True;
         end if;
         declare
            Unit : constant Wide_String := Gnat_Unit_Name (S);
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
      use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded;
      use Utilities, Analyzer;
   begin
      --
      -- Help
      --
      if Is_Present (Option => 'h') then

         if Parameter_Count = 0 then
            Action := Help;
         else
            Action := Help_Rule;
            for I in Natural range 1.. Parameter_Count loop
               Options_Commands := Options_Commands & "help " & To_Wide_String (Parameter (I)) & ';';
            end loop;
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
      -- Initialize options
      --

      -- Output options
      Utilities.Debug_Option   := Is_Present (Option => 'd');
      Utilities.Verbose_Option := Is_Present (Option => 'v');
      Overwrite_Option         := Is_Present (Option => 'w');

      if Is_Present (Option => 'S') then
         declare
            Temp_Value : Integer;
            use Framework.Reports;
         begin
            Temp_Value := Value ('S',
                                 Default => Stats_Levels'Pos (Stats_Levels'Last),
                                 Explicit_Required => False);
            if Temp_Value not in 0 .. Stats_Levels'Pos (Stats_Levels'Last) then
               Option_Error ("Value for S option must be in range 0 .."
                               & Integer'Image (Stats_Levels'Pos (Stats_Levels'Last)));
            end if;
            Options_Commands := Options_Commands
              & "set statistics" & Integer'Wide_Image (Temp_Value) & ';';
         end;
      end if;

      if Is_Present (Option => 'o') then
         -- modify current output
         Options_Commands := Options_Commands
           & "set output " & To_Wide_String (Value (Option => 'o', Explicit_Required => True)) & ';';
      end if;

      if Is_Present (Option => 't') then
         -- modify current trace
         Options_Commands := Options_Commands
           & "set trace " & To_Wide_String (Value (Option => 't', Explicit_Required => True)) & ';';
      end if;

      Framework.Reports.Warning_As_Error_Option := Is_Present (Option => 'e');
      Framework.Reports.Skip_Warning_Option     := Is_Present (Option => 'E');

      -- Process options
      Recursive_Option := Is_Present (Option => 'r');
      Ignore_Option    := Is_Present (Option => 'i');
      Unit_Option      := Is_Present (Option => 'u');
      Spec_Option      := Is_Present (Option => 's');
      Exit_Option      := Is_Present (Option => 'x');

      if Action /= Check and Parameter_Count = 0 then
         Option_Error ("At least one unit/file required");
      end if;

      if Action = Dependents then
         if Is_Present (Option => 'l') or Is_Present (Option => 'f') then
            Option_Error ("No rule can be specified with -D option");
         elsif Is_Present (Option => 'I') then
            Option_Error ("-D and -I options cannot be specified together");
         end if;
      elsif Action /= Interactive_Process
        and not Is_Present (Option => 'l')
        and not Is_Present (Option => 'f')
      then
         Option_Error("No rules specified");
      end if;

      if Is_Present (Option => 'F') then
         -- Output format
         Options_Commands := Options_Commands
           & "set format " & To_Wide_String (Value (Option => 'F', Explicit_Required => True)) & ';';
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

      if Action /= Check then
         for I in Natural range 1 .. Parameter_Count loop
            Add_Unit (To_Wide_String (Parameter (I)));
         end loop;
         Spec_Option := Spec_Option or not Body_Found;
      end if;

   exception
      when Occur : Analyzer.Options_Error =>
         Option_Error (Occur);
      when Overwrite_Error =>
         Option_Error ("File " & Value (Option => 'o') & " already exists, use ""-w"" to overwrite");
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
      return Implementation_Options.Initialize_String (Utilities.Debug_Option);
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
