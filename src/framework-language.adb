----------------------------------------------------------------------
--  Framework.Language - Package body                               --
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

----------------------------------------------------------------------
--  !!!  WARNING !!!                                                --
--                                                                  --
--  This  package  must  be the  target  of  a  pragma Elaborate    --
--  for all rules that instantiate one of its generics.             --
--                                                                  --
--  Therefore, this package must  not contain a statement part, nor --
--  call any outer function (or instantiate any generic) as part    --
--  of  the  elaboration of its declarations.                       --
--                                                                  --
--  The package cannot be made preelaborable due to dependencies    --
--  to non-preelaborable units.                                     --
--                                                                  --
-- (and  if you  don't understand  what this  stuff is  about, just --
--  stick to the rule!)                                             --
----------------------------------------------------------------------

-- Ada
with
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed;

-- Adactl
with
  Adactl_Options,
  Framework.Language.Commands,
  Framework.Language.Scanner,
  Framework.Reports,
  Framework.Rules_Manager,
  Framework.Variables.Shared_Types;
package body Framework.Language is
   use Framework.Language.Scanner, Utilities;

   -- Algorithm
   --
   -- This is a classical recursive descent parser, following the grammar given in the specification.
   -- Invariant:
   --   when a parsing subprogram is called, the current token is the first one it has to care about
   --   when a parsing subprogram is left, it leaves in the current token the first one that is not for it.

   ------------------------------------------------------
   --  Internal utilities                              --
   ------------------------------------------------------

   In_Parameters : Boolean := False;
   Last_Was_Go   : Boolean := True;
   -- False if any "controlling" command (check, search, count, set) has been entered
   -- since the last go command.


   -- Invariants for the following parsing functions:
   -- On entrance, Current_Token is the first token of the corresponding syntax
   --   This is checked by the function itself, not the caller.
   -- On exit, Current_Token is the first token not in the corresponding syntax

   --------------------
   -- Next_Parameter --
   --------------------

   procedure Next_Parameter is
   begin
      case Current_Token.Kind is
         when Comma =>
            Next_Token;
            -- In_Parameters remains True
         when Right_Parenthesis =>
            Next_Token;
            In_Parameters := False;
         when others =>
            Syntax_Error ("',' or ')' expected after parameter", Current_Token.Position);
      end case;
   end Next_Parameter;

   -------------------
   -- Get_Rule_Name --
   -------------------

   function Get_Rule_Name (Allow_All : Boolean := False) return Wide_String is
      use Framework.Rules_Manager;
   begin
      if Current_Token.Kind /= Name then
         Syntax_Error ("Rule identifier expected, found " & Image (Current_Token), Current_Token.Position);
      end if;

      declare
         Result : constant Wide_String := To_Upper (Image (Current_Token));
      begin
         if not Is_Rule_Name (Result) and then not (Allow_All and Result = "ALL") then
            Syntax_Error ("Not a rule name: " & Result, Current_Token.Position);
         end if;

         Next_Token;
         if Current_Token.Kind = Left_Parenthesis then
            Next_Token;
            In_Parameters := True;
         end if;
         return Result;
      end;
   end Get_Rule_Name;

   -------------------
   -- Close_Command --
   -------------------

   procedure Close_Command is
   begin
      if Current_Token.Kind /= Semi_Colon then
         Syntax_Error ("Semi-colon expected", Current_Token.Position);
      end if;

      Activate_Prompt;
      Next_Token;
   end Close_Command;

   -------------
   -- Compile --
   -------------

   -------------
   -- Compile --
   -------------

   procedure Compile is
      use Rules_Manager, Framework.Language.Commands, Framework.Variables, Framework.Variables.Shared_Types;

      procedure Process_Error (Occur : Ada.Exceptions.Exception_Occurrence) is
         use Ada.Exceptions, Ada.Characters.Handling;
      begin
         User_Message (To_Wide_String (Exception_Message (Occur)));
         Rule_Error_Occurred := True;
         -- Ignore till next semi-colon (or Eof)
         In_Parameters := False;
         loop
            case Current_Token.Kind is
               when Semi_Colon =>
                  Close_Command;
                  exit;
               when Eof =>
                  exit;
               when others =>
                  begin
                     Next_Token (No_Delay => True);
                  exception
                     when User_Error =>
                        -- Encountered bad characters => Ignore
                        null;
                  end;
            end case;
         end loop;
      end Process_Error;

      procedure Process_Controls (Label : in Wide_String) is
      -- Only controls (or '(') can follow a label
      begin
         if Label /= "" then
            Next_Token;
            if Current_Token.Kind /= Colon then
               if Current_Token.Kind = Name and then Current_Token.Key in Type_Keys then
                  Syntax_Error ("Missing "":"" after label", Current_Token.Position);
               else
                  Syntax_Error ("Unknown command " & Label, Current_Token.Position);
               end if;
            end if;
            Next_Token;
         end if;

         loop
            if Current_Token.Kind /= Name then
               Syntax_Error ("Unexpected element for control", Current_Token.Position);
            end if;

            case Current_Token.Key is
               when Key_Check =>
                  Next_Token;
                  Add_Control (Label, Check, Get_Rule_Name);
               when Key_Search =>
                  Next_Token;
                  Add_Control (Label, Search, Get_Rule_Name);
               when Key_Count =>
                  Next_Token;
                  Add_Control (Label, Count, Get_Rule_Name);
               when others =>
                  Syntax_Error ("Only ""Check"", ""Search"", or ""Count"" allowed for control",
                                Current_Token.Position);
            end case;

            if Current_Token.Kind /= Comma then
               Close_Command;
               exit;
            end if;

            Next_Token;
         end loop;
         Last_Was_Go := False;
      end Process_Controls;

   begin   -- Compile
      -- Set up initial token
      begin
         Next_Token (No_Delay => True);
         -- No_Delay is true to get the error here if there is a parse error in the first token
      exception
         when Occur : Utilities.User_Error =>
            Process_Error (Occur);
      end;

      loop
         begin
            case Current_Token.Kind is
               when Eof =>
                  exit;

               when Name =>
                  case Current_Token.Key is
                     when Key_Check =>
                        Process_Controls ("");

                     when Key_Clear =>
                        Next_Token;
                        if Current_Token.Kind /= Name then
                           Syntax_Error ("""all"" or Rule name expected", Current_Token.Position);
                        end if;

                        if Current_Token.Key = Key_All then
                           Next_Token;
                           Close_Command;

                           Command_All (Clear);

                        else
                           loop
                              Command (Image (Current_Token), Clear);
                              Next_Token;
                              exit when Current_Token.Kind /= Comma;
                              Next_Token;
                              if Current_Token.Kind /= Name then
                                 Syntax_Error ("Rule name expected", Current_Token.Position);
                              end if;
                           end loop;
                           Close_Command;
                        end if;

                     when Key_Count =>
                        Process_Controls ("");

                     when Key_Go =>
                        Next_Token;
                        Close_Command;
                        Last_Was_Go := True;

                        Go_Command;

                     when Key_Help =>
                        Next_Token (Force_String => True);
                        if Current_Token.Kind = Semi_Colon then
                           Close_Command;

                           Help_Command ("COMMANDS");
                           Help_Command ("RULES");
                        else
                           -- The simpler solution is to provide help messages as parameters are parsed,
                           -- but this gives unpleasant behaviour in interactive mode when there is a
                           -- syntax error. Therefore, we first accumulate names, then give all helps.
                           declare
                              use Ada.Strings, Ada.Strings.Wide_Fixed;
                              Line  : constant Wide_String := Image (Current_Token);
                              Start : Natural := Line'First;
                              Stop  : Natural;
                              Inx   : Rules_Count := 0;
                              Rule_Names : array (Rules_Count range 1 .. Number_Of_Rules) of Unbounded_Wide_String;
                           begin
                              loop
                                 if Inx = Rule_Names'Last then
                                    -- This can happen only if the user specified the same rule
                                    -- several times, and listed more names than there are rules (or used
                                    -- some of the special keywords in addition to rule names).
                                    -- Extremely unlikely in practice, but not a reason for not being careful...
                                    Syntax_Error ("Too many rule names in ""Help"" command", Current_Token.Position);
                                 end if;
                                 Inx := Inx + 1;
                                 Stop := Index (Line, ",", From => Start);
                                 if Stop = 0 then
                                    Stop := Line'Last+1;
                                 end if;
                                 Rule_Names (Inx) := To_Unbounded_Wide_String (Trim (Line (Start .. Stop-1), Both));
                                 Start := Stop + 1;
                                 exit when Start > Line'Last;
                              end loop;
                              Next_Token;

                              Help_Command (To_Wide_String (Rule_Names (1)));
                              for Name : Unbounded_Wide_String of Rule_Names (2 .. Inx) loop
                                 User_Message ("----");
                                 Help_Command (To_Wide_String (Name));
                              end loop;

                              -- Note: Close command *after* providing help, since in case of errors
                              -- we assume that the command is not yet closed (see handler)
                              Close_Command;
                           end;
                        end if;

                     when Key_Inhibit =>
                        Next_Token;

                        Inhibit_Command (Get_Rule_Name (Allow_All => True));
                        Close_Command;

                     when Key_Message =>
                        Next_Token;
                        declare
                           Mess : constant Wide_String := (if Current_Token.Kind = String_Value
                                                           then Image (Current_Token)
                                                           else "");
                           With_Pause : Boolean := False;
                        begin
                           if Current_Token.Kind = String_Value then
                              Next_Token;
                           end if;

                           if Is_String (Current_Token, "PAUSE") then
                              With_Pause := True;
                              Next_Token;
                           end if;

                           Close_Command;

                           Message_Command (Mess, With_Pause);
                        end;

                     when Key_Rule_File_Off =>
                        Next_Token;

                        if Current_Token.Kind /= String_Value then
                           Syntax_Error ("File name pattern expected", Current_Token.Position);
                        end if;

                        declare
                           Pattern : constant Wide_String := Image (Current_Token);
                        begin
                           Next_Token;

                           if Current_Token.Kind /= Name then
                              Syntax_Error ("""all"" or Rule name expected", Current_Token.Position);
                           end if;

                           if Current_Token.Key = Key_All then
                              Next_Token;
                              Close_Command;
                              File_Disable ("ALL", Pattern);

                           else
                              loop
                                 File_Disable (Image (Current_Token), Pattern);
                                 Next_Token;
                                 exit when Current_Token.Kind /= Comma;
                                 Next_Token;
                                 if Current_Token.Kind /= Name then
                                    Syntax_Error ("Rule name expected", Current_Token.Position);
                                 end if;
                              end loop;
                              Close_Command;
                           end if;
                        end;

                     when Key_Quit =>
                        Next_Token;
                        Close_Command;
                        exit;

                     when Key_Search =>
                        Process_Controls ("");

                     when Key_Set =>
                        Next_Token;
                        if Current_Token.Kind /= Name then
                           Syntax_Error ("Variable name expected", Current_Token.Position);
                        end if;

                        declare
                           Option : constant Wide_String := To_Upper (Image (Current_Token));
                        begin
                           -- Special options: file name, requires Next_Token (Force_String => True)
                           if Option in "OUTPUT" | "NEW_OUTPUT" then
                              Next_Token (Force_String => True);
                              if Current_Token.Kind /= Name then
                                 Syntax_Error ("File name expected", Current_Token.Position);
                              end if;
                              declare
                                 Output : constant Wide_String := Image (Current_Token);
                              begin
                                 Next_Token;
                                 Close_Command;

                                 Set_Output_Command (Output, Force_Overwrite => Option = "NEW_OUTPUT");
                              end;

                           elsif Option = "TRACE" then
                              Next_Token (Force_String => True);
                              if Current_Token.Kind /= Name then
                                 Syntax_Error ("File name expected", Current_Token.Position);
                              end if;
                              declare
                                 Trace : constant Wide_String := Image (Current_Token);
                              begin
                                 Next_Token;
                                 Close_Command;

                                 Set_Trace_Command (Trace);
                              end;

                           elsif Option = "FORMAT" then
                              Next_Token;

                              case Current_Token.Kind is
                                 when Name =>
                                    declare
                                       use Framework.Reports;
                                    begin
                                       Set_Output_Format (Image (Current_Token));
                                    exception
                                       when Constraint_Error =>
                                          Syntax_Error ("Unknown format identifier """ & Image (Current_Token) & '"',
                                                        Current_Token.Position);
                                    end;
                                 when String_Value =>
                                    Set_Variable ("FORMAT", Val => Image (Current_Token), Bounding => Exact);
                                 when others =>
                                    Syntax_Error ("Illegal value for Format, identifier or string expected",
                                                  Current_Token.Position);
                              end case;

                              Next_Token;
                              Close_Command;

                           else   -- Not file options, regular variables
                              Next_Token;

                              if Current_Token.Kind = Period then
                                 -- Rule variable
                                 Next_Token;
                                 if Current_Token.Kind /= Name then
                                    Syntax_Error ("Variable name expected", Current_Token.Position);
                                 end if;
                                 declare
                                    Variable : constant Wide_String := Option & '.' & Image (Current_Token);
                                    Bounding  : Bounding_Kind := Exact;
                                 begin
                                    Next_Token;
                                    if Current_Token.Kind = Name then
                                       if To_Upper (Image (Current_Token)) = "MIN" then
                                          Bounding := Min;
                                          Next_Token;
                                       elsif To_Upper (Image (Current_Token)) = "MAX" then
                                          Bounding := Max;
                                          Next_Token;
                                       end if;
                                    end if;

                                    if Current_Token.Kind in Value_Token_Kind then
                                       Set_Variable (Variable, Val => Image (Current_Token), Bounding => Bounding);
                                       Next_Token;
                                    else  -- default
                                       Set_Variable (Variable, Val => "", Bounding => Bounding);
                                    end if;
                                 exception
                                    when No_Such_Variable =>
                                       Syntax_Error ("Unknown variable " & Variable, Current_Token.Position);
                                    when Exact_Required =>
                                       Syntax_Error ("Min/Max not allowed for " & Variable, Current_Token.Position);
                                    when Constraint_Error =>
                                       Syntax_Error ("Illegal value for " & Variable & ": " & Image (Current_Token),
                                                     Current_Token.Position);
                                 end;
                              else
                                 declare
                                    Bounding  : Bounding_Kind := Exact;
                                 begin
                                    if Current_Token.Kind = Name then
                                       if To_Upper (Image (Current_Token)) = "MIN" then
                                          Bounding := Min;
                                          Next_Token;
                                       elsif To_Upper (Image (Current_Token)) = "MAX" then
                                          Bounding := Max;
                                          Next_Token;
                                       end if;
                                    end if;

                                    if Current_Token.Kind in Value_Token_Kind then
                                       Set_Variable (Option, Val => Image (Current_Token), Bounding => Bounding);
                                       Next_Token;
                                    else  -- default
                                       Set_Variable (Option, Val => "", Bounding => Bounding);
                                    end if;
                                 exception
                                    when No_Such_Variable =>
                                       Syntax_Error ("Unknown variable " & Option, Current_Token.Position);
                                    when Exact_Required =>
                                       Syntax_Error ("Min/Max not allowed for " & Option, Current_Token.Position);
                                    when Constraint_Error =>
                                       Syntax_Error ("Illegal value for " & Option & ": " & Image (Current_Token),
                                                     Current_Token.Position);
                                 end;
                              end if;

                              Close_Command;
                           end if;
                        end;
                        Last_Was_Go := False;

                        -- Mirror Debug and Verbose options
                        Utilities.Debug_Option   := Adactl_Options.Debug_Option.Value   = On;
                        Utilities.Verbose_Option := Adactl_Options.Verbose_Option.Value = On;

                     when Key_Source =>
                        Next_Token (Force_String => True);
                        if Current_Token.Kind /= Name then
                           Syntax_Error ("File name expected", Current_Token.Position);
                        end if;

                        declare
                           Source  : constant Wide_String := Image (Current_Token);
                           Pos     : constant Location    := Current_Token.Position;
                           Success : Boolean;
                        begin
                           Next_Token;
                           if Source = "" then
                              Success := True;
                           elsif Source (1) in '/' | '\'
                             or else (Source'Length >= 3
                                      and then Source (2) = ':'
                                      and then Source (3) in '/' | '\')
                           then
                              -- Absolute path
                              Source_Command (Source, Success);
                           else
                              -- Try it relative to the current file
                              Source_Command (Reference_Dir & Source, Success);
                              if not Success then
                                 -- Try it from path
                                 declare
                                    Path_Source : constant Wide_String := Locate_Regular_File (Source, "ADACTL_PATH");
                                 begin
                                    if Path_Source /= "" then
                                       Source_Command (Path_Source, Success);
                                    end if;
                                 end;
                              end if;
                           end if;

                           if Success then
                              Close_Command;
                           else
                              Syntax_Error ("Sourced file " & Source & " not found", Pos);
                           end if;
                        end;

                     when Not_A_Key
                        | Profile_Keys -- Profile keys allowed as labels
                          =>
                        -- Must be a label
                        Process_Controls (Image (Current_Token));
                  end case;

               when String_Value =>
                  Process_Controls (Image (Current_Token));

               when others =>
                  Syntax_Error ("Command or label expected", Current_Token.Position);
            end case;
         exception
            when Occur : Utilities.User_Error =>
               Process_Error (Occur);
         end;
      end loop;
   end Compile;

   ---------------------------------
   -- Common_Enumerated_Utilities --
   ---------------------------------

   generic
      type Flags is (<>);
      Prefix   : Wide_String := "";
      Box_Pos  : in Integer  := -1; -- 'Pos of the modifier that corresponds to "<>", or -1 if none
      Pars_Pos : in Integer  := -1; -- 'Pos of the modifier that corresponds to "()", or -1 if none
   package Common_Enumerated_Utilities is
      function Image (Item : Flags; In_Case : Utilities.Casing := Utilities.Upper_Case) return Wide_String;

      type Flag_Set is array (Flags) of Boolean;
      procedure Help_On_Flags (Header      : Wide_String := "";
                               Footer      : Wide_String := "";
                               Extra_Value : Wide_String := "NONE";
                               Expected    : Flag_Set    := (others => True));
   end Common_Enumerated_Utilities;

   package body Common_Enumerated_Utilities is
      function Image (Item : Flags; In_Case : Utilities.Casing := Utilities.Upper_Case) return Wide_String is
         Img : constant Wide_String := Flags'Wide_Image (Item);
      begin
         -- Remove prefix and adjust case
         if Flags'Pos (Item) = Box_Pos then
            return "<>";
         elsif Flags'Pos (Item) = Pars_Pos then
            return "()";
         elsif In_Case = Upper_Case then  -- Already upper case
            return Img (Prefix'Length + 1 .. Img'Last);
         else
            return Set_Casing (Img (Prefix'Length + 1 .. Img'Last), In_Case);
         end if;
      end Image;

      procedure Help_On_Flags (Header      : Wide_String := "";
                               Footer      : Wide_String := "";
                               Extra_Value : Wide_String := "NONE";
                               Expected    : Flag_Set    := (others => True))
      is
         -- Pretty print of values of flags.
         -- Values are arranged in columns.
         -- The number of columns is computed assuming that each column is True_Width wide,
         -- except for the first one that can contain Extra_Value if provided.
         -- then the actual width is adjusted to what is actually needed, to make it prettier
         -- looking.
         -- However, if the previous (pessimistic) computation would give only one column,
         -- we force the number of columns to 2, and see if it fits with actual lengths.
         -- If not, we force back the number of columns to 1.
         -- More sophisticated optimization would be overkill.
         Display_Width : constant := 79;
         True_Width    : constant Natural := Flags'Width - Prefix'Length;

         function Default_Nb_Col return Positive is
         begin
            if Extra_Value in "NONE" | "" then
               return Natural'Max (2, 1 + (Display_Width - Header'Length
                                            - True_Width - 3      -- Width of 1st col
                                           ) / (True_Width + 3)); -- 3 => " | "
            else
               return Natural'Max (2, 1 + (Display_Width - Header'Length
                                            - Natural'Max (True_Width, Extra_Value'Length) - 3 -- Width of 1st col
                                           ) / (True_Width + 3));                              -- 3 => " | "

            end if;
         end Default_Nb_Col;

         Buffer      : Wide_String (1 .. Display_Width);
         Index       : Natural;
         Nb_Col      : Natural := Default_Nb_Col;
         Col_Widthes : array (1 .. Nb_Col) of Natural := (others => 0);
         Current_Col : Natural;
         First_Flag  : Flags;
         Last_Flag   : Flags;
      begin  -- Help_On_Flags
         if Extra_Value = "NONE" then
            Current_Col := 1;
            First_Flag  := Flags'First;
         elsif Extra_Value = "" then
            Current_Col := 1;
            First_Flag  := Flags'Succ (Flags'First);
         else
            Col_Widthes (1) := Extra_Value'Length;
            Current_Col     := 2;
            First_Flag      := Flags'Succ (Flags'First);
         end if;

         -- We assume here that Expected /= Empty_Set (not worth checking)
         for I in reverse Expected'Range loop
            if Expected (I) then
               Last_Flag := I;
               exit;
            end if;
         end loop;

         for F in Flags range First_Flag .. Last_Flag loop
            if Expected (F) then
               declare
                  Length : constant Natural := Image (F)'Length;
               begin
                  if Length > Col_Widthes (Current_Col) then
                     Col_Widthes (Current_Col) := Length;
                  end if;
                  if Current_Col = Nb_Col then
                     Current_Col := 1;
                  else
                     Current_Col := Current_Col + 1;
                  end if;
               end;
            end if;
         end loop;

         -- 2 colums: it may have been forced, check if it fits
         if Nb_Col = 2
           and then Header'Length
                  + Col_Widthes (1) + 3
                  + Col_Widthes (2) + 3 > Display_Width
         then
            Nb_Col := 1;
            if Extra_Value = "NONE" then
               Col_Widthes (1) := True_Width;
            else
               Col_Widthes (1) := Natural'Max (True_Width, Extra_Value'Length);
            end if;
         end if;

         Buffer := (others => ' ');
         Buffer (1 .. Header'Length) := Header;
         Index := Header'Length;

         Current_Col := 1;
         if Extra_Value = "NONE" then
            First_Flag := Flags'First;
         elsif Extra_Value = "" then
            First_Flag := Flags'Succ (Flags'First);
         else
            Index := Index + 1;  -- Add space
            Buffer (Index + 1 .. Index + Extra_Value'Length) := Extra_Value;
            Index := Index + Col_Widthes (Current_Col) + 1;

            Buffer (Index + 1) := '|';
            Index := Index + 1;

            if Nb_Col = 1 then
               User_Message (Buffer (1 .. Index));
               Current_Col := 1;
               Buffer := (others => ' ');
               Index := Header'Length;
            else
               Current_Col := 2;
            end if;

            -- Gnat warns about Constraint_Error being raised by the following statement
            -- when instantiated with a Flag type that has only one value.
            -- But in this case, Extra_Value must be "NONE", so it is OK.
            pragma Warnings (Off);
            First_Flag := Flags'Succ (Flags'First);
            pragma Warnings (On);
         end if;

         for I in Flags range First_Flag .. Last_Flag loop
            if Expected (I) then
               declare
                  Img : constant Wide_String := Image (I, Lower_Case);
               begin
                  Index := Index + 1;  -- Add space

                  Buffer (Index + 1 .. Index + Img'Length) := Img;
                  if I = Last_Flag then
                     Index := Index + Img'Length;
                     User_Message (Buffer (1 .. Index));
                     exit;
                  end if;

                  Index := Index + Col_Widthes (Current_Col) + 1;
                  Buffer (Index + 1) := '|';
                  Index := Index + 1;

                  if Current_Col = Nb_Col then
                     User_Message (Buffer (1 .. Index));
                     Current_Col := 1;
                     Buffer := (others => ' ');
                     Index := Header'Length;
                  else
                     Current_Col := Current_Col + 1;
                  end if;
               end;
            end if;
         end loop;

         if Footer /= "" then
            User_Message ((1 .. Header'Length + 1 => ' ') & Footer);
         end if;
      end Help_On_Flags;
   end Common_Enumerated_Utilities;

   ------------------------------------------------------
   --  Exported subprograms                            --
   ------------------------------------------------------

   -------------
   -- Execute --
   -------------

   procedure Execute (Command_String : Wide_String) is
   begin
      Set_Prompt ("");
      Start_Scan (From_String => True, Source => Command_String);
      Compile;
   end Execute;

   ---------------------
   -- Source_Location --
   ---------------------

   function Source_Location return Location is
   begin
      return Current_Token.Position;
   end Source_Location;

   ----------------------
   -- Parameter_Exists --
   ----------------------

   function Parameter_Exists return Boolean is
   begin
      return In_Parameters;
   end Parameter_Exists;

   --------------------------
   -- Is_Integer_Parameter --
   --------------------------

   function Is_Integer_Parameter return Boolean is
   begin
      if not In_Parameters then
         Failure ("Is_Integer_Parameter called when not in parameters");
      end if;

      return Current_Token.Kind = Integer_Value;
   end Is_Integer_Parameter;

   ------------------------
   -- Is_Float_Parameter --
   ------------------------

   function Is_Float_Parameter return Boolean is
   begin
      if not In_Parameters then
         Failure ("Is_Float_Parameter called when not in parameters");
      end if;

      return Current_Token.Kind = Float_Value;
   end Is_Float_Parameter;

   ------------------------
   -- Is_String_Parameter --
   ------------------------

   function Is_String_Parameter return Boolean is
   begin
      if not In_Parameters then
         Failure ("Is_Sting_Parameter called when not in parameters");
      end if;

      return Current_Token.Kind = String_Value;
   end Is_String_Parameter;

   ---------------------------
   -- Get_Integer_Parameter --
   ---------------------------

   function Get_Integer_Parameter (Min : Thick_Queries.Biggest_Int := Thick_Queries.Biggest_Int'First;
                                   Max : Thick_Queries.Biggest_Int := Thick_Queries.Biggest_Int'Last)
                                   return Thick_Queries.Biggest_Int
   is
      Result : constant Thick_Queries.Biggest_Int := Get_Integer_Modifier (Min, Max);
   begin
      Next_Parameter;
      return Result;
   end Get_Integer_Parameter;

   function Get_Integer_Parameter (Min : Asis.ASIS_Integer := Asis.ASIS_Integer'First;
                                   Max : Asis.ASIS_Integer := Asis.ASIS_Integer'Last)
                                   return Asis.ASIS_Integer
   is
      Result : constant Asis.ASIS_Integer := Get_Integer_Modifier (Min, Max);
   begin
      Next_Parameter;
      return Result;
   end Get_Integer_Parameter;


   --------------------------
   -- Get_Integer_Modifier --
   --------------------------

   function Get_Integer_Modifier (Min : Thick_Queries.Biggest_Int := Thick_Queries.Biggest_Int'First;
                                  Max : Thick_Queries.Biggest_Int := Thick_Queries.Biggest_Int'Last)
                                  return Thick_Queries.Biggest_Int
   is
      use Thick_Queries;
   begin
      if not In_Parameters then
         Failure ("Get_Integer_Parameter called when not in parameters");
      end if;

      case Current_Token.Kind is
         when Integer_Value =>
            declare
               Result : constant Biggest_Int := Current_Token.Value;
            begin
               Next_Token;
               if Result not in Min .. Max then
                  if Max = Biggest_Int'Last then
                     Syntax_Error ("Parameter must be >= "
                                   & Biggest_Int_Img (Min),
                                   Current_Token.Position);
                  elsif Min = Biggest_Int'First then
                     Syntax_Error ("Parameter must be <= "
                                   & Biggest_Int_Img (Max),
                                   Current_Token.Position);
                  else
                     Syntax_Error ("Parameter must be in range "
                                   & Biggest_Int_Img (Min)
                                   & " .. "
                                   & Biggest_Int_Img (Max),
                                   Current_Token.Position);
                  end if;
               end if;
               return Result;
            end;
         when Float_Value =>
            Syntax_Error ("Integer value expected", Current_Token.Position);
         when Bad_Integer =>
            Syntax_Error ("Bad integer value (too many digits?)", Current_Token.Position);
         when Name | Bad_Float =>
            Syntax_Error ("Integer parameter expected", Current_Token.Position);
         when others =>
            Syntax_Error ("Parameter expected", Current_Token.Position);
      end case;
   end Get_Integer_Modifier;

   function Get_Integer_Modifier (Min : Asis.ASIS_Integer := Asis.ASIS_Integer'First;
                                  Max : Asis.ASIS_Integer := Asis.ASIS_Integer'Last)
                                  return Asis.ASIS_Integer
   is
      use Thick_Queries;
      use type Asis.ASIS_Integer;   --## rule line off Unnecessary_Use_Clause Reduceable_Scope ## Gela compatibility
      Result : constant Biggest_Int := Get_Integer_Modifier;
   begin
      if Result not in Biggest_Int (Min) .. Biggest_Int (Max) then
         if Max = Asis.ASIS_Integer'Last then
            Syntax_Error ("Parameter must be >= "
                          & ASIS_Integer_Img (Min),
                          Current_Token.Position);
         elsif Min = Asis.ASIS_Integer'First then
            Syntax_Error ("Parameter must be <= "
                          & ASIS_Integer_Img (Max),
                          Current_Token.Position);
         else
            Syntax_Error ("Parameter must be in range "
                          & ASIS_Integer_Img (Min)
                          & " .. "
                          & ASIS_Integer_Img (Max),
                          Current_Token.Position);
         end if;
      end if;
      return Asis.ASIS_Integer (Result);
   end Get_Integer_Modifier;


   -------------------------
   -- Get_Float_Parameter --
   -------------------------

   function Get_Float_Parameter return Float is
   begin
      if not In_Parameters then
         Failure ("Get_Float_Parameter called when not in parameters");
      end if;

      case Current_Token.Kind is
         when Float_Value =>
            declare
               Result : constant Float := Current_Token.Fvalue;
            begin
               Next_Token;
               Next_Parameter;
               return Result;
            end;
         when Integer_Value =>
            -- Well, we can accept it...
            declare
               Result : constant Float := Float (Current_Token.Value);
            begin
               Next_Token;
               Next_Parameter;
               return Result;
            end;
         when Bad_Integer | Bad_Float =>
            Syntax_Error ("Bad real value (too many digits?)", Current_Token.Position);
         when Name =>
            Syntax_Error ("Float parameter expected", Current_Token.Position);
         when others =>
            Syntax_Error ("Parameter expected", Current_Token.Position);
      end case;
   end Get_Float_Parameter;

   ------------------------
   -- Get_Name_Parameter --
   ------------------------

   function Get_Name_Parameter return Wide_String is
      Initial_Tick : Boolean := False;
   begin
      if not In_Parameters then
         Failure ("Get_Name_Parameter called when not in parameters");
      end if;

      if Current_Token.Kind = String_Value then
         -- Take it as an operator's name
         declare
            Result : constant Wide_String := '"' & To_Upper (Image (Current_Token)) & '"';
         begin
            Next_Token;
            Next_Parameter;
            return Result;
         end;
      end if;

      if Current_Token.Kind = Tick then
         Initial_Tick := True;
         Next_Token;
      end if;

      if Current_Token.Kind /= Name then
         Syntax_Error ("Name expected", Current_Token.Position);
      end if;

      declare
         Result : constant Wide_String := To_Upper (Image (Current_Token));
      begin
         Next_Token;
         if Current_Token.Kind = Tick then
            -- We must accept 'class'input...
            return Choose (Initial_Tick, "'", "") & Result & Get_Name_Parameter;
         else
            Next_Parameter;
            return Choose (Initial_Tick, "'", "") & Result;
         end if;
      end;
   end Get_Name_Parameter;

   --------------------------
   -- Get_String_Parameter --
   --------------------------

   function Get_String_Parameter return Wide_String is
      Result : constant Wide_String := Get_String_Modifier;
   begin
      Next_Parameter;
      return Result;
   end Get_String_Parameter;

   -------------------------
   -- Get_String_Modifier --
   -------------------------

   function Get_String_Modifier return Wide_String is
   begin
      if not In_Parameters then
         Failure ("Get_String_Parameter called when not in parameters");
      end if;

      if Current_Token.Kind /= String_Value then
         Syntax_Error ("String expected", Current_Token.Position);
      end if;

      declare
         Result : constant Wide_String := Image (Current_Token);
      begin
         Next_Token;
         return Result;
      end;
   end Get_String_Modifier;

   --------------------------
   -- Get_Entity_Parameter --
   --------------------------

   function Get_Entity_Parameter (Allow_Extended : Entity_Specification_Kinds_Set := Nothing_OK;
                                  Ghost          : Wide_String := "") return Entity_Specification
   is
      Result : constant Entity_Specification := Get_Entity_Modifier (Allow_Extended, Ghost);
   begin
      Next_Parameter;
      return Result;
   end Get_Entity_Parameter;

   -------------------------
   -- Get_Entity_Modifier --
   -------------------------

   function Get_Entity_Modifier (Allow_Extended : Entity_Specification_Kinds_Set := Nothing_OK;
                                 Ghost          : Wide_String := "") return Entity_Specification
   is
      -- Information set by the parsing functions:
      Qualified  : Boolean;

      -- Forward declarations:
      function Full_Name return Wide_String;
      function Profile return Wide_String;

      function Identifier return Wide_String is
      begin
         case Current_Token.Kind is
            when Name =>
               declare
                  Name : constant Wide_String := To_Upper (Image (Current_Token));
               begin
                  Next_Token;
                  return Name;
               end;
            when String_Value =>
               -- Assume it is an operator
               declare
                  Name : constant Wide_String := '"' & To_Upper (Image (Current_Token)) & '"';
               begin
                  Next_Token;
                  return Name;
               end;
            when others =>
               Syntax_Error ("Identifier expected", Current_Token.Position);
         end case;
      end Identifier;

      function Type_Spec return Wide_String is
         type Access_Forms is (No_Access, Access_Object, Access_Function, Access_Procedure);
         subtype Access_SP is Access_Forms range Access_Function .. Access_Procedure;
         Access_Form : Access_Forms := No_Access;

         function Formatted_Name (Name : Wide_String; Add_Standard : Boolean) return Wide_String is
         begin
            case Access_Form is
               when No_Access =>
                  return Choose (Add_Standard, "STANDARD.", "") & Name;
               when Access_Object =>
                  return "*O" & Choose (Add_Standard, "STANDARD.", "") & Name;
               when  Access_Function =>
                  return "*F" & Choose (Add_Standard, "STANDARD.", "") & Name;
               when Access_Procedure =>
                  return "*P" & Choose (Add_Standard, "STANDARD.", "") & Name;
            end case;
         end Formatted_Name;

      begin   -- Type_Spec
         if Current_Token.Kind = Name and then Current_Token.Key = Key_Access then
            Next_Token;
            if Current_Token.Kind = Name then
               case Current_Token.Key is
                  when Key_Procedure =>
                     Access_Form := Access_Procedure;
                     Next_Token;
                  when Key_Function =>
                     Access_Form := Access_Function;
                     Next_Token;
                  when others =>
                     Access_Form := Access_Object;
               end case;
            else
               Access_Form := Access_Object;
            end if;
         end if;

         if Access_Form in Access_SP then
            -- no identifier, just a profile
            if Current_Token.Kind /= Left_Bracket then
               Syntax_Error ("""{"" expected", Current_Token.Position);
            end if;
            Next_Token;

            if Current_Token.Kind = Right_Bracket then
               Next_Token;
               return Formatted_Name ("{}", Add_Standard => False);
            end if;

            declare
               Profile1 : constant Wide_String := Profile;
            begin
               if Current_Token.Kind /= Right_Bracket then
                  Syntax_Error ("Missing ""}""", Current_Token.Position);
               end if;

               Next_Token;
               return Formatted_Name ('{' & Profile1 & '}', Add_Standard => False);
            end;

         else
            -- If not qualified, assume the identifier is declared in Standard
            Qualified := False;
            declare
               Raw_Name : constant Wide_String := Full_Name;  -- Intermediate necessary to ensure evaluation order
            begin
               return Formatted_Name (Raw_Name, Add_Standard => not Qualified);
            end;
         end if;
      end Type_Spec;

      function Profile return Wide_String is

         function Parameter_List return Wide_String is
            Parameter1 : constant Wide_String := Type_Spec;
         begin  -- Parameter_List
            if Current_Token.Kind = Semi_Colon then
               Next_Token;
               return Parameter1 & ';' & Parameter_List;
            else
               return Parameter1;
            end if;
         end Parameter_List;

      begin  -- Profile
         if Current_Token.Kind = Name and then Current_Token.Key = Key_Return then
            -- return alone, no parameters
            Next_Token;
            return ':' & Type_Spec;
         end if;

         declare
            List1 : constant Wide_String := Parameter_List;
         begin
            if Current_Token.Kind /= Name or else Current_Token.Key /= Key_Return then
               return List1;
            end if;

            -- We have a "return" here
            Next_Token;
            return List1 & ':' & Type_Spec;
         end;
      end Profile;

      function Typed_Name return Wide_String is
         Name1 : constant Wide_String := Identifier;
      begin
         if Current_Token.Kind /= Left_Bracket then
            return Name1;
         end if;

         Next_Token;
         if Current_Token.Kind = Right_Bracket then
            Next_Token;
            return Name1 & "{}";
         end if;

         declare
            Profile1 : constant Wide_String := Profile;
         begin
            if Current_Token.Kind /= Right_Bracket then
               Syntax_Error ("Missing ""}""", Current_Token.Position);
            end if;

            Next_Token;
            return Name1 & '{' & Profile1 & '}';
         end;
      end Typed_Name;

      function Attributed_Name return Wide_String is
         function Attribute_List return Wide_String is
            Name1 : constant Wide_String := Identifier;
         begin
            if Current_Token.Kind = Tick then
               Next_Token;
               return Name1 & ''' & Attribute_List;
            else
               return Name1;
            end if;
         end Attribute_List;

         Name1 : constant Wide_String := Typed_Name;
      begin   -- Attributed_Name
         if Current_Token.Kind = Tick then
            Next_Token;
            return Name1 & ''' & Attribute_List;
         else
            return Name1;
         end if;
      end Attributed_Name;

      function Full_Name return Wide_String is
         Ident1 : constant Wide_String := Attributed_Name;
      begin
         if Current_Token.Kind = Period then
            Next_Token;
            Qualified := True;
            return Ident1 & '.' & Full_Name;
         else
            return Ident1;
         end if;
      end Full_Name;

   begin  -- Get_Entity_Modifier
      if not In_Parameters then
         Failure ("Get_Entity_Parameter called when not in parameters");
      end if;

      case Current_Token.Kind is
         when Left_Angle =>
            if not Allow_Extended (Box) then
               Syntax_Error ("Entity name expected", Current_Token.Position);
            end if;

            Next_Token;
            if Current_Token.Kind /= Right_Angle then
               Syntax_Error (""">"" expected", Current_Token.Position);
            end if;

            Next_Token;
            return (Kind => Box);

         when Equal =>
            if not Allow_Extended (Equal) then
               Syntax_Error ("Entity name expected", Current_Token.Position);
            end if;

            Next_Token;
            return (Kind => Equal);

         when Left_Parenthesis =>
            if not Allow_Extended (Regular_Id) then
               Syntax_Error ("Entity name expected", Current_Token.Position);
            end if;

            Next_Token;
            if Current_Token.Kind /= Right_Parenthesis then
               Syntax_Error (""")"" expected", Current_Token.Position);
            end if;

            Next_Token;
            return (Kind          => Regular_Id,
                    Specification => To_Unbounded_Wide_String ("()"));

         when String_Value =>
            -- Can be an operator
            return (Kind          => Regular_Id,
                    Specification => To_Unbounded_Wide_String (Full_Name));

         when Name =>
            if Current_Token.Key /= Key_All then
               -- Normal case, no "all"
               return (Kind          => Regular_Id,
                       Specification => To_Unbounded_Wide_String (Full_Name));
            end if;

            -- "all"
            Next_Token;
            if Current_Token.Kind = Tick then
               -- "all 'image"
               Next_Token;
               return (Kind          => All_Id,
                       Specification => To_Unbounded_Wide_String (''' & Identifier));
            else
               return (Kind          => All_Id,
                       Specification => To_Unbounded_Wide_String (Attributed_Name));
            end if;

         when Comma | Right_Parenthesis =>
            -- "ghost" parameter
            if Ghost /= "" then
               return Value (Ghost);
            end if;

            Syntax_Error ("Entity specification expected", Current_Token.Position);

         when others =>
            Syntax_Error ("Entity specification expected", Current_Token.Position);
      end case;
   end Get_Entity_Modifier;

   ------------------------
   -- Get_File_Parameter --
   ------------------------

   function Get_File_Parameter return Wide_String is
      Name : constant Wide_String := Get_String_Parameter;
   begin
      if Name = "" then
         Syntax_Error ("Empty file name", Current_Token.Position);
      end if;

      if Name (1) in '/' | '\'
        or else (Name'Length >= 3 and then Name (2) = ':' and then Name (3) in '/' | '\')
      then
         -- Absolute path
         return Name;
      end if;

      -- Here we have a relative path, make it relative to the directory of the rules file
     return Reference_Dir & Name;
   end Get_File_Parameter;

   ------------------------
   -- Get_Null_Parameter --
   ------------------------

   procedure Get_Null_Parameter is
   begin
      Next_Parameter;
   end Get_Null_Parameter;

   ------------------
   -- Get_Modifier --
   ------------------

   function Get_Modifier (True_KW  : Wide_String;
                          False_KW : Wide_String := "";
                          Default  : Boolean := False) return Boolean
   is
   begin
      if Current_Token.Kind = Name then
         if To_Upper (Image (Current_Token)) = True_KW then
            Next_Token;
            return True;
         elsif To_Upper (Image (Current_Token)) = False_KW then
            Next_Token;
            return False;
         end if;
      end if;
      return Default;
   end Get_Modifier;

   ------------------------
   -- Modifier_Utilities --
   ------------------------

   package body Modifier_Utilities is
      package Local_Utilities is new Common_Enumerated_Utilities (Modifiers, Prefix, Box_Pos, Pars_Pos);

      procedure Get_Modifier (Modifier : out Modifiers; Found : out Boolean; Expected : in Modifier_Set) is
      begin
         case Current_Token.Kind is
            when Name =>
               declare
                  To_Compare : constant Wide_String := To_Upper (Prefix & Image (Current_Token));
               begin
                  for Idx in Modifiers loop
                     if Expected (Idx) and then To_Compare = Modifiers'Wide_Image (Idx) then
                        Next_Token;
                        Modifier := Idx;
                        Found    := True;
                        return;
                     end if;
                  end loop;
               end;
            when Left_Angle =>
               if Box_Pos >= 0 and then Expected (Modifiers'Val (Box_Pos)) then
                  Next_Token;
                  if Current_Token.Kind /= Right_Angle then
                     Syntax_Error (""">"" Expected", Current_Token.Position);
                  end if;
                  Next_Token;
                  Modifier := Modifiers'Val (Box_Pos);
                  Found    := True;
                  return;
               end if;
            when Left_Parenthesis =>
               if Pars_Pos >= 0 and then Expected (Modifiers'Val (Pars_Pos)) then
                  Next_Token;
                  if Current_Token.Kind /= Right_Parenthesis then
                     Syntax_Error (""")"" Expected", Current_Token.Position);
                  end if;
                  Next_Token;
                  Modifier := Modifiers'Val (Pars_Pos);
                  Found    := True;
                  return;
               end if;
            when others =>
               null;
         end case;
         Found := False;
      end Get_Modifier;

      function Get_Modifier (Required : Boolean;
                             Expected : Modifier_Set := Full_Set;
                             Default  : Modifiers    := Modifiers'First) return Modifiers
      is
         Present : Boolean;
         Result  : Modifiers;
      begin
         if not In_Parameters then
            Failure ("Get_Modifier called when not in parameters");
         end if;

         Get_Modifier (Result, Present, Expected);
         if Present then
            return Result;
         elsif Required then
            Syntax_Error ("modifier expected", Current_Token.Position);
         else
            return Default;
         end if;
      end Get_Modifier;

      procedure Get_Negatable_Modifier (Modifier : out Modifiers;
                                        Negated  : out Boolean;
                                        Found    : out Boolean;
                                        Expected : in Modifier_Set)
      is
      begin
         Negated := Get_Modifier ("NOT");
         Get_Modifier (Modifier, Found, Expected);
         if not Found and Negated then
            Back_Token;
         end if;
      end Get_Negatable_Modifier;

      function Get_Modifier_Set (No_Parameter : Boolean         := False;
                                 Expected     : Modifier_Set    := Full_Set;
                                 Getter       : Modifier_Getter := null)
                                 return Modifier_Set
      is
         Result         : Modifier_Set := Empty_Set;
         Modifier       : Modifiers;
         Next_Modifiers : Modifier_Set;
         Present        : Boolean;
      begin
         if not In_Parameters then
            Failure ("Get_Modifier_Set called when not in parameters");
         end if;

         loop
            if Getter = null then
               Get_Modifier (Modifier, Present, Expected);
               exit when not Present;
               Result (Modifier) := True;
            else
               Getter (Next_Modifiers, Present, Expected);
               exit when not Present;
               Result := Result or Next_Modifiers;
            end if;

            if No_Parameter then
               -- separating '|' required
               case Current_Token.Kind is
                  when Vertical_Bar =>
                     Next_Token;
                     if Current_Token.Kind /= Name then
                        Syntax_Error ("Keyword expected after '|'", Current_Token.Position);
                     end if;
                  when Name =>
                     -- This branch not strictly necessary, but gives a more user-friendly message
                     Syntax_Error ("'|' expected between keywords", Current_Token.Position);
                  when others =>
                     exit;
               end case;
            end if;
         end loop;

         if No_Parameter then
            if Result = Empty_Set then
               Syntax_Error ("Keyword expected, use option -h <rule name> for a list of allowable keywords",
                             Current_Token.Position);
            end if;
            Next_Parameter;
         end if;

         return Result;
      end Get_Modifier_Set;

      function Image (Item    : Modifiers;
                      In_Case : Utilities.Casing := Utilities.Upper_Case) return Wide_String
                      renames Local_Utilities.Image;

      procedure Help_On_Modifiers (Header      : Wide_String := "";
                                   Footer      : Wide_String := "";
                                   Extra_Value : Wide_String := "NONE";
                                   Expected    : Modifier_Set := Full_Set)
      is
      begin
         Local_Utilities.Help_On_Flags (Header, Footer, Extra_Value, Local_Utilities.Flag_Set (Expected));
      end Help_On_Modifiers;

      function Image (Set     : Unconstrained_Modifier_Set;
                      Default : Unconstrained_Modifier_Set := Empty_Set) return Wide_String
      is
      begin
         if Set = (Set'Range => False) or else Set = Default then
            return "";
         elsif Set'First = Set'Last then
            -- only one element
            return Image (Set'First, Lower_Case) & ' ';
         else
            for M in Modifiers range Set'First .. Modifiers'Pred (Set'Last) loop
               if Set (M) then
                  return Image (M, Lower_Case) & ' ' & Image (Set (Modifiers'Succ (M) .. Set'Last));
               end if;
            end loop;
            -- If we are here, Set (Set'Last) is the only True element
            return Image (Set'Last, Lower_Case) & ' ';
         end if;
      end Image;

      function Get_Modifier_List (Expected : Modifier_Set := Full_Set) return Modifier_List is
         Modifier : Modifiers;
         Present  : Boolean;

      begin
         if not In_Parameters then
            Failure ("Get_Modifier_List called when not in parameters");
         end if;

         Get_Modifier (Modifier, Present, Expected);
         if not Present then
            return Empty_List;
         end if;

         return Modifier & Get_Modifier_List (Expected);
      end Get_Modifier_List;

      function Image (List : Modifier_List) return Wide_String is
         use type Asis.ASIS_Integer;   --## rule line off Unnecessary_Use_Clause Reduceable_Scope ## Gela compatibility
      begin
         if List = Empty_List then
            return "";
         end if;

         return Image (List (List'First), Lower_Case)
           & ' '
           & Image (List (List'First + 1 .. List'Last));
      end Image;

   end Modifier_Utilities;

   --------------------
   -- Flag_Utilities --
   --------------------

   package body Flag_Utilities is
      package Local_Utilities is new Common_Enumerated_Utilities (Flags, Prefix);

      ------------------------
      -- Get_Flag_Parameter --
      ------------------------

      function Get_Flag_Parameter (Allow_Any : Boolean) return Flags is
      begin
         if not In_Parameters then
            Failure ("Get_Flag_Parameter called when not in parameters");
         end if;

         if Current_Token.Kind = Name then
            declare
               To_Compare : constant Wide_String := To_Upper (Prefix & Image (Current_Token));
            begin
               for Key in Flags loop
                  if To_Compare = Flags'Wide_Image (Key) then
                     if Allow_Any and then Key = Flags'First then
                        -- Oops, the user specified the special value
                        Syntax_Error ("Not a valid parameter: " & Image (Current_Token),
                                      Current_Token.Position);
                     end if;

                     Next_Token;
                     Next_Parameter;
                     return Key;
                  end if;
               end loop;
            end;
         end if;

         -- Here: not a Name, or unrecognized keyword
         if Allow_Any then
            -- Keep the current token
            return Flags'First;
         end if;

         if Current_Token.Kind = Name then
            Syntax_Error ("Unknown keyword """
                            & Image (Current_Token)
                            & """, use option -h <rule name> for a list of allowable keywords",
                          Current_Token.Position);
         else
            Syntax_Error ("Keyword expected, use option -h <rule name> for a list of allowable keywords",
                          Current_Token.Position);
         end if;
      end Get_Flag_Parameter;

      -----------
      -- Image --
      -----------

      function Image (Item : Flags; In_Case : Utilities.Casing := Utilities.Upper_Case) return Wide_String
                      renames Local_Utilities.Image;

      -------------------
      -- Help_On_Flags --
      -------------------

      procedure Help_On_Flags (Header      : Wide_String := "";
                               Footer      : Wide_String := "";
                               Extra_Value : Wide_String := "NONE")
      is
      begin
         Local_Utilities.Help_On_Flags (Header, Footer, Extra_Value);
      end Help_On_Flags;
   end Flag_Utilities;

   ---------------------
   -- Parameter_Error --
   ---------------------

   procedure Parameter_Error (Rule : Wide_String; Message : Wide_String) is
   begin
      Parameter_Error (Rule, Message, Current_Token.Position);
   end Parameter_Error;

   procedure Parameter_Error (Rule : Wide_String; Message : Wide_String; Position : Location) is
   begin
      Error (Image (Position) & ": "
             & "Parameter: "
             & Rule & ": "
             & Message);
   end Parameter_Error;

   ------------------
   -- Syntax_Error --
   ------------------

   procedure Syntax_Error (Message : Wide_String; Position : Location) is
   begin
      Error (Image (Position) & ": "
               & "Syntax: "
               & Message);
   end Syntax_Error;

   ----------------------
   -- Go_Command_Found --
   ----------------------

   function Go_Command_Found return Boolean is
   begin
      return Last_Was_Go;
   end Go_Command_Found;

   -----------------
   -- Had_Failure --
   -----------------

   function Had_Failure return Boolean is
   begin
      return Failure_Occurred;
   end Had_Failure;

   ----------------
   -- Had_Errors --
   ----------------

   function Had_Errors return Boolean is
   begin
      return Rule_Error_Occurred;
   end Had_Errors;

end Framework.Language;
