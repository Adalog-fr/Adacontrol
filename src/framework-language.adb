----------------------------------------------------------------------
--  Framework.Language - Package body                               --
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
  Ada.IO_Exceptions,
  Ada.Strings.Wide_Fixed;

-- Adalog
with
  Utilities;

-- Adactl
with
  Adactl_Options,
  Framework.Language.Commands,
  Framework.Language.Scanner,
  Framework.Rules_Manager,
  Ruler;
package body Framework.Language is
   use Framework.Language.Scanner, Utilities;

   ------------------------------------------------------
   --  Internal utilities                              --
   ------------------------------------------------------

   In_Parameters : Boolean := False;
   Last_Was_Go   : Boolean;


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
            Syntax_Error ("Unexpected element after parameter", Current_Token.Position);
      end case;
   end Next_Parameter;

   -------------------
   -- Get_Rule_Name --
   -------------------

   function Get_Rule_Name return Wide_String is
   begin
      if Current_Token.Kind /= Name then
         Syntax_Error ("Rule identifier expected", Current_Token.Position);
      end if;

      declare
         use Framework.Rules_Manager;
         Result : constant Wide_String := To_Upper (Current_Token.Text (1..Current_Token.Length));
      begin
         if Is_Rule_Name (Result) then
            Next_Token;
            if Current_Token.Kind = Left_Parenthesis then
               Next_Token;
               In_Parameters := True;
            end if;
            return Result;
         else
            Syntax_Error ("Not a rule name: " & Result, Current_Token.Position);
         end if;
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

   procedure Compile is
      use Rules_Manager, Framework.Language.Commands, Ada.IO_Exceptions;
   begin
      -- Set up initial token
      Next_Token;

      while Current_Token.Kind /= EoF loop
         Last_Was_Go := False;

         if Current_Token.Kind /= Name then
            Syntax_Error ("Command or label expected", Current_Token.Position);
         end if;

         case Current_Token.Key is
            when Key_Check =>
               Next_Token;
               Add_Use ("", Check, Get_Rule_Name);
               Close_Command;

            when Key_Clear =>
               Next_Token;
               if Current_Token.Kind /= Name then
                  Syntax_Error ("""all"", ""counts"", or Rule name expected", Current_Token.Position);
               end if;

               if Current_Token.Key = Key_All then
                  Next_Token;
                  Close_Command;

                  Command_All (Clear);
               else
                  loop
                     Command (Current_Token.Text (1 .. Current_Token.Length), Clear);
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
               Next_Token;
               Add_Use ("", Count, Get_Rule_Name);
               Close_Command;

            when Key_Go =>
               Next_Token;
               Close_Command;

               Last_Was_Go := True;
               Go_Command;

            when Key_Help =>
               Next_Token;
               if Current_Token.Kind = Semi_Colon then
                  Close_Command;

                  User_Message ("Commands:");
                  Help_Command;
                  User_Message ("Rules:");
                  Help_Names;

               elsif Current_Token.Kind = Name and then Current_Token.Key = Key_All then
                  Next_Token;
                  Close_Command;

                  Help_All;

               else
                  -- The simpler solution is to provide help messages as rule names are parsed,
                  -- but this gives unpleasant behaviour in interactive mode when there is a
                  -- syntax error. Therefore, we first accumulate names, then give all helps.
                  declare
                     Rule_Names : array (1 .. Number_Of_Rules) of Unbounded_Wide_String;
                     Inx        : Natural := 0;
                  begin
                     loop
                        if Current_Token.Kind /= Name then
                           Syntax_Error ("Rule name expected", Current_Token.Position);
                        end if;
                        if Inx = Rule_Names'Last then
                           -- This can happen only if the user specified the same rule
                           -- several times, and listed more names than there are rules.
                           -- Extremely unlikely in practice, but not a reason for not being careful...
                           Syntax_Error ("Too many rule names in ""Help"" command", Current_Token.Position);
                        end if;
                        Inx := Inx + 1;
                        Rule_Names (Inx) := To_Unbounded_Wide_String (Current_Token.Text
                                                                      (1 .. Current_Token.Length));
                        Next_Token;
                        exit when Current_Token.Kind /= Comma;
                        Next_Token;
                     end loop;
                     Close_Command;

                     for I in 1 .. Inx loop
                        Help (To_Wide_String (Rule_Names (I)));
                     end loop;
                  end;
               end if;

            when Key_Inhibit =>
               Next_Token;
               Ruler.Inhibit (Get_Rule_Name);
               Close_Command;

            when Key_Message =>
               Next_Token (Force_String => True);
               if Current_Token.Kind /= Name then
                  Syntax_Error ("Message expected", Current_Token.Position);
               end if;
               declare
                  Mess : constant Wide_String := Current_Token.Text (1 .. Current_Token.Length);
               begin
                  Next_Token;
                  Close_Command;

                  User_Message (Mess);
               end;

            when Key_Quit =>
               Next_Token;
               Close_Command;
               exit;

            when Key_Search =>
               Next_Token;
               Add_Use ("", Search, Get_Rule_Name);
               Close_Command;

            when Key_Set =>
               Next_Token;
               declare
                  Option : constant Wide_String := To_Upper (Current_Token.Text (1 .. Current_Token.Length));
                  State  : Boolean;
                  use Adactl_Options;
               begin
                  if Option = "OUTPUT" then
                     Next_Token (Force_String => True);
                     if Current_Token.Kind /= Name then
                        Syntax_Error ("File name expected", Current_Token.Position);
                     end if;
                     declare
                        Output : constant Wide_String := Current_Token.Text (1 .. Current_Token.Length);
                     begin
                        Next_Token;
                        Close_Command;

                        Set_Output_Command (Output);
                     end;

                  else
                     Next_Token;
                     if Is_String (Current_Token, "ON") then
                        State := True;
                     elsif Is_String (Current_Token, "OFF") then
                        State := False;
                     else
                        Syntax_Error ("""on"" or ""off"" expected", Current_Token.Position);
                     end if;

                     if Option = "VERBOSE" then
                        Verbose_Option := State;
                     elsif Option = "DEBUG" then
                        Debug_Option := State;
                     elsif Option = "IGNORE" then
                        Ignore_Option := True;
                     else
                        Syntax_Error ("Unrecognised parameter: """ & Option &'"', Current_Token.Position);
                     end if;
                     Next_Token;
                     Close_Command;
                  end if;
               end;

            when Key_Source =>
               Next_Token (Force_String => True);
               if Current_Token.Kind /= Name then
                  Syntax_Error ("Expect file name after ""Source""", Current_Token.Position);
               end if;

               declare
                  Source     : constant Wide_String := Current_Token.Text (1 .. Current_Token.Length);
                  Source_Pos : constant Location    := Current_Token.Position;
               begin
                  Next_Token;
                  Close_Command;

                  Source_Command (Source);
               exception
                  when Name_Error =>
                     Syntax_Error ("Sourced file " & Source & " not found", Source_Pos);
               end;

            when Not_A_Key
              | Profile_Keys -- Profile keys and "not" allowed as labels
              | Key_Not
              =>
               -- Must be a label
               declare
                  Label : constant Wide_String := Current_Token.Text (1 .. Current_Token.Length);
               begin
                  Next_Token;
                  if Current_Token.Kind /= Colon then
                     Syntax_Error ("Unknown command " & Label, Current_Token.Position);
                  end if;
                  Next_Token;
                  if Current_Token.Kind /= Name then
                     Syntax_Error ("Unexpected element after label", Current_Token.Position);
                  end if;

                  case Current_Token.Key is
                     when Key_Check =>
                        Next_Token;
                        Add_Use (Label, Check, Get_Rule_Name);
                     when Key_Search =>
                        Next_Token;
                        Add_Use (Label, Search, Get_Rule_Name);
                      when Key_Count =>
                        Next_Token;
                        Add_Use (Label, Count, Get_Rule_Name);
                    when others =>
                        Syntax_Error ("Only ""Check"", ""Search"", or ""Count"" allowed after label",
                                      Current_Token.Position);
                  end case;
               end;
               Close_Command;
         end case;

      end loop;
   end Compile;

   ------------------------------------------------------
   --  Exported subprograms                            --
   ------------------------------------------------------

   -------------
   -- Execute --
   -------------

   procedure Execute (Commands : Wide_String) is
   begin
      Set_Prompt ("");
      Start_Scan (From_String => True, Source => Commands);
      Compile;
   end Execute;

   ----------------------
   -- Parameter_Exists --
   ----------------------

   function Parameter_Exists return Boolean is
   begin
      return In_Parameters;
   end Parameter_Exists;

   ---------------------------
   -- Get_Integer_Parameter --
   ---------------------------

   function Get_Integer_Parameter return Integer is
   begin
      if not In_Parameters then
         Failure ("Get_Integer_Parameter called when not in parameters");
      end if;

      if Current_Token.Kind = Integer_Value then
         declare
            Result : constant Integer := Current_Token.Value;
         begin
            Next_Token;
            Next_Parameter;
            return Result;
         end;
      elsif Current_Token.Kind = Name then
         Syntax_Error ("Integer parameter expected", Current_Token.Position);
      else
         Syntax_Error ("Parameter expected", Current_Token.Position);
      end if;
   end Get_Integer_Parameter;

   --------------------------
   -- Get_String_Parameter --
   --------------------------

   function Get_String_Parameter return Wide_String is
      Initial_Tick : Boolean := False;
   begin
      if not In_Parameters then
         Failure ("Get_String_Parameter called when not in parameters");
      end if;

      if Current_Token.Kind = Tick then
         Initial_Tick := True;
         Next_Token;
      end if;

      if Current_Token.Kind /= Name then
         Syntax_Error ("Parameter expected", Current_Token.Position);
      end if;

      declare
         Result : constant Wide_String := Current_Token.Text (1 .. Current_Token.Length);
      begin
         Next_Token;
         if Current_Token.Kind = Tick then
            -- We must accept 'class'input...
            return Choose (Initial_Tick, "'", "") & Result & Get_String_Parameter;
         else
            Next_Parameter;
            return Choose (Initial_Tick, "'", "") & Result;
         end if;
      end;
   end Get_String_Parameter;

   --------------------------
   -- Get_Entity_Parameter --
   --------------------------

   function Get_Entity_Parameter return Entity_Specification is

      -- Information set by the parsing functions:
      Qualified  : Boolean;

      -- Forward declaration:
      function Full_Name return Wide_String;

      function Identifier return Wide_String is
      begin
         if Current_Token.Kind = Name then
            declare
               Name : constant Wide_String := To_Upper (Current_Token.Text (1..Current_Token.Length));
            begin
               Next_Token;
               return Name;
            end;
         else
            Syntax_Error ("Identifier expected", Current_Token.Position);
         end if;
      end Identifier;

      function Profile_List return Wide_String is
         With_Access : Boolean := False;
      begin
         if Current_Token.Kind = Name and then Current_Token.Key = Key_Access then
            With_Access := True;
            Next_Token;
         end if;

         -- If not qualified, assume the identifier is declared in Standard
         Qualified := False;
         declare
            function Formated_Name (Name : Wide_String) return Wide_String is
            begin
               if Qualified then
                  if With_Access then
                     return '*' & Name;
                  else
                     return Name;
                  end if;
               else
                  if With_Access then
                     return '*' & "STANDARD." & Name;
                  else
                     return "STANDARD." & Name;
                  end if;
               end if;
            end Formated_Name;

            Name1 : constant Wide_String := Formated_Name (Full_Name);
         begin
            if Current_Token.Kind = Semi_Colon then
               Next_Token;
               return Name1 & ';' & Profile_List;
            else
               return Name1;
            end if;
         end;
      end Profile_List;

      function Profile return Wide_String is
      begin
         if Current_Token.Kind = Name and then Current_Token.Key = Key_Return then
            Next_Token;
            Qualified := False;
            declare
               Result_Type : constant Wide_String := Full_Name;
            begin
               -- If not qualified, assume the identifier is declared in Standard
               if not Qualified then
                  return ':' & "STANDARD." & Result_Type;
               else
                  return ':' & Result_Type;
               end if;
            end;
         end if;

         declare
            List1 : constant Wide_String := Profile_List;
         begin
            if Current_Token.Kind = Name and then Current_Token.Key = Key_Return then
               Next_Token;
               Qualified := False;
               declare
                  Result_Type : constant Wide_String := Full_Name;
               begin
                  -- If not qualified, assume the identifier is declared in Standard
                  if not Qualified then
                     return List1 & ':' & "STANDARD." & Result_Type;
                  else
                     return List1 & ':' & Result_Type;
                  end if;
               end;
            else
               return List1;
            end if;
         end;
      end Profile;

      function Typed_Name return Wide_String is
         Name1 : constant Wide_String := Identifier;
      begin
         if Current_Token.Kind = Left_Bracket then
            Next_Token;
            if Current_Token.Kind = Right_Bracket then
               Next_Token;
               return Name1 & "{}";
            else
               declare
                  Profile1 : constant Wide_String := Profile;
               begin
                  if Current_Token.Kind = Right_Bracket then
                     Next_Token;
                  else
                     Syntax_Error ("Missing ""}""", Current_Token.Position);
                  end if;
                  return Name1 & '{' & Profile1 & '}';
               end;
            end if;
         else
            return Name1;
         end if;
      end Typed_Name;

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

      function Attributed_Name return Wide_String is
         Name1 : constant Wide_String := Typed_Name;
      begin
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

      use Ada.Strings.Wide_Unbounded;
   begin  -- Get_Entity_Parameter
      if not In_Parameters then
         Failure ("Get_Entity_Parameter called when not in parameters");
      end if;

      if Current_Token.Kind = Name and then Current_Token.Key = Key_All then
         Next_Token;

         if Current_Token.Kind = Tick then
            -- "all 'image"
            Next_Token;
            declare
               Result : constant Wide_String := Identifier;
            begin
               Next_Parameter;
               return (Is_Box        => False,
                       Is_All        => True,
                       Specification => To_Unbounded_Wide_String (''' & Result));
            end;
         else
            declare
               Result : constant Wide_String := Attributed_Name;
            begin
               Next_Parameter;
               return (Is_Box        => False,
                       Is_All        => True,
                       Specification => To_Unbounded_Wide_String (Result));
            end;
         end if;

      elsif Current_Token.Kind = Left_Angle then
         Next_Token;
         if Current_Token.Kind = Right_Angle then
            Next_Token;
            Next_Parameter;
            return (Is_Box => True);
         else
            Syntax_Error (""">"" expected", Current_Token.Position);
         end if;

      else
         declare
            Result : constant Wide_String := Full_Name;
         begin
            Next_Parameter;
            return (Is_Box        => False,
                    Is_All        => False,
                    Specification => To_Unbounded_Wide_String (Result));
        end;
     end if;
   end Get_Entity_Parameter;

   ------------------
   -- Get_Modifier --
   ------------------

   function Get_Modifier (True_KW  : Wide_String;
                          False_KW : Wide_String := "";
                          Default  : Boolean := False) return Boolean
   is
   begin
      if Current_Token.Kind = Name then
         if To_Upper (Current_Token.Text (1..Current_Token.Length)) = True_KW then
            Next_Token;
            return True;
         elsif To_Upper (Current_Token.Text (1..Current_Token.Length)) = False_KW then
            Next_Token;
           return False;
         end if;
      end if;
      return Default;
   end Get_Modifier;

   ------------------------
   -- Get_Flag_Parameter --
   ------------------------

   function Get_Flag_Parameter return Flags is
   begin
      if not In_Parameters then
         Failure ("Get_Flag_Parameter called when not in parameters");
      end if;

      if Current_Token.Kind = Name then
         declare
            To_Compare : constant Wide_String := To_Upper (Prefix &
                                                           Current_Token.Text (1 .. Current_Token.Length));
         begin
            for Key in Flags loop
               if To_Compare = Flags'Wide_Image (Key) then
                  if Allow_Any and then Key = Flags'First then
                     -- Oops, the user specified the special value
                     Syntax_Error ("Not a valid parameter: " & Current_Token.Text (1 .. Current_Token.Length),
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

      Syntax_Error ("Keyword expected, use option -h <rule name> for a list of allowable keywords",
                    Current_Token.Position);
   end Get_Flag_Parameter;

   ------------------
   -- Adjust_Image --
   ------------------

   function Adjust_Image (Original : Wide_String) return Wide_String is
      use Ada.Strings.Wide_Fixed;

      Pos   : Natural;
      Start : Natural;
   begin
      Pos := Index (Original, ":");
      if Pos = 0 then
         -- Find a real * meaning "access", discard the "*" and "**" operators
         Start := Original'First;
         loop
            Pos := Index (Original (Start .. Original'Last), "*");

            if Pos = 0 then
               -- No * found
               return Original;

            elsif Original (Pos+1) = '"' then
               -- "*" operator
               Start := Pos+2;

            elsif Original (Pos+1) = '*' then
               -- "**" operator
               Start := Pos+3;

            else
               -- Real access parameter
               exit;
            end if;
         end loop;

         return
           Original (Original'First..Pos - 1) &
           " access " &
           Adjust_Image (Original (Pos + 1 .. Original'Last));

      else
         return
           Adjust_Image (Original (Original'First..Pos - 1)) &
           " return " &
           Adjust_Image (Original (Pos + 1 .. Original'Last));
      end if;
   end Adjust_Image;

   ---------------------
   -- Parameter_Error --
   ---------------------

   procedure Parameter_Error (Message : Wide_String) is
   begin
      Syntax_Error (Message, Current_Token.Position);
   end Parameter_Error;

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
      return Failure_Occured;
   end Had_Failure;

end Framework.Language;
