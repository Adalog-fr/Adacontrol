----------------------------------------------------------------------
--  Framework.Language.Scanner - Package body                       --
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

with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Strings.Wide_Fixed,
  Ada.Wide_Text_IO;

with -- Application specific units
  Utilities;

package body Framework.Language.Scanner is
   use Ada.Wide_Text_IO;

   ------------------------------------------------------------------
   -- Internal utilities                                           --
   ------------------------------------------------------------------

   -- Invariants:
   -- The_Token is the current token
   -- Cur_Char is the next character to process, undefined if
   --   At_Eol is true meaning that the current character is the
   --   end of line.
   -- To get a more natural behaviour in interactive mode, Next_Token
   -- just marks the token as delayed, actual scanning of token will take
   -- place at the first call to Current_Token.

   The_Token     : Token;
   Token_Delayed : Boolean := True;
   String_Token  : Boolean;

   Origin_Is_String : Boolean := False;
   Cur_Char         : Wide_Character;
   At_Eol           : Boolean;
   Source_String    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Source_Last      : Natural;

   Current_File   : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Current_Line   : Asis.Text.Line_Number;
   Current_Column : Asis.Text.Character_Position;
   Current_Prompt : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Prompt_Active  : Boolean := False;

   ---------------
   -- Next_Char --
   ---------------

   --  Buffer size is arbitrary, make it big enough for (almost) all
   --  input lines to fit in. Note that the size of input lines is NOT
   --  limited by the buffer size, it is just a matter of optimization
   Buffer   : Wide_String (1..200);
   Buf_Inx  : Natural := 1;
   Buf_Last : Natural := 0;

   procedure Next_Char is
      use Ada.Strings.Wide_Fixed;
      use type Asis.ASIS_Integer;   -- Gela-ASIS compatibility
   begin
      if Buf_Inx = Buf_Last and Buf_Last = Buffer'Last then
         -- Buffer was too short, read next part
         -- (may read an empty string, but it's OK)
         if Origin_Is_String then
            Buf_Last := Integer'Min (Buffer'Length, Length (Source_String) - Source_Last);
            Buffer (1 .. Buf_Last) := Slice (Source_String, Source_Last + 1, Source_Last + Buf_Last);
            Source_Last := Source_Last + Buf_Last;
         else
            Get_Line (Current_Input, Buffer, Buf_Last);
         end if;
         Buf_Inx        := 1;
         Current_Column := Current_Column + 1;

      elsif At_Eol then
         if Origin_Is_String then
            -- if From_String, End of Line => End Of File
            raise End_Error;
         end if;

         if Current_Prompt /= Null_Unbounded_Wide_String then
            if Prompt_Active then
               -- Here, we have fresh new user input
               -- => Cancel any previous error flag
               Rule_Error_Occurred := False;
               Put (Current_Error, To_Wide_String (Current_Prompt) & ": ");
            else
               Put (Current_Error, Length (Current_Prompt) * '.' & ": ");
            end if;
         end if;

         Get_Line (Current_Input, Buffer, Buf_Last);
         Buf_Inx        := 1;
         Current_Line   := Current_Line + 1;
         Current_Column := 1;

      else
         Buf_Inx        := Buf_Inx + 1;
         Current_Column := Current_Column + 1;
      end if;

      if Buf_Inx > Buf_Last then
         -- Includes case of empty line
         At_Eol := True;
         return;
      end if;

      At_Eol        := False;
      Prompt_Active := False;
      Cur_Char      := Buffer (Buf_Inx);
   end Next_Char;

   ---------------------
   -- Look_Ahead_Char --
   ---------------------

   function Look_Ahead_Char return Wide_Character is
   begin
      if Buf_Inx = Buf_Last and Buf_Last = Buffer'Last then
         -- Buffer was too short, read next part
          -- (may read an empty string, but it's OK)
          -- Keep current char in buffer to maintain invariant
         Buffer (1) := Cur_Char;
         if Origin_Is_String then
            Buf_Last := Integer'Min (Buffer'Length, Length (Source_String) - Source_Last) - 1;
            Buffer (2 .. Buf_Last) := Slice (Source_String, Source_Last + 1, Source_Last + Buf_Last);
            Source_Last := Source_Last + Buf_Last;
         else
            Get_Line (Current_Input, Buffer (2 .. Buffer'Last), Buf_Last);
         end if;
         Buf_Inx := 1;
      end if;

      if Buf_Inx = Buffer'Last then
         -- End of line, pretend there is an extra space
         return ' ';
      end if;
      return Buffer (Buf_Inx + 1);
   end Look_Ahead_Char;


   -----------------------
   -- Actual_Next_Token --
   -----------------------

   -- The following declaration ensures that we get an error if we add a Character_Token
   -- and forget to modify the following elements.
   Char_Tokens : constant Wide_String (Token_Kind'Pos (Character_Token_Kind'First) ..
                                       Token_Kind'Pos (Character_Token_Kind'Last))
     := "{}()<>|':;,.=";
   Char_Token_Values : constant array (Char_Tokens'Range) of Token
     := ((Kind => Left_Bracket,      Position => Null_Location),
         (Kind => Right_Bracket,     Position => Null_Location),
         (Kind => Left_Parenthesis,  Position => Null_Location),
         (Kind => Right_Parenthesis, Position => Null_Location),
         (Kind => Left_Angle,        Position => Null_Location),
         (Kind => Right_Angle,       Position => Null_Location),
         (Kind => Vertical_Bar,      Position => Null_Location),
         (Kind => Tick,              Position => Null_Location),
         (Kind => Colon,             Position => Null_Location),
         (Kind => Semi_Colon,        Position => Null_Location),
         (Kind => Comma,             Position => Null_Location),
         (Kind => Period,            Position => Null_Location),
         (Kind => Equal,             Position => Null_Location));

   procedure Actual_Next_Token (Force_String : Boolean := False) is
      use Ada.Strings.Wide_Fixed;
      use Thick_Queries, Utilities;

      First_Line   : Asis.Text.Line_Number;
      First_Column : Asis.Text.Character_Position;

      procedure Get_Name (Extended : Boolean) is
         use Ada.Characters.Handling;
      begin
         if Extended and Cur_Char = ';' then
            -- Empty string
            The_Token := (Kind        => Name,
                          Position    => Create_Location (To_Wide_String (Current_File), First_Line, First_Column),
                          Name_Length => 0,
                          Name_Text   => (others => ' '),
                          Key         => Not_A_Key);
            return;
         end if;

         The_Token := (Kind        => Name,
                       Position    => Create_Location (To_Wide_String (Current_File), First_Line, First_Column),
                       Name_Length => 1,
                       Name_Text   => (1 => Cur_Char, others => ' '),
                       Key         => Not_A_Key);
         Next_Char;

         loop   --## rule line off simplifiable_statements ## exit OK, since we have several ones
            exit when At_Eol;

            if Extended then
               exit when Cur_Char = ';';
            else
               exit when Cur_Char <= ' ';
               exit when not Is_Letter (To_Character (Cur_Char))
                 and not Is_Digit (To_Character (Cur_Char))
                 and Cur_Char /= '_';
            end if;

            if The_Token.Name_Length = The_Token.Name_Text'Last then
               Syntax_Error ("Identifier too long", The_Token.Position);
            end if;
            The_Token.Name_Length := The_Token.Name_Length + 1;
            The_Token.Name_Text (The_Token.Name_Length) := Cur_Char;
            Next_Char;
         end loop;
      end Get_Name;

      procedure Get_String  is
         Quote_Char : constant Wide_Character := Cur_Char;
      begin
         The_Token := (Kind          => String_Value,
                       Position      => Create_Location (To_Wide_String (Current_File), First_Line, First_Column),
                       String_Text   => Null_Unbounded_Wide_String);
         Next_Char;
         loop
            if At_Eol then
               Syntax_Error ("Unterminated quoted string", The_Token.Position);
            end if;
            if Cur_Char = Quote_Char then
               Next_Char;
               exit when At_Eol or Cur_Char /= Quote_Char;
            end if;

            Append (The_Token.String_Text, Cur_Char);
            Next_Char;
         end loop;
      end Get_String;

      function Get_Integer return Biggest_Int is
         -- Precondition: Cur_Char in '0'..'9' or '-'
         Result   : Biggest_Int;
         Negative : Boolean     := False;
         Num_Base : Biggest_Int := 10;

         function Get_Numeral (Base : in Biggest_Int; Stop_On_E : Boolean) return Biggest_Int is
            -- Precondition: Cur_Char in '0'..'9', 'a'..'f', 'A'..'F'
            Num        : Biggest_Int := 0;
            Digit      : Biggest_Int;
            Prev_Is_US : Boolean := False;
         begin
            while not At_Eol loop
               if Stop_On_E and then Cur_Char in 'e' | 'E' then
                  exit;
               end if;
               case Cur_Char is
                  when '0' .. '9' =>
                     Digit      := Wide_Character'Pos (Cur_Char) - Wide_Character'Pos ('0');
                     Prev_Is_US := False;
                  when 'a' .. 'f' =>
                     Digit      := Wide_Character'Pos (Cur_Char) - Wide_Character'Pos ('a') + 10;
                     Prev_Is_US := False;
                  when 'A' .. 'F' =>
                     Digit      := Wide_Character'Pos (Cur_Char) - Wide_Character'Pos ('A') + 10;
                     Prev_Is_US := False;
                  when '_' =>
                     if Prev_Is_US then
                        Syntax_Error ("Consecutive underscores not allowed in numbers",
                                      Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
                     end if;
                     Prev_Is_US := True;
                  when others =>
                     exit;
               end case;
               Next_Char;

               if Digit >= Base then
                  Syntax_Error ("Invalid character in number",
                                Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
               end if;
               if not Prev_Is_US then
                  Num := Num * Base + Digit;
               end if;
            end loop;
            if Prev_Is_US then
               Syntax_Error ("Trailing underscores not allowed in numbers",
                             Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
            end if;
            return Num;
         end Get_Numeral;
      begin   -- Get_Integer
         if Cur_Char = '-' then
            Negative := True;
            Next_Char;
            if Cur_Char not in '0' .. '9' then
               Syntax_Error ("Invalid character in number",
                             Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
            end if;
         end if;

         Result := Get_Numeral (Base => 10, Stop_On_E => True);

         if Cur_Char = '#' then
            Next_Char;
            Num_Base := Result;
            if Num_Base not in 2..16 then
               Syntax_Error ("Invalid value for base",
                             Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
            end if;
            Result   := Get_Numeral (Num_Base, Stop_On_E => False);
            if Cur_Char /= '#' then
               Syntax_Error ("Missing closing '#' in based number",
                             Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
            end if;
            Next_Char;
         end if;

         if Cur_Char in 'e' | 'E' then
            Next_Char;
            if Cur_Char not in '0' .. '9' then
               Syntax_Error ("Exponent must be followed by (unsigned) number",
                             Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
            end if;
            Result := Result * Num_Base ** Integer (Get_Numeral(Base => 10, Stop_On_E => True));
         end if;

         if Negative then
            Result := -Result;
         end if;
         return Result;
      end Get_Integer;

   begin   -- Actual_Next_Token
      Token_Delayed := False;

      if The_Token.Kind = Eof then
         -- Eof found => stay there
         return;
      end if;

      loop
         if At_Eol then
            -- Skip empty lines
            Next_Char;
         elsif Cur_Char = '#' or else (Cur_Char = '-' and then Look_Ahead_Char = '-') then
            -- Skip comment
            while not At_Eol loop
               Next_Char;
            end loop;
         elsif Cur_Char > ' ' then
            exit;
         else
            Next_Char;
         end if;
      end loop;

      -- Here we have read a non-blank character
      First_Line   := Current_Line;
      First_Column := Current_Column;

      if Force_String then
         Get_Name (Extended => True);

      else
         case Cur_Char is
            when '{' | '}' | '(' | ')' | '<' | '>' | '|' | ''' | ':' | ';' | ',' | '.' | '=' =>
               The_Token := Char_Token_Values (Index (Char_Tokens, Cur_Char & ""));
               The_Token.Position := Create_Location (To_Wide_String (Current_File), First_Line, First_Column);
               Next_Char;

            when '0' .. '9' | '-' =>
               declare
                  Integer_Part    : Biggest_Int;
                  Fractional_Part : Float;
                  Exponent_Part   : Integer := 0;
                  Exponent_Sign   : Integer := +1;
               begin
                  begin
                     Integer_Part := Get_Integer;
                  exception
                     when Constraint_Error =>
                        The_Token := (Kind     => Bad_Integer,
                                      Position =>
                                         Create_Location (To_Wide_String (Current_File), First_Line, First_Column));
                        return;
                  end;

                  if Cur_Char = '.' then
                     Next_Char;
                     if Cur_Char not in '0' .. '9' then
                        Syntax_Error ("Illegal real value",
                                      Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
                     end if;

                     begin
                        Fractional_Part := Float (Get_Integer);
                     exception
                        when Constraint_Error =>
                           The_Token := (Kind     => Bad_Float,
                                         Position =>
                                            Create_Location (To_Wide_String (Current_File), First_Line, First_Column));
                           return;
                     end;

                     while Fractional_Part >= 1.0 loop
                        Fractional_Part := Fractional_Part / 10.0;
                     end loop;
                     if Integer_Part < 0 then
                        Fractional_Part := -Fractional_Part;
                     end if;

                     if Cur_Char in 'e' | 'E' then
                        Next_Char;
                        case Cur_Char is
                           when '+' =>
                              Next_Char;
                           when '-' =>
                              Exponent_Sign := -1;
                              Next_Char;
                           when others =>
                              null;
                        end case;

                        if Cur_Char not in '0' .. '9' then
                           Syntax_Error ("Illegal exponent of real value",
                                         Create_Location (
                                           To_Wide_String (Current_File),
                                           Current_Line,
                                           Current_Column));
                        end if;

                        Exponent_Part := Integer (Get_Integer);
                     end if;

                     The_Token := (Kind     => Float_Value,
                                   Position =>
                                     Create_Location (To_Wide_String (Current_File), First_Line, First_Column),
                                   Fvalue   => (Float (Integer_Part) + Fractional_Part)
                                               * 10.0 ** (Exponent_Sign * Exponent_Part));
                  else
                     The_Token := (Kind     => Integer_Value,
                                   Position =>
                                     Create_Location (To_Wide_String (Current_File), First_Line, First_Column),
                                   Value    => Integer_Part);
                  end if;
               end;

            when '~' | '"' =>
               Get_String;

            when 'a' .. 'z' | 'A' .. 'Z' | '_' => -- We allow '_' because of "_anonymous_"
               Get_Name (Extended => False);

               -- Check for keywords
               declare
                  To_Check : constant Wide_String
                    := "KEY_" & To_Upper (The_Token.Name_Text (1..The_Token.Name_Length));
               begin
                  for Key in Key_Kind range Key_Kind'First .. Key_Kind'Pred (Not_A_Key) loop
                     if To_Check = Key_Kind'Wide_Image (Key) then
                        The_Token.Key := Key;
                        exit;
                     end if;
                  end loop;
               end;

            when others =>
               declare
                  Bad_Char : constant Wide_Character := Cur_Char;
               begin
                  Next_Char;
                  Syntax_Error ("Unexpected character: " & Bad_Char,
                                Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
               end;
         end case;
      end if;
   exception
      when End_Error =>
         The_Token := (Kind => Eof,
                       Position => Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
      when others =>
         The_Token := (Kind     => Bad_Token,
                       Position => Create_Location (To_Wide_String (Current_File), First_Line, First_Column));
         raise;
   end Actual_Next_Token;

   ------------------------------------------------------------------
   -- Exported subprograms                                         --
   ------------------------------------------------------------------

   -------------------
   -- Current_Token --
   -------------------

   function Current_Token return Token is
   begin
      if Token_Delayed then
         Actual_Next_Token (String_Token);
      end if;
      return The_Token;
   end Current_Token;

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token (Force_String : Boolean := False; No_Delay : Boolean := False) is
   begin
      if No_Delay then
         Actual_Next_Token (Force_String);
      else
         Token_Delayed := True;
         String_Token  := Force_String;
      end if;
   end Next_Token;

   ----------------
   -- Set_Prompt --
   ----------------

   procedure Set_Prompt (Prompt : Wide_String) is
   begin
      Current_Prompt := To_Unbounded_Wide_String (Prompt);
      Prompt_Active  := True;
   end Set_Prompt;

   ---------------------
   -- Activate_Prompt --
   ---------------------

   procedure Activate_Prompt is
   begin
      Prompt_Active := True;
   end Activate_Prompt;

   ----------------
   -- Start_Scan --
   ----------------

   procedure Start_Scan (From_String : Boolean; Source : Wide_String) is
      use type Asis.ASIS_Integer;   -- Gela-ASIS compatibility
   begin
      Origin_Is_String := From_String;
      Current_Line     := 1;
      Current_Column   := 0;

      if Origin_Is_String then
         Source_String := To_Unbounded_Wide_String (Source);
         Current_File  := Null_Unbounded_Wide_String;

         Buf_Last := Integer'Min (Buffer'Length, Length (Source_String));
         Buffer (1 .. Buf_Last) := Slice (Source_String, 1, Buf_Last);
         Source_Last := Buf_Last;

      else
         Current_File := To_Unbounded_Wide_String (Source);
         if Current_Prompt /= Null_Unbounded_Wide_String then
            Put (Current_Error, To_Wide_String (Current_Prompt) & ": ");
         end if;

         -- Get a non-empty line:
         loop
            Get_Line (Current_Input, Buffer, Buf_Last);
            exit when Buf_Last >= 1;
            Current_Line := Current_Line + 1;
         end loop;
      end if;

      Buf_Inx       := 1;
      Cur_Char      := Buffer (Buf_Inx);
      At_Eol        := Buf_Inx > Buf_Last;   -- True for empty input string
      Prompt_Active := False;
      The_Token     := (Kind => Semi_Colon, Position => The_Token.Position); -- Make sure it is not Eof

   exception
      when End_Error =>
         The_Token := (Kind     => Eof,
                       Position => Create_Location (To_Wide_String (Current_File), Current_Line, Current_Column));
  end Start_Scan;

   -----------
   -- Image --
   -----------

   function Image (T : Token; Quote_String : Boolean := False) return Wide_String is
      use Thick_Queries;

      function Double_Quotes (S : Wide_String) return Wide_String is
      begin
         for I in S'Range loop
            if S (I) = '"' then
               return S (S'First .. I) & '"' & Double_Quotes (S (I + 1 .. S'Last));
            end if;
         end loop;
         return S;
      end Double_Quotes;

   begin  -- Image
      case T.Kind is
         when Name =>
            return T.Name_Text (1 .. T.Name_Length);
         when Integer_Value =>
            return  Biggest_Int_Img (T.Value);
         when Float_Value =>
            declare
               Result : constant Wide_String := Float'Wide_Image (T.Fvalue);
            begin
               if T.Fvalue < 0.0 then
                  return Result;
               else
                  return Result (2 .. Result'Length);
               end if;
            end;
         when String_Value =>
            if Quote_String then
               return '"' & Double_Quotes (To_Wide_String (T.String_Text)) & '"';
            else
               return To_Wide_String (T.String_Text);
            end if;
         when Bad_Integer | Bad_Float | Bad_Token =>
            return "#####";
         when Character_Token_Kind =>
            return (1 => Char_Tokens (Character_Token_Kind'Pos (T.Kind)));
         when Eof =>
            Utilities.Failure ("Token image for Eof");
      end case;
   end Image;

   ----------------
   -- Save_State --
   ----------------

   procedure Save_State (State : out Scanner_State) is
   begin
      State := (The_Token,
                Token_Delayed,
                Origin_Is_String,
                Cur_Char,
                At_Eol,
                Source_String,
                Source_Last,

                Buffer,
                Buf_Inx,
                Buf_Last,

                Current_File,
                Current_Line,
                Current_Column,
                Current_Prompt,
                Prompt_Active);
   end Save_State;

   -------------------
   -- Restore_State --
   -------------------

   procedure Restore_State (State : in  Scanner_State) is
   begin
      The_Token        := State.The_Token;
      Token_Delayed    := State.Token_Delayed;
      Origin_Is_String := State.Origin_Is_String;
      Cur_Char         := State.Cur_Char;
      At_Eol           := State.At_Eol;
      Source_String    := State.Source_String;
      Source_Last      := State.Source_Last;

      Buffer           := State.Buffer;
      Buf_Inx          := State.Buf_Inx;
      Buf_Last         := State.Buf_Last;

      Current_File     := State.Current_File;
      Current_Line     := State.Current_Line;
      Current_Column   := State.Current_Column;
      Current_Prompt   := State.Current_Prompt;
      Prompt_Active    := State.Prompt_Active;
   end Restore_State;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (T : Token; Expected : Wide_String) return Boolean is
      use Utilities;
   begin
      if T.Kind /= Name then
         return False;
      end if;

      return To_Upper (T.Name_Text (1 .. T.Name_Length)) = Expected;
   end Is_String;

   -------------------
   -- Reference_Dir --
   -------------------

   function Reference_Dir return Wide_String is
      F_Name : constant Wide_String := To_Wide_String (Current_File);
      Last : Natural := F_Name'Last;
   begin
      while Last >= 1 and then (F_Name (Last) /= '/' and F_Name (Last) /= '\') loop
         Last := Last - 1;
      end loop;
      return F_Name (1..Last);
   end Reference_Dir;

end Framework.Language.Scanner;
