----------------------------------------------------------------------
--  Framework.Language.Scanner - Package body                       --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Code Cheker  is free software;  you can redistribute  it and/or --
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

with   -- Standard Ada units
  Ada.Wide_Text_IO,
  Ada.Characters.Handling,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

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

   The_Token   : Token;

   Origin        : Origin_Kind := From_File;
   Cur_Char      : Wide_Character;
   At_Eol        : Boolean;
   Source_File   : File_Type;
   Source_String : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Source_Last   : Natural;

   Current_Line   : Asis.Text.Line_Number;
   Current_Column : Asis.Text.Character_Position;

   ------------------
   -- Syntax_Error --
   ------------------

   procedure Syntax_Error (Message : Wide_String; Position : Location) is
      use Utilities;
   begin
      Error (Image (Position)
               & ": "
               & Message);
   end Syntax_Error;

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
      use Ada.Strings.Wide_Unbounded;
   begin
      if Buf_Inx = Buf_Last and Buf_Last = Buffer'Last then
         -- Buffer was too short, read next part
         -- (may read an empty string, but it's OK)
         case Origin is
            when From_String =>
               Buf_Last := Integer'Min (Buffer'Length, Length (Source_String) - Source_Last);
               Buffer (1 .. Buf_Last) := Slice (Source_String, Source_Last + 1, Source_Last + Buf_Last);
               Source_Last := Source_Last + Buf_Last;
            when From_File =>
               Get_Line (Source_File, Buffer, Buf_Last);
            when From_Console =>
               Get_Line (Current_Input, Buffer, Buf_Last);
         end case;
         Buf_Inx        := 1;
         Current_Column := Current_Column + 1;

      elsif At_Eol then
         case Origin is
            when From_String =>
               -- if From_String, End of Line => End Of File
               raise End_Error;
            when From_File =>
               Get_Line (Source_File, Buffer, Buf_Last);
               Buf_Inx        := 1;
               Current_Line   := Current_Line + 1;
               Current_Column := 1;
            when From_Console =>
               Put (Current_Error, "....: ");
               Get_Line (Current_Input, Buffer, Buf_Last);
               Buf_Inx        := 1;
               Current_Line   := Current_Line + 1;
               Current_Column := 1;
         end case;

      else
         Buf_Inx        := Buf_Inx + 1;
         Current_Column := Current_Column + 1;
      end if;

      if Buf_Inx > Buf_Last then
         -- Includes case of empty line
         At_Eol := True;
         return;
      end if;

      At_Eol   := False;
      Cur_Char := Buffer (Buf_Inx);
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
          case Origin is
             when From_String =>
                Buf_Last := Integer'Min (Buffer'Size, Length (Source_String) - Source_Last) - 1;
                Buffer (2 .. Buf_Last) := Slice (Source_String, Source_Last + 1, Source_Last + Buf_Last);
                Source_Last := Source_Last + Buf_Last;
             when From_File =>
                Get_Line (Source_File, Buffer (2 .. Buffer'Last), Buf_Last);
             when From_Console =>
                Get_Line (Current_Input, Buffer (2 .. Buffer'Last), Buf_Last);
          end case;
          Buf_Inx := 1;
       end if;

       if Buf_Inx = Buffer'Last then
          -- End of line, pretend there is an extra space
          return ' ';
       else
          return Buffer (Buf_Inx + 1);
       end if;
  end Look_Ahead_Char;

   ------------------------------------------------------------------
   -- Exported subprograms                                         --
   ------------------------------------------------------------------

   -------------------
   -- Current_Token --
   -------------------

   function Current_Token return Token is
   begin
      return The_Token;
   end Current_Token;

   ----------------
   -- Next_Token --
   ----------------

   -- The following declaration ensures that we get an error if we add a Character_Token
   -- and forget to modify the following elements.
   Char_Tokens : constant Wide_String (Token_Kind'Pos (Character_Token_Kind'First) ..
                                       Token_Kind'Pos (Character_Token_Kind'Last))
     := "{}()<>:;,.";
   Char_Token_Values : array (Char_Tokens'Range) of Token
     := ((Kind => Left_Bracket, Position     => Null_Location),
         (Kind =>Right_Bracket, Position     => Null_Location),
         (Kind =>Left_Parenthesis, Position  => Null_Location),
         (Kind =>Right_Parenthesis, Position => Null_Location),
         (Kind =>Left_Angle, Position        => Null_Location),
         (Kind =>Right_Angle, Position       => Null_Location),
         (Kind =>Colon, Position             => Null_Location),
         (Kind =>Semi_Colon, Position        => Null_Location),
         (Kind =>Comma, Position             => Null_Location),
         (Kind =>Period, Position            => Null_Location));

   procedure Next_Token is
      use Ada.Strings.Wide_Fixed;
      use Utilities;

      In_Quotes    : Boolean; -- True if inside "..."
      Value        : Integer;
      First_Line   : Asis.Text.Line_Number;
      First_Column : Asis.Text.Character_Position;
   begin
      if The_Token.Kind = EoF then
         -- EoF found => stay there
         return;
      end if;

      loop
         if At_EoL then
            -- Skip empty lines
            Next_Char;
         elsif Cur_Char = '#' or else (Cur_Char = '-' and then Look_Ahead_Char = '-') then
            -- Skip comment
            while not At_Eol loop
               Next_char;
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

      case Cur_Char is
         when '{' | '}' | '(' | ')' | '<' | '>' | ':' | ';' | ',' | '.' =>
            The_Token := Char_Token_Values (Index (Char_Tokens, Cur_Char & ""));
            The_Token.Position.First_Line   := First_Line;
            The_Token.Position.First_Column := First_Column;
            Next_Char;

         when '0' .. '9' =>
            Value := Wide_Character'Pos (Cur_Char) - Wide_Character'Pos ('0');
            loop
               Next_Char;
               exit when At_Eol or else Cur_Char not in '0' .. '9';
               Value := Value*10 + Wide_Character'Pos (Cur_Char) - Wide_Character'Pos ('0');
            end loop;
            The_Token := (Kind     => Integer_Value,
                          Position => (The_Token.Position.File_Name, First_Line, First_Column),
                          Value    => Value);

         when 'a'..'z' | 'A'..'Z' | '_' =>
            The_Token := (Kind     => Name,
                          Position => (The_Token.Position.File_Name, First_Line, First_Column),
                          Length   => 1,
                          Text     => (1 => Cur_Char, Others => ' '),
                          Key      => Not_A_Key);
            In_Quotes := Cur_Char = '"';
            begin
               Next_Char;
               loop
                  exit when At_Eol;

                  if Cur_Char = '"' then
                     In_Quotes := not In_Quotes;

                  elsif not In_Quotes then
                     exit when Cur_Char <= ' ' or Index (Char_Tokens, Cur_Char & "") /= 0;
                  end if;

                  if The_Token.Length = The_Token.Text'Last then
                     Syntax_Error ("Identifier too long", The_Token.Position);
                     return;
                  end if;
                  The_Token.Length := The_Token.Length + 1;
                  The_Token.Text (The_Token.Length) := Cur_Char;
                  Next_Char;
               end loop;

               -- Check for keywords
               if To_Upper (The_Token.Text (1..The_Token.Length)) = "ACCESS" then
                  The_Token.Key := Key_Access;
               elsif To_Upper (The_Token.Text (1..The_Token.Length)) = "ALL" then
                  The_Token.Key := Key_All;
               elsif To_Upper (The_Token.Text (1..The_Token.Length)) = "RETURN" then
                  The_Token.Key := Key_Return;
               end if;
            end;
         when others =>
            Syntax_Error ("Unexpected character: " & Cur_Char,
                          (The_Token.Position.File_Name, Current_Line, Current_Column));
      end case;
   exception
      when End_Error =>
         case Origin is
            when From_String | From_Console =>
               null;
            when From_File =>
               Close (Source_File);
         end case;
         The_Token := (Kind => EoF,
                       Position => (The_Token.Position.File_Name, Current_Line, Current_Column));
   end Next_Token;

   ----------------
   -- Start_Scan --
   ----------------

   procedure Start_Scan (Source_Kind : Origin_Kind; Source : Wide_String := "") is
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;
   begin
      Origin := Source_Kind;
      case Origin is
         when From_String =>
            Source_String := To_Unbounded_Wide_String (Source);

            Buf_Last := Integer'Min (Buffer'Length, Length (Source_String));
            Buffer (1 .. Buf_Last) := Slice (Source_String, 1, Buf_Last);
            Source_Last := Buf_Last;

         when From_File =>
            Open (Source_File, In_File, Name => To_String (Source));
            Get_Line (Source_File, Buffer, Buf_Last);

            The_Token.Position.File_Name := To_Unbounded_Wide_String (Source);
            for I in Char_Token_Values'Range loop
               Char_Token_Values (I).Position.File_Name := To_Unbounded_Wide_String (Source);
            end loop;

         when From_Console =>
            The_Token.Position.File_Name := To_Unbounded_Wide_String ("Console");
            Put (Current_Error, "Rule: ");
            Get_Line (Current_Input, Buffer, Buf_Last);

            for I in Char_Token_Values'Range loop
               Char_Token_Values (I).Position.File_Name := To_Unbounded_Wide_String ("Console");
            end loop;

            if Buf_Last = 0 then
               -- Empty line
               The_Token := (Kind => EoF, Position => The_Token.Position);
            end if;
      end case;

      Current_Line   := 1;
      Current_Column := 0;
      Buf_Inx        := 1;
      Cur_Char       := Buffer (Buf_Inx);
      At_Eol         := False;
   exception
      when Name_Error =>
         Utilities.Error ("Unable to find rule file " & Source);
      when End_Error =>
         case Origin is
            when From_String | From_Console =>
               null;
            when From_File =>
               Close (Source_File);
         end case;
         The_Token := (Kind => EoF,
                       Position => (The_Token.Position.File_Name, Current_Line, Current_Column));
  end Start_Scan;

   -----------
   -- Image --
   -----------

   function Image (T : Token) return Wide_String is
   begin
      case T.Kind is
         when Name =>
            return T.Text (1 .. T.Length);
         when Integer_Value =>
            declare
               Image : constant Wide_String := Integer'Wide_Image (T.Value);
            begin
               return Image (2 .. Image'Length);
            end;
         when Character_Token_Kind =>
            return (1 => Char_Tokens (Character_Token_Kind'Pos (T.Kind)));
         when EoF =>
            Utilities.Failure ("Token image for EoF");
      end case;
   end Image;

end Framework.Language.Scanner;

