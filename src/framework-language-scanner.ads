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

--  Ada
with
   Ada.Strings.Wide_Unbounded;

-- ASIS
private with
   Asis.Text;

-- Adalog
with
  Thick_Queries;
private package Framework.Language.Scanner is
   -- Scans from current input

   type Token_Kind is (Name, Integer_Value, Float_Value, String_Value,

                       Bad_Token, Bad_Integer, Bad_Float,

                       Left_Bracket, Right_Bracket, Left_Parenthesis, Right_Parenthesis,
                       Left_Angle,   Right_Angle,   Vertical_Bar,
                       Tick, Colon,  Semi_Colon, Comma, Period, Equal,
                       -- Modify Character_Token_Kind if you add to this list

                       Eof);

   --Tokens that correspond to true values
   subtype Value_Token_Kind is Token_Kind range Name .. String_Value;
   --Tokens made of a single character
   subtype Character_Token_Kind is Token_Kind range Left_Bracket .. Equal;

   -- Keywords
   -- Following values must have the form "Key_" & Keyword
   -- except Not_A_Key, wich must be the last value.
   type Key_Kind is (Key_Access,    Key_All,    Key_Function,            -- Profile keywords
                     Key_Procedure, Key_Return,
                     Key_Clear,     Key_Help,   Key_Inhibit, Key_Go,     -- Command keywords
                     Key_Message,   Key_Quit,   Key_Rule_File_Off,
                     Key_Set,       Key_Source, Key_Check,
                     Key_Search,    Key_Count,                           -- Control kind keywords
                     Not_A_Key);                                         -- not a keyword
   subtype Profile_Keys is Key_Kind range Key_Access .. Key_Return;
   subtype Type_Keys    is Key_Kind range Key_Check  .. Key_Count;

   type Token (Kind : Token_Kind := Semi_Colon) is
      record
         Position : Location;
         case Kind is
            when Name =>
               Name_Length : Natural;
               Name_Text   : Wide_String (1..Max_ID_Length);
               Key         : Key_Kind;
            when Integer_Value =>
               Value : Thick_Queries.Biggest_Int;
            when Float_Value =>
               Fvalue      : Float;
            when String_Value =>
               String_Text   : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
            when others =>
               null;
         end case;
      end record;

   function Is_String (T : Token; Expected : Wide_String) return Boolean;
   -- Returns whether the T is a Name whose content is the given expected value
   -- Casing is irrelevant, Expected must be given in upper case.

   procedure Start_Scan (From_String : Boolean; Source : Wide_String);
   procedure Set_Prompt (Prompt : Wide_String);
   procedure Activate_Prompt;

   function Current_Token return Token;

   function Image (T : Token; Quote_String : Boolean := False) return Wide_String;
   -- If Quote_String is True, string tokens are returned as Ada quoted strings (surrounded by '"',
   -- inner '"' doubled).
   -- If false, the real value of the string is returned

   procedure Next_Token (Force_String : Boolean := False; No_Delay : Boolean := False);
   -- Advance to next token;
   -- If Force_String, take all characters until the next space or semi-colon as a string token
   -- If No_Delay, do it immediately

   function Reference_Dir return Wide_String;
   -- Returns the reference directory (i.e. the directory part of the current rules file,
   -- or "" if there is no rules files (or if it has no directory part)

   type Scanner_State is private;
   procedure Save_State    (State : out Scanner_State);
   procedure Restore_State (State : in  Scanner_State);

private
   type Scanner_State is
      record
         The_Token     : Token;
         Token_Delayed : Boolean;

         Origin_Is_String : Boolean := False;
         Cur_Char         : Wide_Character;
         At_Eol           : Boolean;
         Source_String    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Source_Last      : Natural;

         Buffer   : Wide_String (1..200);
         Buf_Inx  : Natural;
         Buf_Last : Natural;

         Current_File   : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Current_Line   : Asis.Text.Line_Number;
         Current_Column : Asis.Text.Character_Position;
         Current_Prompt : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Prompt_Active  : Boolean := False;
      end record;
end Framework.Language.Scanner;
