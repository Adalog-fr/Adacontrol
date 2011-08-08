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

private package Framework.Language.Scanner is
   -- Scans from current input
   Max_ID_Length : constant := 250;

   type Token_Kind is (Name, Integer_Value,

                       Left_Bracket, Right_Bracket, Left_Parenthesis, Right_Parenthesis,
                       Left_Angle,   Right_Angle,
                       Colon, Semi_Colon, Comma, Period,

                       EoF);

   --Tokens made of a single character
   subtype Character_Token_Kind is Token_Kind range Left_Bracket .. Period;

   type Key_Kind   is (Key_Access, Key_All, Key_Return, Not_A_Key);

   type Token (Kind : Token_Kind := Semi_Colon) is
      record
         Position : Location;
         case Kind is
            when Name =>
               Length : Positive;
               Text   : Wide_String (1..Max_ID_Length);
               Key    : Key_Kind;
            when Integer_Value =>
               Value : Integer;
            when others =>
               null;
         end case;
      end record;

   type Origin_Kind is (From_String, From_File, From_Console);
   procedure Start_Scan (Source_Kind : Origin_Kind; Source : Wide_String := "");

   function Current_Token return Token;

   function Image (T : Token) return Wide_String;

   procedure Next_Token;
   -- Advance to next token;

   procedure Syntax_Error (Message : Wide_String; Position : Location);
   pragma No_Return (Syntax_Error);
end Framework.Language.Scanner;
