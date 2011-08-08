----------------------------------------------------------------------
--  Framework.Language - Package body                               --
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

with
  Ada.Characters.Handling,
  Ada.Strings.Wide_Fixed;
with
  Framework.Language.Scanner,
  Framework.Rules_Manager,
  Utilities;
package body Framework.Language is
   use Framework.Language.Scanner, Utilities;

   ------------------------------------------------------
   --  Internal utilities                              --
   ------------------------------------------------------

   type Syntax_State is (Rule_Start, Check_Search, Rule_Name, Parameters, Rule_End);
   State : Syntax_State;

   Token_1 : Token;  -- Current active token
   Token_2 : Token;  -- Look-ahead token
   Look_Ahead_Active : Boolean := False;

   -- Invariants for the following parsing functions:
   -- On entrance, Token_1 is the first token of the corresponding syntax
   --   This is checked by the function itself, not the caller.
   -- On exit, Token_1 is the first token not in the corresponding syntax

   ----------------
   -- Skip_Token --
   ----------------

   procedure Skip_Token is
      --  Move to next token
   begin
      if Look_Ahead_Active then
         Token_1 := Token_2;
         Look_Ahead_Active := False;
      else
         Next_Token;
         Token_1 := Current_Token;
      end if;
   end Skip_Token;

   ----------------------
   -- Look_Ahead_Token --
   ----------------------

   procedure Look_Ahead_Token is
   begin
      Next_Token;
      Token_2           := Current_Token;
      Look_Ahead_Active := True;
   end Look_Ahead_Token;


   --------------------
   -- Next_Parameter --
   --------------------

   procedure Next_Parameter is
   begin
      case Token_1.Kind is
         when Comma =>
            Skip_Token;
            -- State remains Parameter
         when Right_Parenthesis =>
            Skip_Token;
            State := Rule_End;
         when others =>
            Syntax_Error ("Unexpected element after parameter", Token_1.Position);
      end case;
   end Next_Parameter;

   -----------------
   -- Rule_Exists --
   -----------------

   function Rule_Exists return Boolean is
   begin
      return Token_1.Kind /= EoF;
   end Rule_Exists;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label return Wide_String is
   begin
      if State /= Rule_Start then
         Failure ("Illegal state: " & Syntax_State'Wide_Image (State));
      end if;

      if Token_1.Kind /= Name then
         Syntax_Error ("Identifier expected", Token_1.Position);
      end if;

      State := Check_Search;
      Look_Ahead_Token;
      if Token_2.Kind = Colon then
         declare
            Result : constant Wide_String :=  Token_1.Text (1 .. Token_1.Length);
         begin
            Skip_Token;  -- The identifier
            Skip_Token;  -- The colon
            return Result;
         end;
      else
         return "";
      end if;
   end Get_Label;

   -------------------
   -- Get_Rule_Type --
   -------------------

   function Get_Rule_Type return Rule_Types is
   begin
      if State /= Check_Search then
         Failure ("Illegal state: " & Syntax_State'Wide_Image (State));
      end if;

      declare
         Result : constant Rule_Types
           :=Rule_Types'Wide_Value (Token_1.Text (1 .. Token_1.Length));
         -- raises Constraint_Error if not a Name, or not "Check" or "Search"
      begin
         Skip_Token;
         State := Rule_Name;
         return Result;
      end;

   exception
      when Constraint_Error =>
         Syntax_Error ("""Check"" or ""Search"" expected", Token_1.Position);
   end Get_Rule_Type;

   --------------
   -- Get_Name --
   --------------

   function Get_Name return Wide_String is
   begin
      if State /= Rule_Name then
         Failure ("Illegal state: " & Syntax_State'Wide_Image (State));
      end if;

      if Token_1.Kind /= Name then
         Syntax_Error ("Rule identifier expected", Token_1.Position);
      end if;

      declare
         use Framework.Rules_Manager;
         Result : constant Wide_String := To_Upper (Token_1.Text (1..Token_1.Length));
      begin
         if Is_Rule_Name (Result) then
            Skip_Token;
            if Token_1.Kind = Left_Parenthesis then
               Skip_Token;
               State := Parameters;
            else
               State := Rule_End;
            end if;
            return Result;
         else
            Syntax_Error ("Not a rule name: " & Result, Token_1.Position);
         end if;
      end;
   end Get_Name;

   ----------------
   -- Close_Rule --
   ----------------

   procedure Close_Rule is
   begin
      if State /= Rule_End then
         Failure ("Illegal state: " & Syntax_State'Wide_Image (State));
      end if;

      if Token_1.Kind /= Semi_Colon then
         Syntax_Error ("Semi-colon expected", Token_1.Position);
      end if;
      -- We do not skip the token to avoid reading in the case where
      -- we compile only one rule

      State := Rule_Start;
   end Close_Rule;

   -------------
   -- Compile --
   -------------

   procedure Compile (Only_One_Rule : Boolean := False) is
      use Rules_Manager;
   begin
      State := Rule_Start;
      loop
         -- Set up initial token
         Skip_Token;

         exit when not Rule_Exists;

         -- Note:
         -- Assigning to intermediate constants is required, since we depend
         -- on the various Get_XX functions being called in the right order.
         -- (i.e.: do not put the Get_XX inside the call to Add_Use)
         declare
            Label     : constant Wide_String := Get_Label;
            Rule_Type : constant Rule_Types  := Get_Rule_Type;
            Rule_Name : constant Wide_String := Get_Name;
         begin
            Add_Use (Label, Rule_Type, Rule_Name);
            Close_Rule;
         end;
         exit when Only_One_Rule;
      end loop;
   end Compile;

   ------------------------------------------------------
   --  Exported subprograms                            --
   ------------------------------------------------------

   -----------------
   -- Compile_File --
   -----------------

   procedure Compile_File (Name   : String) is
      use Ada.Characters.Handling;
   begin
      Start_Scan (From_File, To_Wide_String (Name));
      Compile;
   end Compile_File;

   -------------------
   -- Compile_String --
   -------------------

   procedure Compile_String (Source : String) is
      use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Wide_Fixed;
      Wide_Source : constant Wide_String := Trim (To_Wide_String (Source), Both);
   begin
      if Wide_Source (Wide_Source'Last) = ';' then
         Start_Scan (From_String, Wide_Source);
      else
         -- As a courtesy, provide the missing final ';'
         Start_Scan (From_String, Wide_Source & ';');
      end if;
      Compile;
   end Compile_String;

   ----------------------
   -- Parameter_Exists --
   ----------------------

   function Parameter_Exists return Boolean is
   begin
      return State = Parameters;
   end Parameter_Exists;

   ---------------------------
   -- Get_Integer_Parameter --
   ---------------------------

   function Get_Integer_Parameter return Integer is
   begin
      if State /= Parameters then
         Failure ("Illegal state: " & Syntax_State'Wide_Image (State));
      end if;

      if Token_1.Kind = Integer_Value then
         declare
            Result : constant Integer := Token_1.Value;
         begin
            Skip_Token;
            Next_Parameter;
            return Result;
         end;
      elsif Token_1.Kind = Name then
         Syntax_Error ("Integer parameter expected", Token_1.Position);
      else
         Syntax_Error ("Parameter expected", Token_1.Position);
      end if;
   end Get_Integer_Parameter;

   --------------------------
   -- Get_String_Parameter --
   --------------------------

   function Get_String_Parameter return Wide_String is
      Saved_Token : Token;
   begin
      if State /= Parameters then
         Failure ("Illegal state: " & Syntax_State'Wide_Image (State));
      end if;

      -- Everything must be treated as part of the returned value,
      -- except "," and ")" (or EoF, which will generate an error later)
      case Token_1.Kind is
         when Comma | Right_Parenthesis | EoF =>
            Next_Parameter;
            return "";
         when others =>
            Saved_Token := Token_1;
            Skip_Token;
            return Image (Saved_Token) & Get_String_Parameter;
      end case;
   end Get_String_Parameter;

   --------------------------
   -- Get_Entity_Parameter --
   --------------------------

   function Get_Entity_Parameter return Entity_Specification is

      -- Information set by the parsing functions:
      With_All   : Boolean;
      Qualified  : Boolean;
      Overloaded : Boolean;

      -- Forward declaration:
      function Full_Name return Wide_String;

      function Identifier return Wide_String is
      begin
         if Token_1.Kind = Name and then Token_1.Key = Not_A_Key then
            declare
               Name : constant Wide_String := To_Upper (Token_1.Text (1..Token_1.Length));
            begin
               Skip_Token;
               return Name;
            end;
         else
            Syntax_Error ("Identifier expected", Token_1.Position);
         end if;
      end Identifier;

      function Profile_List return Wide_String is
         With_Access : Boolean := False;
      begin
         if Token_1.Kind = Name and then Token_1.Key = Key_Access then
            With_Access := True;
            Skip_Token;
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
            if Token_1.Kind = Semi_Colon then
               Skip_Token;
               return Name1 & ';' & Profile_List;
            else
               return Name1;
            end if;
         end;
      end Profile_List;

      function Profile return Wide_String is
      begin
         if Token_1.Kind = Name and then Token_1.Key = Key_Return then
            Skip_Token;
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
            if Token_1.Kind = Name and then Token_1.Key = Key_Return then
               Skip_Token;
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
         if Token_1.Kind = Left_Bracket then
            Skip_Token;
            Overloaded := True;
            if Token_1.Kind = Right_Bracket then
               Skip_Token;
               return Name1 & "{}";
            else
               declare
                  Profile1 : constant Wide_String := Profile;
               begin
                  if Token_1.Kind = Right_Bracket then
                     Skip_Token;
                  else
                     Syntax_Error ("Missing ""}""", Token_1.Position);
                  end if;
                  return Name1 & '{' & Profile1 & '}';
               end;
            end if;
         else
            return Name1;
         end if;
      end Typed_Name;

      function Full_Name return Wide_String is
         Ident1 : constant Wide_String := Typed_Name;
      begin
         if Token_1.Kind = Period then
            Skip_Token;
            Qualified := True;
            return Ident1 & '.' & Full_Name;
         else
            return Ident1;
         end if;
      end Full_Name;

      use Ada.Strings.Wide_Unbounded;
   begin
      if Token_1.Kind = Name and then Token_1.Key = Key_All then
         With_All := True;
         Skip_Token;
         declare
            Result : constant Wide_String := Typed_Name;
         begin
            Next_Parameter;
            return (Is_Box        => False,
                    Is_All        => With_All,
                    Is_Overloaded => Overloaded,
                    Specification => To_Unbounded_Wide_String (Result));
         end;

      elsif Token_1.Kind = Left_Angle then
         Skip_Token;
         if Token_1.Kind = Right_Angle then
            Skip_Token;
            Next_Parameter;
            return (Is_Box => True);
         else
            Syntax_Error (""">"" expected", Token_1.Position);
         end if;

      else
         With_All := False;
         declare
            Result : constant Wide_String := Full_Name;
         begin
            Next_Parameter;
            return (Is_Box        => False,
                    Is_All        => With_All,
                    Is_Overloaded => Overloaded,
                    Specification => To_Unbounded_Wide_String (Result));
        end;
     end if;
   end Get_Entity_Parameter;

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
      Syntax_Error (Message, Token_1.Position);
   end Parameter_Error;

end Framework.Language;
