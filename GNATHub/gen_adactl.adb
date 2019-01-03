----------------------------------------------------------------------
--  Gen_Adactl - Procedure body                                     --
--                                                                  --
--  This software is (c) Adalog 2004-2018.                          --
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

-- Usage: gen_adactl sonar.rules|sonar.profile|rules [ -|<input file> [<output file>]]
--
-- Structure of generated file for sonar.rules command:
--  <?xml version='1.0' encoding='UTF-8'?>
--  <rules>
--    <rule>
--      <key> ... </key>
--      <name> ... </name>
--      <description> ... </description>
--      <tag>adacontrol</tag>
--    </rule>
--  </rules>
--
-- Structure of generated file for sonar.profile command:
-- <rule>
--   <key> ... </key>
--   <repositoryKey>adacontrol</repositoryKey>
-- </rule>
--
-- rules command:
-- Simply list rule names

with
  Ada.Characters.Handling,
  Ada.Command_Line,
  Ada.Exceptions,
  Ada.Strings.Fixed,
  Ada.Strings.Maps,
  Ada.Strings.Unbounded,
  Ada.Text_IO;

procedure Gen_Adactl is
   use Ada.Text_IO;

   type Commands is (Sonar_Rules, Sonar_Profile, Rules);
   Command : Commands;

   F_In  : File_Type;
   F_Out : File_Type;

   Cancelled : exception;

   type States is (Skip_To_Head, Skip_To_Rule, In_Rule, Terminated);
   State : States := Skip_To_Head;

   Rule_Count : Natural := 0;
   Line_Count : Natural := 0;

   Head_Tag    : constant String := "@chapter Rules reference";
   Rule_Tag    : constant String := "@section";
   Details_Tag : constant String := "@subsection";
   End_Tag     : constant String := "@chapter";

   procedure Initialize is
      use Ada.Characters.Handling, Ada.Command_Line;
   begin
      if Argument_Count = 0 then
         Put_Line (Standard_Error, "Function required");
      end if;

      if Argument_Count = 0 or else Argument (1) = "-h" then
         Put_Line (Standard_Error,
                   "Usage: gen_adactl sonar.rules|sonar.profile|rules [ -|<input file> [<output file>]]");
         raise Cancelled;
      elsif To_Upper (Argument (1)) = "SONAR.RULES" then
         Command := Sonar_Rules;
      elsif To_Upper (Argument (1)) = "SONAR.PROFILE" then
         Command := Sonar_Profile;
      elsif To_Upper (Argument (1)) = "RULES" then
         Command := Rules;
      else
         Put_Line (Standard_Error, "Unknown command");
         raise Cancelled;
      end if;

      if Argument_Count > 1 then
         if Argument (2) /= "-" then
            Open (F_In, In_File, Argument (2));
            Set_Input (F_In);
         end if;
         if Argument_Count > 2 then
            Create (F_Out, Out_File, Argument (3));
            Set_Output (F_Out);
         end if;
         if Argument_Count > 3 then
            Put_Line (Standard_Error, "Too many parameters");
            raise Cancelled;
         end if;
      end if;

      case Command is
         when Sonar_Rules =>
            Put_Line ("<?xml version='1.0' encoding='UTF-8'?>");
            Put_Line ("<rules>");
         when Sonar_Profile =>
            Put_Line ("<?xml version='1.0' encoding='UTF-8'?>");
            Put_Line ("<profile>");
            Put_Line ("  <name>AdaControl way</name>");
            Put_Line ("  <language>ada</language>");
            Put_Line ("  <rules>");
         when Rules =>
            null;
      end case;
   end Initialize;

   procedure Finalize is
   begin
      case Command is
         when Sonar_Rules =>
            Put_Line ("</rules>");
         when Sonar_Profile =>
            Put_Line ("  </rules>");
            Put_Line ("</profile>");
         when Rules =>
            null;
      end case;

      if Is_Open (F_In) then
         Set_Input (Standard_Input);
         Close (F_In);
      end if;
      if Is_Open (F_Out) then
         Set_Output (Standard_Output);
         Close (F_Out);
      else
         New_Line (Standard_Error);
      end if;

      Put_Line (Standard_Error, "Rules processed:" & Natural'Image (Rule_Count));
   end Finalize;

   procedure Process_Line (Line : String) is
      use Ada.Strings, Ada.Strings.Fixed;
      Name_Start : Positive;
      Name_End   : Natural;

      function Match (Text : String; Reference : String) return Boolean is
         (Text'Length >= Reference'Length
            and then Text (Text'First .. Text'First + Reference'Length - 1) = Reference);

      function No_Bracket (Original : String) return String is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;
      begin
         for C : Character of Original loop
            case C is
               when '<' =>
                  Append (Result, "&lt;");
               when '>' =>
                  Append (Result, "&gt;");
               when others =>
                  Append (Result, C);
            end case;
         end loop;
         return To_String (Result);
      end No_Bracket;

      procedure Rule_Start is
         use Ada.Characters.Handling, Ada.Strings.Maps;
      begin
         Rule_Count := Rule_Count + 1;
         Find_Token (Line,
                     From  => Rule_Tag'Length + 1,
                     Set   => To_Set (Ranges => (('a', 'z'), ('A', 'Z'), ('0', '9'), ('_', '_'))),
                     Test  => Inside,
                     First => Name_Start,
                     Last  => Name_End);

         --## Rule off Multiple_Elements
         case Command is
            when Sonar_Rules =>
               Put ("   <rule>"); New_Line;
               Put ("      <key>");  Put (To_Lower (Line (Name_Start .. Name_End))); Put ("</key>"); New_Line;
               Put ("      <name>"); Put (Line (Name_Start .. Name_End)); Put ("</name>"); New_Line;
               Put ("      <description>"); New_Line;
            when Sonar_Profile =>
               Put ("   <rule>"); New_Line;
               Put ("      <key>");  Put (To_Lower (Line (Name_Start .. Name_End))); Put ("</key>"); New_Line;
            when Rules =>
               Put_Line (Line (Name_Start .. Name_End));
         end case;
         --## Rule on Multiple_Elements
      end Rule_Start;

      procedure Rule_End is
      begin
         case Command is
            when Sonar_Rules =>
               Put_Line ("      </description>");
               Put_Line ("      <tag>adacontrol</tag>");
               Put_Line ("   </rule>");
               New_Line;
            when Sonar_Profile =>
               Put_Line ("      <repositoryKey>adacontrol</repositoryKey>");
               Put_Line ("   </rule>");
            when Rules =>
               null;
         end case;
      end Rule_End;

      function To_HTML (Source : String) return String is
      -- replace @code{...} with <code>...</code>
      --         @b{...}    with <strong>...</strong>
      --         <  >       with &lt; &gt;
         Start, Stop : Natural;

         function Matching_Index (From : Positive) return Natural is
            Open_Count : Natural := 0;
         begin
            for I in Positive range From .. Source'Last loop
               case Source (I) is
                  when '{' =>
                     Open_Count := Open_Count + 1;
                  when '}' =>
                     Open_Count := Open_Count - 1;
                     if Open_Count = 0 then
                        return I;
                     end if;
                  when others =>
                     null;
               end case;
            end loop;

            return 0;
         end Matching_Index;

      begin   -- To_HTML
         Start := Index (Source, "@code{");
         if Start /= 0 then
            Stop := Matching_Index (From => Start + 5);
            return         To_HTML (Source (Source'First .. Start - 1))
              & "<code>" & To_HTML (Source (Start + 6    .. Stop - 1)) & "</code>"
              &            To_HTML (Source (Stop + 1     .. Source'Last));
         end if;

         Start := Index (Source, "@b{");
         if Start /= 0 then
            Stop := Matching_Index (From => Start + 2);
            return           To_HTML (Source (Source'First .. Start - 1))
              & "<strong>" & To_HTML (Source (Start + 3    .. Stop - 1)) & "</strong>"
              &              To_HTML (Source (Stop + 1     .. Source'Last));
         end if;

         return No_Bracket (Source);
      exception
         when others =>
            Put_Line (Standard_Error, "Start=" & Natural'Image (Start) & ", Stop=" & Natural'Image (Stop));
            raise;
      end To_HTML;
   begin   -- Process_Line
      Line_Count := Line_Count + 1;

      case State is
         when Skip_To_Head =>
            if Match (Line, Head_Tag) then
               State := Skip_To_Rule;
            end if;

         when Skip_To_Rule =>
            if Match (Line, Rule_Tag) then
               State := In_Rule;
               Rule_Start;
            elsif Match (Line, End_Tag) then
               State := Terminated;
            end if;

         when In_Rule =>
            if Match (Line, Details_Tag) then
               Rule_End;
               State := Skip_To_Rule;
            elsif Match (Line, End_Tag) then
               Rule_End;
               State := Terminated;
            else
               case Command is
                  when Sonar_Rules =>
                     Put_Line (To_HTML (Line));
                  when Sonar_Profile =>
                     null;
                  when Rules =>
                     null;
               end case;
            end if;

         when Terminated =>
            raise Program_Error with "not terminated";
      end case;

   exception
      when Occur : others =>
         Put_Line (Standard_Error, "*** Unexpected exception, source line" & Natural'Image (Line_Count));
         Put_Line (Standard_Error, Line);
         Put_Line (Ada.Exceptions.Exception_Information (Occur));
         raise;
   end Process_Line;

begin  -- Gen_Adactl
   Initialize;
   loop
      Process_Line (Get_Line);
      exit when State = Terminated;
   end loop;
   Finalize;

exception
   when Cancelled =>
      null;
   when End_Error =>
      Finalize;
end Gen_Adactl;
