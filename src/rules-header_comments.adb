----------------------------------------------------------------------
--  Rules.Header_Comments - Package body                            --
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
  Ada.Exceptions,
  Ada.Strings.Wide_Unbounded,
  Ada.Wide_Text_IO;

-- Adalog
with
  Implementation_Options,
  String_Matching,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Header_Comments is
   use Framework;

   subtype Pattern_String is Wide_String (1 .. 512);  -- Size is arbitrary, should be sufficient

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Uninitialized : constant Integer := 0;

   Ctl_Labels : array (Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Comments   : array (Control_Kinds) of Integer := (others => Uninitialized);

   type Subrules is (Minimum, Model);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   Reported       : array (Control_Kinds) of Boolean;
   Model_File     : Ada.Wide_Text_IO.File_Type;
   Model_Kind     : Control_Kinds;
   Model_Label    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Model_Reported : Boolean;

   Wide_HT : constant Wide_Character := Ada.Characters.Handling.To_Wide_Character (ASCII.HT);

   -- The same pattern can be used several times, hence it needs to be global
   pragma Warnings (Off);
   -- Gnat warns that Pattern and Pat_Last may be referenced before they have a value,
   -- but this cannot happen because Line_Repeat is initialized to No_Repeat (in Enter_Unit)
   Pattern  : Pattern_String;
   Pat_Last : Natural;
   pragma Warnings (Off);

   Stop_Pattern  : Pattern_String;
   Stop_Pat_Last : Natural;
   Stop_Has_Star : Boolean;

   type Line_Match_States is (Repeat, Single);
   Matcher_State : Line_Match_States;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Characters.Handling, Ada.Exceptions, Ada.Strings.Wide_Unbounded, Ada.Wide_Text_IO;
      use Framework.Language, String_Matching, Subrules_Flag_Utilities;

      Buff    : Pattern_String;
      Last    : Natural;
      Subrule : Subrules;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "kind of check required");
      end if;
      Subrule := Get_Flag_Parameter (Allow_Any => False);

      case Subrule is
         when Minimum =>
            if Comments (Ctl_Kind) /= Uninitialized then
               Parameter_Error (Rule_Id, "rule already specified");
            elsif not Parameter_Exists then
               Parameter_Error (Rule_Id, "number of comment lines required");
            end if;
            Comments   (Ctl_Kind) := Get_Integer_Parameter (Min => 1);
            Ctl_Labels (Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);

         when Model =>
            if Is_Open (Model_File) then
               Parameter_Error (Rule_Id, "model file already specified");
            elsif not Parameter_Exists then
               Parameter_Error (Rule_Id, "name of model file required");
            end if;

            begin
               Open (Model_File,
                     In_File,
                     To_String (Get_File_Parameter),
                     Form => Implementation_Options.Form_Parameters);
            exception
               when Name_Error =>
                  Parameter_Error (Rule_Id, "model file not found");
            end;
            -- check all patterns now to avoid problems while checking.
            loop
               begin
                  Get_Line (Model_File, Buff, Last);
                  if Buff (1 .. Last) = "*" then
                     begin
                        Get_Line (Model_File, Buff, Last);
                        if Buff (1 .. Last) = "*" then
                           Parameter_Error (Rule_Id, "several ""*"" lines in a row at "
                                              & To_Wide_String (Name (Model_File)) & ':'
                                              & Ada.Wide_Text_IO.Count'Wide_Image (Line (Model_File)));
                        end if;
                     exception
                        when End_Error =>
                           Parameter_Error (Rule_Id, "pattern file terminated by ""*"" line");
                     end;
                  end if;

                  if Last = Buff'Last then     --## rule line off Simplifiable_Statements ## If_For_Case
                     Parameter_Error (Rule_Id, "pattern too long at "
                                        & To_Wide_String (Name (Model_File)) & ':'
                                        & Ada.Wide_Text_IO.Count'Wide_Image (Line (Model_File)));
                  elsif Last /= 0 then
                     declare
                        Pat : constant Compiled_Pattern := Compile (Buff (1 .. Last));
                        pragma Unreferenced (Pat);
                     begin
                        null;
                     end;
                  end if;
               exception
                  when Occur : Pattern_Error =>
                     Parameter_Error (Rule_Id, "incorrect pattern "
                                      & " (" & To_Wide_String (Exception_Message (Occur)) & ") at "
                                      & To_Wide_String (Name (Model_File)) & ':'
                                      & Ada.Wide_Text_IO.Count'Wide_Image (Line (Model_File))
                                      & ": " & Buff (1 .. Last));
                  when End_Error =>
                     exit;
               end;
            end loop;
            Model_Kind  := Ctl_Kind;
            Model_Label := To_Unbounded_Wide_String (Ctl_Label);
      end case;

      Rule_Used := True;
   end Add_Control;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags ("Parameter (1):");
      User_Message ("For minimum:");
      User_Message ("   Parameter (2) : <Required number of comment lines>");
      User_Message ("For model:");
      User_Message ("   Parameter (2) : <model file>");
      User_Message ("Control that  each unit starts with at least indicated number of comment lines");
      User_Message ("or matches the specified model");
   end Help;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Wide_Text_IO;
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used    := False;
            Comments     := (others => Uninitialized);
            if Is_Open (Model_File) then
               Close (Model_File);
            end if;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ----------------
   -- Enter_Unit --
   ----------------

   procedure Enter_Unit is
      use Ada.Wide_Text_IO;
   begin
      for R in Control_Kinds loop
         Reported (R) := Comments (R) = Uninitialized;
      end loop;
      Model_Reported := False;
      if Is_Open (Model_File) then
         Reset (Model_File, In_File);
         Matcher_State := Single;
      end if;
   end Enter_Unit;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Location) is
      use Framework.Reports;
      use Ada.Strings.Wide_Unbounded;
      Line_Num : Natural;

      procedure Check_Comments_Number (Ctl_Kind : Control_Kinds) is
      begin
         if Comments (Ctl_Kind) < 1 or Reported (Ctl_Kind) then
            return;
         end if;

         if Line_Num > Comments (Ctl_Kind) then
            Reported (Ctl_Kind) := True;
            return;
         end if;

         for Inx in Natural range Line'First .. Line'Last - 1 loop
            if Line (Inx) /= ' ' and Line (Inx) /= Wide_HT then
               if Line (Inx) = '-' and Line (Inx + 1) = '-' then
                  -- OK, comment line
                  return;
               end if;
            end if;
         end loop;

         -- Here we have a non-comment line in the range where a check is required
         Report (Rule_Id, To_Wide_String (Ctl_Labels (Ctl_Kind)), Ctl_Kind, Loc,
                 "not enough header comment lines");
         Reported (Ctl_Kind) := True;
         if Ctl_Kind = Check and Comments (Search) >= 1 then
            Reported (Search) := True;
         end if;
      end Check_Comments_Number;

      procedure Check_Model is
         use Ada.Wide_Text_IO;

         function Line_Match (With_Pattern : Wide_String; Last : Natural) return Boolean is
            use String_Matching;
            -- True matching that considers that the empty line matches only the empty pattern
         begin
            if Line'Length = 0 or Last = 0 then
               return Last = Line'Length;
            else
               return Match (Line, With_Pattern (1 .. Last));
            end if;
         end Line_Match;

      begin
         if not Is_Open (Model_File) or Model_Reported then
            return;
         end if;

         case Matcher_State is
            when Single =>
               begin
                  Get_Line (Model_File, Pattern, Pat_Last);
               exception
                  when End_Error =>
                     Model_Reported := True;
                     return;
               end;

               if Pattern (1 .. Pat_Last) = "*" then
                  -- Remember that a "*" line is always followed by a regular line
                  -- (checked in Add_Control)
                  Get_Line (Model_File, Pattern, Pat_Last);

                  begin
                     Get_Line (Model_File, Stop_Pattern, Stop_Pat_Last);
                     Matcher_State := Repeat;
                     Stop_Has_Star := Stop_Pattern (1 .. Stop_Pat_Last) = "*" ;
                     if Stop_Has_Star then
                        Get_Line (Model_File, Stop_Pattern, Stop_Pat_Last);
                     end if;
                  exception
                     when End_Error =>
                        -- Nothing after "*" pattern: no need to check further
                        Model_Reported := True;
                        return;
                  end;

                  -- Retry in Repeat state
                  Check_Model;

               elsif Line_Match (Pattern, Pat_Last) then
                  null;

               else
                  Report (Rule_Id, To_Wide_String (Model_Label), Model_Kind, Loc,
                          "line does not match pattern """ & Pattern (1 .. Pat_Last) & '"');
                  Model_Reported := True;
               end if;

            when Repeat =>
               -- Check the stopping pattern first, to avoid "greedy" effects
               if Line_Match (Stop_Pattern, Stop_Pat_Last) then
                  Pattern  := Stop_Pattern;
                  Pat_Last := Stop_Pat_Last;
                  if Stop_Has_Star then
                     -- Stay in Repeat state
                     begin
                        Get_Line (Model_File, Stop_Pattern, Stop_Pat_Last);
                        Stop_Has_Star := Stop_Pattern (1 .. Stop_Pat_Last) = "*" ;
                        if Stop_Has_Star then
                           Get_Line (Model_File, Stop_Pattern, Stop_Pat_Last);
                        end if;
                     exception
                        when End_Error =>
                           -- Nothing after "*" pattern: no need to check further
                           Model_Reported := True;
                     end;
                  else
                     Matcher_State := Single;
                  end if;

               elsif Line_Match (Pattern, Pat_Last) then
                  null;

               else
                  Report (Rule_Id, To_Wide_String (Model_Label), Model_Kind, Loc,
                          "line does not match pattern """ & Stop_Pattern (1 .. Stop_Pat_Last) & '"');
                  Model_Reported := True;
               end if;
         end case;
      end Check_Model;

   begin
      if not Rule_Used
        or (Reported = (Control_Kinds => True) and Model_Reported)
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Line_Num := Get_First_Line (Loc);

      for R in Control_Kinds loop
         Check_Comments_Number (R);
      end loop;

      Check_Model;
  end Process_Line;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Textual,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Header_Comments;
