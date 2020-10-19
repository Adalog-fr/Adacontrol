----------------------------------------------------------------------
--  Rules.Header_Comments - Package body                            --
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

-- Ada
with
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Unbounded,
     Ada.Wide_Text_IO;

-- ASIS
with
  Asis.Text;

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

   -- Algorithm:
   --
   -- For model subrule:
   -- Current_Pattern holds the descriptor that must be matched by the current line.
   -- Next_Pattern holds the pattern that comes after it; in case of an optionally repeated
   -- pattern, it is checked first. Therefore, if the line matches both the current_pattern
   -- and the Next_Pattern, it is not considered a repetition of the Current_Pattern (avoid
   -- "greedy" effects).

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Uninitialized : constant Asis.ASIS_Integer := 0;

   Ctl_Labels : array (Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Comments   : array (Control_Kinds) of Asis.ASIS_Integer := (others => Uninitialized);

   type Subrules is (Minimum, Model);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   Reported       : array (Control_Kinds) of Boolean;
   Model_File     : Ada.Wide_Text_IO.File_Type;
   Model_Kind     : Control_Kinds;
   Model_Label    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Model_Reported : Boolean;

   Wide_HT : constant Wide_Character := Ada.Characters.Handling.To_Wide_Character (ASCII.HT);

   subtype Pattern_Length is Natural range 0 .. 512; -- Size is arbitrary, should be sufficient
   type Pattern_Descr (Length : Pattern_Length := 0) is
      record
         Required : Natural;
         Optional : Natural;
         Pattern  : Wide_String (1 .. Length);
      end record;
   Model_Sentinel : constant Pattern_Descr := (2, 0, 0, ".*");
   -- Used to indicate end of model. Pattern matches everything, but the sentinel
   -- is recognizable because Required = 0 and Optional = 0

   Current_Pattern : Pattern_Descr;
   Next_Pattern    : Pattern_Descr;

   --------------------
   -- Get_Repetition --
   --------------------

   package Natural_IO is new Ada.Wide_Text_IO.Integer_IO (Natural);

   procedure Get_Repetition (Buffer : Wide_String; Min, Max : out Natural) is
      use Ada.Wide_Text_IO, Natural_IO;

      Inx : Positive := 2; -- Assertion: Buffer (1) = '{'
      Last : Positive;

      procedure Skip_Blanks is
         -- Post-condition: Inx <= Buffer'Last
      begin
         loop
            if Inx > Buffer'Last then
               raise Data_Error;
            end if;
            exit when Buffer (Inx) /= ' ';
            Inx := Inx + 1;
         end loop;
      end Skip_Blanks;
   begin   -- Get_Repetition
      declare
         -- because of Bug [H621-003], Gnat 6.1.1 (Last is wrong if 'First is not 1)
         Gnat_Bug : constant Wide_String (1 .. Buffer'Last - Inx + 1) := Buffer (Inx .. Buffer'Last);
      begin
         Get (Gnat_Bug, Min, Last);
         Last := Last + Inx - 1;
      end;
      Inx := Last + 1;
      Skip_Blanks;

      if Buffer (Inx) /= ',' then
         raise Data_Error;
      end if;
      Inx := Inx + 1;
      Skip_Blanks;

      if Buffer (Inx) = '}' then
         Max := Natural'Last;
      else
         declare
            -- because of Bug [H621-003], Gnat 6.1.1 (Last is wrong if 'First is not 1)
            Gnat_Bug : constant Wide_String (1 .. Buffer'Last - Inx + 1) := Buffer (Inx .. Buffer'Last);
         begin
            Get (Gnat_Bug, Max, Last);
            Last := Last + Inx - 1;
         end;
         Inx := Last + 1;
         Skip_Blanks;
         if Buffer (Inx) /= '}' then
            raise Data_Error;
         end if;
      end if;
   end Get_Repetition;

   -----------------
   -- Get_Pattern --
   -----------------

   procedure Get_Pattern (Pat : out Pattern_Descr) is
      use Ada.Wide_Text_IO;
      Pat_Str  : Wide_String (1 .. Pattern_Length'Last);
      Pat_Last : Natural;
      Pat_Min  : Natural;
      Pat_Max  : Natural;
   begin
      Get_Line (Model_File, Pat_Str, Pat_Last);

      case Pat_Str (1) is
         when '*' =>
            Pat_Min := 0;
            Pat_Max := Natural'Last;
            Get_Line (Model_File, Pat_Str, Pat_Last);
         when '+' =>
            Pat_Min := 1;
            Pat_Max := Natural'Last;
            Get_Line (Model_File, Pat_Str, Pat_Last);
         when '?' =>
            Pat_Min := 0;
            Pat_Max := 1;
            Get_Line (Model_File, Pat_Str, Pat_Last);
         when '{' =>
            Get_Repetition (Pat_Str (1 .. Pat_Last), Pat_Min, Pat_Max);
            Get_Line (Model_File, Pat_Str, Pat_Last);
         when others =>
            Pat_Min := 1;
            Pat_Max := 1;
      end case;
      Pat := (Pat_Last, Pat_Min, Pat_Max-Pat_Min, Pat_Str (1 .. Pat_Last));
   end Get_Pattern;

   -----------------------
   -- Next_Pattern_Line --
   -----------------------

   procedure Next_Pattern_Line is
      use Ada.Wide_Text_IO;
   begin
      if Next_Pattern = Model_Sentinel then
         Model_Reported := True;
         return;
      end if;

      Current_Pattern := Next_Pattern;
      Get_Pattern (Next_Pattern);
   exception
      when End_Error =>
         Next_Pattern := Model_Sentinel;
   end Next_Pattern_Line;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Characters.Handling, Ada.Exceptions, Ada.Strings.Wide_Unbounded, Ada.Wide_Text_IO;
      use Framework.Language, String_Matching, Subrules_Flag_Utilities;

      Buff    : Wide_String (1 .. Pattern_Length'Last);
      Last    : Natural;
      Subrule : Subrules;
      Min     : Natural;
      Max     : Natural;

      procedure Model_Error (Mess : Wide_String) is
      begin
         Parameter_Error (Rule_Id, Mess
                                   & " at "
                                   & To_Wide_String (Name (Model_File)) & ':'
                                   & Ada.Wide_Text_IO.Count'Wide_Image (Line (Model_File))
                                   & ": " & Buff (1 .. Last));
      end Model_Error;

      use Asis;      --## Rule line off Reduceable_Scope Unnecessary_use_clause
                     --   Gela-ASIS compatibility
   begin   -- Add_Control
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
                  case Buff (1) is
                     when '*' | '+' | '?' =>
                        begin
                           Get_Line (Model_File, Buff, Last);
                        exception
                           when End_Error =>
                              Parameter_Error (Rule_Id, "pattern file terminated by line repetition indication");
                        end;
                     when '{' =>
                        begin
                           Get_Repetition (Buff (1 .. Last), Min, Max);
                           Get_Line (Model_File, Buff, Last);
                           if Max = 0 or Max < Min then
                              Model_Error ("Maximum value must be > 0 and >= minimum");
                           end if;
                        exception
                           when Data_Error =>
                              Model_Error ("illegal syntax for repetition indication");
                           when End_Error =>
                              Parameter_Error (Rule_Id, "pattern file terminated by line repetition indication");
                        end;
                     when others =>
                        null;
                  end case;

                  if Last = Buff'Last then     --## rule line off Simplifiable_Statements ## If_For_Case
                     Parameter_Error (Rule_Id, "pattern too long at "
                                        & To_Wide_String (Ada.Wide_Text_IO.Name (Model_File)) & ':'
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
                     Model_Error ("incorrect pattern "
                                  & " (" & To_Wide_String (Exception_Message (Occur)) & ')');
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
      User_Message ("Control that  each unit starts with at least indicated number of comment lines");
      User_Message ("or matches the specified model");
      User_Message;
      Help_On_Flags ("Parameter(1):");
      User_Message;
      User_Message ("For minimum:");
      User_Message ("Parameter(2): <Required number of comment lines>");
      User_Message;
      User_Message ("For model:");
      User_Message ("Parameter(2): <model file>");
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
      use type Asis.ASIS_Integer;   --## rule line off Unnecessary_Use_Clause Reduceable_Scope ## Gela compatibility
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      for R in Control_Kinds loop
         Reported (R) := Comments (R) = Uninitialized;
      end loop;
      Model_Reported := False;
      if Is_Open (Model_File) then
         Reset (Model_File, In_File);
         Get_Pattern (Next_Pattern);
         Next_Pattern_Line;
      end if;
   end Enter_Unit;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Locations.Location) is
      use Framework.Locations, Framework.Reports;
      use Ada.Strings.Wide_Unbounded;

      Line_Num : Asis.Text.Line_Number;

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

         function Line_Match (With_Pattern : Wide_String) return Boolean is
            use String_Matching;
            -- True matching that considers that the empty line matches only the empty pattern
         begin
            if Line'Length = 0 or With_Pattern'Length = 0 then
               return With_Pattern'Length = Line'Length;
            else
               return Match (Line, With_Pattern);
            end if;
         end Line_Match;

      begin  -- Check_Model
         if not Is_Open (Model_File) or Model_Reported then
            return;
         end if;

         loop
            if Current_Pattern.Required > 0 then
               if Line_Match (Current_Pattern.Pattern) then
                  Current_Pattern.Required := Current_Pattern.Required - 1;
                  if Current_Pattern.Required = 0 and Current_Pattern.Optional = 0 then
                     Next_Pattern_Line;
                  end if;
               else
                  Report (Rule_Id, To_Wide_String (Model_Label), Model_Kind, Loc,
                          "line does not match pattern """ & Current_Pattern.Pattern & '"');
                  Model_Reported := True;
               end if;
               exit;
            end if;

            -- Current_Pattern.Required = 0 here
            -- Check the next pattern first, to avoid "greedy" effects
            -- Note that it works only one pattern forward. Room for improvements.
            if Line_Match (Next_Pattern.Pattern) then
               Next_Pattern_Line;
               exit when Model_Reported;   -- End of Model
               -- Here we don't exit the loop, and will therefore recheck the same
               -- line against the next pattern, now in Current_Pattern.
            elsif Line_Match (Current_Pattern.Pattern) then
               Current_Pattern.Optional := Current_Pattern.Optional - 1;
               if Current_Pattern.Optional = 0 then
                  Next_Pattern_Line;
               end if;
               exit;
            elsif Next_Pattern.Required = 0 then
               -- Maybe the next pattern is not here, but it matches further down
               -- let's give it another try
               Next_Pattern_Line;
               exit when Model_Reported;   -- End of Model
               -- Here we don't exit the loop, and will therefore recheck the same
               -- line against the next pattern, now in Current_Pattern.
            else
               Report (Rule_Id, To_Wide_String (Model_Label), Model_Kind, Loc,
                       "line does not match pattern """ & Next_Pattern.Pattern & '"');
               Model_Reported := True;
               exit;
            end if;
         end loop;
      end Check_Model;

   begin  -- Process_Line
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

begin  -- Rules.Header_Comments
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Textual,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Header_Comments;
