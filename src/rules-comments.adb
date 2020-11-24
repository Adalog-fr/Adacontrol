----------------------------------------------------------------------
--  Rules.Comments - Package body                                   --
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
  Ada.Unchecked_Deallocation,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Text;

-- Adalog
with
  A4G_Bugs,
  String_Matching,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Reports.Fixes,
  Framework.Language,
  Framework.Language.Shared_Keys;
pragma Elaborate (Framework.Language);

package body Rules.Comments is
   use Framework, Framework.Control_Manager, Framework.Language.Shared_Keys;

   type Subrules is (Pattern, Position, Terminating, Unnamed_Begin, Unnamed_End_Record);
   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules);
   type Subrules_Set is array (Subrules) of Boolean;
   No_Rule : constant Subrules_Set := (others => False);

   Rule_Used : Subrules_Set := No_Rule;
   Save_Used : Subrules_Set;

   -- Declarations for Pattern
   type Pattern_Access is access String_Matching.Compiled_Pattern;

   type Pattern_Context;   -- for subrule Pattern
   type Pattern_Context_Access is access Pattern_Context;
   type Pattern_Context is new Basic_Rule_Context with
      record
         Pattern : Pattern_Access;
         Next    : Pattern_Context_Access;
      end record;

   type Pattern_Node;     -- for subrule Terminating
   type Pattern_Node_Access is access Pattern_Node;
   type Pattern_Node is
      record
         Pattern : Pattern_Access;
         Next    : Pattern_Node_Access;
      end record;

   Pattern_Contexts : Pattern_Context_Access;

   -- Declarations for Position
   Pos_Labels : array (Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Pos_Bounds : array (Control_Kinds) of Bounds_Values := (others => Unlimited_Bounds);

   -- Declarations for Terminating
   type Terminating_Context is new Basic_Rule_Context with
      record
         Begin_Allowed      : Boolean;
         End_Allowed        : Boolean;
         End_Record_Allowed : Boolean;
         Pattern_List       : Pattern_Node_Access;
      end record;

   Terminating_Contexts : Terminating_Context;

   -- Declarations for Unnamed_Begin
   type Units is (U_All, U_Procedure, U_Function, U_Entry, U_Package, U_Task);
   subtype True_Units is Units range Units'Succ (U_All) .. Units'Last;
   package Units_Flags_Utilities is new Framework.Language.Flag_Utilities (Units, Prefix => "U_");

   type Decl_Conditions is (Always, Declaration, Program_Unit);
   package Decl_Conditions_Utilities is new Framework.Language.Modifier_Utilities (Decl_Conditions);

   type Unnamed_Begin_Context is new Basic_Rule_Context with
      record
         Condition : Decl_Conditions;
         Nb_Lines  : Asis.Text.Line_Number;
      end record;

   Units_Used             : array (True_Units) of Boolean := (others => False);
   Unnamed_Begin_Contexts : array (True_Units) of Unnamed_Begin_Context;

   -- Declarations for Unnamed_End_Record
   type Unnamed_End_Record_Context is new Basic_Rule_Context with
      record
         Used     : Boolean := False;
         Nb_Lines : Asis.Text.Line_Number;
      end record;

   Unnamed_End_Record_Contexts : array (Control_Kinds) of Unnamed_End_Record_Context;

   -----------
   -- Clear --
   -----------

   procedure Clear (Rec : in out Pattern_Context_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Pattern_Context, Pattern_Context_Access);
      procedure Free is new Ada.Unchecked_Deallocation (String_Matching.Compiled_Pattern, Pattern_Access);
      Temp : Pattern_Context_Access := Rec;
   begin
      while Rec /= null loop
         Temp := Rec.Next;
         Free (Rec.Pattern);
         Free (Rec);
         Rec := Temp;
      end loop;
   end Clear;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded, Asis.Text;
      use Framework.Language, String_Matching;
      use Decl_Conditions_Utilities, Subrules_Flags_Utilities, Units_Flags_Utilities, Utilities;
      Sr  : Subrules;
      Dc  : Decl_Conditions;
      Min : Line_Number;
      Un  : Units;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      Sr := Get_Flag_Parameter (Allow_Any => False);

      case Sr is
         when Pattern =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "at least one parameter required");
            end if;

            while Parameter_Exists loop
               declare
                  Pat : constant Wide_String := Get_String_Parameter;
               begin
                  Pattern_Contexts := new Pattern_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                          Pattern => new Compiled_Pattern'(Compile (Pat, Ignore_Case => True)),
                                          Next    => Pattern_Contexts);
               exception
                  when Pattern_Error =>
                     Parameter_Error (Rule_Id, "Incorrect pattern: " & Pat);
               end;
            end loop;

         when Position =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "position parameter required");
            end if;

            if Pos_Bounds (Ctl_Kind) /= Unlimited_Bounds then
               Parameter_Error (Rule_Id,
                                "subrule ""Position"" already specified for " & Control_Kinds'Wide_Image(Ctl_Kind));
            end if;

            Pos_Bounds (Ctl_Kind) := Get_Bounds_Parameters (Rule_Id, Bound_Min => 1, Allow_Single => True);
            Pos_Labels (Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Rule_Used (Position ) := True;

         when Terminating =>
            if Rule_Used (Terminating) then
               Parameter_Error (Rule_Id, "subrule ""Terminating"" already specified");
            end if;

            Basic_Rule_Context (Terminating_Contexts) := Basic.New_Context (Ctl_Kind, Ctl_Label);
            Terminating_Contexts.Begin_Allowed        := False;
            Terminating_Contexts.End_Allowed          := False;
            if Parameter_Exists then
               while Parameter_Exists loop
                  if Is_String_Parameter then
                     declare
                        Pat : constant Wide_String := Get_String_Parameter;
                     begin
                        -- Since the rule is allowed only once, the context is the same for
                        -- every element of the list. Oh, well...
                        Terminating_Contexts.Pattern_List := new Pattern_Node'
                          (Pattern => new Compiled_Pattern'(Compile (Pat, Ignore_Case => True)),
                           Next    => Terminating_Contexts.Pattern_List);
                     exception
                        when Pattern_Error =>
                           Parameter_Error (Rule_Id, "Incorrect pattern: " & Pat);
                     end;
                  else
                     declare
                        Key : constant Wide_String := Get_Name_Parameter;
                     begin
                        if Key = "BEGIN" then
                           Terminating_Contexts.Begin_Allowed := True;
                        elsif Key = "END" then
                           Terminating_Contexts.End_Allowed := True;
                        elsif Key = "END_RECORD" then
                           Terminating_Contexts.End_Record_Allowed := True;
                        else
                           Parameter_Error (Rule_Id, "Incorrect keyword: " & Key);
                        end if;
                     end;
                  end if;
               end loop;
            else
               Terminating_Contexts.Pattern_List := null;
            end if;

         when Unnamed_Begin =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "Parameter required");
            end if;

            while Parameter_Exists loop
               Dc  := Get_Modifier (Required  => False);
               if Get_Modifier ("MIN") then
                  Min := Get_Integer_Modifier (Min => 0);
               else
                  Min := 0;
               end if;
               Un := Get_Flag_Parameter (Allow_Any => False);
               if Un = U_All then
                  if Units_Used /= (True_Units => False) then
                     Parameter_Error (Rule_Id, "Subrule already specified");
                  end if;
                  Unnamed_Begin_Contexts := (others => (Basic.New_Context (Ctl_Kind, Ctl_Label) with Dc, Min));
                  Units_Used             := (others => True);
               else
                  if Units_Used (Un) then
                     Parameter_Error (Rule_Id, "Subrule already specified for " & Image (Un, Lower_Case));
                  end if;
                  Unnamed_Begin_Contexts (Un) := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Dc, Min);
                  Units_Used (Un)             := True;
               end if;
            end loop;

         when Unnamed_End_Record =>
            if Unnamed_End_Record_Contexts (Ctl_Kind).Used then
               Parameter_Error (Rule_Id,
                                "Subrule ""Unnamed_End_Record"" already specified for"
                                & Control_Kinds'Wide_Image (Ctl_Kind));
            end if;

            Unnamed_End_Record_Contexts (Ctl_Kind) := (Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                                       Used      => True,
                                                       Nb_Lines  => (if Parameter_Exists then Get_Integer_Parameter
                                                                                         else 0));
      end case;
      Rule_Used (Sr) := True;
   end Add_Control;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control the content or placement of comments");
      User_Message;
      Subrules_Flags_Utilities.Help_On_Flags ("Parameter(1)");
      User_Message;
      User_Message ("for pattern:");
      User_Message ("Parameter(2..): ""<comment pattern>""");
      User_Message;
      User_Message ("for position:");
      User_Message ("Parameter(2..3): [<bound>] <value>");
      Help_On_Bounds (Header => "      <bound>     : ");
      User_Message ("                 (at least one parameter required)");
      User_Message;
      User_Message ("for terminating:");
      User_Message ("Parameter(2..): ""<allowed pattern>"" | begin | end");
      User_Message;
      User_Message ("for unnamed_begin:");
      User_Message ("Parameter(2..): [<condition>] [min <value>] <unit>");
      Decl_Conditions_Utilities.Help_On_Modifiers (Header => "<condition>:");
      Units_Flags_Utilities.Help_On_Flags         (Header => "<unit>     :");
      User_Message;
      User_Message ("for unnamed_end_record:");
      User_Message ("Parameter(2): <nb lines>");
   end Help;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := No_Rule;
            Clear (Pattern_Contexts);
            Units_Used := (others => False);
            for C : Unnamed_End_Record_Context of Unnamed_End_Record_Contexts loop
               C.Used := False;
            end loop;

         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ----------------------
   -- Check_Identifier --
   ----------------------

   -- Check that a comment containing the name of Identifier appears after location After (but on the same line),
   -- unless Optional.
   -- Cont is the context for the error message, where Keyword is the keyword(s) of the check
   -- (i.e. "begin", "end record")
   type Identifier_Status is (Ok, Not_Reported, Reported);
   procedure Check_Identifier (Identifier : in  Asis.Element;
                               After      : in  Framework.Locations.Location;
                               Optional   : in  Boolean;
                               Keyword    : in  Wide_String;
                               Cont       : in  Basic_Rule_Context'Class;
                               Result     : out Identifier_Status)
   is
      use Ada.Strings, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Maps;
      use Asis, Asis.Text;
      use Framework.Locations, Framework.Reports, Thick_Queries, Utilities;

      Line_Nb    : constant Line_Number  := Get_First_Line (After);
      Line_Text  : constant Program_Text := To_Upper (Trim (Line_Image
                                                            (Lines (Identifier, Line_Nb, Line_Nb) (Line_Nb)),
                                                      Side => Right));
      -- Ignore trailing spaces to avoid problems in the replacement of the comment
      Comment_Pos : Natural := 0;
      Name_Inx    : Natural;
      In_String   : Boolean := False;
   begin
      -- Find start of comment, but beware of string literals
      for Inx in Line_Text'Range loop
         if In_String then
            if Line_Text (Inx) = '"' then
               In_String := False;
            end if;
         else
            case Line_Text (Inx) is
               when '-' =>
                  if Inx /= Line_Text'Last and then Line_Text (Inx + 1) = '-' then
                     -- Comment found
                     Comment_Pos := Inx;
                     exit;
                  end if;
               when '"' =>
                  In_String := True;
               when others =>
                  null;
            end case;
         end if;
      end loop;

      if Comment_Pos = 0 then -- No comment
         if Optional then
            Result := Not_Reported;
         else
            Result := Reported;
            Report (Rule_Id,
                    Cont,
                    After+1,
                    '"' & Keyword & """ has no comment naming " & Extended_Name_Image (Identifier));
            if Cont.Ctl_Kind /= Count then
               Fixes.Insert ("  -- " & Extended_Name_Image (Identifier), From => After + 1);
            end if;
         end if;
      elsif Comment_Pos = Line_Text'Last - 1 then  -- Empty comment
         if Optional then
            Result := Not_Reported;
         else
            Result := Reported;
            Report (Rule_Id,
                    Cont,
                    After + 1,
                    '"' & Keyword & """ has no comment naming " & Extended_Name_Image (Identifier));
            if Cont.Ctl_Kind /= Count then
               Fixes.Insert (' ' & Extended_Name_Image (Identifier),
                             From => Create_Location (Get_File_Name (After), Line_Nb, Line_Text'Last) + 1);
            end if;
         end if;
      else
         declare
            Name : constant Program_Text := Extended_Name_Image (Identifier);
         begin
            Name_Inx := Comment_Pos + 2;
            while Name_Inx <= Line_Text'Last and then Is_In (Line_Text (Name_Inx), Separators) loop
               Name_Inx := Name_Inx + 1;
            end loop;

            if Line_Text (Name_Inx .. Positive'Min (Line_Text'Last, Name_Inx + Name'Length - 1)) = To_Upper (Name)
              and then (Line_Text'Last = Name_Inx + Name'Length - 1
                        or else Line_Text (Name_Inx + Name'Length) in ' ' | Wide_HT | ',')
            then
               -- Identifer alone or followed by space or ','
               Result := Ok;
            else
               Result := Reported;
               Report (Rule_Id,
                       Cont,
                       After + 1,
                       '"' & Keyword & """ comment does not name " & Extended_Name_Image (Identifier));
               if Cont.Ctl_Kind /= Count then
                  Fixes.Replace (From   => Create_Location (Get_File_Name (After), Line_Nb, Name_Inx),
                                 Length => Line_Text'Last - Name_Inx + 1,
                                 By     => Name);
               end if;
            end if;
         end;
      end if;
   end Check_Identifier;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Locations.Location) is
      use Framework.Locations, Framework.Reports, String_Matching, Thick_Queries, Utilities;
      use Ada.Strings.Wide_Maps, Ada.Strings.Wide_Unbounded;
      use Asis.Text;   -- Gela-ASIS compatibility

      type Found_State is (Nothing_Found, Begin_Found, End_Found, End_Record_Found, Others_Found);
      State     : Found_State := Nothing_Found;
      Inx       : Natural;
      In_String : Boolean := False;

      Current_P : Pattern_Context_Access;
      Current_T : Pattern_Node_Access;
      Start     : Natural := 0;
      Matched   : Boolean;
   begin
      if not (Rule_Used (Pattern) or Rule_Used (Terminating) or Rule_Used (Position)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Find start of comment, note if there is anything but spaces before it
      Inx := Line'First;
      while Inx <= Line'Last loop  -- Can't use a "for" loop because we skip characters
         if In_String then
            if Line (Inx) = '"' then
               In_String := False;
            end if;
         else
            case Line (Inx) is
               when Wide_Character'First .. ' ' =>
                  null;
               when '-' =>
                  if Inx /= Line'Last and then Line (Inx + 1) = '-' then
                     -- Comment found
                     Start := Inx;
                     exit;
                  end if;
                  State := Others_Found;
               when '"' =>
                  In_String := True;
                  State     := Others_Found;
               when 'b' | 'B' =>
                  if State /= Nothing_Found then
                     State := Others_Found;
                  elsif Inx + 4 <= Line'Last and then To_Upper (Line (Inx .. Inx + 4)) = "BEGIN" then
                     State := Begin_Found;
                     Inx := Inx + 4;
                  else
                     State := Others_Found;
                  end if;
               when 'e' | 'E' =>
                  if State /= Nothing_Found then
                     State := Others_Found;
                  elsif Inx + 2 <= Line'Last and then To_Upper (Line (Inx .. Inx + 2)) = "END" then
                     State := End_Found;
                     Inx := Inx + 2;
                  else
                     State := Others_Found;
                  end if;
               when 'r' | 'R' =>
                  if State = End_Found
                    and then Inx + 5 <= Line'Last
                    and then To_Upper (Line (Inx .. Inx + 5)) = "RECORD"
                  then
                     State := End_Record_Found;
                  end if;
               when ';' =>
                  if State /= End_Found then
                     -- Allow semi-colon following "end"
                     State := Others_Found;
                  end if;
               when others =>
                  State := Others_Found;
            end case;
         end if;
         Inx := Inx + 1;
      end loop;

      if Start = 0 then
         return;
      end if;

      if Rule_Used (Position) then
         if not Is_In (Biggest_Int (Start), Pos_Bounds (Check)) then
            Report (Rule_Id,
                    To_Wide_String (Pos_Labels (Check)),
                    Check,
                    Loc,
                    "comment start position is " & Bound_Image (Pos_Bounds (Check)));
         elsif not Is_In (Biggest_Int (Start), Pos_Bounds (Search)) then
            Report (Rule_Id,
                    To_Wide_String (Pos_Labels (Search)),
                    Search,
                    Loc,
                    "comment start position is " & Bound_Image (Pos_Bounds (Search)));
         end if;

         if not Is_In (Biggest_Int (Start), Pos_Bounds (Count)) then
            Report (Rule_Id,
                    To_Wide_String (Pos_Labels (Count)),
                    Count,
                    Loc,
                    "");
         end if;
      end if;

      -- Skip spaces following "--"
      Start := Start + 2;
      while Start <= Line'Last and then Is_In (Line (Start), Separators) loop
         Start := Start + 1;
      end loop;

      if Rule_Used (Pattern) and Start <= Line'Last then
         Current_P := Pattern_Contexts;
         while Current_P /= null loop
            if Match (Line (Start .. Line'Last), Current_P.Pattern.all) then
               Report (Rule_Id,
                       Current_P.all,
                       Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Character_Position (Start)),
                       '"' & Line (Start .. Line'Last) & '"');
            end if;
            Current_P := Current_P.Next;
         end loop;
      end if;

      if Rule_Used (Terminating) and State /= Nothing_Found then
         Matched := False;
         if Terminating_Contexts.Begin_Allowed and State = Begin_Found then
            Matched := True;
         elsif Terminating_Contexts.End_Allowed and State = End_Found then
            Matched := True;
         elsif Terminating_Contexts.End_Record_Allowed and State = End_Record_Found then
            Matched := True;
         else
            Current_T := Terminating_Contexts.Pattern_List;
            while Current_T /= null loop
               if Match (Line (Start .. Line'Last), Current_T.Pattern.all) then
                  Matched := True;
                  exit;
               end if;
               Current_T := Current_T.Next;
            end loop;
         end if;
         if not Matched then
            Report (Rule_Id,
                    Terminating_Contexts,
                    Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Character_Position (Start)),
                    "Not an allowed terminating comment");
         end if;
      end if;
   end Process_Line;

   --------------------------
   -- Process_Program_Unit --
   --------------------------

   procedure Process_Program_Unit (Unit : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Framework.Locations, Thick_Queries, Utilities;


      Un : Units;
   begin   -- Process_Program_Unit
      if not Rule_Used (Unnamed_Begin) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Unit) is
         when A_Procedure_Body_Declaration =>
            Un := U_Procedure;
         when A_Null_Procedure_Declaration =>
            -- there is no begin... not applicable
            return;
         when A_Function_Body_Declaration =>
            Un := U_Function;
         when An_Entry_Body_Declaration =>
            Un := U_Entry;
         when A_Package_Body_Declaration =>
            Un := U_Package;
         when A_Task_Body_Declaration =>
            Un := U_Task;
         when others =>
            Failure ("Bad program unit", Unit);
      end case;

      if not Units_Used (Un) then
         return;
      end if;

      declare
         Stmts    : constant Statement_List := Statements (Unit);
         Optional : Boolean := True;
         Ignored : Identifier_Status;
      begin
         if Stmts = Nil_Element_List then
            return;
         end if;

         if A4G_Bugs.Last_Line_Number (Stmts (Stmts'Last)) - A4G_Bugs.First_Line_Number (Stmts (Stmts'First)) + 1
            >= Unnamed_Begin_Contexts (Un).Nb_Lines
         then
            case Unnamed_Begin_Contexts (Un).Condition is
               when Always =>
                  Optional := False;
               when Declaration =>
                  Optional := Declarative_Items (Unit) = Nil_Element_List;
               when Program_Unit =>
                  for D : Asis.Element of Declarative_Items (Unit) loop
                     case Declaration_Kind (D) is
                        when A_Procedure_Declaration
                           | A_Null_Procedure_Declaration
                           | A_Generic_Procedure_Declaration
                           | A_Procedure_Body_Declaration

                           | A_Function_Declaration
                           | An_Expression_Function_Declaration   -- Ada 2012
                           | A_Generic_Function_Declaration
                           | A_Function_Body_Declaration

                           | An_Entry_Body_Declaration

                           | A_Package_Declaration
                           | A_Generic_Package_Declaration
                           | A_Package_Body_Declaration

                           | A_Task_Body_Declaration
                           | A_Protected_Body_Declaration
                           =>
                           Optional := False;
                           exit;
                        when others =>
                           null;
                     end case;
                  end loop;
            end case;
         end if;

         Check_Identifier (Identifier => Names (Unit) (1),
                           After      => Get_Previous_Word_Location (Stmts, "BEGIN") + 4,
                           Optional   => Optional,
                           Keyword    => "begin",
                           Cont       => Unnamed_Begin_Contexts (Un),
                           Result     => Ignored);
      end;
   end Process_Program_Unit;

   -------------------------------
   -- Process_Record_Definition --
   -------------------------------

   procedure Process_Record_Definition (Def : Asis.Definition) is
      use Asis.Declarations, Asis.Elements;
      use Framework.Locations;
      -- Reminder: A_Record_Definition in A_Record_Type_Definition in An_Ordinary_Type_Declaration
   begin
      if not Rule_Used (Unnamed_End_Record) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         use Asis.Text;
         Decl   : constant Asis.Declaration   := Enclosing_Element (Enclosing_Element (Def));
         Name   : constant Asis.Defining_Name := Names (Decl) (1);
         Loc    : constant Location           := Get_End_Location (Decl);
         Length : constant Line_Number        :=   A4G_Bugs.Last_Line_Number  (Decl)
                                                 - A4G_Bugs.First_Line_Number (Decl)
                                                 + 1;
         Result : Identifier_Status;
      begin
         if Unnamed_End_Record_Contexts (Check).Used then
            Check_Identifier (Identifier => Name,
                              After      => Loc,
                              Optional   => Length <= Unnamed_End_Record_Contexts (Check).Nb_Lines,
                              Keyword    => "end record",
                              Cont       => Unnamed_End_Record_Contexts (Check),
                              Result     => Result);
         else
            Result := Not_Reported;
         end if;

         if Unnamed_End_Record_Contexts (Search).Used and Result = Not_Reported then
            Check_Identifier (Identifier => Name,
                              After      => Loc,
                              Optional   => Length <= Unnamed_End_Record_Contexts (Search).Nb_Lines,
                              Keyword    => "end record",
                              Cont       => Unnamed_End_Record_Contexts (Search),
                              Result     => Result);
         end if;

         if Unnamed_End_Record_Contexts (Count).Used then
            pragma Warnings (Off, Result); -- Warning that Result is modified and not used
            Check_Identifier (Identifier => Name,
                              After      => Loc,
                              Optional   => Length <= Unnamed_End_Record_Contexts (Count).Nb_Lines,
                              Keyword    => "end record",
                              Cont       => Unnamed_End_Record_Contexts (Count),
                              Result     => Result);
         end if;
      end;
   end Process_Record_Definition;

begin  -- Rules.Comments
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic_Textual,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Comments;
