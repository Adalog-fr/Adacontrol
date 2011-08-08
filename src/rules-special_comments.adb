----------------------------------------------------------------------
--  Rules.Special_Comments - Package body                           --
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
  Ada.Characters.Latin_1,
  Ada.Unchecked_Deallocation,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Text;

-- Adalog
with
  String_Matching,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Special_Comments is
   use Framework;

   type Subrules is (Pattern, Unnamed_Begin);
   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules);
   type Subrules_Set is array (Subrules) of Boolean;
   No_Rule : constant Subrules_Set := (others => False);

   type Units is (U_All, U_Procedure, U_Function, U_Entry, U_Package, U_Task);
   subtype True_Units is Units range Units'Succ (U_All) .. Units'Last;
   package Units_Flags_Utilities is new Framework.Language.Flag_Utilities (Units, Prefix => "U_");

   type Decl_Conditions is (Always, Declaration, Program_Unit);
   package Decl_Conditions_Utilities is new Framework.Language.Modifier_Utilities (Decl_Conditions);

   Rule_Used : Subrules_Set := No_Rule;
   Save_Used : Subrules_Set;

   type Pattern_Context;
   type Pattern_Context_Access is access Pattern_Context;
   type Pattern_Access is access String_Matching.Compiled_Pattern;
   type Pattern_Context is new Basic_Rule_Context with
      record
         Pattern : Pattern_Access;
         Next    : Pattern_Context_Access;
      end record;

   type Unnamed_Context is new Basic_Rule_Context with
      record
         Condition : Decl_Conditions;
      end record;

   Pattern_Contexts : Pattern_Context_Access;

   Units_Used       : array (True_Units) of Boolean := (others => False);
   Unnamed_Contexts : array (True_Units) of Unnamed_Context;

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
      use String_Matching, Framework.Language;
      use Decl_Conditions_Utilities, Subrules_Flags_Utilities, Units_Flags_Utilities, Utilities;
      Sr : Subrules;
      Dc : Decl_Conditions;
      Un : Units;
   begin
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
         when Unnamed_Begin =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "Parameter required");
            end if;

            while Parameter_Exists loop
               Dc := Get_Modifier       (Required  => False);
               Un := Get_Flag_Parameter (Allow_Any => False);
               if Un = U_All then
                  if Units_Used /= (True_Units => False) then
                     Parameter_Error (Rule_Id, "subrule already specified");
                  end if;
                  Unnamed_Contexts := (others => (Basic.New_Context (Ctl_Kind, Ctl_Label) with Dc));
                  Units_Used       := (others => True);
               else
                  if Units_Used (Un) then
                     Parameter_Error (Rule_Id, "subrule already specified for " & Image (Un, Lower_Case));
                  end if;
                  Unnamed_Contexts (Un) := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Dc);
                  Units_Used (Un)       := True;
               end if;
            end loop;
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
      Subrules_Flags_Utilities.Help_On_Flags ("Parameter (1)");
      User_Message ("for pattern:");
      User_Message ("   Parameter(2..): ""<comment pattern>""");
      User_Message ("for unnamed_begin:");
      User_Message ("   Parameter(2..): [<condition>] <unit>");
      Decl_Conditions_Utilities.Help_On_Modifiers (Header => "      <condition>:");
      Units_Flags_Utilities.Help_On_Flags         (Header => "           <unit>:");
      User_Message ("Control comments that match the specified pattern");
      User_Message ("or ""begin"" without a comment identifying the program unit");
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
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ------------------
   -- Process_Line --
   ------------------

   Separators : constant Ada.Strings.Wide_Maps.Wide_Character_Set
     := Ada.Strings.Wide_Maps.To_Set (Ada.Characters.Handling.To_Wide_String (' ' & Ada.Characters.Latin_1.HT));

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Location) is
      use String_Matching, Framework.Reports, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Maps;
      Current : Pattern_Context_Access;
      Start   : Natural;
   begin
      if not Rule_Used (Pattern) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Start := Index (Line, "--");
      if Start = 0 then
         return;
      end if;

      Start := Start + 2;
      while Start <= Line'Last and then Is_In (Line (Start), Separators) loop
         Start := Start + 1;
      end loop;
      if Start > Line'Last then
         return;
      end if;

      Current := Pattern_Contexts;
      while Current /= null loop
         if Match (Line (Start .. Line'Last), Current.Pattern.all) then
            Report (Rule_Id,
                    Current.all,
                    Create_Location (Get_File_Name (Loc), Get_First_Line (Loc), Start),
                    '"' & Line (Start .. Line'Last) & '"');
         end if;
         Current := Current.Next;
      end loop;
   end Process_Line;

   --------------------------
   -- Process_Program_Unit --
   --------------------------

   procedure Process_Program_Unit (Unit : Asis.Declaration) is
      use Ada.Strings, Ada.Strings.Wide_Fixed;
      use Asis, Asis.Declarations, Asis.Elements, Asis.Text;
      use Reports, Thick_Queries, Utilities;

      Un : Units;
   begin
      if not Rule_Used (Unnamed_Begin) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Unit) is
         when A_Procedure_Body_Declaration =>
            Un := U_Procedure;
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

      case Unnamed_Contexts (Un).Condition is
         when Always =>
            null;
         when Declaration =>
            if Declarative_Items (Unit) = Nil_Element_List then
               return;
            end if;
         when Program_Unit =>
            declare
               Decls      : constant Asis.Declaration_List := Declarative_Items (Unit);
               Unit_Found : Boolean := False;
            begin
               for D in Decls'Range loop
                  case Declaration_Kind (Decls (D)) is
                     when A_Procedure_Declaration
                        | A_Generic_Procedure_Declaration
                        | A_Procedure_Body_Declaration

                        | A_Function_Declaration
                        | A_Generic_Function_Declaration
                        | A_Function_Body_Declaration

                        | An_Entry_Body_Declaration

                        | A_Package_Declaration
                        | A_Generic_Package_Declaration
                        | A_Package_Body_Declaration

                        | A_Task_Body_Declaration
                        | A_Protected_Body_Declaration
                          =>
                        Unit_Found := True;
                        exit;
                     when others =>
                        null;
                  end case;
               end loop;
               if not Unit_Found then
                  return;
               end if;
            end;
      end case;

      declare
         Stmts : constant Statement_List := Statements (Unit);
      begin
         if Stmts = Nil_Element_List then
            return;
         end if;
         declare
            Begin_Loc  : constant Location     := Get_Previous_Word_Location (Stmts, "BEGIN");
            Begin_Line : constant Natural      := Get_First_Line (Begin_Loc);
            Begin_Text : constant Program_Text := To_Upper (Line_Image
                                                            (Lines (Unit, Begin_Line, Begin_Line) (Begin_Line)));
            Comment_Pos : constant Natural := Index (Begin_Text, "--");
            Name_Inx    : Natural;
         begin
            if Comment_Pos = 0 then
               Report (Rule_Id,
                       Unnamed_Contexts (Un),
                       Begin_Loc,
                       """begin"" has no unit name comment for " & Defining_Name_Image (Names (Unit) (1)));
            else
               Name_Inx := Index (Begin_Text (Comment_Pos .. Begin_Text'Last), " ", Going => Backward);
               if Name_Inx = 0 then
                  Name_Inx := Comment_Pos + 2; -- just after "--"
               else
                  Name_Inx := Name_Inx + 1;
               end if;
               if Begin_Text (Name_Inx .. Begin_Text'Last) /= To_Upper (Defining_Name_Image (Names (Unit) (1))) then
                  Report (Rule_Id,
                          Unnamed_Contexts (Un),
                          Begin_Loc,
                          """begin"" comment does not name " & Defining_Name_Image (Names (Unit) (1)));
               end if;
            end if;
         end;
      end;
   end Process_Program_Unit;

begin  -- Rules.Special_Comments
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic_Textual,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Special_Comments;
