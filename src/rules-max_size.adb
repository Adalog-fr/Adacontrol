----------------------------------------------------------------------
--  Rules.Max_Size - Package body                                   --
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
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Elements,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Max_Size is
   use Framework;
   use Asis.Text;

   type Statement_Names is (Stmt_Accept,        Stmt_Block,       Stmt_Case, Stmt_Case_Branch,
                            Stmt_If,            Stmt_If_Branch,   Stmt_Loop, Stmt_Simple_Block,
                            Stmt_Unnamed_Block, Stmt_Unnamed_Loop);

   package Statement_Flags_Utilities is new Framework.Language.Flag_Utilities (Statement_Names, "STMT_");
   use Statement_Flags_Utilities;

   Unused : constant Line_Number := Line_Number'Last;

   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Rule_Label : array (Statement_Names, Rule_Types) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type Max_Table is array (Rule_Types) of Line_Number;
   Maximum : array (Statement_Names) of Max_Table  := (others => (others => Unused));

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      Help_On_Flags ("Parameter(1):");
      User_Message  ("Parameter(2): maximum acceptable number of lines");
      User_Message  ("Control the maximum length of Ada statements");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language, Ada.Strings.Wide_Unbounded, Utilities;
      Stmt : Statement_Names;
      Max  : Line_Number_Positive;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "parameters required");
      end if;

      Stmt := Get_Flag_Parameter (Allow_Any => False);
      if Maximum (Stmt) (Rule_Type) /= Unused then
         Parameter_Error (Rule_Id, "statement already given for "
                                   & To_Lower (Rule_Types'Wide_Image (Rule_Type)) & ": "
                                   & Image (Stmt));
      end if;

      begin
         Max := Get_Integer_Parameter (Min => 1, Max => Unused - 1);
      exception
         when Constraint_Error =>
            Parameter_Error (Rule_Id, "maximum value negative or too big");
      end;

      Rule_Used                    := True;
      Rule_Label (Stmt, Rule_Type) := To_Unbounded_Wide_String (Label);
      Maximum (Stmt) (Rule_Type)    := Max;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := False;
            Maximum    := (others => (others => Unused));
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Statement : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements;

      procedure Do_Report (Stmt : in Statement_Names; Element : Asis.Element := Statement) is
         use Ada.Strings.Wide_Unbounded;
         use Framework.Reports, Utilities;

         Length : Line_Number_Positive;
         Loc    : Location;
      begin
         if Element_Kind (Element) = A_Path then
            declare
               Stats : constant Asis.Statement_List := Sequence_Of_Statements (Element);
            begin
               Loc    := Get_Location (Stats (Stats'First));
               Length := Last_Line_Number (Stats (Stats'Last)) - First_Line_Number (Stats (Stats'First)) + 1;
            end;
         else
            Loc    := Get_Location (Element);
            Length := Last_Line_Number (Element) - First_Line_Number (Element) + 1;
         end if;

         if Length > Maximum (Stmt) (Check) then
            Report (Rule_Id,
                    To_Wide_String (Rule_Label (Stmt, Check)),
                    Check,
                    Loc,
                    "statement """ & Image (Stmt)
                    & """ is more than " & Integer_Img (Maximum (Stmt) (Check)) & " lines long ("
                    & Integer_Img (Length) & ')');
         elsif Length > Maximum (Stmt) (Search) then
            Report (Rule_Id,
                    To_Wide_String (Rule_Label (Stmt, Search)),
                    Search,
                    Loc,
                    "statement """ & Image (Stmt)
                    & """ is more than " & Integer_Img (Maximum (Stmt) (Search)) & " lines long ("
                    & Integer_Img (Length) & ')');
         end if;

         if Length > Maximum (Stmt) (Count) then
            Report (Rule_Id,
                    To_Wide_String (Rule_Label (Stmt, Count)),
                    Count,
                    Loc,
                    "");
         end if;
      end Do_Report;

   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Statement_Kind (Statement) is
         when An_Accept_Statement =>
            Do_Report (Stmt_Accept);

         when A_Block_Statement =>
            Do_Report (Stmt_Block);
            if Is_Nil (Statement_Identifier (Statement)) then
               Do_Report (Stmt_Unnamed_Block);
            end if;
            if not Is_Declare_Block (Statement) then
               Do_Report (Stmt_Simple_Block);
            end if;

         when A_Case_Statement =>
            Do_Report (Stmt_Case);
            if Maximum (Stmt_Case_Branch) /= (Rule_Types => Unused) then
               declare
                  Paths : constant Asis.Path_List := Statement_Paths (Statement);
               begin
                  for P in Paths'Range loop
                     Do_Report (Stmt_Case_Branch, Element => Paths (P));
                  end loop;
               end;
            end if;

         when A_Loop_Statement
            | A_While_Loop_Statement
            | A_For_Loop_Statement
              =>
            Do_Report (Stmt_Loop);
            if Is_Nil (Statement_Identifier (Statement)) then
               Do_Report (Stmt_Unnamed_Loop);
            end if;

         when An_If_Statement =>
            Do_Report (Stmt_If);
            if Maximum (Stmt_If_Branch) /= (Rule_Types => Unused) then
               declare
                  Paths : constant Asis.Path_List := Statement_Paths (Statement);
               begin
                  for P in Paths'Range loop
                     Do_Report (Stmt_If_Branch, Element => Paths (P));
                  end loop;
               end;
            end if;

         when others =>
            null;
      end case;
   end Process_Statement;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB    => Help'Access,
                                     Add_Use_CB => Add_Use'Access,
                                     Command_CB => Command'Access);
end Rules.Max_Size;
