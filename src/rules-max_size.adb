----------------------------------------------------------------------
--  Rules.Max_Size - Package body                                   --
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
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Elements,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  A4G_Bugs,
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Max_Size is
   use Framework;
   use Asis.Text;

   type Subrules is (SR_Accept,         SR_Block,          SR_Case,           SR_Case_Branch,
                     SR_If,             SR_If_Branch,      SR_Loop,           SR_Simple_Block,
                     SR_Unnamed_Block,  SR_Unnamed_Loop,

                     SR_Package_Spec,   SR_Package_Body,   SR_Procedure_Body, SR_Function_Body,
                     SR_Protected_Spec, SR_Protected_Body, SR_Entry_Body,
                     SR_Task_Spec,      SR_Task_Body,

                     SR_Unit);
   subtype SR_Statements is Subrules range SR_Accept .. SR_Unnamed_Loop;

   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "SR_");
   use Subrules_Flags_Utilities;

   Unused : constant Line_Number := Line_Number'Last;

   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Ctl_Labels : array (Subrules, Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type Max_Table is array (Control_Kinds) of Line_Number;
   Maximum : array (Subrules) of Max_Table  := (others => (others => Unused));

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control the maximum length of Ada statements");
      User_Message;
      Help_On_Flags ("Parameter(1):");
      User_Message  ("Parameter(2): maximum acceptable number of lines");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Ada.Strings.Wide_Unbounded, Utilities;

      Subrule : Subrules;
      Max     : Line_Number_Positive;

      use type Asis.ASIS_Integer;   -- Gela-ASIS compatibility
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "parameters required");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);
      if Maximum (Subrule) (Ctl_Kind) /= Unused then
         Parameter_Error (Rule_Id, "statement already given for "
                                   & To_Lower (Control_Kinds'Wide_Image (Ctl_Kind)) & ": "
                                   & Image (Subrule, Lower_Case));
      end if;

      begin
         Max := Get_Integer_Parameter (Min => 1, Max => Unused - 1);  -- Gela-ASIS compatibility
      exception
         when Constraint_Error =>
            Parameter_Error (Rule_Id, "maximum value negative or too big");
      end;

      Rule_Used                      := True;
      Ctl_Labels (Subrule, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
      Maximum (Subrule) (Ctl_Kind)   := Max;
   end Add_Control;

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


   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Stmt : in Subrules; Measured_Elem : Asis.Element) is
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports, Utilities;

      Length : Line_Number_Positive;
      Loc    : Location;
   begin
      if Element_Kind (Measured_Elem) = A_Path then
         declare
            Stats : constant Asis.Statement_List := Sequence_Of_Statements (Measured_Elem);
         begin
            Loc    := Get_End_Location (Stats (Stats'Last));
            Length :=   A4G_Bugs.Last_Line_Number  (Stats (Stats'Last))
                      - A4G_Bugs.First_Line_Number (Stats (Stats'First))
                      + 1;
         end;
      else
         Loc    := Get_End_Location (Measured_Elem);
         Length := A4G_Bugs.Last_Line_Number (Measured_Elem) - A4G_Bugs.First_Line_Number (Measured_Elem) + 1;
      end if;

      if Length > Maximum (Stmt) (Check) then
         Report (Rule_Id,
                 To_Wide_String (Ctl_Labels (Stmt, Check)),
                 Check,
                 Loc,
                 Choose (Stmt in SR_Statements, "statement", "declaration of") & " """ & Image (Stmt, Lower_Case)
                 & """ is more than " & ASIS_Integer_Img (Maximum (Stmt) (Check)) & " lines long ("
                 & ASIS_Integer_Img (Length) & ')');
      elsif Length > Maximum (Stmt) (Search) then
         Report (Rule_Id,
                 To_Wide_String (Ctl_Labels (Stmt, Search)),
                 Search,
                 Loc,
                 Choose (Stmt in SR_Statements, "statement", "declaration of") & " """ & Image (Stmt, Lower_Case)
                 & """ is more than " & ASIS_Integer_Img (Maximum (Stmt) (Search)) & " lines long ("
                 & ASIS_Integer_Img (Length) & ')');
      end if;

      if Length > Maximum (Stmt) (Count) then
         Report (Rule_Id,
                 To_Wide_String (Ctl_Labels (Stmt, Count)),
                 Count,
                 Loc,
                 "");
      end if;
   end Do_Report;


   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Element (Element : in Asis.Element) is
      use Asis, Asis.Elements, Asis.Statements;
      use Utilities;

   begin  -- Process_Element
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Element_Kind (Element) is
         when A_Statement =>
            case Statement_Kind (Element) is
               when An_Accept_Statement =>
                  Do_Report (SR_Accept, Element);

               when A_Block_Statement =>
                  Do_Report (SR_Block, Element);
                  if Is_Nil (Statement_Identifier (Element)) then
                     Do_Report (SR_Unnamed_Block, Element);
                  end if;
                  if not Is_Declare_Block (Element) then
                     Do_Report (SR_Simple_Block, Element);
                  end if;

               when A_Case_Statement =>
                  Do_Report (SR_Case, Element);
                  if Maximum (SR_Case_Branch) /= (Control_Kinds => Unused) then
                     for Path : Asis.Path of Statement_Paths (Element) loop
                        Do_Report (SR_Case_Branch, Measured_Elem => Path);
                     end loop;
                  end if;

               when A_Loop_Statement
                  | A_While_Loop_Statement
                  | A_For_Loop_Statement
                  =>
                  Do_Report (SR_Loop, Element);
                  if Is_Nil (Statement_Identifier (Element)) then
                     Do_Report (SR_Unnamed_Loop, Element);
                  end if;

               when An_If_Statement =>
                  Do_Report (SR_If, Element);
                  if Maximum (SR_If_Branch) /= (Control_Kinds => Unused) then
                     for Path : Asis.Path of Statement_Paths (Element) loop
                        Do_Report (SR_If_Branch, Measured_Elem => Path);
                     end loop;
                  end if;

               when others =>
                  null;
            end case;

         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Package_Declaration  | A_Generic_Package_Declaration =>
                  Do_Report (SR_Package_Spec, Element);
               when A_Package_Body_Declaration =>
                  Do_Report (SR_Package_Body, Element);
               when A_Procedure_Body_Declaration =>
                  Do_Report (SR_Procedure_Body, Element);
               when A_Null_Procedure_Declaration =>
                  null;  -- Minimal value...
               when A_Function_Body_Declaration =>
                  Do_Report (SR_Function_Body, Element);
               when A_Task_Type_Declaration | A_Single_Task_Declaration =>
                  Do_Report (SR_Task_Spec, Element);
               when A_Task_Body_Declaration =>
                  Do_Report (SR_Task_Body, Element);
               when A_Protected_Type_Declaration | A_Single_Protected_Declaration =>
                  Do_Report (SR_Protected_Spec, Element);
               when A_Protected_Body_Declaration =>
                  Do_Report (SR_Protected_Body, Element);
               when An_Entry_Body_Declaration =>
                  Do_Report (SR_Entry_Body, Element);
               when others =>
                  Failure ("Process_Element: unexpected element", Element);
            end case;

         when others =>
            null;
      end case;

   end Process_Element;


   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Unit : in Asis.Compilation_Unit) is
      use Asis.Elements;
   begin
      if not Rule_Used or else Maximum (SR_Unit) = (Control_Kinds => Unused) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Do_Report (SR_Unit, Unit_Declaration (Unit));
   end Process_Unit;

begin  -- Rules.Max_Size
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Max_Size;
