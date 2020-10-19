----------------------------------------------------------------------
--  Rules.Max_Statement_Nesting - Package body                      --
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
  Asis.Elements;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Max_Statement_Nesting is

   use Asis, Framework;

   -- Note: Stmt_All must stay last.
   type Subrules is (Stmt_Block, Stmt_Case, Stmt_If, Stmt_Loop, Stmt_All);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "STMT_");

   subtype Controlled_Statements is Asis.Statement_Kinds range An_If_Statement .. A_Block_Statement;

   type Usage is array (Subrules) of Control_Kinds_Set;
   Rule_Used : Usage := (others => (others => False));
   Save_Used : Usage;

   Ctl_Labels : array (Subrules, Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Ctl_Values : array (Subrules, Control_Kinds) of Asis.ASIS_Natural;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrules_Flag_Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control max nesting of compound statements");
      User_Message;
      Help_On_Flags ("Parameter(1):");
      User_Message  ("Parameter(2): nesting depth");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

    procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities, Ada.Strings.Wide_Unbounded;
      Subrule : Subrules;
    begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "two parameters required");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);
      if Rule_Used (Subrule) (Ctl_Kind) then
         Parameter_Error (Rule_Id, "rule already specified for " & Control_Kinds'Wide_Image (Ctl_Kind));
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "Two parameters required");
      end if;

      Ctl_Values    (Subrule, Ctl_Kind) := Get_Integer_Parameter (Min => 1);
      Ctl_Labels    (Subrule, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
      Rule_Used (Subrule)(Ctl_Kind) := True;
    end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => (others => False));
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => (others => False));
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Statement : in Asis.Statement) is
      use Asis.Elements;
      use Thick_Queries;
      Unit_Name : constant Asis.Defining_Name := Enclosing_Program_Unit (Statement, Including_Accept => True);
      Elem      : Asis.Element := Statement;
      Counts    : array (Subrules) of Asis.ASIS_Natural := (others => 0);

      procedure Count (Stmt : Subrules) is
      begin
         if Rule_Used (Stmt) /= (Control_Kinds => False)
           or else Rule_Used (Stmt_All) /= (Control_Kinds => False)
         then
            Counts (Stmt)     := Counts (Stmt) + 1;
            Counts (Stmt_All) := Counts (Stmt_All) + 1;
         end if;
      end Count;

      procedure Do_Report (Stmt : Subrules) is
         use Ada.Strings.Wide_Unbounded;
         use Subrules_Flag_Utilities, Framework.Locations, Framework.Reports, Utilities;
      begin
         if Rule_Used (Stmt)(Check) and then Counts (Stmt) > Ctl_Values (Stmt, Check) then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Labels (Stmt, Check)),
                    Check,
                    Get_Location (Statement),
                    Image (Stmt, Lower_Case)
                    & " statements nesting deeper than "
                    & ASIS_Integer_Img (Ctl_Values (Stmt, Check)));
         elsif Rule_Used (Stmt)(Search) and then Counts (Stmt) > Ctl_Values (Stmt, Search) then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Labels (Stmt, Search)),
                    Search,
                    Get_Location (Statement),
                    Image (Stmt, Lower_Case)
                    & " statements nesting deeper than "
                    & ASIS_Integer_Img (Ctl_Values (Stmt, Search)));
         end if;

         if Rule_Used (Stmt)(Count) and then Counts (Stmt) > Ctl_Values (Stmt, Count) then
            Report (Rule_Id,
                    To_Wide_String (Ctl_Labels (Stmt, Count)),
                    Count,
                    Get_Location (Statement),
                    Image (Stmt, Lower_Case)
                    & " statements nesting deeper than "
                    & ASIS_Integer_Img (Ctl_Values (Stmt, Count)));
         end if;

      end Do_Report;

   begin  -- Process_Statement
      if Rule_Used = (Subrules => (Control_Kinds => False)) then
         return;
      end if;

      if Statement_Kind (Statement) not in Controlled_Statements then
         return;
      end if;

      loop
         case Statement_Kind (Elem) is
            when A_Block_Statement =>
               Count (Stmt_Block);
            when A_Case_Statement =>
               Count (Stmt_Case);
            when An_If_Statement =>
               Count (Stmt_If);
            when A_Loop_Statement
               | A_While_Loop_Statement
               | A_For_Loop_Statement
               =>
               Count (Stmt_Loop);
            when others =>
               null;
         end case;
         Elem := Enclosing_Element (Elem);
         exit when Is_Nil (Elem)
           or else not Is_Equal (Unit_Name, Enclosing_Program_Unit (Elem, Including_Accept => True));
      end loop;

      Do_Report (Stmt_All);

      case Controlled_Statements (Statement_Kind (Statement)) is
         when A_Block_Statement =>
            Do_Report (Stmt_Block);
         when An_If_Statement =>
            Do_Report (Stmt_If);
         when A_Case_Statement =>
            Do_Report (Stmt_Case);
         when A_Loop_Statement
            | A_While_Loop_Statement
            | A_For_Loop_Statement
            =>
            Do_Report (Stmt_Loop);
      end case;
   end Process_Statement;

begin  -- Rules.Max_Statement_Nesting
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Max_Statement_Nesting;
