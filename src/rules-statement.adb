----------------------------------------------------------------------
--  Rules.Statement - Package body                                  --
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
  ASIS.Elements,
  ASIS.Statements;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Statement is
   use Framework;

   type Statement_Names is (Stmt_Abort,      Stmt_Asynchronous_Select, Stmt_Case_Others,
                            Stmt_Delay,      Stmt_Delay_Until,         Stmt_Exit,
                            Stmt_Goto,       Stmt_Raise,               Stmt_Requeue,
                            Stmt_Unnamed_Exit);

   -----------
   -- Image --
   -----------

   function Image (Stmt : Statement_Names) return Wide_String is
      use Utilities;
      Img : constant Wide_String := To_Lower (Statement_Names'Wide_Image (Stmt));
   begin
      -- Remove "STMT_"
      return Img (6 .. Img'Last);
   end Image;

   type Usage_Flags is array (Statement_Names) of Boolean;
   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;
   Usage     : array (Statement_Names) of Simple_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): abort | asynchronous_select | case_others | delay | delay_until");
      User_Message ("              | exit | goto | raise | requeue | unnamed_exit");
      User_Message ("Control occurrences of Ada statements");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;
      Stmt    : Statement_Names;

      function Get_Statement_Parameter is new Get_Flag_Parameter (Flags     => Statement_Names,
                                                                  Allow_Any => False,
                                                                  Prefix    => "STMT_");
   begin
      if not Parameter_Exists then
         Parameter_Error ("At least one parameter required for rule " & Rule_Id);
      end if;

      while Parameter_Exists loop
         Stmt := Get_Statement_Parameter;
         if Rule_Used (Stmt) then
            Parameter_Error ("Statement already given for rule " & Rule_Id
                             & ": " & Image (stmt));
         end if;

         Rule_Used (Stmt) := True;
         Usage (Stmt)     := (Rule_Type, To_Unbounded_Wide_String (Label));
      end loop;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Element : in Asis.Statement) is
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Elements, Asis.Statements, Framework.Reports;
      Stmt : Statement_Names;
   begin
      if Rule_Used = (Statement_Names => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Statement_Kind (Element) is
         when An_Abort_Statement =>
            Stmt := Stmt_Abort;
         when A_Delay_Relative_Statement =>
            Stmt := Stmt_Delay;
         when A_Delay_Until_Statement =>
            Stmt := Stmt_Delay_Until;
         when An_Exit_Statement=>
            Stmt := Stmt_Exit;
            if Rule_Used (Stmt_Unnamed_Exit)
              and then Is_Nil (Exit_Loop_Name (Element))
              and then not Is_Nil (Statement_Identifier (Corresponding_Loop_Exited (Element)))
            then
               Stmt := Stmt_Unnamed_Exit;
            end if;
         when A_Goto_Statement =>
            Stmt := Stmt_Goto;
         when A_Raise_Statement =>
            Stmt := Stmt_Raise;
         when A_Requeue_Statement | A_Requeue_Statement_With_Abort =>
            Stmt := Stmt_Requeue;
         when An_Asynchronous_Select_Statement =>
            Stmt := Stmt_Asynchronous_Select;
         when others =>
            return;
      end case;

      if not Rule_Used (Stmt) then
         return;
      end if;

      Report (Rule_Id,
              To_Wide_String (Usage(Stmt).Rule_Label),
              Usage (Stmt).Rule_Type,
              Get_Location (Element),
              "use of statement """ & Image (stmt) & '"');
   end Process_Statement;

   --------------------
   -- Process_Others --
   --------------------

   procedure Process_Others (Definition : in Asis.Definition) is
      use Asis, Asis.Elements, Framework.Reports, Ada.Strings.Wide_Unbounded;
   begin
       if not Rule_Used (Stmt_Case_Others) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Path_Kind (Enclosing_Element (Definition)) = A_Case_Path then
         Report (Rule_Id,
                 To_Wide_String (Usage(Stmt_Case_Others).Rule_Label),
                 Usage (Stmt_Case_Others).Rule_Type,
                 Get_Location (Definition),
                 "use of ""others"" in ""case"" statement");
      end if;
   end Process_Others;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access);
end Rules.Statement;
