----------------------------------------------------------------------
--  Rules.Terminating_Tasks - Package body                          --
--                                                                  --
--  This software is (c) Adalog 2004-2005.                          --
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

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Iterator,
  Asis.Statements;

-- Adalog
with
  Utilities;

package body Rules.Terminating_Tasks is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- This algorithm is quite simple since it looks for terminating tasks, i.e.
   -- tasks that have statements terminating it.
   --
   -- First of all, we need to retrieve the task body statements.
   --
   -- Then, we look if the last statement is an unconditional loop.
   --    * When this last statement is not an unconditional loop, the task may
   --      be considered as a terminating task.
   --    * In any other case, we need to check that this unconditional loop does
   --      not contain any `exit' statement refering to it, nor any `terminate`
   --      alternative within a `select' block.
   --      When we match one of these cases, the task may be considered as a
   --      terminating task.


   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Usage     : Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control task termination.");
      User_Message;
      User_Message ("Parameter(s): none");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
   begin
      if Parameter_Exists then
         Parameter_Error (Rule_Id, "No parameter allowed");
      end if;

      if Rule_Used then
         Parameter_Error (Rule_Id, "Rule can be specified only once");
      end if;

      Rule_Used := True;
      Usage     := Basic.New_Context (Ctl_Kind, Ctl_Label);
   end Add_Control;


   -------------
   -- Command --
   -------------

   procedure Command (Action : in Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -----------------------
   -- Process_Task_Body --
   -----------------------

   procedure Process_Task_Body (Body_Decl : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Framework.Locations, Framework.Reports;

      Last_Statement : Asis.Statement;

      --------------
      -- Traverse --
      --------------

      procedure Pre_Procedure  (Element : in     Asis.Element;
                                Control : in out Asis.Traverse_Control;
                                State   : in out Null_State);

      procedure Traverse is new Asis.Iterator.Traverse_Element (Null_State, Pre_Procedure, Null_State_Procedure);

      procedure Pre_Procedure  (Element : in     Asis.Element;
                                Control : in out Asis.Traverse_Control;
                                State   : in out Null_State)
      is
         pragma Unreferenced (Control, State);

         use Asis.Statements;
      begin
         case Statement_Kind (Element) is
            when An_Exit_Statement =>
               if Is_Equal (Last_Statement, Corresponding_Loop_Exited (Element)) then
                  Report (Rule_Id,
                          Usage,
                          Get_Location (Last_Statement),
                          "unconditional loop exited from " & Image (Get_Location (Element)));
               end if;

            when A_Terminate_Alternative_Statement =>
               Report (Rule_Id,
                       Usage,
                       Get_Location (Last_Statement),
                       "terminate alternative at " & Image (Get_Location (Element)));

            when others =>
               null;
         end case;
      end Pre_Procedure;

      The_Control : Asis.Traverse_Control := Continue;
      The_State   : Null_State;
   begin  -- Process_Task_Body
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         -- Retrieve the task body statements
         Task_Statements : constant Asis.Statement_List := Body_Statements (Body_Decl);
      begin
         -- Retrieve the last statement of the task body
         Last_Statement := Task_Statements (Task_Statements'Last);
         case Statement_Kind (Last_Statement) is
            when A_Loop_Statement =>
               -- Check for terminating task
               Traverse (Last_Statement, The_Control, The_State);
            when others =>
               Report (Rule_Id,
                       Usage,
                       Get_Location (Last_Statement),
                       "last statement is not an unconditional loop");
         end case;
      end;
   end Process_Task_Body;

begin  -- Rules.Terminating_Tasks
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Terminating_Tasks;
