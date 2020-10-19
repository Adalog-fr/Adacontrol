----------------------------------------------------------------------
--  Rules.Abnormal_Function_Return - Package body                   --
--                                                                  --
--  This software is (c) CSEE and Adalog 2004-2006.                 --
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

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Statements;
package body Rules.Abnormal_Function_Return is
   use Framework, Framework.Control_Manager;

   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Rule_Context : Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control functions that can propagate Program_Error due to not executing a return statement");
      User_Message;
      User_Message ("Parameter(s): none");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Framework.Language;

   begin
      if Parameter_Exists then
         Parameter_Error (Rule_Id, "No parameter allowed");
      end if;

      if Rule_Used then
         Parameter_Error (Rule_Id, "this rule can be specified only once");
      else
         Rule_Context := Basic.New_Context (Ctl_Kind, Ctl_Label);
         Rule_Used    := True;
      end if;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
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

   ---------------------------
   -- Process_Function_Body --
   ---------------------------

   procedure Process_Function_Body (Function_Body : in Asis.Expression) is
      use Asis.Declarations, Asis.Statements;
      use Thick_Queries;

      procedure Check (Stmt : Asis.Statement) is
         use Asis, Asis.Elements;
         use Framework.Locations, Framework.Reports, Utilities;
      begin
         case Statement_Kind (Stmt) is
            when A_Return_Statement
              | A_Raise_Statement
              =>
               null;

            when An_Extended_Return_Statement =>
               if not Is_Nil (First_Exiting_Statement (Extended_Return_Statements (Stmt))) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Stmt),
                          "Sequence of statements terminated by exitable extended ""return""");
               end if;

            when A_Procedure_Call_Statement =>
               declare
                  SP_Name : constant Wide_String := To_Upper (Full_Name_Image (Called_Simple_Name (Stmt)));
               begin
                  if SP_Name = "ADA.EXCEPTIONS.RAISE_EXCEPTION"
                    or else SP_Name = "ADA.EXCEPTIONS.RERAISE_OCCURRENCE"
                    or else Corresponding_Pragma_Set (Called_Simple_Name (Stmt)) (A_No_Return_Pragma)
                    or else Corresponding_Aspects (Called_Simple_Name (Stmt), "NO_RETURN") /= Nil_Element_List
                  then
                     return;
                  end if;
               end;
               Report (Rule_Id,
                       Rule_Context,
                       Get_Location (Stmt),
                       "Sequence of statements not terminated by ""return"" or ""raise""");

            when A_Block_Statement =>
               Check (Last_Effective_Statement (Block_Statements (Stmt)));

               for H : Asis.Exception_Handler of Block_Exception_Handlers (Stmt) loop
                  Check (Last_Effective_Statement (Handler_Statements (H)));
               end loop;

            when An_If_Statement =>
               declare
                  Paths : constant Asis.Path_List := Statement_Paths (Stmt);
               begin
                  for This_Path : Asis.Path of Paths loop
                     Check (Last_Effective_Statement (Sequence_Of_Statements (This_Path)));
                  end loop;
                  if Path_Kind (Paths (Paths'Last)) /= An_Else_Path then
                     Report (Rule_Id,
                       Rule_Context,
                       Get_Location (Stmt),
                       "Missing ""else"" path containing ""return"" or ""raise""");
                  end if;
               end;

            when A_Case_Statement =>
               for This_Path : Asis.Path of Statement_Paths (Stmt) loop
                  Check (Last_Effective_Statement (Sequence_Of_Statements (This_Path)));
               end loop;

            when A_Loop_Statement =>
               -- Non exiting loops (i.e. plain loops without transfer outside) that possibly contain
               -- return statements are OK (without any return, they are infinite)
               if not Is_Nil (First_Exiting_Statement (Loop_Statements (Stmt),
                              Include_Returns => False))
               then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Stmt),
                          "Sequence of statements not terminated by ""return"" or ""raise""");
               end if;

            when others =>
               Report (Rule_Id,
                       Rule_Context,
                       Get_Location (Stmt),
                       "Sequence of statements not terminated by ""return"" or ""raise""");
         end case;
      end Check;

   begin   -- Process_Function_Body
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check (Last_Effective_Statement (Body_Statements (Function_Body)));

      for H : Asis.Exception_Handler of Body_Exception_Handlers (Function_Body) loop
         Check (Last_Effective_Statement (Handler_Statements (H)));
      end loop;
   end Process_Function_Body;

begin  -- Rules.Abnormal_Function_Return
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Abnormal_Function_Return;
