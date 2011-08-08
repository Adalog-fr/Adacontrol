----------------------------------------------------------------------
--  Rules.Abnormal_Function_Return - Package body                   --
--                                                                  --
--  This  software  is  (c)  CSEE  and Adalog  2004-2006.  The  Ada --
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

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Statements;
package body Rules.Abnormal_Function_Return is
   use Framework;

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
      User_Message ("Parameter(s): none");
      User_Message ("Control functions that can propagate Program_Error due to not executing a return statement");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language;

   begin
      if Parameter_Exists then
         Parameter_Error (Rule_Id, "No parameter allowed");
      end if;

      if Rule_Used then
         Parameter_Error (Rule_Id, "this rule can be specified only once");
      else
         Rule_Context := Basic.New_Context (Rule_Type, Label);
         Rule_Used    := True;
      end if;
   end Add_Use;

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
         use Framework.Reports, Utilities;

         Last_Statement : Asis.Statement := Stmt;
      begin
         while Statement_Kind (Last_Statement) = A_Block_Statement loop
            declare
               Block_Stmts : constant Asis.Statement_List := Block_Statements (Last_Statement);
            begin
               Last_Statement := Block_Stmts (Block_Stmts'Last);
            end;
         end loop;

         case Statement_Kind (Last_Statement) is
            when A_Return_Statement
              | A_Raise_Statement
              =>
               return;
            when A_Procedure_Call_Statement =>
               declare
                  SP_Name : constant Wide_String := To_Upper (Full_Name_Image (Called_Simple_Name (Last_Statement)));
               begin
                  if SP_Name = "ADA.EXCEPTIONS.RAISE_EXCEPTION"
                    or else SP_Name = "ADA.EXCEPTIONS.RERAISE_OCCURRENCE"
                  then
                     return;
                  end if;
               end;
            when others =>
               null;
         end case;

         Report (Rule_Id,
                 Rule_Context,
                 Get_Location (Last_Statement),
                 "Sequence of statements not terminated by ""return"" or ""raise""");
      end Check;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Body_Stmts : constant Asis.Statement_List := Body_Statements (Function_Body);
      begin
         Check (Last_Effective_Statement (Body_Stmts));
      end;

      declare
         Handlers : constant Asis.Exception_Handler_List := Body_Exception_Handlers (Function_Body);
      begin
         for H in Handlers'Range loop
            declare
               Handler_Stmts : constant Asis.Statement_List := Handler_Statements (Handlers (H));
            begin
               Check (Last_Effective_Statement (Handler_Stmts));
            end;
         end loop;
      end;
   end Process_Function_Body;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB    => Help'Access,
                                     Add_Use_CB => Add_Use'Access,
                                     Command_CB => Command'Access);
end Rules.Abnormal_Function_Return;
