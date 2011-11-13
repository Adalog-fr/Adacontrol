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

-- ASIS
with
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Adactl_Constants,
  Framework.Language,
  Framework.Scope_Manager;
pragma Elaborate (Framework.Language);

package body Rules.Statements is
   use Adactl_Constants, Framework, Framework.Control_Manager;

   -- all "filtered_raise" subrules (i.e. raise subrules except the plain one) must stay together
   type Subrules is (Stmt_Any_Statement,

                     Stmt_Abort,                   Stmt_Accept_Return,          Stmt_Assignment,
                     Stmt_Asynchronous_Select,

                     Stmt_Block,

                     Stmt_Case,                    Stmt_Case_Others,            Stmt_Case_Others_Null,
                     Stmt_Code,                    Stmt_Conditional_Entry_Call,

                     Stmt_Declare_Block,           Stmt_Delay,                  Stmt_Delay_Until,
                     Stmt_Dispatching_Call,

                     Stmt_Effective_Declare_Block, Stmt_Entry_Call,             Stmt_Entry_Return,
                     Stmt_Exception_Others,        Stmt_Exception_Others_Null,  Stmt_Exit,
                     Stmt_Exit_Expanded_Name,      Stmt_Exit_For_Loop,          Stmt_Exit_Outer_Loop,
                     Stmt_Exit_While_Loop,         Stmt_Exited_Extended_Return, Stmt_Extended_Return,

                     Stmt_For_Loop,                Stmt_Function_Return,

                     Stmt_Goto,

                     Stmt_If,                      Stmt_If_Elsif,               Stmt_Inherited_Procedure_Call,

                     Stmt_Labelled,                Stmt_Loop_Return,

                     Stmt_Multiple_Exits,

                     Stmt_No_Else,                 Stmt_Null,

                     Stmt_Procedure_Return,

                     Stmt_Raise,

                     -- all "filtered_raise"
                     Stmt_Raise_Locally_Handled,  Stmt_Raise_Nonpublic,         Stmt_Raise_Standard,
                     Stmt_Reraise,

                     Stmt_Requeue,

                     Stmt_Selective_Accept,       Stmt_Simple_Loop,

                     Stmt_Terminate,              Stmt_Timed_Entry_Call,

                     Stmt_Unconditional_Exit,     Stmt_Unnamed_Block,           Stmt_Unnamed_Exit,
                     Stmt_Unnamed_Loop_Exited,    Stmt_Unnamed_For_Loop,        Stmt_Unnamed_Multiple_Loop,
                     Stmt_Unnamed_Simple_Loop,    Stmt_Unnamed_While_Loop,      Stmt_Untyped_For,

                     Stmt_While_Loop);

   subtype Filtered_Raise_Subrules is Subrules range Stmt_Raise .. Stmt_Reraise;
   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "STMT_");
   use Subrules_Flags_Utilities;

   type Usage_Flags is array (Subrules) of Boolean;
   No_Rule   : constant Usage_Flags := (others => False);
   Rule_Used : Usage_Flags := No_Rule;
   Save_Used : Usage_Flags;
   Usage     : array (Subrules) of Basic_Rule_Context;

   -- For Stmt_Unnamed_Multiple_Loop:
   type Loops_Level is range 0 .. Max_Loop_Nesting;
   Body_Depth  : Framework.Scope_Manager.Scope_Range := 0;
   Loops_Depth : array (Framework.Scope_Manager.Scope_Range) of Loops_Level;
   Top_Loop    : array (Framework.Scope_Manager.Scope_Range) of Asis.Statement;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control occurrences of Ada statements");
      User_Message;
      Help_On_Flags ("Parameter(s):");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Utilities;
      Subrule : Subrules;

   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      while Parameter_Exists loop
         Subrule := Get_Flag_Parameter (Allow_Any => False);
         if Rule_Used (Subrule) then
            Parameter_Error (Rule_Id, "statement already given: " & Image (Subrule, Lower_Case));
         end if;

         Rule_Used (Subrule) := True;
         Usage (Subrule)     := Basic.New_Context (Ctl_Kind, Ctl_Label);
      end loop;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := No_Rule;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Element : in Asis.Statement) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries, Utilities;

      procedure Do_Report (Stmt : in Subrules; Loc : Location := Get_Location (Element)) is
         use Framework.Reports;
      begin
         if not Rule_Used (Stmt) then
            return;
         end if;

         Report (Rule_Id,
                 Usage (Stmt),
                 Loc,
                 "use of statement """ & Image (Stmt, Lower_Case) & '"');
      end Do_Report;

      procedure Do_Report (Stmt : in Subrules; Extra_Info : Wide_String) is
         use Framework.Reports;
      begin
         if not Rule_Used (Stmt) then
            return;
         end if;

         Report (Rule_Id,
                 Usage (Stmt),
                 Get_Location (Element),
                 "use of statement """ & Image (Stmt, Lower_Case) & """, " & Extra_Info);
      end Do_Report;

      procedure Check_Filtered_Raise (Exc : Asis.Expression) is
         -- process filtered (i.e. non trivial) raises.
         -- common to a real raise and to a call to Raise_Exception
         use Asis.Compilation_Units;

         Handler         : Asis.Exception_Handler;
         Decl_Place      : Asis.Declaration;
         Is_Standard_Exc : Boolean := False;
      begin
         if Rule_Used (Stmt_Raise_Standard) or Rule_Used (Stmt_Raise_Nonpublic) then
            Is_Standard_Exc := To_Upper (Unit_Full_Name (Definition_Compilation_Unit (Exc))) = "STANDARD";
         end if;

         if Rule_Used (Stmt_Raise_Standard) and Is_Standard_Exc then
            Do_Report (Stmt_Raise_Standard);
         end if;

         if Rule_Used (Stmt_Raise_Nonpublic) and not Is_Standard_Exc then
            Decl_Place := Enclosing_Element (A4G_Bugs.Corresponding_Name_Declaration (Exc));
            -- Report if it is in the visible part of the package spec which is the current
            -- compilation unit
            -- NB: Corresponding_Declaration of a proper body returns Nil_Element, therefore
            --     this case is automatically eliminated
            if (    Declaration_Kind (Decl_Place) /= A_Package_Declaration
                and Declaration_Kind (Decl_Place) /= A_Generic_Package_Declaration)
              or else not Is_Equal (Decl_Place,
                Corresponding_Declaration
                  (Unit_Declaration
                     (Enclosing_Compilation_Unit (Element))))
              or else Is_Part_Of (A4G_Bugs.Corresponding_Name_Declaration (Exc),
                                  Private_Part_Declarative_Items (Decl_Place))
            then
               Do_Report (Stmt_Raise_Nonpublic);
            end if;
         end if;

         if Rule_Used (Stmt_Raise_Locally_Handled) then
            Handler := Corresponding_Static_Exception_Handler (Exc, Element, Include_Others => True);
            if not Is_Nil (Handler) then
               Do_Report (Stmt_Raise_Locally_Handled, "handler at " & Image (Get_Location (Handler)));
            end if;
         end if;
      end Check_Filtered_Raise;

   begin  -- Process_Statement
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Do_Report (Stmt_Any_Statement);

      if not Is_Nil (Label_Names (Element)) then
         Do_Report (Stmt_Labelled);
      end if;

      case Statement_Kind (Element) is
         when Not_A_Statement =>
            Failure ("Not a statement");

         when An_Abort_Statement =>
            Do_Report (Stmt_Abort);

         when An_Accept_Statement =>
            null;

         when An_Assignment_Statement =>
            Do_Report (Stmt_Assignment);

         when An_Asynchronous_Select_Statement =>
            Do_Report (Stmt_Asynchronous_Select);

         when A_Block_Statement =>
            Do_Report (Stmt_Block);
            if Is_Nil (Statement_Identifier (Element)) then
               Do_Report (Stmt_Unnamed_Block);
            end if;
            if Is_Declare_Block (Element) then
               Do_Report (Stmt_Declare_Block);
               if Rule_Used (Stmt_Effective_Declare_Block) then
                  declare
                     Decls : constant Asis.Declarative_Item_List := Block_Declarative_Items (Element,
                                                                                             Include_Pragmas => False);
                  begin
                     for D in Decls'Range loop
                        if Clause_Kind (Decls (D)) not in A_Use_Package_Clause .. A_Use_Type_Clause then
                           Do_Report (Stmt_Effective_Declare_Block);
                           exit;
                        end if;
                     end loop;
                  end;
               end if;
            end if;

         when A_Case_Statement =>
            Do_Report (Stmt_Case);

         when A_Code_Statement =>
            Do_Report (Stmt_Code);

         when A_Conditional_Entry_Call_Statement =>
            Do_Report (Stmt_Conditional_Entry_Call);

         when A_Delay_Relative_Statement =>
            Do_Report (Stmt_Delay);

         when A_Delay_Until_Statement =>
            Do_Report (Stmt_Delay_Until);

         when An_Entry_Call_Statement =>
            Do_Report (Stmt_Entry_Call);

         when An_Exit_Statement =>
            declare
               Exited_Loop : constant Asis.Statement := Corresponding_Loop_Exited (Element);
            begin
               if Is_Nil (Exit_Condition (Element)) then
                  Do_Report (Stmt_Unconditional_Exit);
               end if;

               if Is_Nil (Statement_Identifier (Exited_Loop)) then
                  Do_Report (Stmt_Unnamed_Loop_Exited);
               elsif Is_Nil (Exit_Loop_Name (Element)) then
                  Do_Report (Stmt_Unnamed_Exit);
               end if;

               if Expression_Kind (Exit_Loop_Name (Element)) = A_Selected_Component then
                  Do_Report (Stmt_Exit_Expanded_Name);
               end if;

               if Rule_Used (Stmt_Exit_For_Loop)
                 and then Statement_Kind (Exited_Loop) = A_For_Loop_Statement
               then
                  Do_Report (Stmt_Exit_For_Loop);
               elsif Rule_Used (Stmt_Exit_While_Loop)
                 and then Statement_Kind (Exited_Loop) = A_While_Loop_Statement
               then
                  Do_Report (Stmt_Exit_While_Loop);
               else
                  Do_Report (Stmt_Exit);
               end if;

               if Rule_Used (Stmt_Exit_Outer_Loop) then
                  declare
                     Enclosing_Loop : Asis.Statement := Enclosing_Element (Element);
                  begin
                     while Statement_Kind (Enclosing_Loop) not in A_Loop_Statement .. A_For_Loop_Statement loop
                        Enclosing_Loop := Enclosing_Element (Enclosing_Loop);
                     end loop;

                     if not Is_Equal (Enclosing_Loop, Exited_Loop) then
                        Do_Report (Stmt_Exit_Outer_Loop,
                                   Extra_Info => "exiting from loop at " & Image (Get_Location (Exited_Loop)));
                     end if;
                  end;
               end if;
            end;

         when An_Extended_Return_Statement =>
            if Loops_Depth (Body_Depth) > 0 then
               Do_Report (Stmt_Loop_Return);
            end if;
            Do_Report (Stmt_Extended_Return);
            -- Nothing else to do (compared to regular Return statement):
            -- Extended returns apply only to functions, and Function_Return is checked from Process_Function_Body

            declare
               Dirty : constant Asis.Statement := First_Exiting_Statement (Extended_Return_Statements (Element),
                                                                           Include_Returns => False);
            begin
               if not Is_Nil (Dirty) then
                  Do_Report (Stmt_Exited_Extended_Return, "exiting at " & Image (Get_Location (Dirty)));
               end if;
            end;

         when A_For_Loop_Statement =>
            Do_Report (Stmt_For_Loop);

            if Is_Nil (Statement_Identifier (Element)) then
               Do_Report (Stmt_Unnamed_For_Loop);
            end if;

            if Discrete_Range_Kind (Specification_Subtype_Definition
                                    (For_Loop_Parameter_Specification
                                     (Element))) = A_Discrete_Simple_Expression_Range
            then
               Do_Report (Stmt_Untyped_For);
            end if;

         when A_Goto_Statement =>
            Do_Report (Stmt_Goto);

         when An_If_Statement =>
            Do_Report (Stmt_If);
            declare
               Paths : constant Asis.Path_List := Statement_Paths (Element);
            begin
               if Path_Kind (Paths (Paths'Last)) /= An_Else_Path then
                  Do_Report (Stmt_No_Else);
               end if;
               if Paths'Length >= 2 and then Path_Kind (Paths (2)) = An_Elsif_Path then
                  Do_Report (Stmt_If_Elsif);
               end if;
            end;

         when A_Loop_Statement =>
            Do_Report (Stmt_Simple_Loop);

            if Is_Nil (Statement_Identifier (Element)) then
               Do_Report (Stmt_Unnamed_Simple_Loop);
            end if;

         when A_Null_Statement =>
            Do_Report (Stmt_Null);

         when A_Procedure_Call_Statement =>
            if Is_Dispatching_Call (Element) then
               Do_Report (Stmt_Dispatching_Call);
            end if;

            if Rule_Used (Stmt_Inherited_Procedure_Call) then
               declare
                  Called : constant Asis.Expression := Ultimate_Name (Called_Simple_Name (Element));
               begin
                  if not Is_Nil (Called)          -- Dynamic call (explicit or implicit dereference)
                    and then Expression_Kind (Called) /= An_Attribute_Reference
                    and then Is_Part_Of_Inherited (Corresponding_Name_Definition (Called))
                  then
                     Do_Report (Stmt_Inherited_Procedure_Call);
                  end if;
               end;
            end if;

            if (Rule_Used and Usage_Flags'(Filtered_Raise_Subrules => True, others => False)) /= No_Rule then
               declare
                  Called : constant Asis.Expression := Called_Simple_Name (Element);
               begin
                  if not Is_Nil (Called) then  -- is nil if called from access to SP
                     declare
                        Called_Name : constant Wide_String := To_Upper (Full_Name_Image (Called));
                     begin
                        if Called_Name = "ADA.EXCEPTIONS.RERAISE_OCCURRENCE" then
                           if Rule_Used (Stmt_Reraise) then
                              Do_Report (Stmt_Reraise);
                           end if;
                        elsif Called_Name = "ADA.EXCEPTIONS.RAISE_EXCEPTION" then
                           declare
                              Exc_Param : constant Asis.Expression := Ultimate_Expression
                                                                       (Actual_Parameter
                                                                        (Call_Statement_Parameters
                                                                         (Element, Normalized => True) (1)));
                           begin
                              if Expression_Kind (Exc_Param) = An_Attribute_Reference
                                and then A4G_Bugs.Attribute_Kind (Exc_Param) = An_Identity_Attribute
                              then
                                 Check_Filtered_Raise (Simple_Name (Prefix (Exc_Param)));
                              end if;
                           end;
                        end if;
                     end;
                  end if;
               end;
            end if;

         when A_Raise_Statement =>
            Do_Report (Stmt_Raise);

            if Is_Nil (Raised_Exception (Element)) then
               if Rule_Used (Stmt_Reraise) then
                  Do_Report (Stmt_Reraise);
               end if;
            else
               Check_Filtered_Raise (Simple_Name (Raised_Exception (Element)));
            end if;

         when A_Requeue_Statement | A_Requeue_Statement_With_Abort =>
            Do_Report (Stmt_Requeue);

         when A_Return_Statement =>
            if Loops_Depth (Body_Depth) > 0 then
               Do_Report (Stmt_Loop_Return);
            end if;
            case Declaration_Kind (Enclosing_Element
                                   (Enclosing_Program_Unit (Element, Including_Accept => True)))
            is
               when A_Procedure_Body_Declaration =>
                  Do_Report (Stmt_Procedure_Return);
               when An_Entry_Declaration =>
                  Do_Report (Stmt_Accept_Return);
               when A_Function_Body_Declaration =>
                  -- Function_Return is checked from Process_Function_Body
                  null;
               when An_Entry_Body_Declaration =>
                  Do_Report (Stmt_Entry_Return);
               when others =>
                  Failure ("Return not from subprogram");
            end case;

         when A_Selective_Accept_Statement =>
            Do_Report (Stmt_Selective_Accept);

         when A_Terminate_Alternative_Statement =>
            Do_Report (Stmt_Terminate);

         when A_Timed_Entry_Call_Statement =>
            Do_Report (Stmt_Timed_Entry_Call);

         when A_While_Loop_Statement =>
            Do_Report (Stmt_While_Loop);

            if Is_Nil (Statement_Identifier (Element)) then
               Do_Report (Stmt_Unnamed_While_Loop);
            end if;
      end case;
   end Process_Statement;


   --------------------
   -- Process_Others --
   --------------------

   procedure Process_Others (Definition : in Asis.Definition) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Reports, Thick_Queries;
      Encl : Asis.Element;

   begin
      if not Rule_Used (Stmt_Case_Others)
        and not Rule_Used (Stmt_Case_Others_Null)
        and not Rule_Used (Stmt_Exception_Others_Null)
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Encl := Enclosing_Element (Definition);
      if  Element_Kind (Encl) = An_Exception_Handler then
         if Rule_Used (Stmt_Exception_Others_Null)
           and then Are_Null_Statements (Handler_Statements (Encl))
         then
            Report (Rule_Id,
                    Usage (Stmt_Exception_Others_Null),
                    Get_Location (Definition),
                    "null ""when others"" exception handler");
         elsif Rule_Used (Stmt_Exception_Others) then
            Report (Rule_Id,
                    Usage (Stmt_Exception_Others),
                    Get_Location (Definition),
                    "use of ""when others"" exception handler");
         end if;

      elsif Path_Kind (Enclosing_Element (Definition)) = A_Case_Path then
         if Rule_Used (Stmt_Case_Others_Null)
           and then Are_Null_Statements (Sequence_Of_Statements (Encl))
         then
            Report (Rule_Id,
                    Usage (Stmt_Case_Others_Null),
                    Get_Location (Definition),
                    "null ""when others"" in ""case"" statement");
         elsif Rule_Used (Stmt_Case_Others) then
            Report (Rule_Id,
                    Usage (Stmt_Case_Others),
                    Get_Location (Definition),
                    "use of ""when others"" in ""case"" statement");
         end if;
      end if;
   end Process_Others;

   ---------------------------
   -- Process_Function_Body --
   ---------------------------

   procedure Process_Function_Body (Function_Body : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Iterator, Asis.Statements;

      First_Return : Asis.Statement;

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out Null_State);
      procedure Check is new Traverse_Element (Null_State, Pre_Procedure, Null_State_Procedure);

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out Null_State)
      is
         use Asis.Elements;
         use Framework.Reports;
      begin
         case Statement_Kind (Element) is
            when A_Return_Statement | An_Extended_Return_Statement =>
               if Is_Nil (First_Return) then
                  First_Return := Element;
               else
                  Report (Rule_Id,
                          Usage (Stmt_Function_Return),
                          Get_Location (Element),
                          "return statement already given at " & Image (Get_Location (First_Return)));
               end if;

            when A_Block_Statement =>
               -- Traverse only the statements part
               declare
                  Block_Stmts : constant Asis.Statement_List := Block_Statements (Element);
               begin
                  for I in Block_Stmts'Range loop
                     Check (Block_Stmts (I), Control, State);
                  end loop;
                  Control := Abandon_Children;
               end;
            when others =>
               -- including Not_A_Statement
               null;
         end case;
      end Pre_Procedure;

      State   : Null_State;
      Control : Traverse_Control;
   begin  -- Process_Function_Body
      if not Rule_Used (Stmt_Function_Return) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Body_Stmts : constant Asis.Statement_List := Body_Statements (Function_Body);
      begin
         First_Return := Nil_Element;
         Control      := Continue;
         for I in Body_Stmts'Range loop
            Check (Body_Stmts (I), Control, State);
         end loop;
      end;

      declare
         Handlers : constant Asis.Exception_Handler_List := Body_Exception_Handlers (Function_Body);
      begin
         for H in Handlers'Range loop
            declare
               Handler_Stmts : constant Asis.Statement_List := Handler_Statements (Handlers (H));
            begin
               First_Return := Nil_Element;
               Control      := Continue;
              for I in Handler_Stmts'Range loop
                  Check (Handler_Stmts (I), Control, State);
               end loop;
            end;
         end loop;
      end;
   end Process_Function_Body;

   ---------------------------
   -- Process_Function_Call --
   ---------------------------

   procedure Process_Function_Call (Call : in Asis.Expression) is
      use Asis.Statements, Utilities;
      use Framework.Reports;
   begin
      if not Rule_Used (Stmt_Dispatching_Call) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Dispatching_Call (Call) then
         Report (Rule_Id,
                 Usage (Stmt_Dispatching_Call),
                 Get_Location (Call),
                 "use of statement """ & Image (Stmt_Dispatching_Call, Lower_Case) & '"');
      end if;
   end Process_Function_Call;


   --------------------------
   -- Process_Loop_Statements
   --------------------------

   procedure Process_Loop_Statements (In_Loop : in Asis.Statement) is
      use Asis, Asis.Iterator, Asis.Statements;

      First_Exit : Asis.Statement;

      type State_Info is
      record
         Loop_Statement : Statement;
      end record;

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out State_Info);
      procedure Post_Procedure (Element : in     Asis.Element;
                                Control : in out Traverse_Control;
                                State   : in out State_Info);
      procedure Check is new Traverse_Element (State_Info, Pre_Procedure, Post_Procedure);

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out State_Info)
      is
         use Asis.Elements;
         use Framework.Reports;
      begin
         case Statement_Kind (Element) is
            when An_Exit_Statement =>
               if Is_Equal (Corresponding_Loop_Exited (Element), State.Loop_Statement) then
                  if Is_Nil (First_Exit) then
                     First_Exit := Element;
                  else
                     Report (Rule_Id,
                             Usage (Stmt_Multiple_Exits),
                             Get_Location (Element),
                             "exit statement already given at " & Image (Get_Location (First_Exit)));
                  end if;
               end if;
            when A_Block_Statement =>
               -- Traverse only the statements and exceptions parts
               declare
                  Block_Stmts : constant Asis.Statement_List := Block_Statements (Element);
               begin
                  for I in Block_Stmts'Range loop
                     Check (Block_Stmts (I), Control, State);
                  end loop;
               end;

               declare
                  Block_Handlers : constant Asis.Exception_Handler_List := Block_Exception_Handlers (Element);
               begin
                  for H in Block_Handlers'Range loop
                     declare
                        Handler_Stmts : constant Asis.Statement_List := Handler_Statements (Block_Handlers (H));
                     begin
                        for I in Handler_Stmts'Range loop
                           Check (Handler_Stmts (I), Control, State);
                        end loop;
                     end;
                  end loop;
               end;
               Control := Abandon_Children;
            when others =>
               -- including Not_A_Statement
               null;
         end case;
      end Pre_Procedure;

      procedure Post_Procedure (Element : in     Asis.Element;
                                Control : in out Traverse_Control;
                                State   : in out State_Info) is
         pragma Unreferenced (Element, Control, State);
      begin
         null;
      end Post_Procedure;

      State   : State_Info := (Loop_Statement => In_Loop);
      Control : Traverse_Control;
   begin  -- Process_Loop_Statements
      declare
         Loop_Stmts : constant Asis.Statement_List := Loop_Statements (In_Loop);
      begin
         First_Exit := Nil_Element;
         Control    := Continue;
         for I in Loop_Stmts'Range loop
            Check (Loop_Stmts (I), Control, State);
         end loop;
      end;
   end Process_Loop_Statements;

   ----------------------
   -- Pre_Process_Loop --
   ----------------------

   procedure Pre_Process_Loop  (Stmt : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Reports, Utilities;
   begin
      if not Rule_Used (Stmt_Unnamed_Multiple_Loop)
        and not Rule_Used (Stmt_Multiple_Exits)
        and not Rule_Used (Stmt_Loop_Return)
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (Stmt_Multiple_Exits) then
         Process_Loop_Statements (Stmt);
      end if;

      if Loops_Depth (Body_Depth) = Loops_Level'Last then
         Failure ("Loops nesting deeper than maximum allowed:"
                  & Loops_Level'Wide_Image (Max_Loop_Nesting),
                  Element => Stmt);
      end if;
      Loops_Depth (Body_Depth) := Loops_Depth (Body_Depth) + 1;
      if Loops_Depth (Body_Depth) = 1 then
         Top_Loop (Body_Depth) := Stmt;
         return;
      end if;

      -- It is a nested loop here

      if Rule_Used (Stmt_Unnamed_Multiple_Loop) then
         if not Is_Nil (Top_Loop (Body_Depth)) then
            if Is_Nil (Statement_Identifier (Top_Loop (Body_Depth))) then
               Report (Rule_Id,
                       Usage (Stmt_Unnamed_Multiple_Loop),
                       Get_Location (Top_Loop (Body_Depth)),
                       "Outer loop is not named, inner loop at " & Image (Get_Location (Stmt)));
            end if;
            Top_Loop (Body_Depth) := Nil_Element;
         end if;

         if Is_Nil (Statement_Identifier (Stmt)) then
            Report (Rule_Id,
                    Usage (Stmt_Unnamed_Multiple_Loop),
                    Get_Location (Stmt),
                    "Nested loop is not named");
         end if;
      end if;
   end Pre_Process_Loop;

   -----------------------
   -- Post_Process_Loop --
   -----------------------

   procedure Post_Process_Loop (Stmt : in Asis.Statement) is
      pragma Unreferenced (Stmt);
   begin
      if not Rule_Used (Stmt_Unnamed_Multiple_Loop)
        and not Rule_Used (Stmt_Multiple_Exits)
        and not Rule_Used (Stmt_Loop_Return)
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Loops_Depth (Body_Depth) := Loops_Depth (Body_Depth) - 1;
   end Post_Process_Loop;

   -------------------------
   -- Process_Scope_Enter --
   -------------------------

   procedure Process_Scope_Enter (Scope : in Asis.Element) is
      use Asis, Asis.Elements;
      use Framework.Scope_Manager;
   begin
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Check scopes that are not "bodies"
      case Element_Kind (Scope) is
         when An_Exception_Handler =>
            return;
         when A_Statement =>
            if Statement_Kind (Scope) /= An_Accept_Statement then
               return;
            end if;
         when others =>
            null;
      end case;

      Body_Depth               := Body_Depth + 1;
      Loops_Depth (Body_Depth) := 0;
   end Process_Scope_Enter;

   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Process_Scope_Exit  (Scope : in Asis.Element) is
      use Asis, Asis.Elements;
      use Framework.Scope_Manager;
   begin
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Check scopes that are not "bodies"
      case Element_Kind (Scope) is
         when An_Exception_Handler =>
            return;
         when A_Statement =>
            if Statement_Kind (Scope) /= An_Accept_Statement then
               return;
            end if;
         when others =>
            null;
      end case;

      Body_Depth := Body_Depth - 1;
   end Process_Scope_Exit;


   ----------------
   -- Enter_Unit --
   ----------------

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      use Asis, Asis.Compilation_Units;
   begin
      if Unit_Kind (Unit) not in A_Subunit then
         -- In normal cases, Body_Depth from processing previous units has returned to 0 when
         -- we enter a top-level unit (not a subunit).
         -- However, if a previous unit failed, Body_Depth is left at a possible non-zero value.
         -- If we have several failures, we may end up in Constraint_Error.
         -- So, stay on the safe side and force resetting in all cases.
         Body_Depth := 0;
      end if;
   end Enter_Unit;

begin  -- Rules.Statements
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Statements;
