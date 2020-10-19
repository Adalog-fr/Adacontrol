----------------------------------------------------------------------
--  Rules.Statement - Package body                                  --
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
   Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis.Compilation_Units,
  Asis.Definitions,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  Elements_Set,
  Scope_Manager,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Adactl_Constants,
  Framework.Language,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);

package body Rules.Statements is
   use Adactl_Constants, Framework, Framework.Control_Manager, Framework.Variables, Framework.Variables.Shared_Types;

   -- Algorithm
   --
   -- Subrule Hard_Bounds_Array_For_Loop:
   -- At the time a for loop is traversed, it is easy to check whether the bounds countain references to attributes.
   -- If not, the loop parameter is stored in the element set Not_For_Indexing, meaning that this parameter should
   -- not be used for indexing.
   -- When an indexed variable is traversed, the indexing expression is traversed, and if a reference to an identifier
   -- in Not_For_Indexing is found, the error is reported, and the identifier is removed from Not_For_Indexing to
   -- avoid further messages (since the error is on the for loop statement).


   -- all "filtered_raise" subrules (i.e. raise subrules except the plain one) must stay together
   type Subrules is (Stmt_Any_Statement,

                     Stmt_Abort,                   Stmt_Accept,                 Stmt_Accept_Return,
                     Stmt_Assignment,              Stmt_Asynchronous_Select,

                     Stmt_Backward_Goto,           Stmt_Block,

                     Stmt_Case,                    Stmt_Case_Others,            Stmt_Case_Others_Null,
                     Stmt_Code,                    Stmt_Conditional_Entry_Call,

                     Stmt_Declare_Block,           Stmt_Delay,                  Stmt_Delay_Until,
                     Stmt_Dispatching_Call,        Stmt_Dynamic_Procedure_Call,

                     Stmt_Effective_Declare_Block, Stmt_Entry_Call,             Stmt_Entry_Return,
                     Stmt_Exception_Others,        Stmt_Exception_Others_Null,  Stmt_Exit,
                     Stmt_Exit_Expanded_Name,      Stmt_Exit_For_Loop,          Stmt_Exit_Outer_Loop,
                     Stmt_Exit_Plain_Loop,         Stmt_Exit_While_Loop,        Stmt_Exited_Extended_Return,
                     Stmt_Extended_Return,

                     Stmt_For_Loop,                Stmt_For_In_Loop,            Stmt_For_Iterator_Loop,
                     Stmt_For_Of_Loop,             Stmt_Function_Return,

                     Stmt_Goto,                    Stmt_Goto_Not_Continue,

                     Stmt_Hard_Bounds_Array_For_Loop,

                     Stmt_If,                      Stmt_If_Elsif,               Stmt_Inherited_Procedure_Call,

                     Stmt_Labelled,                Stmt_Loop_Return,

                     Stmt_Multi_For_Of_Loop,       Stmt_Multiple_Exits,

                     Stmt_Named_Exit,              Stmt_No_Else,                Stmt_Null,
                     Stmt_Null_Case_Path,          Stmt_Null_If_Path,           Stmt_Null_Loop_Body,

                     Stmt_Procedure_Call,          Stmt_Procedure_Return,

                     Stmt_Raise,

                     -- all "filtered_raise"
                     Stmt_Raise_Foreign,           Stmt_Raise_Locally_Handled,  Stmt_Raise_Nonpublic,
                     Stmt_Raise_Standard,          Stmt_Reraise,

                     Stmt_Redispatching_Call,     Stmt_Requeue,

                     Stmt_Selective_Accept,       Stmt_Simple_Block,            Stmt_Simple_Loop,

                     Stmt_Terminate,              Stmt_Timed_Entry_Call,

                     Stmt_Unconditional_Exit,     Stmt_Unnamed_Block,           Stmt_Unnamed_Exit,
                     Stmt_Unnamed_Loop_Exited,    Stmt_Unnamed_For_Loop,        Stmt_Unnamed_Multiple_Loop,
                     Stmt_Unnamed_Simple_Block,   Stmt_Unnamed_Simple_Loop,     Stmt_Unnamed_While_Loop,
                     Stmt_Untyped_For,            Stmt_Untyped_For_In,          Stmt_Untyped_For_Of,

                     Stmt_While_Loop);

   subtype Filtered_Raise_Subrules is Subrules range Stmt_Raise_Foreign .. Stmt_Reraise;
   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "STMT_");
   use Subrules_Flags_Utilities;

   type Usage_Flags is array (Subrules) of Boolean;
   No_Rule   : constant Usage_Flags := (others => False);
   Rule_Used : Usage_Flags := No_Rule;
   Save_Used : Usage_Flags;
   Usage     : array (Subrules) of Basic_Rule_Context;

   -- For Stmt_Hard_Bounds_Array_For_Loop
   type Loop_Parameter_Status is (Indexing_OK, Indexing_KO);
   Not_For_Indexing: Elements_Set.Set;

   -- For Stmt_Unnamed_Multiple_Loop:
   type Loops_Level is range 0 .. Max_Loop_Nesting;
   Body_Depth  : Scope_Manager.Scope_Range := 0;
   Loops_Depth : array (Scope_Manager.Scope_Range) of Loops_Level;
   Top_Loop    : array (Scope_Manager.Scope_Range) of Asis.Statement;

   -- Rule variables
   Called_Info       : aliased Extra_Infos_Type.Object := (Value => None);
   Small_Loop_Length : aliased Natural_Type.Object     := (Value => 0);

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
      User_Message;
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Called_Info");
      Help_On_Variable (Rule_Id & ".Small_Loop_Length");
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
            if not Basic.Merge_Context (Usage (Subrule), Ctl_Kind, Ctl_Label) then
               Parameter_Error (Rule_Id, "statement already given: " & Image (Subrule, Lower_Case));
            end if;
         else
            Rule_Used (Subrule) := True;
            Usage (Subrule)     := Basic.New_Context (Ctl_Kind, Ctl_Label);
         end if;
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
            Rule_Used         := No_Rule;
            Called_Info       := (Value => None);
            Small_Loop_Length := (Value => 0);
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

   procedure Process_Statement (Stmt : in Asis.Statement) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Locations, Thick_Queries, Utilities;

      procedure Do_Report (Subrule : in Subrules; Loc : Location := Get_Location (Stmt)) is
         use Framework.Reports;
      begin
         if not Rule_Used (Subrule) then
            return;
         end if;

         Report (Rule_Id,
                 Usage (Subrule),
                 Loc,
                 "use of statement """ & Image (Subrule, Lower_Case) & '"');
      end Do_Report;

      procedure Do_Report (Subrule : in Subrules; Extra_Info : Wide_String) is
         use Framework.Reports;
      begin
         if not Rule_Used (Subrule) then
            return;
         end if;

         Report (Rule_Id,
                 Usage (Subrule),
                 Get_Location (Stmt),
                 "use of statement """ & Image (Subrule, Lower_Case) & """, " & Extra_Info);
      end Do_Report;

      procedure Do_Call_Report (Subrule : in Subrules) is
      begin
         if not Rule_Used (Subrule) then
            return;
         end if;

         if Called_Info.Value = None then
            Do_Report (Subrule);
         else
            declare
               Called : constant Asis.Expression := Called_Simple_Name (Stmt);
            begin
               if Is_Nil (Called) then
                  Do_Report (Subrule, "Dynamic call");
               else
                  case Called_Info.Value is
                     when None =>
                        Failure (Rule_Id & ": impossible value None");
                     when Compact =>
                        Do_Report (Subrule,
                                   Adjust_Image (Full_Name_Image (Called, With_Profile => False)));
                     when Detailed =>
                        Do_Report (Subrule,
                                   Adjust_Image (Full_Name_Image (Called, With_Profile => True)));
                     when Root_Detailed =>
                        Do_Report (Subrule,
                                   Adjust_Image (Full_Name_Image (Ultimate_Name (Called), With_Profile => True)));
                  end case;
               end if;
            end;
         end if;
      end Do_Call_Report;

      procedure Check_Filtered_Raise (Exc : Asis.Expression) is
         -- process filtered (i.e. non trivial) raises.
         -- common to a real raise and to a call to Raise_Exception
         use Asis.Compilation_Units;

         function Is_Parent_Scope (To_Check : Asis.Declaration; Starting_From : Asis.Element) return Boolean is
         -- Check if To_Check is an enclosing scope of Starting_From, or of the specification of some unit that
         -- encloses Starting_From
            Current : Asis.Element := Starting_From;
         begin
            while not Is_Nil (Current) loop
               if Is_Equal (Current, To_Check) then
                  return True;
               elsif Is_Subunit (Current) then
                  Current := Corresponding_Body_Stub (Current);
                  if Is_Equal (Corresponding_Declaration (Current), To_Check) then
                     return True;
                  end if;
               else
                  case Declaration_Kind (Current) is
                     when A_Package_Body_Declaration
                        | A_Task_Body_Declaration
                        | A_Protected_Body_Declaration
                        =>
                        if Is_Equal (Corresponding_Declaration (Current), To_Check) then
                           return True;
                        end if;
                     when others =>
                        null;
                  end case;
               end if;

               Current := Enclosing_Element (Current);
            end loop;
            return False;
         end Is_Parent_Scope;

         Handler         : Asis.Exception_Handler;
         Decl_Place      : Asis.Declaration;
         Is_Standard_Exc : Boolean := False;
      begin   -- Check_Filtered_Raise
         if Rule_Used (Stmt_Raise_Standard) or Rule_Used (Stmt_Raise_Foreign) or Rule_Used (Stmt_Raise_Nonpublic) then
            Is_Standard_Exc := To_Upper (Unit_Full_Name (Definition_Compilation_Unit (Exc))) = "STANDARD";
         end if;

         if Rule_Used (Stmt_Raise_Standard) and Is_Standard_Exc then
            Do_Report (Stmt_Raise_Standard);
         end if;

         if (Rule_Used (Stmt_Raise_Foreign) or Rule_Used (Stmt_Raise_Nonpublic)) and not Is_Standard_Exc then
            Decl_Place := Enclosing_Element (Corresponding_Name_Declaration (Exc));
            -- Report if it is in the visible part of the package spec which is the current
            -- compilation unit
            -- NB: Corresponding_Declaration of a proper body returns Nil_Element, therefore
            --     this case is automatically eliminated
            if Declaration_Kind (Decl_Place) not in A_Package_Declaration | A_Generic_Package_Declaration
              or else Is_Part_Of (Corresponding_Name_Declaration (Exc),
                                  Private_Part_Declarative_Items (Decl_Place))
            then
               Do_Report (Stmt_Raise_Nonpublic);
            end if;

            if    not Is_Parent_Scope (To_Check => Decl_Place, Starting_From => Stmt)
              and not Is_Ancestor (Outer => Enclosing_Compilation_Unit (Decl_Place), -- Exception not from Ancestor
                                   Inner => Corresponding_Declaration (Enclosing_Compilation_Unit (Stmt)))
            then
               Do_Report (Stmt_Raise_Foreign);
            end if;
         end if;

         if Rule_Used (Stmt_Raise_Locally_Handled) then
            Handler := Corresponding_Static_Exception_Handler (Exc, Stmt, Include_Others => True);
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

      if not Is_Nil (Label_Names (Stmt)) then
         Do_Report (Stmt_Labelled);
      end if;

      case Statement_Kind (Stmt) is
         when Not_A_Statement =>
            Failure ("Not a statement");

         when An_Abort_Statement =>
            Do_Report (Stmt_Abort);

         when An_Accept_Statement =>
            Do_Report (Stmt_Accept);

         when An_Assignment_Statement =>
            Do_Report (Stmt_Assignment);

         when An_Asynchronous_Select_Statement =>
            Do_Report (Stmt_Asynchronous_Select);

         when A_Block_Statement =>
            Do_Report (Stmt_Block);

            if Is_Nil (Statement_Identifier (Stmt)) then
               Do_Report (Stmt_Unnamed_Block);
            end if;

            if Is_Declare_Block (Stmt) then
               Do_Report (Stmt_Declare_Block);
               if Rule_Used (Stmt_Effective_Declare_Block) then
                  for D : Asis.Element of Block_Declarative_Items (Stmt, Include_Pragmas => False) loop
                     if Clause_Kind (D) not in A_Use_Package_Clause | A_Use_Type_Clause | A_Use_All_Type_Clause then
                        Do_Report (Stmt_Effective_Declare_Block);
                        exit;
                     end if;
                  end loop;
               end if;
               if Rule_Used (Stmt_Simple_Block) or Rule_Used (Stmt_Unnamed_Simple_Block) then
                  declare
                     Decls : constant Asis.Declarative_Item_List := Block_Declarative_Items (Stmt,
                                                                                             Include_Pragmas => True);
                  begin
                     if Is_Nil (Decls) then
                        Do_Report (Stmt_Simple_Block);

                        if Is_Nil (Statement_Identifier (Stmt)) then
                           Do_Report (Stmt_Unnamed_Simple_Block);
                        end if;
                     end if;
                  end;
               end if;
            elsif Block_Exception_Handlers (Stmt) = Nil_Element_List then
               Do_Report (Stmt_Simple_Block);

               if Is_Nil (Statement_Identifier (Stmt)) then
                  Do_Report (Stmt_Unnamed_Simple_Block);
               end if;
           end if;

         when A_Case_Statement =>
            Do_Report (Stmt_Case);
            if Rule_Used (Stmt_Null_Case_Path) then
               declare
                  Paths : constant Asis.Path_List := Statement_Paths (Stmt);
               begin
                  for P : Asis.Path of Paths loop
                     if Are_Null_Statements (Sequence_Of_Statements (P)) then
                        Do_Report (Stmt_Null_Case_Path, Loc => Get_Location (P));
                     end if;
                  end loop;
               end;
            end if;

         when A_Code_Statement =>
            Do_Report (Stmt_Code);

         when A_Conditional_Entry_Call_Statement =>
            Do_Report (Stmt_Conditional_Entry_Call);

         when A_Delay_Relative_Statement =>
            Do_Report (Stmt_Delay);

         when A_Delay_Until_Statement =>
            Do_Report (Stmt_Delay_Until);

         when An_Entry_Call_Statement =>
            Do_Call_Report (Stmt_Entry_Call);

         when An_Exit_Statement =>
            Do_Report (Stmt_Exit);

            declare
               Exited_Loop : constant Asis.Statement := Corresponding_Loop_Exited (Stmt);
            begin
               case Statement_Kind (Exited_Loop) is
                  when A_For_Loop_Statement =>
                     Do_Report (Stmt_Exit_For_Loop);
                  when A_While_Loop_Statement =>
                     Do_Report (Stmt_Exit_While_Loop);
                  when A_Loop_Statement =>
                     Do_Report (Stmt_Exit_Plain_Loop);
                  when others =>
                     Failure ("Statements: exit not from loop", Exited_Loop);
               end case;

               if Is_Nil (Exit_Condition (Stmt)) then
                  Do_Report (Stmt_Unconditional_Exit);
               end if;

               if Is_Nil (Statement_Identifier (Exited_Loop)) then
                  Do_Report (Stmt_Unnamed_Loop_Exited);
               end if;

               if Is_Nil (Exit_Loop_Name (Stmt)) then
                  if not Is_Nil (Statement_Identifier (Exited_Loop)) then
                     Do_Report (Stmt_Unnamed_Exit);
                  end if;
               else
                  Do_Report (Stmt_Named_Exit);
               end if;

               if Expression_Kind (Exit_Loop_Name (Stmt)) = A_Selected_Component then
                  Do_Report (Stmt_Exit_Expanded_Name);
               end if;

               if Rule_Used (Stmt_Exit_Outer_Loop) then
                  declare
                     Enclosing_Loop : Asis.Statement := Enclosing_Element (Stmt);
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
               Dirty : constant Asis.Statement := First_Exiting_Statement (Extended_Return_Statements (Stmt),
                                                                           Include_Returns => False);
            begin
               if not Is_Nil (Dirty) then
                  Do_Report (Stmt_Exited_Extended_Return, "exiting at " & Image (Get_Location (Dirty)));
               end if;
            end;

         when A_For_Loop_Statement =>
            declare
               Loop_Spec : constant Asis.Declaration := For_Loop_Parameter_Specification (Stmt);
            begin
               Do_Report (Stmt_For_Loop);

               case Declaration_Kind (Loop_Spec) is
                  when A_Loop_Parameter_Specification =>
                     Do_Report (Stmt_For_In_Loop);

                     if Discrete_Range_Kind (Specification_Subtype_Definition
                                             (Loop_Spec)) = A_Discrete_Simple_Expression_Range
                     then
                        Do_Report (Stmt_Untyped_For);
                        Do_Report (Stmt_Untyped_For_In);
                     end if;
                  when An_Element_Iterator_Specification =>
                     Do_Report (Stmt_For_Of_Loop);
                     if Is_Nil (Declarations.Subtype_Indication (Loop_Spec)) then
                        Do_Report (Stmt_Untyped_For);
                        Do_Report (Stmt_Untyped_For_Of);
                     end if;
                     if Rule_Used (Stmt_Multi_For_Of_Loop) then
                        declare
                           use Asis.Definitions;
                           Obj_Def : constant Asis.Definition := Thick_Queries.Corresponding_Expression_Type_Definition
                                                                  (Iteration_Scheme_Name (Loop_Spec));
                        begin
                           case Type_Kind (Obj_Def) is
                              when A_Constrained_Array_Definition =>
                                 if Discrete_Subtype_Definitions (Obj_Def)'Length /= 1 then
                                    Do_Report (Stmt_Multi_For_Of_Loop);
                                 end if;
                              when An_Unconstrained_Array_Definition =>
                                 if Index_Subtype_Definitions (Obj_Def)'Length /= 1 then
                                    Do_Report (Stmt_Multi_For_Of_Loop);
                                 end if;
                              when others =>
                                 -- An user defined iterator => ignore
                                 null;
                           end case;
                        end;
                     end if;
                  when  A_Generalized_Iterator_Specification =>
                     Do_Report (Stmt_For_Iterator_Loop);
                     -- No Untyped_For here, a subtype indication is not allowed by the syntax
                  when others =>
                     Failure ("Process_Statement: unknown ""for"" iterator", Loop_Spec);
               end case;

               if Is_Nil (Statement_Identifier (Stmt))
                 and then Lines_Span_Length (Stmt) > Small_Loop_Length. Value
               then
                  Do_Report (Stmt_Unnamed_For_Loop);
               end if;

               if Are_Null_Statements (Loop_Statements (Stmt)) then
                  Do_Report (Stmt_Null_Loop_Body);
               end if;
            end;

         when A_Goto_Statement =>
            Do_Report (Stmt_Goto);
            if Rule_Used (Stmt_Goto_Not_Continue) then
               declare
                  Target_Stmt         : constant Asis.Statement := Corresponding_Destination_Statement (Stmt);
                  Enclosing_Stmt_List : constant Asis.Statement_List
                    := Thick_Queries.Statements (Enclosing_Element (Target_Stmt));
               begin
                  -- Report, unless the label is on a null statement which is the last statement of a loop
                  if Statement_Kind (Target_Stmt) /= A_Null_Statement
                    or else not Is_Equal (Target_Stmt, Enclosing_Stmt_List (Enclosing_Stmt_List'Last))
                    or else Statement_Kind (Enclosing_Element (Target_Stmt))
                            not in A_Loop_Statement .. A_For_Loop_Statement
                  then
                     Do_Report (Stmt_Goto_Not_Continue);
                  end if;
               end;
            end if;

            if Rule_Used (Stmt_Backward_Goto) then
               declare
                  Target_Stmt_Loc : constant Location := Get_Location (Corresponding_Destination_Statement (Stmt));
                  Stmt_Loc        : constant Location := Get_Location (Stmt);
               begin
                  if Target_Stmt_Loc <= Stmt_Loc then
                     Do_Report (Stmt_Backward_Goto);
                  end if;
               end;
            end if;

         when An_If_Statement =>
            Do_Report (Stmt_If);
            declare
               Paths : constant Asis.Path_List := Statement_Paths (Stmt);
            begin
               if Path_Kind (Paths (Paths'Last)) /= An_Else_Path then
                  Do_Report (Stmt_No_Else);
               end if;
               if Paths'Length >= 2 and then Path_Kind (Paths (2)) = An_Elsif_Path then
                  Do_Report (Stmt_If_Elsif);
               end if;
               if Rule_Used (Stmt_Null_If_Path) then
                  for P : Asis.Path of Paths loop
                     if Are_Null_Statements (Sequence_Of_Statements (P)) then
                        Do_Report (Stmt_Null_If_Path, Loc => Get_Location (P));
                     end if;
                  end loop;
               end if;
            end;

         when A_Loop_Statement =>
            Do_Report (Stmt_Simple_Loop);

            if Is_Nil (Statement_Identifier (Stmt))
              and then Lines_Span_Length (Stmt) > Small_Loop_Length. Value
            then
               Do_Report (Stmt_Unnamed_Simple_Loop);
            end if;

            if Are_Null_Statements (Loop_Statements (Stmt)) then
               Do_Report (Stmt_Null_Loop_Body);
            end if;

         when A_Null_Statement =>
            if not Is_Part_Of_Implicit (Stmt) then  -- Do not report floating labels as null statements
               Do_Report (Stmt_Null);
            end if;

         when A_Procedure_Call_Statement =>
            Do_Call_Report (Stmt_Procedure_Call);

            if Is_Dispatching_Call (Stmt) then
               Do_Report (Stmt_Dispatching_Call);

               if Rule_Used (Stmt_Redispatching_Call) then
                  declare
                     Name : Asis.Defining_Name := Enclosing_Program_Unit (Stmt);
                  begin
                     while not Is_Nil (Name) loop
                        if Is_Dispatching_Operation (Corresponding_Declaration (Enclosing_Element (Name))) then
                           Do_Report (Stmt_Redispatching_Call);
                        end if;
                        Name := Enclosing_Program_Unit (Name);
                     end loop;
                  end;
               end if;
            end if;

            if Is_Nil (Called_Simple_Name (Stmt)) then
               Do_Report (Stmt_Dynamic_Procedure_Call);
            end if;

            if Rule_Used (Stmt_Inherited_Procedure_Call) then
               declare
                  Called : constant Asis.Expression := Ultimate_Name (Called_Simple_Name (Stmt));
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
                  Called : constant Asis.Expression := Called_Simple_Name (Stmt);
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
                                                                         (Stmt, Normalized => True) (1)));
                           begin
                              if Expression_Kind (Exc_Param) = An_Attribute_Reference
                                and then Attribute_Kind (Exc_Param) = An_Identity_Attribute
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

            if Is_Nil (Raised_Exception (Stmt)) then
               if Rule_Used (Stmt_Reraise) then
                  Do_Report (Stmt_Reraise);
               end if;
            else
               Check_Filtered_Raise (Simple_Name (Raised_Exception (Stmt)));
            end if;

         when A_Requeue_Statement | A_Requeue_Statement_With_Abort =>
            Do_Report (Stmt_Requeue);

         when A_Return_Statement =>
            if Loops_Depth (Body_Depth) > 0 then
               Do_Report (Stmt_Loop_Return);
            end if;
            case Declaration_Kind (Enclosing_Element
                                   (Enclosing_Program_Unit (Stmt, Including_Accept => True)))
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

            if Is_Nil (Statement_Identifier (Stmt))
              and then Lines_Span_Length (Stmt) > Small_Loop_Length. Value
            then
               Do_Report (Stmt_Unnamed_While_Loop);
            end if;

            if Are_Null_Statements (Loop_Statements (Stmt)) then
               Do_Report (Stmt_Null_Loop_Body);
            end if;

      end case;
   end Process_Statement;


   --------------------
   -- Process_Others --
   --------------------

   procedure Process_Others (Definition : in Asis.Definition) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports, Thick_Queries;
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

   ------------------------------
   -- Process_Index_Expression --
   ------------------------------

   procedure Process_Index_Expression (Expr : in Asis.Expression) is
      use Asis,Asis.Iterator, Asis.Expressions;

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out Null_State);
      procedure Check_Var is new Traverse_Element (Null_State, Pre_Procedure, Null_State_Procedure);

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out Null_State)
      is
         use Asis.Declarations, Asis.Elements, Asis.Text, Ada.Strings.Wide_Fixed;
         use Elements_Set, Framework.Locations, Framework.Reports;
      begin
         case Expression_Kind (Element) is
            when An_Identifier =>
               if Contains (Not_For_Indexing, Element) then
                  Report (Rule_Id,
                          Usage (Stmt_Hard_Bounds_Array_For_Loop),
                          Get_Location (Specification_Subtype_Definition (Corresponding_Name_Declaration (Element))),
                          "Bounds of loop do not mention 'First, 'Last or 'Range, used for indexing "
                          & Trim (Element_Image (Prefix (Expr)), Ada.Strings.Both)
                          & " at "
                          & Image (Get_Location (Expr)));
                  Delete (Not_For_Indexing, Element);  -- No more messages for this loop
                  Control := Terminate_Immediately;
               end if;
            when An_Attribute_Reference =>
               -- Don't traverse the attribute
               Check_Var (Prefix (Element), Control, State);
               Control := Abandon_Children;
            when others =>
               null;
         end case;
      end Pre_Procedure;

      Control : Traverse_Control;
      State   : Null_State;
   begin  -- Process_Index_Expression
      if not Rule_Used (Stmt_Hard_Bounds_Array_For_Loop) then
         return;
      end if;

      for Index : Asis.Expression of Index_Expressions (Expr) loop
         Control := Continue;
         Check_Var (Index, Control, State);
      end loop;
   end Process_Index_Expression;

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
         use Framework.Locations, Framework.Reports;
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
               for S : Asis.Statement of Block_Statements (Element) loop
                  Check (S, Control, State);
               end loop;
               Control := Abandon_Children;
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

      First_Return := Nil_Element;
      Control      := Continue;
      for S : Asis.Statement of Body_Statements (Function_Body) loop
         Check (S, Control, State);
      end loop;

      for H : Asis.Exception_Handler of Body_Exception_Handlers (Function_Body) loop
         First_Return := Nil_Element;
         Control      := Continue;
         for S : Asis.Statement of Handler_Statements (H) loop
            Check (S, Control, State);
         end loop;
      end loop;
   end Process_Function_Body;


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
         use Framework.Locations, Framework.Reports;
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
               for S : Asis.Statement of Block_Statements (Element) loop
                  Check (S, Control, State);
               end loop;

               for H : Asis.Exception_Handler of Block_Exception_Handlers (Element) loop
                  for S : Asis.Statement of Handler_Statements (H) loop
                     Check (S, Control, State);
                  end loop;
               end loop;
               Control := Abandon_Children;
            when others =>
               -- including Not_A_Statement
               null;
         end case;
      end Pre_Procedure;

      procedure Post_Procedure (Element : in     Asis.Element;
                                Control : in out Traverse_Control;
                                State   : in out State_Info)
      is
         pragma Unreferenced (Element, Control, State);
      begin
         null;
      end Post_Procedure;

      State   : State_Info := (Loop_Statement => In_Loop);
      Control : Traverse_Control;
   begin  -- Process_Loop_Statements
      First_Exit := Nil_Element;
      Control    := Continue;
      for S : Asis.Statement of Loop_Statements (In_Loop) loop
         Check (S, Control, State);
      end loop;
   end Process_Loop_Statements;

   ----------------------------
   -- Process_Loop_Parameter --
   ----------------------------

   procedure Process_Loop_Parameter (Spec : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Iterator;
      use Elements_Set, Utilities;

      Parameter   : constant Asis.Defining_Name := Names (Spec) (1);
      Subtype_Def : constant Asis.Definition    := Specification_Subtype_Definition (Spec);
      Constraint  : Asis.Constraint;

      procedure Pre_Procedure  (Element : in     Asis.Element;
                                Control : in out Traverse_Control;
                                State   : in out Loop_Parameter_Status);
      procedure Post_Procedure (Element : in     Asis.Element;
                                Control : in out Traverse_Control;
                                State   : in out Loop_Parameter_Status) is null;
      procedure Check_Attribute is new Traverse_Element (Loop_Parameter_Status, Pre_Procedure, Post_Procedure);
      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out Loop_Parameter_Status)
      is
      begin
         case Attribute_Kind (Element) is
            when A_First_Attribute | A_Last_Attribute | A_Range_Attribute =>
               State   := Indexing_OK;
               Control := Terminate_Immediately;
            when others =>
               -- Including Not_An_Attribute
               null;
         end case;
      end Pre_Procedure;

      Control : Traverse_Control;
      State   : Loop_Parameter_Status;
   begin  --Process_Loop_Parameter
      case Discrete_Range_Kind (Subtype_Def) is
         when A_Discrete_Subtype_Indication =>
            -- Subtype_simple_name
            Constraint := Subtype_Constraint (Subtype_Def);
            if Is_Nil (Constraint) then
               Add (Not_For_Indexing, Parameter);
            else
               Control := Continue;
               State   := Indexing_KO;
               Check_Attribute (Lower_Bound (Constraint), Control, State);
               if State = Indexing_OK then
                  Control := Continue;
                  State   := Indexing_KO;
                  Check_Attribute (Upper_Bound (Constraint), Control, State);
               end if;
               if State = Indexing_KO then
                  Add (Not_For_Indexing, Parameter);
               end if;
            end if;
         when A_Discrete_Range_Attribute_Reference =>
            null; -- OK for indexing
         when A_Discrete_Simple_Expression_Range =>
            Control := Continue;
            State   := Indexing_KO;
            Check_Attribute (Lower_Bound (Subtype_Def), Control, State);
            if State = Indexing_OK then
               Control := Continue;
               State   := Indexing_KO;
               Check_Attribute (Upper_Bound (Subtype_Def), Control, State);
            end if;
            if State = Indexing_KO then
               Add (Not_For_Indexing, Parameter);
            end if;
         when Not_A_Discrete_Range =>
            Failure ("Process_Loop_Parameter: bad range", Subtype_Def);
      end case;
   end Process_Loop_Parameter;

   ----------------------
   -- Pre_Process_Loop --
   ----------------------

   procedure Pre_Process_Loop  (Stmt : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports, Utilities;
      Loop_Spec : Asis.Declaration;
   begin
      if (Rule_Used and Usage_Flags'(Stmt_Hard_Bounds_Array_For_Loop
                                   | Stmt_Unnamed_Multiple_Loop
                                   | Stmt_Multiple_Exits
                                   | Stmt_Loop_Return => True,
                                   others             => False))
        = No_Rule
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (Stmt_Multiple_Exits) then
         Process_Loop_Statements (Stmt);
      end if;

      if Rule_Used (Stmt_Hard_Bounds_Array_For_Loop) and then Statement_Kind (Stmt) = A_For_Loop_Statement then
         Loop_Spec := For_Loop_Parameter_Specification (Stmt);
         if Declaration_Kind (Loop_Spec) = A_Loop_Parameter_Specification then
            Process_Loop_Parameter (Loop_Spec);
         end if;
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
      use Asis, Asis.Declarations, Asis.Elements, Asis.Statements;
      use Elements_Set;

      Parameter : Asis.Defining_Name;
   begin
      if (Rule_Used and Usage_Flags'(Stmt_Hard_Bounds_Array_For_Loop
                                   | Stmt_Unnamed_Multiple_Loop
                                   | Stmt_Multiple_Exits
                                   | Stmt_Loop_Return => True,
                                   others             => False))
        = No_Rule
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Loops_Depth (Body_Depth) := Loops_Depth (Body_Depth) - 1;

      if Rule_Used (Stmt_Hard_Bounds_Array_For_Loop) and then Statement_Kind (Stmt) = A_For_Loop_Statement then
         Parameter := Names (For_Loop_Parameter_Specification (Stmt)) (1);
         if Contains (Not_For_Indexing, Parameter) then
            Delete (Not_For_Indexing, Parameter);
         end if;
      end if;
   end Post_Process_Loop;

   -------------------------
   -- Process_Scope_Enter --
   -------------------------

   procedure Process_Scope_Enter (Scope : in Asis.Element) is
      use Asis, Asis.Elements;
      use Scope_Manager;
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
      use Scope_Manager;
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
   Framework.Variables.Register (Called_Info'Access,
                                 Variable_Name => Rule_Id & ".CALLED_INFO");
   Framework.Variables.Register (Small_Loop_Length'Access,
                                 Variable_Name => Rule_Id & ".SMALL_LOOP_LENGTH");
end Rules.Statements;
