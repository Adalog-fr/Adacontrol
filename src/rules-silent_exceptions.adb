----------------------------------------------------------------------
--  Rules.Silent_Exceptions - Package body                          --
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

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Adactl_Constants;

package body Rules.Silent_Exceptions is
   use Adactl_Constants, Framework, Framework.Control_Manager;

   -- Algorithm
   --
   -- Path analysis
   -- We traverse recursively all path, setting up a truth table (one for each control kind). The values
   -- tell whether, in the current construct, no path, some paths, or all paths are reporting.
   -- An "and" function computes the result of combining two serial paths, and an "or" function computes the
   -- result of combining two parallel paths.
   --
   -- Management of "with" and "not":
   -- We want to include all exceptions named in a "with" <control spec>, except those named in a "not" <control spec>
   -- This is done by having a list of control specs, where "with" are prepended and "not" are appended.
   -- If there is no "with", we must include all exceptions by default, but this is easy to see since the first
   -- element of the list is a "not".
   --
   -- There is a special case, with separate booleans, for "not others". There is /no/ special case for "with others";
   -- what actually happens is that "others" is taken as a package name. Of course, there is no package named "others",
   -- but since there is a "with <control spec>", all exceptions are uncontrolled - except "others" handler; this
   -- achieves the desired effect. Actually, if the user specified "with junk" it would work as well. We just pretend
   -- we made it on purpose ;-)

   type Usage is array (Control_Kinds) of Boolean;
   Not_Used : constant Usage := (others => False);
   Rule_Used  : Usage := Not_Used;
   Save_Used  : Usage;
   Ctl_Labels : array (Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type Search_Result_Kind is (Neutral, No_Path, Some_Paths, All_Paths);
   -- Neutral is the neutral element for "or" and "and", used for initialization

   type Search_Result is array (Control_Kinds) of Search_Result_Kind;

   type Proc_Context is new Root_Context with
      record
         Usage : Search_Result;
      end record;

   Rule_Uses : Context_Store;

   type Exception_Status is
      record
         Entity     : Entity_Specification;
         Controlled : Boolean;
      end record;
   package Exception_Status_Queues is new Linear_Queue (Exception_Status);

   Others_Entity         : constant Entity_Specification := Value ("others");
   Raise_Entity          : constant Entity_Specification := Value ("raise");
   Reraise_Entity        : constant Entity_Specification := Value ("reraise");
   Explicit_Raise_Entity : constant Entity_Specification := Value ("explicit_raise");

   Special_Exceptions  : array (Control_Kinds) of Exception_Status_Queues.Queue;
   Others_Uncontrolled : array (Control_Kinds) of Boolean := (others => False);

   -- Data for managing "exit" from loops:
   type Active_Loops_Data is
      record
         The_Loop    : Asis.Statement;
         Exit_Result : Search_Result;
      end record;
   type Loops_Level is range 0 .. Max_Loop_Nesting;
   Active_Loops : array (Loops_Level range 1 .. Loops_Level'Last) of Active_Loops_Data;
   Loops_Depth  : Loops_Level;

   ----------
   -- "or" --
   ----------

   -- Combine two parallel paths

   Or_Truth_Table : constant array (Search_Result_Kind, Search_Result_Kind) of Search_Result_Kind :=
   --                Neutral     No_Path     Some_Paths   All_Paths
     (Neutral    => (Neutral,    No_Path,    Some_Paths,  All_Paths),
      No_Path    => (No_Path,    No_Path,    Some_Paths,  Some_Paths),
      Some_Paths => (Some_Paths, Some_Paths, Some_Paths,  Some_Paths),
      All_Paths  => (All_Paths,  Some_Paths, Some_Paths,  All_Paths));

   function "or" (L, R : Search_Result) return Search_Result is
      Result : Search_Result;
   begin
      for I in Result'Range loop
         Result (I) := Or_Truth_Table (L(I), R(I));
      end loop;
      return Result;
   end "or";

   -----------
   -- "and" --
   -----------

   -- Combine two serial paths
   -- Result is the strongest of both paths

   And_Truth_Table : constant array (Search_Result_Kind, Search_Result_Kind) of Search_Result_Kind :=
   --                Neutral     No_Path     Some_Paths   All_Paths
     (Neutral    => (Neutral,    No_Path,    Some_Paths,  All_Paths),
      No_Path    => (No_Path,    No_Path,    Some_Paths,  All_Paths),
      Some_Paths => (Some_Paths, Some_Paths, Some_Paths,  All_Paths),
      All_Paths  => (All_Paths,  All_Paths,  All_Paths,   All_Paths));

   function "and" (L, R : Search_Result) return Search_Result is
      Result : Search_Result;
   begin
      for I in Result'Range loop
         Result (I) := And_Truth_Table (L(I), R(I));
      end loop;
      return Result;
   end "and";

   ----------------
   -- Add_Entity --
   ----------------

   procedure Add_Entity (Entity : Entity_Specification; Ctl_Kind : Control_Kinds) is
      Value  : Proc_Context := (Usage => (others => No_Path));
   begin
      Value.Usage (Ctl_Kind) := All_Paths;
      Associate (Rule_Uses, Entity, Value);
   exception
      when Already_In_Store =>
         Value := Proc_Context (Association (Rule_Uses, Entity));
         Value.Usage (Ctl_Kind) := All_Paths;
         Update (Rule_Uses, Value);
   end Add_Entity;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control exception handlers that do not re-raise an exception ");
      User_Message ("nor call a report procedure");
      User_Message;
      User_Message ("Parameter(s): <control-item> | <report-item>");
      User_Message ("<control-item>: not | with   <exception> | <library unit> | others");
      User_Message ("<report-item>: raise | explicit_raise | reraise |");
      User_Message ("                 return | requeue | <report procedure name>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language, Exception_Status_Queues;
      Entity : Entity_Specification;
   begin
      if Rule_Used (Ctl_Kind) then
         Parameter_Error (Rule_Id,
                          "this rule can be specified only once for each" &
                          " of  check, search, and count");
      end if;
      Ctl_Labels (Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
      Rule_Used  (Ctl_Kind) := True;

      while Parameter_Exists loop
         if Get_Modifier ("NOT") then
            -- uncontrolled exception (or others) or library unit
            Entity := Get_Entity_Parameter;
            if Entity = Others_Entity then
               Others_Uncontrolled (Ctl_Kind) := True;
            else
               Append (Special_Exceptions (Ctl_Kind), (Entity, Controlled => False));
            end if;
         elsif Get_Modifier ("WITH") then
            -- forced controlled exception or library unit
            Append (Special_Exceptions (Ctl_Kind), (Get_Entity_Parameter, Controlled => True));
         else
            Entity := Get_Entity_Parameter;
            if Entity = Raise_Entity then
               Add_Entity (Reraise_Entity,        Ctl_Kind);
               Add_Entity (Explicit_Raise_Entity, Ctl_Kind);
            else
               Add_Entity (Entity, Ctl_Kind);
            end if;
         end if;
      end loop;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Rules_Manager, Exception_Status_Queues;
   begin
      case Action is
         when Clear =>
            Rule_Used   := Not_Used;
            Ctl_Labels  := (others => Null_Unbounded_Wide_String);
            Clear (Rule_Uses);
            for Queue : Exception_Status_Queues.Queue of Special_Exceptions loop
               Clear (Queue);
            end loop;
            Others_Uncontrolled := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Not_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
      Reraise_Context        : constant Root_Context'Class := Association (Rule_Uses, "RERAISE");
      Explicit_Raise_Context : constant Root_Context'Class := Association (Rule_Uses, "EXPLICIT_RAISE");
   begin
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Reraise_Context /= No_Matching_Context then
         for R in Control_Kinds loop
            if Proc_Context (Reraise_Context).Usage (R) = All_Paths then
               Add_Entity (Value ("ADA.EXCEPTIONS.RERAISE_OCCURRENCE"), R);
            end if;
         end loop;
      end if;

      if Explicit_Raise_Context /= No_Matching_Context then
         for R in Control_Kinds loop
            if Proc_Context (Explicit_Raise_Context).Usage (R) = All_Paths then
               Add_Entity (Value ("ADA.EXCEPTIONS.RAISE_EXCEPTION"), R);
            end if;
         end loop;
      end if;

      Balance (Rule_Uses);
   end Prepare;


   ----------------------
   -- Expression_Usage --
   ----------------------

   function Expression_Usage (Element : Asis.Element) return Search_Result;

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Search_Result)
   is
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
   begin
      case Expression_Kind (Element) is
         when A_Function_Call =>
            declare
               Context : constant Root_Context'Class := Matching_Context (Rule_Uses,
                                                                          Called_Simple_Name (Element),
                                                                          Extend_To => All_Extensions);
            begin
               if Context /= No_Matching_Context then
                  State := State and Proc_Context (Context).Usage;
               end if;
            end;

         when An_And_Then_Short_Circuit
           | An_Or_Else_Short_Circuit
           =>
            -- Traverse manually, since right expression is not necessarily executed
            -- But left expression is:
            State := State and Expression_Usage (Short_Circuit_Operation_Left_Expression (Element));
            State := State and (Expression_Usage (Short_Circuit_Operation_Right_Expression (Element))
                                or (others => No_Path));
            Control := Abandon_Children;

         when others =>
            null;
      end case;
   end Pre_Procedure;

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Search_Result)
   is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure;

   procedure Traverse is new Asis.Iterator.Traverse_Element (Search_Result, Pre_Procedure, Post_Procedure);

   -- This function traverses any construct and returns the Search_Result from all expressions
   -- found in the construct.
   function Expression_Usage (Element : Asis.Element) return Search_Result is
      use Asis, Asis.Elements;

      Result : Search_Result := (others => No_Path);
      Control : Asis.Traverse_Control := Continue;
   begin
      if not Is_Nil (Element) then
         Traverse (Element, Control, Result);
      end if;
      return Result;
   end Expression_Usage;

   ----------------------------------
   -- Declarative_Items_List_Usage --
   ----------------------------------

   function Statement_List_Usage (Stmts : Asis.Statement_List) return Search_Result;

   function Declarative_Item_List_Usage (Decls : Asis.Declarative_Item_List) return Search_Result is
      use Asis, Asis.Declarations, Asis.Elements, Utilities;

      Result : Search_Result := (others => No_Path);
   begin
      for Elem : Asis.Element of Decls loop
         case Element_Kind (Elem) is
            when A_Declaration =>
               case Declaration_Kind (Elem) is
                  when A_Task_Type_Declaration
                     | A_Protected_Type_Declaration
                     | A_Single_Task_Declaration
                     | A_Single_Protected_Declaration
                     | A_Procedure_Declaration
                     | A_Null_Procedure_Declaration
                     | A_Function_Declaration
                     | An_Expression_Function_Declaration   -- Ada 2012
                     | A_Procedure_Body_Declaration
                     | A_Function_Body_Declaration
                     | A_Task_Body_Declaration
                     | A_Protected_Body_Declaration
                     | A_Generic_Procedure_Declaration
                     | A_Generic_Function_Declaration
                     | A_Generic_Package_Declaration
                     | A_Formal_Object_Declaration
                     | A_Formal_Type_Declaration
                     | A_Formal_Procedure_Declaration
                     | A_Formal_Function_Declaration
                     | A_Formal_Package_Declaration
                     | A_Formal_Package_Declaration_With_Box
                     =>
                     -- All expressions appearing here are formal
                     -- => ignore
                     null;
                  when A_Package_Body_Declaration =>
                     Result := Result and Declarative_Item_List_Usage (Body_Declarative_Items (Elem));
                     Result := Result and Statement_List_Usage        (Body_Statements        (Elem));
                  when A_Package_Instantiation =>
                     declare
                        Context : constant Root_Context'Class
                          := Matching_Context (Rule_Uses, Generic_Unit_Name (Elem));
                     begin
                        if Context /= No_Matching_Context then
                           Result := Result and Proc_Context(Context).Usage;
                        end if;
                     end;
                     Result := Result and Expression_Usage (Elem);
                  when others =>
                     Result := Result and Expression_Usage (Elem);
               end case;
            when A_Pragma
              | A_Clause
              =>
               null;
            when others =>
               Failure ("Wrong element in declaration list", Elem);
         end case;
      end loop;

      return Result;
   end Declarative_Item_List_Usage;

   --------------------------
   -- Statement_List_Usage --
   --------------------------

   function Statement_List_Usage (Stmts : Asis.Statement_List) return Search_Result is
      use Asis, Asis.Declarations, Asis.Elements,Asis.Statements, Thick_Queries, Utilities;
      Result : Search_Result := (others => No_Path);
   begin
   Statements_Loop:
      for Stmt : Asis.Statement of Stmts loop
         if Label_Names (Stmt) /= Nil_Element_List then
            -- We have a <<label>>
            -- all bets are off
            Result := (others => Some_Paths);
         end if;

         case Statement_Kind (Stmt) is
            when A_Null_Statement
              | A_Goto_Statement
              | A_Code_Statement
              =>
               null;

            when An_Assignment_Statement
              | A_Delay_Relative_Statement
              | A_Delay_Until_Statement
              | A_Terminate_Alternative_Statement
              | An_Abort_Statement
              =>
               Result := Result and Expression_Usage (Stmt);

            when An_Exit_Statement =>
               declare
                  Condition : constant Asis.Expression := Exit_Condition (Stmt);
                  Target    : constant Asis.Statement  := Corresponding_Loop_Exited (Stmt);
               begin
                  Result := Result and Expression_Usage (Condition);

                  for D : Active_Loops_Data of reverse Active_Loops (1 .. Loops_Depth) loop
                     if Is_Equal (Target, D.The_Loop) then
                        D.Exit_Result := D.Exit_Result or Result;
                        exit;
                     end if;
                  end loop;
               end;

            when A_Return_Statement =>
               -- Possible expressions that are part of the return statement must be considered
               Result := Result and Expression_Usage (Stmt);
               declare
                  Context : constant Root_Context'Class := Control_Manager.Association (Rule_Uses, "RETURN");
               begin
                  if Context /= No_Matching_Context then
                     Result := Result and Proc_Context(Context).Usage;
                  end if;
               end;

            when An_Extended_Return_Statement =>
               -- There might be an expression as the initialization of the return object
               Result := Result and Expression_Usage (Initialization_Expression
                                                      (Return_Object_Declaration (Stmt)));
               Result := Result and Statement_List_Usage (Extended_Return_Statements (Stmt));
               declare
                  Context : constant Root_Context'Class := Control_Manager.Association (Rule_Uses, "RETURN");
               begin
                  if Context /= No_Matching_Context then
                     Result := Result and Proc_Context(Context).Usage;
                  end if;
               end;

            when A_Requeue_Statement
              | A_Requeue_Statement_With_Abort
              =>
               -- Possible expressions that are part of the statements must be considered
               Result := Result and Expression_Usage (Stmt);
               declare
                  Context : constant Root_Context'Class := Control_Manager.Association (Rule_Uses, "REQUEUE");
               begin
                  if Context /= No_Matching_Context then
                     Result := Result and Proc_Context(Context).Usage;
                  end if;
               end;

            when A_Procedure_Call_Statement
              | An_Entry_Call_Statement
              =>
               declare
                  Context : constant Root_Context'Class
                    := Matching_Context (Rule_Uses, Called_Simple_Name (Stmt), Extend_To => All_Extensions);
               begin
                  if Context /= No_Matching_Context then
                     Result := Result and Proc_Context(Context).Usage;
                  end if;
               end;
               Result := Result and Expression_Usage (Stmt);

            when A_Raise_Statement =>
               declare
                  Is_Reraise : constant Boolean := Is_Nil (Raised_Exception (Stmt));
                  Context    : constant Root_Context'Class
                    := Control_Manager.Association (Rule_Uses, Choose (Is_Reraise, "RERAISE", "EXPLICIT_RAISE"));
               begin
                  if Context /= No_Matching_Context then
                     Result := Result and Proc_Context(Context).Usage;
                  end if;
               end;

            when An_If_Statement =>
               Result := Result and Expression_Usage (Stmt);
               declare
                  Paths      : constant Asis.Path_List := Statement_Paths (Stmt);
                  If_Usage   : Search_Result := Statement_List_Usage (Sequence_Of_Statements (Paths (1)));
                  Else_Found : Boolean := False;
               begin
                  -- The first condition is always evaluated:
                  Result := Result and Expression_Usage (Condition_Expression (Paths (1)));

                  for Path : Asis.Path of Paths (2 .. Paths'Last) loop
                     case Path_Kind (Path) is
                        when An_Elsif_Path =>
                           If_Usage := If_Usage or (Expression_Usage (Condition_Expression (Path))
                                                    and Statement_List_Usage (Sequence_Of_Statements
                                                                              (Path)));
                        when An_Else_Path =>
                           If_Usage := If_Usage or Statement_List_Usage (Sequence_Of_Statements (Path));
                           Else_Found := True;
                        when others =>
                           -- Including An_If_Path, since it has already been dealt with
                           Failure ("Illegal path", Path);
                     end case;
                  end loop;

                  if not Else_Found then
                     -- No else part, do as if we had else null;
                     If_Usage := If_Usage or (others => No_Path);
                  end if;

                  Result := Result and If_Usage;
               end;

            when  A_Case_Statement
              | A_Selective_Accept_Statement
              | A_Timed_Entry_Call_Statement
              | A_Conditional_Entry_Call_Statement
              | An_Asynchronous_Select_Statement
              =>
               Result := Result and Expression_Usage (Case_Expression (Stmt));
               declare
                  Paths : constant Asis.Path_List := Statement_Paths (Stmt);
                  Temp  : Search_Result := Statement_List_Usage (Sequence_Of_Statements (Paths (1)));
               begin
                  for Path : Asis.Path of Paths (2 .. Paths'Last) loop
                     Temp := Temp or Statement_List_Usage (Sequence_Of_Statements (Path));
                  end loop;
                  Result := Result and Temp;
               end;

            when A_Loop_Statement =>
               if Loops_Depth = Active_Loops'Last then
                  Failure ("Loops nesting deeper than maximum allowed:"
                           & Loops_Level'Wide_Image (Max_Loop_Nesting));
               end if;
               Loops_Depth := Loops_Depth + 1;
               Active_Loops (Loops_Depth) := (Stmt, (others => Neutral));

               Result := Result and Statement_List_Usage (Loop_Statements (Stmt));
               Result := Result or Active_Loops (Loops_Depth).Exit_Result;

               Loops_Depth := Loops_Depth - 1;

            when A_While_Loop_Statement =>
               if Loops_Depth = Active_Loops'Last then
                  Failure ("Loops nesting deeper than maximum allowed:"
                           & Loops_Level'Wide_Image (Max_Loop_Nesting));
               end if;
               Loops_Depth := Loops_Depth + 1;
               Active_Loops (Loops_Depth) := (Stmt, (others => Neutral));

               Result := Result and Expression_Usage (While_Condition (Stmt));
               -- Consider we have a parallel branch which is (others => No_Path) for the case
               -- where the loop is not executed
               Result := Result and
                         (Statement_List_Usage (Loop_Statements (Stmt)) or (others => No_Path));
               Result := Result or Active_Loops (Loops_Depth).Exit_Result;

               Loops_Depth := Loops_Depth - 1;

            when A_For_Loop_Statement =>
               if Loops_Depth = Active_Loops'Last then
                  Failure ("Loops nesting deeper than maximum allowed:"
                           & Loops_Level'Wide_Image (Max_Loop_Nesting));
               end if;
               Loops_Depth := Loops_Depth + 1;
               Active_Loops (Loops_Depth) := (Stmt, (others => Neutral));

               Result := Result and Expression_Usage (For_Loop_Parameter_Specification (Stmt));

               declare
                  Loop_Spec : constant Asis.Declaration := For_Loop_Parameter_Specification (Stmt);
                  Nb_Values : Extended_Biggest_Natural;
               begin
                  case Declaration_Kind (Loop_Spec) is
                     when A_Loop_Parameter_Specification =>
                        Nb_Values := Discrete_Constraining_Lengths (Specification_Subtype_Definition
                                                                    (For_Loop_Parameter_Specification (Stmt))) (1);
                     when An_Element_Iterator_Specification =>
                        -- A for .. of iterator can iterate over all dimensions of a multidimensional array
                        Nb_Values := 1;
                        for L : Extended_Biggest_Natural of Discrete_Constraining_Lengths
                                                             (Iteration_Scheme_Name
                                                               (For_Loop_Parameter_Specification (Stmt)))
                        loop
                           Nb_Values := Nb_Values * L;
                        end loop;
                     when  A_Generalized_Iterator_Specification =>
                        Nb_Values := Not_Static; -- could be pretty much anything...
                     when others =>
                        Failure ("Statement_List_Usage: unknown ""for"" iterator", Loop_Spec);
                  end case;

                  case Nb_Values is
                     when 1 .. Biggest_Int'Last =>   --## Rule line off SIMPLIFIABLE_STATEMENTS ## Biggest_Int'Last
                        -- Always executed
                        Result := Result and Statement_List_Usage (Loop_Statements (Stmt));
                     when 0 =>
                        -- Never executed
                        null;
                     when Not_Static =>
                        -- Consider we have a parallel branch which is (others => No_Path) for the case
                        -- where the loop is not executed
                        Result := Result
                                  and (Statement_List_Usage (Loop_Statements (Stmt)) or (others => No_Path));
                  end case;
                  Result := Result or Active_Loops (Loops_Depth).Exit_Result;
               end;

               Loops_Depth := Loops_Depth - 1;

            when A_Block_Statement =>
               Result := Result and Declarative_Item_List_Usage (Block_Declarative_Items (Stmt));
               Result := Result and Statement_List_Usage (Block_Statements (Stmt));

            when An_Accept_Statement =>
               Result := Result and Expression_Usage (Accept_Entry_Index (Stmt)); -- OK if Nil_Element
               Result := Result and Statement_List_Usage (Accept_Body_Statements (Stmt));

            when Not_A_Statement =>
               Failure ("Not a statement in statements list");
         end case;

         if Result = (Result'Range => All_Paths) then
            -- No need to consider further
            return Result;
         end if;
      end loop Statements_Loop;

      return Result;
   end Statement_List_Usage;

   -------------------------------
   -- Process_Exception_Handler --
   -------------------------------

   procedure Process_Exception_Handler (Handler : in Asis.Exception_Handler) is
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Statements;
      use Framework.Locations, Framework.Reports, Exception_Status_Queues, Utilities;

      Paths_Usage : Search_Result;
      Current     : Cursor;

      All_Uncontrolled : Usage := (Control_Kinds => True);
      procedure Eval_All_Uncontrolled is
         -- Check if this handler handles *only* uncontrolled exceptions for each Control_Kind
         use Asis.Declarations, Asis.Elements;
         use Thick_Queries;

         Handled    : constant Asis.Element_List := Exception_Choices (Handler);
         Status     : Exception_Status;
         Controlled : Boolean;
      begin
         for Ck in Control_Kinds loop
            if Rule_Used (Ck) then
               if Element_Kind (Handled (Handled'First)) = A_Definition then
                  -- when others
                  All_Uncontrolled (Ck) := Others_Uncontrolled (Ck);
               else
                  On_Exception_Names :
                  for H : Asis.Element of Handled loop

                     Current := First (Special_Exceptions (Ck));
                     if Has_Element (Current) then
                        Controlled := not Fetch (Current).Controlled;
                        -- i.e.: If the first element is a "with", default is not controlled
                        --       if the first element is a "not", default is controlled
                     else
                        Controlled := True;
                     end if;
                     while Has_Element (Current) loop
                        Status := Fetch (Current);
                        if Matches (Status.Entity, H, Extend_To => All_Extensions)
                          or else Matches (Status.Entity,
                                           Names (Unit_Declaration (Definition_Compilation_Unit
                                                                    (Ultimate_Name (H)))) (1),
                                           Extend_To => All_Extensions)
                        then
                           Controlled := Status.Controlled;
                        end if;
                        Current := Next (Current);
                     end loop;

                     if Controlled then
                        All_Uncontrolled (Ck) := False;
                        exit On_Exception_Names;
                     end if;
                  end loop On_Exception_Names;
               end if;
            end if;
         end loop;
      end Eval_All_Uncontrolled;

   begin  -- Process_Exception_Handler
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Eval_All_Uncontrolled;
      if All_Uncontrolled = (Control_Kinds => True) then
         return;
      end if;

      Loops_Depth := 0;
      Paths_Usage := Statement_List_Usage (Handler_Statements (Handler));

      -- Note: since Check < Search, if both messages apply, only Check
      --       will be output
      for Ck in Control_Kinds range Check .. Search loop
         if Rule_Used (Ck) and not All_Uncontrolled (Ck) then
            case Paths_Usage (Ck) is
               when Neutral =>
                  Failure ("Wrong path evaluation");

               when No_Path =>
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Labels (Ck)),
                          Ck,
                          Get_Location (Handler),
                          "all paths are silent in exception handler");
                  exit;
               when Some_Paths =>
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Labels (Ck)),
                          Ck,
                          Get_Location (Handler),
                          "some paths are silent in exception handler, check manually");
                  exit;
               when All_Paths =>
                  null;
            end case;
        end if;
      end loop;

      -- Always report count
      if Rule_Used (Count) and not All_Uncontrolled (Count) then
            case Paths_Usage (Count) is
               when Neutral =>
                  Failure ("Wrong path evaluation");

               when No_Path | Some_Paths=>
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Labels (Count)),
                          Count,
                          Get_Location (Handler),
                          ""); -- Message ignored for Count

               when All_Paths =>
                  null;
            end case;
      end if;
   end Process_Exception_Handler;

begin  -- Rules.Silent_Exceptions
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Silent_Exceptions;
