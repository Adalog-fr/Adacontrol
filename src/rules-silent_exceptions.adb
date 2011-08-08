----------------------------------------------------------------------
--  Rules.Silent_Exceptions - Package body                          --
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
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Silent_Exceptions is
   use Framework;

   type Usage is array (Control_Kinds) of Boolean;
   Rule_Used  : Usage := (others => False);
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

   procedure Add_Entity (Entity : Entity_Specification; Use_Rule_Type : Control_Kinds) is
      Value  : Proc_Context := (Usage => (others => No_Path));
   begin
      Value.Usage (Use_Rule_Type) := All_Paths;
      Associate (Rule_Uses, Entity, Value);
   exception
      when Already_In_Store =>
         Value := Proc_Context (Association (Rule_Uses, Entity));
         Value.Usage (Use_Rule_Type) := All_Paths;
         Update (Rule_Uses, Value);
   end Add_Entity;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): <report procedure name> | raise | return | requeue");
      User_Message ("Control exception handlers that do not re-raise an exception ");
      User_Message ("nor call a report procedure");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Use_Rule_Type : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if Rule_Used (Use_Rule_Type) then
         Parameter_Error (Rule_Id,
                          "this rule can be specified only once for each" &
                          " of  check, search, and count");
      end if;
      Ctl_Labels (Use_Rule_Type) := To_Unbounded_Wide_String (Ctl_Label);
      Rule_Used  (Use_Rule_Type) := True;

      while Parameter_Exists loop
         Add_Entity (Get_Entity_Parameter, Use_Rule_Type);
      end loop;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used   := (others => False);
            Ctl_Labels  := (others => Null_Unbounded_Wide_String);
            Clear (Rule_Uses);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
      Raise_Context : constant Root_Context'Class := Association (Rule_Uses, Value ("RAISE"));
   begin
      if Raise_Context /= No_Matching_Context then
         for R in Control_Kinds loop
            if Proc_Context (Raise_Context).Usage (R) = All_Paths then
               Add_Entity (Value ("ADA.EXCEPTIONS.RAISE_EXCEPTION"), R);
               Add_Entity (Value ("ADA.EXCEPTIONS.RERAISE_OCCURRENCE"), R);
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
                            State   : in out Search_Result) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
   begin
      case Expression_Kind (Element) is
         when A_Function_Call =>
            declare
               Context : constant Root_Context'Class := Extended_Matching_Context (Rule_Uses,
                                                                                   Called_Simple_Name (Element));
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
                             State   : in out Search_Result) is
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
      for I in Decls'Range loop
         case Element_Kind (Decls (I)) is
            when A_Declaration =>
               case Declaration_Kind (Decls (I)) is
                  when A_Task_Type_Declaration
                    | A_Protected_Type_Declaration
                    | A_Single_Task_Declaration
                    | A_Single_Protected_Declaration
                    | A_Procedure_Declaration
                    | A_Function_Declaration
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
                     Result := Result and Declarative_Item_List_Usage (Body_Declarative_Items (Decls (I)));
                     Result := Result and Statement_List_Usage        (Body_Statements        (Decls (I)));
                  when A_Package_Instantiation =>
                     declare
                        Context : constant Root_Context'Class
                          := Matching_Context (Rule_Uses, Generic_Unit_Name (Decls (I)));
                     begin
                        if Context /= No_Matching_Context then
                           Result := Result and Proc_Context(Context).Usage;
                        end if;
                     end;
                     Result := Result and Expression_Usage (Decls (I));
                  when others =>
                     Result := Result and Expression_Usage (Decls (I));
               end case;
            when A_Pragma
              | A_Clause
              =>
               null;
            when others =>
               Failure ("Wrong element in declaration list", Decls (I));
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
      for I in Stmts'Range loop
         if Label_Names (Stmts (I)) /= Nil_Element_List then
            -- We have a <<label>>
            -- all bets are off
            Result := (others => Some_Paths);
         end if;

         case Statement_Kind (Stmts (I)) is
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
               Result := Result and Expression_Usage (Stmts (I));

            when An_Exit_Statement =>
               declare
                  Condition : constant Asis.Expression := Exit_Condition (Stmts (I));
                  Target    : constant Asis.Statement  := Corresponding_Loop_Exited (Stmts (I));
               begin
                  Result := Result and Expression_Usage (Condition);

                  for L in reverse Loops_Level range 1 .. Loops_Depth loop
                     if Is_Equal (Target, Active_Loops (L).The_Loop) then
                        Active_Loops (L).Exit_Result := Active_Loops (L).Exit_Result or Result;
                        exit;
                     end if;
                  end loop;
               end;

            when A_Return_Statement =>
               -- Possible expressions that are part of the return statement must be considered
               Result := Result and Expression_Usage (Stmts (I));
               declare
                  Context : constant Root_Context'Class := Framework.Association (Rule_Uses, Value ("RETURN"));
               begin
                  if Context /= No_Matching_Context then
                     Result := Result and Proc_Context(Context).Usage;
                  end if;
               end;

            when A_Requeue_Statement
              | A_Requeue_Statement_With_Abort
              =>
               -- Possible expressions that are part of the statements must be considered
               Result := Result and Expression_Usage (Stmts (I));
               declare
                  Context : constant Root_Context'Class := Framework.Association (Rule_Uses, Value ("REQUEUE"));
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
                    := Extended_Matching_Context (Rule_Uses, Called_Simple_Name (Stmts (I)));
               begin
                  if Context /= No_Matching_Context then
                     Result := Result and Proc_Context(Context).Usage;
                  end if;
               end;
               Result := Result and Expression_Usage (Stmts (I));

            when A_Raise_Statement =>
               declare
                  Context : constant Root_Context'Class := Framework.Association (Rule_Uses, Value ("RAISE"));
               begin
                  if Context /= No_Matching_Context then
                     Result := Result and Proc_Context(Context).Usage;
                  end if;
               end;

            when An_If_Statement =>
               Result := Result and Expression_Usage (Stmts (I));
               declare
                  Paths      : constant Asis.Path_List := Statement_Paths (Stmts (I));
                  If_Usage   : Search_Result := Statement_List_Usage (Sequence_Of_Statements (Paths (1)));
                  Else_Found : Boolean := False;
               begin
                  -- The first condition is always evaluated:
                  Result := Result and Expression_Usage (Condition_Expression (Paths (1)));

                  for J in Positive range 2 .. Paths'Last loop
                     case Path_Kind (Paths (J)) is
                        when An_Elsif_Path =>
                           If_Usage := If_Usage or (Expression_Usage (Condition_Expression (Paths (J)))
                                                    and Statement_List_Usage (Sequence_Of_Statements
                                                                              (Paths (J))));
                        when An_Else_Path =>
                           If_Usage := If_Usage or Statement_List_Usage (Sequence_Of_Statements (Paths (J)));
                           Else_Found := True;
                        when others =>
                           -- Including An_If_Path, since it has already been dealt with
                           Failure ("Illegal path", Paths (J));
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
               Result := Result and Expression_Usage (Case_Expression (Stmts (I)));
               declare
                  Paths : constant Asis.Path_List := Statement_Paths (Stmts (I));
                  Temp  : Search_Result := Statement_List_Usage (Sequence_Of_Statements (Paths (1)));
               begin
                  for P in Positive range 2 .. Paths'Last loop
                     Temp := Temp or Statement_List_Usage (Sequence_Of_Statements (Paths (P)));
                  end loop;
                  Result := Result and Temp;
               end;

            when A_Loop_Statement =>
               if Loops_Depth = Active_Loops'Last then
                  Failure ("Loops nesting deeper than maximum allowed:"
                           & Loops_Level'Wide_Image (Max_Loop_Nesting));
               end if;
               Loops_Depth := Loops_Depth + 1;
               Active_Loops (Loops_Depth) := (Stmts (I), (others => Neutral));

               Result := Result and Statement_List_Usage (Loop_Statements (Stmts (I)));
               Result := Result or Active_Loops (Loops_Depth).Exit_Result;

               Loops_Depth := Loops_Depth - 1;

            when A_While_Loop_Statement =>
               if Loops_Depth = Active_Loops'Last then
                  Failure ("Loops nesting deeper than maximum allowed:"
                           & Loops_Level'Wide_Image (Max_Loop_Nesting));
               end if;
               Loops_Depth := Loops_Depth + 1;
               Active_Loops (Loops_Depth) := (Stmts (I), (others => Neutral));

               Result := Result and Expression_Usage (While_Condition (Stmts (I)));
               -- Consider we have a parallel branch which is (others => No_Path) for the case
               -- where the loop is not executed
               Result := Result and
                         (Statement_List_Usage (Loop_Statements (Stmts (I))) or (others => No_Path));
               Result := Result or Active_Loops (Loops_Depth).Exit_Result;

               Loops_Depth := Loops_Depth - 1;

            when A_For_Loop_Statement =>
               if Loops_Depth = Active_Loops'Last then
                  Failure ("Loops nesting deeper than maximum allowed:"
                           & Loops_Level'Wide_Image (Max_Loop_Nesting));
               end if;
               Loops_Depth := Loops_Depth + 1;
               Active_Loops (Loops_Depth) := (Stmts (I), (others => Neutral));

               Result := Result and Expression_Usage (For_Loop_Parameter_Specification (Stmts (I)));
               case Discrete_Constraining_Lengths (Specification_Subtype_Definition
                                                   (For_Loop_Parameter_Specification (Stmts (I))))(1)
               is
                  when 1 .. Biggest_Int'Last =>
                     -- Always executed
                     Result := Result and Statement_List_Usage (Loop_Statements (Stmts (I)));
                  when 0 =>
                     -- Never executed
                     null;
                  when Not_Static =>
                     -- Consider we have a parallel branch which is (others => No_Path) for the case
                     -- where the loop is not executed
                     Result := Result
                               and (Statement_List_Usage (Loop_Statements (Stmts (I))) or (others => No_Path));
               end case;
               Result := Result or Active_Loops (Loops_Depth).Exit_Result;

               Loops_Depth := Loops_Depth - 1;

            when A_Block_Statement =>
               Result := Result and Declarative_Item_List_Usage (Block_Declarative_Items (Stmts (I)));
               Result := Result and Statement_List_Usage (Block_Statements (Stmts (I)));

            when An_Accept_Statement =>
               Result := Result and Expression_Usage (Accept_Entry_Index (Stmts (I))); -- OK if Nil_Element
               Result := Result and Statement_List_Usage (Accept_Body_Statements (Stmts (I)));

            when Not_A_Statement =>
               Failure ("Not a statement in statements list");

            when others =>  -- Compatibility Ada 2005
               -- Better be careful...
               Result := (others => Some_Paths);
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
      use Asis.Statements;
      use Framework.Reports, Utilities;
      use Ada.Strings.Wide_Unbounded;

      Paths_Usage : Search_Result;
   begin
      if Rule_Used = (Rule_Used'Range => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Loops_Depth := 0;
      Paths_Usage := Statement_List_Usage (Handler_Statements (Handler));

      -- Note: since Check < Search, if both messages apply, only Check
      --       will be output
      for I in Control_Kinds range Check .. Search loop
         if Rule_Used (I) then
            case Paths_Usage (I) is
               when Neutral =>
                  Failure ("Wrong path evaluation");

               when No_Path =>
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Labels (I)),
                          I,
                          Get_Location (Handler),
                          "all paths are silent in exception handler");
                  exit;
               when Some_Paths =>
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Labels (I)),
                          I,
                          Get_Location (Handler),
                          "some paths are silent in exception handler, check manually");
                  exit;
               when All_Paths =>
                  null;
            end case;
        end if;
      end loop;

      -- Always report count
      if Rule_Used (Count) then
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

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Silent_Exceptions;
