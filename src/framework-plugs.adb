----------------------------------------------------------------------
--  Framework.Plugs - Package body                                  --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2014.           --
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
  Asis.Elements;

-- All general rules
with
  Rules.Abnormal_Function_Return,
  Rules.Actual_Parameters,
  Rules.Allocators,
  Rules.Array_Declarations,
  Rules.Aspects,
  Rules.Assignments,
  Rules.Barrier_Expressions,
  Rules.Case_Statement,
  Rules.Characters,
  Rules.Comments,
  Rules.Declarations,
  Rules.Dependencies,
  Rules.Derivations,
  Rules.Directly_Accessed_Globals,
  Rules.Duplicate_Initialization_Calls,
  Rules.Entities,
  Rules.Entity_Inside_Exception,
  Rules.Exception_Propagation,
  Rules.Expressions,
  Rules.Generic_Aliasing,
  Rules.Global_References,
  Rules.Header_Comments,
  Rules.Improper_Initialization,
  Rules.Instantiations,
  Rules.Insufficient_Parameters,
  Rules.Known_Exceptions,
  Rules.Local_Access,
  Rules.Local_Hiding,
  Rules.Max_Blank_Lines,
  Rules.Max_Call_Depth,
  Rules.Max_Expression_Items,
  Rules.Max_Line_Length,
  Rules.Max_Nesting,
  Rules.Max_Primitives,
  Rules.Max_Size,
  Rules.Max_Statement_Nesting,
  Rules.Movable_Accept_Statements,
  Rules.Naming_Convention,
  Rules.No_Operator_Usage,
  Rules.Non_Static,
  Rules.Not_Elaboration_Calls,
  Rules.Not_Selected_Name,
  Rules.Object_Declarations,
  Rules.Parameter_Aliasing,
  Rules.Parameter_Declarations,
  Rules.Positional_Associations,
  Rules.Potentially_Blocking_Operations,
  Rules.Pragmas,
  Rules.Record_Declarations,
  Rules.Reduceable_Scope,
  Rules.Renaming_Declarations,
  Rules.Representation_Clauses,
  Rules.Return_Statements,
  Rules.Return_Type,
  Rules.Side_Effect_Parameters,
  Rules.Silent_Exceptions,
  Rules.Simplifiable_Expressions,
  Rules.Simplifiable_Statements,
  Rules.Statements,
  Rules.Style,
  Rules.Terminating_Tasks,
  Rules.Type_Initial_Values,
  Rules.Type_Usage,
  Rules.Unit_Pattern,
  Rules.Units,
  Rules.Unnecessary_Use_Clause,
  Rules.Unsafe_Elaboration,
  Rules.Unsafe_Paired_Calls,
  Rules.Unsafe_Unchecked_Conversion,
  Rules.Usage,
  Rules.Use_Clauses,
  Rules.With_Clauses;
package body Framework.Plugs is

   ----------------
   -- Enter_Unit --
   ----------------

   -- This procedure is called before any processing of a library unit.
   -- Plug calls here for rules that need initialization for every unit.

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
   begin
      Rules.Max_Blank_Lines.    Enter_Unit               (Unit);
      Rules.Dependencies.       Enter_Unit               (Unit);
      Rules.Declarations.       Process_Unit             (Unit);
      Rules.Statements.         Enter_Unit               (Unit);
      Rules.Unit_Pattern.       Process_Compilation_Unit (Unit);
      Rules.Units.              Process_Unit             (Unit);
      Rules.Unsafe_Elaboration. Process_Unit             (Unit);
   end Enter_Unit;


   --------------------------
   -- Exit_Context_Clauses --
   --------------------------

   -- This procedure is called after all context clauses have been traversed,
   -- before traversing the unit itself

   procedure Exit_Context_Clauses (Unit : in Asis.Compilation_Unit) is
   begin
      Rules.Dependencies.Exit_Context_Clauses (Unit);
   end Exit_Context_Clauses;

   ---------------
   -- Exit_Unit --
   ---------------

   -- This procedure is called after all processing of a library unit.
   -- Plug calls here for rules that need finalization for every unit.

   procedure Exit_Unit (Unit : in Asis.Compilation_Unit) is
   begin
      Rules.Max_Size.     Process_Unit      (Unit);
      Rules.With_Clauses. Process_Unit_Exit (Unit);
   end Exit_Unit;

   -----------------
   -- Enter_Scope --
   -----------------

   -- This procedure is called whenever we enter (Pre_Procedure) into a
   -- construct where elements can be declared. Since this happen in various
   -- places in the tree and is likely to be quite common use, it is easier to
   -- plug calls here rather than in every place that might require it.

   procedure Enter_Scope (Element : in Asis.Element) is
   begin
      Rules.Max_Nesting.Process_Scope_Enter (Element);
      Rules.Statements. Process_Scope_Enter (Element);
   end Enter_Scope;

   ------------------------
   -- Enter_Private_Part --
   ------------------------

   procedure Enter_Private_Part   (Element : in Asis.Element) is
      pragma Unreferenced (Element);
   begin
      Rules.Max_Primitives. Process_Private_Part;
   end Enter_Private_Part;

   ----------------
   -- Exit_Scope --
   ----------------

   -- This procedure is called whenever we exit (Post_Procedure) from a
   -- construct where elements can be declared. Since this happen in various
   -- places in the tree and is likely to be quite common use, it is easier to
   -- plug calls here rather than in every place that might require it.

   procedure Exit_Scope (Element : in Asis.Element) is
   begin
      Rules.Max_Nesting.            Process_Scope_Exit (Element);
      Rules.No_Operator_Usage.      Process_Scope_Exit (Element);
      Rules.Object_Declarations.    Process_Scope_Exit;
      Rules.Reduceable_Scope.       Process_Scope_Exit (Element);
      Rules.Statements.             Process_Scope_Exit (Element);
      Rules.Type_Initial_Values.    Process_Scope_Exit (Element);
      Rules.Unnecessary_Use_Clause. Process_Scope_Exit (Element);
      Rules.Max_Primitives.         Process_Scope_Exit (Element);
   end Exit_Scope;

   --------------------------
   -- Enter_Statement_List --
   --------------------------

   -- Plug calls here to rules that need to process a Sequence_of_statements.
   -- Element is one of the elements to which Thick_Queries.Statements applies
   -- (i.e. a statement container)
   procedure Enter_Statement_List (Element : in Asis.Element) is
   begin
      Rules.Assignments. Process_Statement_Container (Element);
   end Enter_Statement_List;

   ---------------------
   -- True_Identifier --
   ---------------------

   --  Plug calls here to rules that need to process all occurrences
   --  of "true" identifiers, including operator symbols and
   --  enumeration literals, but excluding identifiers that are pragma
   --  names or attributes selectors

   procedure True_Identifier (Element : in Asis.Expression) is
   begin
      Rules.Directly_Accessed_Globals. Process_Identifier (Element);
      Rules.Entities.                  Process_Identifier (Element);
      Rules.Not_Selected_Name.         Process_Identifier (Element);
      Rules.Object_Declarations.       Process_Identifier (Element);
      Rules.Reduceable_Scope.          Process_Identifier (Element);
      Rules.Unnecessary_Use_Clause.    Process_Identifier (Element);
      Rules.Style.                     Process_Identifier (Element);
      Rules.Usage.                     Process_Identifier (Element);
      Rules.With_Clauses.              Process_Identifier (Element);
   end True_Identifier;

   -------------------
   -- Pre_Procedure --
   -------------------

   procedure Pre_Procedure (Element : in Asis.Element) is
      use Asis, Asis.Elements;
   begin
      case Element_Kind (Element) is
         when A_Clause =>
            Rules.Style. Process_Element (Element);

            case Clause_Kind (Element) is
               when A_Use_Package_Clause =>
                  Rules.Reduceable_Scope. Process_Use_Clause (Element);
                  Rules.Use_Clauses.      Process_Use_Clause (Element);
               when A_Use_Type_Clause =>
                  Rules.Reduceable_Scope. Process_Use_Clause (Element);
                  Rules.Use_Clauses.      Process_Use_Clause (Element);
               when A_Use_All_Type_Clause =>
                  Rules.Reduceable_Scope. Process_Use_Clause (Element);
                  Rules.Use_Clauses.      Process_Use_Clause (Element);
               when A_Representation_Clause =>
                  Rules.Object_Declarations.    Process_Representation_Clause (Element);
                  Rules.Representation_Clauses. Process_Clause                (Element);
               when A_With_Clause =>
                  Rules.Dependencies.       Process_With_Clause (Element);
                  Rules.Local_Hiding.       Process_With_Clause (Element);
                  Rules.Units.              Process_With_Clause (Element);
                  Rules.With_Clauses.       Process_With_Clause (Element);
               when others =>
                  null;
            end case;

         when A_Defining_Name =>
            case Defining_Name_Kind (Element) is
               when A_Defining_Identifier =>
                  Rules.Local_Hiding.      Process_Defining_Name (Element);
                  Rules.Naming_Convention. Process_Defining_Name (Element);
                  Rules.Reduceable_Scope.  Process_Defining_Name (Element);
                  Rules.Style.             Process_Identifier    (Element);

               when A_Defining_Enumeration_Literal =>
                  Rules.Local_Hiding.      Process_Defining_Name (Element);
                  Rules.Naming_Convention. Process_Defining_Name (Element);
                  Rules.Style.             Process_Identifier    (Element);

               when A_Defining_Operator_Symbol =>
                  Rules.Local_Hiding.      Process_Defining_Name (Element);
                  Rules.Reduceable_Scope.  Process_Defining_Name (Element);
                  Rules.Style.             Process_Identifier    (Element);

               when others =>
                  null;
            end case;

         when A_Declaration =>
            Rules.Declarations.          Process_Declaration (Element);
            Rules.Exception_Propagation. Process_Declaration (Element);
            Rules.Style.                 Process_Element     (Element);

            case Declaration_Kind (Element) is

               when A_Parameter_Specification =>
                  Rules.Object_Declarations. Process_Declaration (Element);
                  Rules.Usage.               Process_Declaration (Element);

               when An_Ordinary_Type_Declaration =>
                  Rules.Unit_Pattern.        Process_Type_Declaration (Element);
                  Rules.Type_Initial_Values. Process_Type_Declaration (Element);
                  Rules.Usage.               Process_Declaration      (Element);
                  Rules.Max_Primitives.      Process_Type_Declaration (Element);

               when A_Private_Type_Declaration
                  | A_Private_Extension_Declaration
                  =>
                  Rules.Type_Initial_Values. Process_Type_Declaration (Element);
                  Rules.Max_Primitives.      Process_Type_Declaration      (Element);

               when A_Subtype_Declaration =>
                  Rules.Usage. Process_Declaration (Element);

               when A_Variable_Declaration =>
                  Rules.Array_Declarations.        Process_Object_Declaration   (Element);
                  Rules.Assignments.               Process_Object_Declaration   (Element);
                  Rules.Directly_Accessed_Globals. Process_Variable_Declaration (Element);
                  Rules.Known_Exceptions.          Process_Object_Declaration   (Element);
                  Rules.Non_Static.                Process_Object_Declaration   (Element);
                  Rules.Object_Declarations.       Process_Declaration          (Element);
                  Rules.Usage.                     Process_Declaration          (Element);

               when A_Constant_Declaration =>
                  Rules.Array_Declarations.  Process_Object_Declaration   (Element);
                  Rules.Assignments.         Process_Object_Declaration   (Element);
                  Rules.Known_Exceptions.    Process_Object_Declaration   (Element);
                  Rules.Type_Initial_Values. Process_Constant_Declaration (Element);
                  Rules.Non_Static.          Process_Object_Declaration   (Element);
                  Rules.Object_Declarations. Process_Declaration          (Element);
                  Rules.Usage.               Process_Declaration          (Element);

               when A_Deferred_Constant_Declaration
                 | A_Number_Declaration
                 =>
                  Rules.Usage. Process_Declaration (Element);

               when A_Loop_Parameter_Specification | An_Element_Iterator_Specification =>
                  Rules.Object_Declarations. Process_Declaration (Element);

               when A_Function_Body_Declaration =>
                  Rules.Abnormal_Function_Return. Process_Function_Body        (Element);
                  Rules.Comments.                 Process_Program_Unit         (Element);
                  Rules.Derivations.              Process_Callable             (Element);
                  Rules.Exception_Propagation.    Process_SP_Declaration       (Element);
                  Rules.Global_References.        Process_Body                 (Element);
                  Rules.Improper_Initialization.  Process_Structure            (Element);
                  Rules.Max_Size.                 Process_Element              (Element);
                  Rules.Parameter_Declarations.   Process_Declaration          (Element);
                  Rules.Return_Statements.        Initialize_Counter           (Element);
                  Rules.Return_Type.              Process_Function_Declaration (Element);
                  Rules.Statements.               Process_Function_Body        (Element);
                  Rules.Style.                    Process_Construct            (Element);
                  Rules.Style.                    Process_Declaration          (Element);
                  Rules.Unit_Pattern.             Process_Program_Unit         (Element);
                  Rules.Usage.                    Process_Declaration          (Element);

               when A_Procedure_Body_Declaration =>
                  Rules.Comments.                Process_Program_Unit   (Element);
                  Rules.Derivations.             Process_Callable       (Element);
                  Rules.Exception_Propagation.   Process_SP_Declaration (Element);
                  Rules.Global_References.       Process_Body           (Element);
                  Rules.Improper_Initialization. Process_Structure      (Element);
                  Rules.Max_Size.                Process_Element        (Element);
                  Rules.Parameter_Declarations.  Process_Declaration    (Element);
                  Rules.Return_Statements.       Initialize_Counter     (Element);
                  Rules.Style.                   Process_Construct      (Element);
                  Rules.Style.                   Process_Declaration    (Element);
                  Rules.Unit_Pattern.            Process_Program_Unit   (Element);
                  Rules.Usage.                   Process_Declaration    (Element);

               when A_Null_Procedure_Declaration =>
                  Rules.Comments.                Process_Program_Unit           (Element);
                  Rules.Derivations.             Process_Callable               (Element);
                  Rules.Exception_Propagation.   Process_SP_Declaration         (Element);
                  Rules.Global_References.       Process_Body                   (Element);
                  Rules.Improper_Initialization. Process_Structure              (Element);
                  Rules.Max_Size.                Process_Element                (Element);
                  Rules.Parameter_Declarations.  Process_Declaration            (Element);
                  Rules.Style.                   Process_Construct              (Element);
                  Rules.Style.                   Process_Declaration            (Element);
                  Rules.Unit_Pattern.            Process_Program_Unit           (Element);
                  Rules.Usage.                   Process_Declaration            (Element);
                  Rules.Max_Primitives.          Process_Subprogram_Declaration (Element);

               when An_Entry_Body_Declaration =>
                  Rules.Barrier_Expressions.     Process_Entry_Declaration (Element);
                  Rules.Comments.                Process_Program_Unit      (Element);
                  Rules.Global_References.       Process_Body              (Element);
                  Rules.Improper_Initialization. Process_Structure         (Element);
                  Rules.Max_Size.                Process_Element           (Element);
                  Rules.Return_Statements.       Initialize_Counter        (Element);
                  Rules.Style.                   Process_Construct         (Element);
                  Rules.Style.                   Process_Declaration       (Element);
                  Rules.Unit_Pattern.            Process_Program_Unit      (Element);

               when A_Package_Declaration =>
                  Rules.Max_Size.     Process_Element      (Element);
                  Rules.Style.        Process_Construct    (Element);
                  Rules.Unit_Pattern. Process_Program_Unit (Element);
                  Rules.Usage.        Process_Declaration  (Element);

               when A_Task_Type_Declaration
                 | A_Protected_Type_Declaration
                  =>
                  Rules.Derivations.         Process_Synchronized     (Element);
                  Rules.Max_Size.            Process_Element          (Element);
                  Rules.Style.               Process_Construct        (Element);
                  Rules.Usage.               Process_Declaration      (Element);
                  Rules.Max_Primitives.      Process_Type_Declaration (Element);

               when A_Package_Body_Declaration =>
                  Rules.Comments.                Process_Program_Unit (Element);
                  Rules.Improper_Initialization. Process_Structure    (Element);
                  Rules.Max_Size.                Process_Element      (Element);
                  Rules.Unit_Pattern.            Process_Program_Unit (Element);
                  Rules.Style.                   Process_Construct    (Element);

               when A_Generic_Package_Declaration =>
                  Rules.Max_Size.       Process_Element      (Element);
                  Rules.Style.          Process_Construct    (Element);
                  Rules.Style.          Process_Declaration  (Element);
                  Rules.Unit_Pattern.   Process_Program_Unit (Element);
                  Rules.Usage.          Process_Declaration  (Element);

               when A_Protected_Body_Declaration =>
                  Rules.Max_Size.                        Process_Element        (Element);
                  Rules.Style.                           Process_Construct      (Element);
                  Rules.Potentially_Blocking_Operations. Process_Protected_Body (Element);

               when A_Single_Task_Declaration
                 | A_Single_Protected_Declaration
                 =>
                  Rules.Derivations. Process_Synchronized (Element);
                  Rules.Max_Size.    Process_Element      (Element);
                  Rules.Style.       Process_Construct    (Element);
                  Rules.Usage.       Process_Declaration  (Element);

               when A_Task_Body_Declaration
                 =>
                  Rules.Comments.                Process_Program_Unit (Element);
                  Rules.Exception_Propagation.   Process_Task_Body    (Element);
                  Rules.Global_References.       Process_Body         (Element);
                  Rules.Improper_Initialization. Process_Structure    (Element);
                  Rules.Max_Size.                Process_Element      (Element);
                  Rules.Style.                   Process_Construct    (Element);
                  Rules.Terminating_Tasks.       Process_Task_Body    (Element);

               when An_Exception_Declaration =>
                  Rules.Usage. Process_Declaration (Element);

               when A_Package_Instantiation =>
                  Rules.Actual_Parameters.      Process_Call_Or_Instantiation (Element);
                  Rules.Derivations.            Process_Instantiation         (Element);
                  Rules.Exception_Propagation.  Process_Instantiation         (Element);
                  Rules.Generic_Aliasing.       Process_Instantiation         (Element);
                  Rules.Instantiations.         Process_Instantiation         (Element);
                  Rules.Non_Static.             Process_Instantiation         (Element);
                  Rules.Reduceable_Scope.       Process_Instantiation         (Element);
                  Rules.Return_Type.            Process_Instantiation         (Element);
                  Rules.Side_Effect_Parameters. Process_Call_Or_Instantiation (Element);
                  Rules.Unnecessary_Use_Clause. Process_Instantiation         (Element);
                  Rules.Usage.                  Process_Instantiation         (Element);
                  Rules.With_Clauses.           Process_Instantiation         (Element);
                  Rules.Max_Primitives.         Process_Instantiation         (Element);

               when A_Formal_Package_Declaration =>
                  Rules.Actual_Parameters.      Process_Call_Or_Instantiation (Element);
                  Rules.Side_Effect_Parameters. Process_Call_Or_Instantiation (Element);
                  Rules.Unnecessary_Use_Clause. Process_Instantiation         (Element);
                  Rules.Usage.                  Process_Instantiation (Element);

               when A_Formal_Package_Declaration_With_Box =>
                  Rules.Usage.                  Process_Instantiation (Element);

               when A_Procedure_Instantiation =>
                  Rules.Actual_Parameters.      Process_Call_Or_Instantiation (Element);
                  Rules.Derivations.            Process_Instantiation         (Element);
                  Rules.Exception_Propagation.  Process_Instantiation         (Element);
                  Rules.Generic_Aliasing.       Process_Instantiation         (Element);
                  Rules.Instantiations.         Process_Instantiation         (Element);
                  Rules.Max_Primitives.         Process_Instantiation         (Element);
                  Rules.Non_Static.             Process_Instantiation         (Element);
                  Rules.Parameter_Declarations. Process_Declaration           (Element);
                  Rules.Reduceable_Scope.       Process_Instantiation         (Element);
                  Rules.Return_Type.            Process_Instantiation         (Element);
                  Rules.Side_Effect_Parameters. Process_Call_Or_Instantiation (Element);
                  Rules.Unnecessary_Use_Clause. Process_Instantiation         (Element);
                  Rules.Usage.                  Process_Instantiation         (Element);
                  Rules.With_Clauses.           Process_Instantiation         (Element);

               when A_Function_Instantiation =>
                  Rules.Actual_Parameters.           Process_Call_Or_Instantiation (Element);
                  Rules.Derivations.                 Process_Instantiation         (Element);
                  Rules.Exception_Propagation.       Process_Instantiation         (Element);
                  Rules.Generic_Aliasing.            Process_Instantiation         (Element);
                  Rules.Instantiations.              Process_Instantiation         (Element);
                  Rules.Parameter_Declarations.      Process_Declaration           (Element);
                  Rules.Non_Static.                  Process_Instantiation         (Element);
                  Rules.Reduceable_Scope.            Process_Instantiation         (Element);
                  Rules.Return_Type.                 Process_Instantiation         (Element);
                  Rules.Side_Effect_Parameters.      Process_Call_Or_Instantiation (Element);
                  Rules.Unnecessary_Use_Clause.      Process_Instantiation         (Element);
                  Rules.Unsafe_Unchecked_Conversion. Process_Instantiation         (Element);
                  Rules.Usage.                       Process_Instantiation         (Element);
                  Rules.With_Clauses.                Process_Instantiation         (Element);
                  Rules.Max_Primitives.              Process_Instantiation         (Element);

               when A_Procedure_Declaration
                  | A_Procedure_Body_Stub
                    =>
                  Rules.Derivations.            Process_Callable               (Element);
                  Rules.Parameter_Declarations. Process_Declaration            (Element);
                  Rules.Style.                  Process_Declaration            (Element);
                  Rules.Usage.                  Process_Declaration            (Element);
                  Rules.Max_Primitives.         Process_Subprogram_Declaration (Element);

               when A_Function_Declaration
                  | An_Expression_Function_Declaration
                  | A_Function_Body_Stub
                    =>
                  Rules.Derivations.            Process_Callable               (Element);
                  Rules.Parameter_Declarations. Process_Declaration            (Element);
                  Rules.Return_Type.            Process_Function_Declaration   (Element);
                  Rules.Style.                  Process_Declaration            (Element);
                  Rules.Usage.                  Process_Declaration            (Element);
                  Rules.Max_Primitives.         Process_Subprogram_Declaration (Element);

               when An_Entry_Declaration =>
                  Rules.Derivations.            Process_Callable    (Element);
                  Rules.Parameter_Declarations. Process_Declaration (Element);
                  Rules.Style.                  Process_Declaration (Element);

               when A_Generic_Procedure_Declaration =>
                  Rules.Parameter_Declarations. Process_Declaration (Element);
                  Rules.Style.                  Process_Declaration (Element);
                  Rules.Usage.                  Process_Declaration (Element);

               when A_Generic_Function_Declaration =>
                  Rules.Parameter_Declarations. Process_Declaration          (Element);
                  Rules.Return_Type.            Process_Function_Declaration (Element);
                  Rules.Style.                  Process_Declaration          (Element);
                  Rules.Usage.                  Process_Declaration          (Element);

               when A_Procedure_Renaming_Declaration =>
                  Rules.Derivations.           Process_Callable    (Element);
                  Rules.Renaming_Declarations. Process_Renaming_Declaration (Element);
                  Rules.Style.                 Process_Declaration (Element);

               when A_Function_Renaming_Declaration =>
                  Rules.Derivations.           Process_Callable             (Element);
                  Rules.Renaming_Declarations. Process_Renaming_Declaration (Element);
                  Rules.Return_Type.           Process_Function_Declaration (Element);
                  Rules.Style.                 Process_Declaration          (Element);

               when An_Object_Renaming_Declaration -- other renamings
                  | An_Exception_Renaming_Declaration
                  | A_Package_Renaming_Declaration
                  | A_Generic_Package_Renaming_Declaration
                  | A_Generic_Procedure_Renaming_Declaration
                  | A_Generic_Function_Renaming_Declaration
                  =>
                  Rules.Renaming_Declarations. Process_Renaming_Declaration (Element);

               when A_Formal_Procedure_Declaration   =>
                  Rules.Style. Process_Declaration (Element);

               when A_Formal_Function_Declaration =>
                  Rules.Return_Type. Process_Function_Declaration (Element);
                  Rules.Style.       Process_Declaration          (Element);

               when An_Incomplete_Type_Declaration =>
                  Rules.Max_Primitives. Process_Type_Declaration (Element);

               when A_Tagged_Incomplete_Type_Declaration =>
                  Rules.Max_Primitives. Process_Type_Declaration (Element);

               when others =>
                 null;
            end case;

         when A_Definition =>
            case Definition_Kind (Element) is
               when A_Type_Definition =>
                  case Type_Kind (Element) is
                     when A_Signed_Integer_Type_Definition
                        | A_Modular_Type_Definition
                        =>
                        Rules.No_Operator_Usage. Process_Type_Definition (Element);
                     when A_Derived_Type_Definition =>
                        Rules.No_Operator_Usage. Process_Type_Definition (Element);
                        Rules.Derivations.       Process_Derivation (Element);
                     when A_Derived_Record_Extension_Definition
                        | An_Interface_Type_Definition
                        =>
                        Rules.Derivations. Process_Derivation (Element);
                     when A_Constrained_Array_Definition =>
                        Rules.Array_Declarations. Process_Array_Definition             (Element);
                        Rules.No_Operator_Usage.  Process_Array_Definition             (Element);
                        Rules.Non_Static.         Process_Constrained_Array_Definition (Element);
                        Rules.Type_Usage.         Process_Array_Definition             (Element);
                     when An_Unconstrained_Array_Definition =>
                        Rules.Array_Declarations. Process_Array_Definition (Element);
                        Rules.No_Operator_Usage.  Process_Array_Definition (Element);
                        Rules.Type_Usage.         Process_Array_Definition (Element);
                     when others =>
                        null;
                  end case;

               when A_Private_Extension_Definition =>
                  Rules.Derivations.Process_Derivation (Element);

               when A_Formal_Type_Definition =>
                  case Formal_Type_Kind (Element) is
                     when A_Formal_Derived_Type_Definition
                        | A_Formal_Interface_Type_Definition
                        =>
                        Rules.Derivations. Process_Derivation (Element);
                     when others =>
                        null;
                  end case;

               when A_Subtype_Indication =>
                  Rules.Declarations. Process_Definition (Element);

               when A_Constraint =>
                  case Constraint_Kind (Element) is
                     when A_Discriminant_Constraint =>
                        Rules.Non_Static. Process_Discriminant_Constraint (Element);
                     when An_Index_Constraint =>
                        Rules.Array_Declarations. Process_Index_Constraint (Element);
                        Rules.Non_Static.         Process_Index_Constraint (Element);
                     when others =>
                        null;
                  end case;

               when An_Others_Choice =>
                  Rules.Statements.Process_Others (Element);

               when A_Discrete_Range
                 | A_Discrete_Subtype_Definition
                 =>
                  Rules.Declarations.            Process_Definition (Element);
                  Rules.Expressions.             Process_Range      (Element);
                  Rules.Simplifiable_Expressions.Process_Range      (Element);

               when A_Record_Definition =>
                  Rules.Record_Declarations.Process_Record_Definition (Element);
                  Rules.Comments.           Process_Record_Definition (Element);

               when A_Variant_Part =>
                  Rules.Declarations.Process_Definition (Element);

               when An_Access_Definition =>
                  Rules.Declarations.Process_Access_Definition (Element);

               when An_Aspect_Specification =>
                  Rules.Aspects.Process_Aspect (Element);
                  Rules.Style.  Process_Aspect (Element);

               when A_Task_Definition | A_Protected_Definition =>
                  Rules.Derivations.Process_Derivation (Element);

               when others =>
                  null;
            end case;

         when An_Exception_Handler =>
            Rules.Entity_Inside_Exception. Process_Exception_Handler   (Element);
            Rules.Silent_Exceptions.       Process_Exception_Handler   (Element);
            Rules.Simplifiable_Statements. Process_Exception_Handler   (Element);

         when A_Statement =>
            Rules.Style.                   Process_Element   (Element);
            Rules.Max_Size.                Process_Element   (Element);
            Rules.Max_Statement_Nesting.   Process_Statement (Element);
            Rules.Simplifiable_Statements. Process_Statement (Element);
            Rules.Statements.              Process_Statement (Element);

            case Statement_Kind (Element) is
               when An_Assignment_Statement =>
                  Rules.Assignments.      Process_Assignment (Element);
                  Rules.Known_Exceptions. Process_Assignment (Element);

               when A_Procedure_Call_Statement =>
                  Rules.Max_Call_Depth.                 Process_Call                  (Element);
                  Rules.Actual_Parameters.              Process_Call_Or_Instantiation (Element);
                  Rules.Duplicate_Initialization_Calls. Process_Procedure_Call        (Element);
                  Rules.Expressions.                    Process_Call                  (Element);
                  Rules.Exception_Propagation.          Process_Call                  (Element);
                  Rules.Insufficient_Parameters.        Process_Call                  (Element);
                  Rules.Not_Elaboration_Calls.          Process_Call                  (Element);
                  Rules.Parameter_Aliasing.             Process_Call                  (Element);
                  Rules.Side_Effect_Parameters.         Process_Call_Or_Instantiation (Element);
                  Rules.Unsafe_Paired_Calls.            Process_Call                  (Element);

               when An_Entry_Call_Statement =>
                  Rules.Max_Call_Depth.          Process_Call                  (Element);
                  Rules.Actual_Parameters.       Process_Call_Or_Instantiation (Element);
                  Rules.Exception_Propagation.   Process_Call                  (Element);
                  Rules.Expressions.             Process_Call                  (Element);
                  Rules.Insufficient_Parameters. Process_Call                  (Element);
                  Rules.Not_Elaboration_Calls.   Process_Call                  (Element);
                  Rules.Parameter_Aliasing.      Process_Call                  (Element);
                  Rules.Side_Effect_Parameters.  Process_Call_Or_Instantiation (Element);
                  Rules.Unsafe_Paired_Calls.     Process_Call                  (Element);

               when An_If_Statement =>
                  Rules.Style.                   Process_Compound_Statement (Element);

               when A_Case_Statement =>
                  Rules.Case_Statement. Process_Case_Statement     (Element);
                  Rules.Style.          Process_Compound_Statement (Element);

               when An_Accept_Statement =>
                  Rules.Declarations.              Process_Statement           (Element);
                  Rules.Improper_Initialization.   Process_Structure           (Element);
                  Rules.Movable_Accept_Statements. Process_Accept_Statement    (Element);
                  Rules.Return_Statements.         Initialize_Counter          (Element);
                  Rules.Style.                     Process_Compound_Statement  (Element);

               when A_Block_Statement =>
                  Rules.Declarations.            Process_Statement           (Element);
                  Rules.Improper_Initialization. Process_Structure           (Element);
                  Rules.Return_Statements.       Initialize_Counter          (Element);
                  Rules.Style.                   Process_Compound_Statement  (Element);

               when A_Loop_Statement
                 | A_For_Loop_Statement
                 =>
                  Rules.Statements.            Pre_Process_Loop            (Element);
                  Rules.Style.                 Process_Compound_Statement  (Element);

               when A_While_Loop_Statement =>
                  Rules.Statements.           Pre_Process_Loop            (Element);
                  Rules.Style.                Process_Compound_Statement  (Element);

               when An_Exit_Statement =>
                  Rules.Unsafe_Paired_Calls. Process_Breaking_Statement (Element);

               when A_Goto_Statement =>
                  Rules.Unsafe_Paired_Calls. Process_Breaking_Statement (Element);

               when A_Return_Statement =>
                  Rules.Return_Statements. Process_Return_Statement (Element);
                  Rules.Unsafe_Paired_Calls. Process_Breaking_Statement (Element);

               when An_Extended_Return_Statement =>
                  Rules.Declarations.            Process_Statement (Element);
                  Rules.Improper_Initialization. Process_Structure (Element);
                  Rules.Return_Statements.       Process_Return_Statement (Element);
                  Rules.Unsafe_Paired_Calls.     Process_Breaking_Statement (Element);

               when A_Selective_Accept_Statement
                 | A_Timed_Entry_Call_Statement
                 | A_Conditional_Entry_Call_Statement
                 | An_Asynchronous_Select_Statement
                 =>
                  Rules.Style. Process_Compound_Statement (Element);

               when others =>
                  null;
            end case;

         when A_Path =>
            case Path_Kind (Element) is
               when A_Case_Path =>
                  Rules.Case_Statement. Process_Path (Element);
               when others =>
                  null;
            end case;


         when An_Expression =>
            Rules.Expressions.          Process_Expression (Element);
            Rules.Max_Expression_Items. Process_Expression (Element);

            case Expression_Kind (Element) is
               when An_Attribute_Reference =>
                  Rules.Entities.     Process_Attribute (Element);
                  Rules.Local_Access. Process_Attribute (Element);
                  Rules.Style.        Process_Attribute (Element);
                  Rules.Type_Usage.   Process_Attribute (Element);

               when An_Operator_Symbol =>
                  Rules.No_Operator_Usage. Process_Operator (Element);

               when An_In_Membership_Test
                  | A_Not_In_Membership_Test
                  =>
                  Rules.No_Operator_Usage. Process_Membership (Element);

               when An_Allocation_From_Subtype
                 | An_Allocation_From_Qualified_Expression
                 =>
                  Rules.Allocators.Process_Allocator            (Element);
                  Rules.Not_Elaboration_Calls.Process_Allocator (Element);

                when A_Function_Call =>
                  Rules.Actual_Parameters.        Process_Call_Or_Instantiation (Element);
                  Rules.Exception_Propagation.    Process_Call                  (Element);
                  Rules.Expressions.              Process_Call                  (Element);
                  Rules.Insufficient_Parameters.  Process_Call                  (Element);
                  Rules.Known_Exceptions.         Process_Function_Call         (Element);
                  Rules.Max_Call_Depth.           Process_Call                  (Element);
                  Rules.Not_Elaboration_Calls.    Process_Call                  (Element);
                  Rules.Parameter_Aliasing.       Process_Call                  (Element);
                  Rules.Side_Effect_Parameters.   Process_Call_Or_Instantiation (Element);
                  Rules.Simplifiable_Expressions. Process_Call                  (Element);

               when An_And_Then_Short_Circuit
                  | An_Or_Else_Short_Circuit
                  =>
                  Rules.Simplifiable_Expressions.Process_Short_Circuit (Element);

               when A_Parenthesized_Expression =>
                  Rules.Simplifiable_Expressions. Process_Parenthesized (Element);

               when A_Character_Literal =>
                  Rules.Reduceable_Scope. Process_Identifier (Element);
                  Rules.Style.            Process_Literal    (Element);

               when An_Integer_Literal
                  | A_Real_Literal
                  | A_String_Literal
                  =>
                  Rules.Style. Process_Literal (Element);

               when A_Type_Conversion =>
                  Rules.Simplifiable_Expressions. Process_Conversion (Element);

               when An_Indexed_Component =>
                  Rules.Known_Exceptions. Process_Index_Expression (Element);
                  Rules.Non_Static.       Process_Index_Expression (Element);
                  Rules.Statements.       Process_Index_Expression (Element);

               when A_Selected_Component =>
                  Rules.Known_Exceptions. Process_Selected_Component (Element);

               when An_If_Expression =>
                  Rules.Simplifiable_Expressions.Process_If_Expression (Element);

               when An_Explicit_Dereference =>
                  Rules.Known_Exceptions.Process_Dereference (Element);

               when A_Raise_Expression =>
                  Rules.Known_Exceptions.Process_Raise_Expression (Element);

               when others =>
                  null;
            end case;

         when An_Association =>
            Rules.Positional_Associations. Process_Association (Element);

         when A_Pragma =>
            Rules.Object_Declarations. Process_Pragma  (Element);
            Rules.Pragmas.             Process_Pragma  (Element);
            Rules.Style.               Process_Element (Element);
            Rules.Style.               Process_Pragma  (Element);

         when others =>
            null;
      end case;
   end Pre_Procedure;

   --------------------
   -- Post_Procedure --
   --------------------

   procedure Post_Procedure (Element : in Asis.Element) is
      use Asis, Asis.Elements;
   begin
      case Element_Kind (Element) is
         when A_Clause =>
            case Clause_Kind (Element) is
               when A_Use_Package_Clause =>
                  Rules.Unnecessary_Use_Clause. Process_Use_Clause (Element);
               when A_Use_Type_Clause | A_Use_All_Type_Clause =>
                  Rules.Unnecessary_Use_Clause. Process_Use_Clause (Element);
               when others =>
                  null;
            end case;

         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Body_Declaration
                  | A_Procedure_Body_Declaration
                  | An_Entry_Body_Declaration
                  =>
                  Rules.Return_Statements. Finalize_Counter (Element);

               when A_Package_Body_Declaration =>
                  Rules.Directly_Accessed_Globals. Post_Process_Package_Body (Element);
               when A_Renaming_Declaration =>
                  Rules.Style. Process_Renaming (Element);
               when others =>
                  null;
            end case;

         when An_Expression =>
            case Expression_Kind (Element) is
               when A_Function_Call
                  | An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit
                  | An_In_Membership_Test     | A_Not_In_Membership_Test
                  =>
                  Rules.Expressions. Post_Process_Call (Element);
               when others =>
                  null;
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_Loop_Statement
                 | A_While_Loop_Statement
                 | A_For_Loop_Statement
                 =>
                  Rules.Statements. Post_Process_Loop (Element);
               when An_Accept_Statement
                  | A_Block_Statement
                  =>
                  Rules.Return_Statements. Finalize_Counter (Element);
               when others =>
                  null;
            end case;

         when A_Path =>
            case Path_Kind (Element) is
               when A_Select_Path | An_Or_Path =>
                  Rules.Usage. Post_Process_Path (Element);
               when others =>
                  null;
            end case;
        when others =>
            null;
      end case;
   end Post_Procedure;

   ---------------------
   -- Text_Enter_Unit --
   ---------------------

   procedure Text_Enter_Unit (Unit : in Asis.Compilation_Unit) is
      pragma Unreferenced (Unit);
   begin
      Rules.Header_Comments. Enter_Unit;
   end Text_Enter_Unit;

   -------------------
   -- Text_Analysis --
   -------------------

   procedure Text_Analysis (Line : Asis.Program_Text; Loc : Location) is
   begin
      Rules.Characters.       Process_Line (Line, Loc);
      Rules.Header_Comments.  Process_Line (Line, Loc);
      Rules.Max_Blank_Lines.  Process_Line (Line, Loc);
      Rules.Max_Line_Length.  Process_Line (Line, Loc);
      Rules.Comments.         Process_Line (Line, Loc);
      Rules.Style.            Process_Line (Line, Loc);
   end Text_Analysis;

end Framework.Plugs;
