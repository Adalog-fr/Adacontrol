----------------------------------------------------------------------
--  Framework.Plugs - Package body                                  --
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
  Asis.Elements;

-- All general rules
with
  Rules.Allocators,
  Rules.Declaration,
  Rules.Default_Parameter,
  Rules.Entities,
  Rules.Entity_Inside_Exception,
  Rules.Exception_Propagation,
  Rules.Instantiations,
  Rules.Local_Hiding,
  Rules.Local_Instantiation,
  Rules.Max_Nesting,
  Rules.Naming_Convention,
  Rules.No_Closing_Name,
  Rules.Not_Elaboration_Calls,
  Rules.Parameter_Aliasing,
  Rules.Pragmas,
  Rules.Real_Operators,
  Rules.Representation_Clauses,
  Rules.Side_Effect_Parameters,
  Rules.Silent_Exceptions,
  Rules.Simplifiable_Expressions,
  Rules.Specification_Objects,
  Rules.Statement,
  Rules.Unnecessary_Use,
  Rules.Use_Clauses,
  Rules.When_Others_Null;

package body Framework.Plugs is

   ----------------
   -- Enter_Unit --
   ----------------

   -- This procedure is called before any processing of a library unit.
   -- Plug calls here for rules that need initialization for every unit.

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
   begin
      null;
   end Enter_Unit;

   ---------------
   -- Exit_Unit --
   ---------------

   -- This procedure is called after all processing of a library unit.
   -- Plug calls here for rules that need finalization for every unit.

   procedure Exit_Unit (Unit : in Asis.Compilation_Unit) is
   begin
      null;
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
   end Enter_Scope;

   ----------------
   -- Exit_Scope --
   ----------------

   -- This procedure is called whenever we exit (Post_Procedure) from a
   -- construct where elements can be declared. Since this happen in various
   -- places in the tree and is likely to be quite common use, it is easier to
   -- plug calls here rather than in every place that might require it.

   procedure Exit_Scope (Element : in Asis.Element) is
   begin
      Rules.Unnecessary_Use. Process_Scope_Exit (Element);
      Rules.Max_Nesting.     Process_Scope_Exit (Element);
   end Exit_Scope;

   ---------------------
   -- True_Identifier --
   ---------------------

   --  Plug calls here to rules that need to process all occurrences
   --  of "true" identifiers, including operator symbols and
   --  enumeration literals, but excluding identifiers that are pragma
   --  names or attributes selectors

   procedure True_Identifier (Element : in Asis.Expression) is
   begin
      Rules.Entities.              Process_Identifier (Element);
      Rules.Unnecessary_Use.       Process_Identifier (Element);
      Rules.Specification_Objects. Process_Identifier (Element);
   end True_Identifier;

   -------------------
   -- Pre_Procedure --
   -------------------

   procedure Pre_Procedure (Element : in Asis.Element) is
      use Asis, Asis.Elements;
   begin
      case Element_Kind (Element) is
         when A_Clause =>
            case Clause_Kind (Element) is
               when A_Use_Package_Clause =>
                  Rules.Use_Clauses.Process_Use_Clause (Element);
               when A_Representation_Clause =>
                  Rules.Representation_Clauses.Process_Clause (Element);
               when A_With_Clause =>
                  Rules.Local_Hiding.Process_With_Clause (Element);
               when others =>
                  null;
            end case;

         when A_Defining_Name =>
            case Defining_Name_Kind (Element) is
               when A_Defining_Identifier
                 | A_Defining_Enumeration_Literal
                 =>
                  Rules.Local_Hiding.      Process_Defining_Name (Element);
                  Rules.Naming_Convention. Process_Defining_Name (Element);
               when others =>
                  null;
            end case;

         when A_Declaration =>
            Rules.Declaration.Process_Declaration (Element);

            case Declaration_Kind (Element) is
               when A_Variable_Declaration
                 | A_Constant_Declaration
                 | A_Deferred_Constant_Declaration
                 | A_Number_Declaration
                 =>
                  Rules.Specification_Objects.Process_Object_Declaration (Element);

               when A_Package_Declaration
                 | A_Package_Body_Declaration
                 | A_Generic_Package_Declaration
                 | A_Task_Type_Declaration
                 | A_Single_Task_Declaration
                 | A_Protected_Type_Declaration
                 | A_Single_Protected_Declaration
                 | A_Protected_Body_Declaration
                 | An_Entry_Body_Declaration
                 =>
                  Rules.No_Closing_Name.Process_Construct (Element);
               when A_Task_Body_Declaration
                 =>
                  Rules.No_Closing_Name.       Process_Construct (Element);
                  Rules.Exception_Propagation. Process_Task_Body (Element);

               when A_Procedure_Body_Declaration
                 | A_Function_Body_Declaration
                 =>
                  Rules.No_Closing_Name.      Process_Construct      (Element);
                  Rules.Exception_Propagation.Process_SP_Declaration (Element);

               when A_Package_Instantiation =>
                  Rules.Default_Parameter.      Process_Call_Or_Instantiation (Element);
                  Rules.Instantiations.         Process_Instantiation         (Element);
                  Rules.Local_Instantiation.    Process_Instantiation         (Element);
                  Rules.Side_Effect_Parameters. Process_Call_Or_Instantiation (Element);
                  Rules.Unnecessary_Use.        Process_Instantiation         (Element);
                  Rules.Specification_Objects.  Process_Package_Instantiation (Element);
                  Rules.Exception_Propagation.  Process_Instantiation         (Element);

               when A_Formal_Package_Declaration =>
                  Rules.Default_Parameter.      Process_Call_Or_Instantiation (Element);
                  Rules.Side_Effect_Parameters. Process_Call_Or_Instantiation (Element);
                  Rules.Unnecessary_Use.        Process_Instantiation         (Element);
                  Rules.Specification_Objects.  Process_Package_Instantiation (Element);

               when A_Formal_Package_Declaration_With_Box =>
                  Rules.Specification_Objects.  Process_Package_Instantiation (Element);

               when A_Procedure_Instantiation
                 | A_Function_Instantiation
                 =>
                  Rules.Default_Parameter.      Process_Call_Or_Instantiation (Element);
                  Rules.Instantiations.         Process_Instantiation         (Element);
                  Rules.Local_Instantiation.    Process_Instantiation         (Element);
                  Rules.Side_Effect_Parameters. Process_Call_Or_Instantiation (Element);
                  Rules.Unnecessary_Use.        Process_Instantiation         (Element);
                  Rules.Exception_Propagation.  Process_Instantiation         (Element);
               when others =>
                 null;
            end case;

         when A_Definition =>
            case Definition_Kind (Element) is
               when An_Others_Choice =>
                  Rules.Statement.Process_Others (Element);
               when A_Discrete_Range
                 | A_Discrete_Subtype_Definition =>
                  Rules.Simplifiable_Expressions.Process_Range (Element);
               when others =>
                  null;
            end case;

         when An_Exception_Handler =>
            Rules.Silent_Exceptions.       Process_Exception_Handler (Element);
            Rules.When_Others_Null.        Process_Exception_Handler (Element);
            Rules.Entity_Inside_Exception. Process_Exception_Handler (Element);

         when A_Statement =>
            Rules.Statement.Process_Statement (Element);

            case Statement_Kind (Element) is
               when A_Procedure_Call_Statement
                 | An_Entry_Call_Statement
                 =>
                  Rules.Parameter_Aliasing.     Process_Call                  (Element);
                  Rules.Default_Parameter.      Process_Call_Or_Instantiation (Element);
                  Rules.Exception_Propagation.  Process_Call                  (Element);
                  Rules.Not_Elaboration_Calls.  Process_Call                  (Element);
                  Rules.Side_Effect_Parameters. Process_Call_Or_Instantiation (Element);
                  Rules.Specification_Objects.  Process_Call                  (Element);

               when A_Case_Statement =>
                  Rules.Simplifiable_Expressions. Process_Case_Or_If     (Element);
                  Rules.When_Others_Null.         Process_Case_Statement (Element);

               when An_Assignment_Statement =>
                  Rules.Specification_Objects.Process_Assignment_Statement (Element);
               when others =>
                  null;
            end case;

         when A_Path =>
            case Path_Kind (Element) is
               when An_If_Path
                 | An_Elsif_Path
                 =>
                  Rules.Simplifiable_Expressions. Process_Case_Or_If (Element);
               when others =>
                  null;
            end case;

         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Attribute_Reference =>
                  Rules.Entities.Process_Identifier (Element);

               when An_Allocation_From_Subtype
                 | An_Allocation_From_Qualified_Expression
                 =>
                  Rules.Allocators.Process_Allocator (Element);

                when A_Function_Call =>
                  Rules.Default_Parameter.        Process_Call_Or_Instantiation (Element);
                  Rules.Exception_Propagation.    Process_Call                  (Element);
                  Rules.Not_Elaboration_Calls.    Process_Call                  (Element);
                  Rules.Side_Effect_Parameters.   Process_Call_Or_Instantiation (Element);
                  Rules.Real_Operators.           Process_Function_Call         (Element);
                  Rules.Simplifiable_Expressions. Process_Call                  (Element);

               when others =>
                  null;
            end case;

         when A_Pragma =>
            Rules.Pragmas.Process_Pragma (Element);

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
                  Rules.Unnecessary_Use.Process_Use_Clause (Element);
               when others =>
                  null;
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
               when An_Assignment_Statement =>
                  Rules.Specification_Objects.Post_Process;

               when A_Procedure_Call_Statement
                 | An_Entry_Call_Statement
                 =>
                  Rules.Specification_Objects.Post_Process;
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end Post_Procedure;

end Framework.Plugs;
