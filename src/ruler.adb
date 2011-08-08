----------------------------------------------------------------------
--  Ruler - Package body                                            --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Code Cheker  is free software;  you can redistribute  it and/or --
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
  Ada.Characters.Handling,
  Ada.Exceptions;

-- ASIS
with
  Asis,
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Exceptions,
  Asis.Expressions,
  Asis.Iterator;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Rules_Manager,
  Framework.Scope_Manager,
  Adactl_Options,

  -- All rules
  Rules.Allocators,
  Rules.Attributes,
  Rules.Default_Parameter,
  Rules.Entity,
  Rules.Entity_Inside_Exception,
  Rules.Instantiations,
  Rules.Local_Hiding,
  Rules.Local_Instantiation,
  Rules.Max_Nesting,
  Rules.No_Closing_Name,
  Rules.Not_Elaboration_Calls,
  Rules.Parameter_Aliasing,
  Rules.Pragmas,
  Rules.Silent_Exceptions,
  Rules.Unnecessary_Use,
  Rules.Use_Clauses;

-- Pragmas
pragma Elaborate_All (Asis.Iterator);

package body Ruler is

   Stub_Nesting : Natural := 0;
   --  Depth of stubs traversal of proper bodies
   --  (proper bodies are traversed at the place of the corresponding stub)
   --  If a rule does not want to be activated for proper bodies, include the
   --  the call in an:
   --     if Stub_Nesting = 0 then ...

   Failure_Occured : Boolean := False;

   -- Info type for Traverse:
   type Info is record
      Pragma_Or_Attribute_Level : Natural;
   end record;
   -- Pragma_Or_Attribute_Level:
   -- Used to trace whether we are in a pragma or attribute (see procedure True_Identifer);
   -- We need a counter rather than a boolean, because attributes may have multiple levels
   -- (i.e. T'Base'First)

   -----------------
   -- Enter_Unit --
   -----------------

   -- This procedure is called before any processing of a library unit.
   -- Plug calls here for rules that need initialization for every unit.

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
   begin
      Framework.Scope_Manager.Enter_Unit (Unit);

       -- Plug rules below this line:
   end Enter_Unit;

   -----------------
   -- Exit_Unit --
   -----------------

   -- This procedure is called after all processing of a library unit.
   -- Plug calls here for rules that need finalization for every unit.

   procedure Exit_Unit (Unit : in Asis.Compilation_Unit) is
      use Framework.Scope_Manager;
   begin
      Utilities.Assert (Current_Depth = 0,
                        "Not null depth at unit exit");
      -- Plug rules below this line:

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
      Framework.Scope_Manager.Enter_Scope (Element);

      -- Plug rules below this line:
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
       -- Plug rules below this line:
      Rules.Unnecessary_Use.Process_Scope_Exit (Element);
      Rules.Max_Nesting.Process_Scope_Exit (Element);

      -- No more rules below this line:
      Framework.Scope_Manager.Exit_Scope (Element);
   end Exit_Scope;

   ---------------------
   -- True_Identifier --
   ---------------------

   --  Plug calls here to rules that need to process all occurrences
   --  of "true" identifiers, including operator symbols and
   --  enumeration literals, but excluding identifiers that are pragma
   --  names or attributes selectors

   procedure True_Identifier (Element : in Asis.Expression;
                              State   : in Info) is
   begin
      if State.Pragma_Or_Attribute_Level /= 0 then
         return;
      end if;

      -- Plug rules below this line:
      Rules.Entity.Process_Identifier (Element);
      Rules.Unnecessary_Use.Process_Identifier (Element);
   end True_Identifier;

   --------------
   -- Traverse --
   --------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Info);
   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info);

   procedure Traverse is new Asis.Iterator.Traverse_Element
     (Info, Pre_Procedure, Post_Procedure);

   -------------------
   -- Pre_Procedure --
   -------------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Info) is
      use Asis, Asis.Elements, Asis.Declarations, Asis.Expressions;
   begin
      case Element_Kind (Element) is
         when A_Clause =>
            case Clause_Kind (Element) is
               when A_Use_Package_Clause =>
                  Rules.Use_Clauses.Process_Use_Clause (Element);
               when others =>
                  null;
            end case;

         when A_Defining_Name =>
            case Defining_Name_Kind (Element) is
               when A_Defining_Identifier
                 | A_Defining_Enumeration_Literal
                 =>
                  Rules.Local_Hiding.Process_Defining_Name (Element);
               when others => null;
            end case;

         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Declaration
                 | A_Procedure_Declaration
                 | An_Entry_Declaration

                 | A_Generic_Procedure_Declaration
                 | A_Generic_Function_Declaration
                 | A_Formal_Procedure_Declaration
                 | A_Formal_Function_Declaration
                 =>
                  Enter_Scope (Element);

                  -- Plug rules below this line:

               when A_Package_Declaration
                 | A_Procedure_Body_Declaration
                 | A_Function_Body_Declaration
                 | A_Package_Body_Declaration
                 | A_Generic_Package_Declaration
                 | A_Task_Type_Declaration
                 | A_Single_Task_Declaration
                 | A_Task_Body_Declaration
                 | A_Protected_Type_Declaration
                 | A_Single_Protected_Declaration
                 | A_Protected_Body_Declaration
                 | An_Entry_Body_Declaration
                 =>
                  Enter_Scope (Element);

                  -- Plug rules below this line:
                  Rules.No_Closing_Name.Process_Construct (Element);

               when A_Package_Renaming_Declaration
                 | A_Procedure_Renaming_Declaration
                 | A_Function_Renaming_Declaration
                 | A_Generic_Package_Renaming_Declaration
                 | A_Generic_Procedure_Renaming_Declaration
                 | A_Generic_Function_Renaming_Declaration
                 =>
                  Enter_Scope (Element);

                  -- Plug rules below this line:

               when A_Package_Instantiation
                 | A_Procedure_Instantiation
                 | A_Function_Instantiation
                 =>
                  Enter_Scope (Element);

                  -- Plug rules below this line:
                  Rules.Default_Parameter.Process_Call_Or_Instantiation (Element);
                  Rules.Instantiations.Process_Instantiation (Element);
                  Rules.Local_Instantiation.Process_Instantiation (Element);
                  Rules.Unnecessary_Use.Process_Instantiation (Element);

               when A_Body_Stub =>
                  -- Plug rules below this line:

                  -- No more rules below this line:

                  -- Process proper bodies at the place of the stub
                  Stub_Nesting := Stub_Nesting + 1;
                  Traverse (Corresponding_Subunit (Element), Control, State);
                  Stub_Nesting := Stub_Nesting - 1;
              when others =>
                 null;
            end case;

            -- Plug rules for all declarations below this line:

         when An_Exception_Handler =>
            Enter_Scope (Element);

            -- Plug rules below this line:
            Rules.Silent_Exceptions.Process_Exception_Handler (Element);
            Rules.Entity_Inside_Exception.Process_Exception_Handler (Element);

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_For_Loop_Statement
                 | A_Block_Statement
                 | An_Accept_statement
                 =>
                  Enter_Scope (Element);

                  -- Plug rules below this line:

              when A_Procedure_Call_Statement
                 | An_Entry_Call_Statement
                 =>
                  Rules.Parameter_Aliasing.Process_Call (Element);
                  Rules.Default_Parameter.Process_Call_Or_Instantiation (Element);
                  Rules.Not_Elaboration_Calls.Process_Subprogram_Call (Element);

               when others =>
                  null;
            end case;

            -- Plug rules for all statements below this line:

         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Attribute_Reference =>
                  -- Process the prefix, cancel other identifier processing
                  Traverse (Prefix (Element), Control, State);
                  State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level + 1;

                  -- Plug rules below this line:
                  Rules.Attributes.Process_Attribute (Element);
               when An_Identifier
                 | An_Operator_Symbol
                 | An_Enumeration_Literal
                 =>
                  True_Identifier (Element, State);
               when An_Allocation_From_Subtype | An_Allocation_From_Qualified_Expression =>
                  Rules.Allocators.Process_Allocator (Element);
               when A_Function_Call =>
                  Rules.Default_Parameter.Process_Call_Or_Instantiation (Element);
                  Rules.Not_Elaboration_Calls.Process_Subprogram_Call (Element);
               when others =>
                  null;
            end case;

            -- Plug rules for all expressions below this line:

         when A_Pragma =>
            -- Cancel other identifier processing
            State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level + 1;

            -- Plug rules below this line:
            Rules.Pragmas.Process_Pragma (Element);

         when others =>
            null;
      end case;
   exception
      when others =>
         Utilities.Trace ("Exception in Pre_Procedure", Element, With_Source => True);
         raise;
   end Pre_Procedure;

   --------------------
   -- Post_Procedure --
   --------------------

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info) is
      pragma Unreferenced (Control);
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

         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Declaration
                 | A_Procedure_Declaration
                 | An_Entry_Declaration

                 | A_Generic_Procedure_Declaration
                 | A_Generic_Function_Declaration
                 | A_Formal_Procedure_Declaration
                 | A_Formal_Function_Declaration
                 =>
                  -- Plug rules below this line:

                  -- No more rules below this line:
                  Exit_Scope (Element);

                when A_Package_Declaration
                 | A_Procedure_Body_Declaration
                 | A_Function_Body_Declaration
                 | A_Package_Body_Declaration
                 | A_Generic_Package_Declaration
                 | A_Task_Type_Declaration
                 | A_Single_Task_Declaration
                 | A_Task_Body_Declaration
                 | A_Protected_Type_Declaration
                 | A_Single_Protected_Declaration
                 | A_Protected_Body_Declaration
                 | An_Entry_Body_Declaration
                 =>
                  -- Plug rules below this line:

                  -- No more rules below this line:
                  Exit_Scope (Element);

               when A_Package_Renaming_Declaration
                 | A_Procedure_Renaming_Declaration
                 | A_Function_Renaming_Declaration
                 | A_Generic_Package_Renaming_Declaration
                 | A_Generic_Procedure_Renaming_Declaration
                 | A_Generic_Function_Renaming_Declaration
                 =>
                  -- Plug rules below this line:

                  -- No more rules below this line:
                  Exit_Scope (Element);

               when A_Package_Instantiation
                | A_Procedure_Instantiation
                | A_Function_Instantiation
                 =>
                  Exit_Scope (Element);

              when others =>
                  null;
            end case;

         when An_Exception_Handler =>
            -- Plug rules below this line:

            -- No more rules below this line:
            Exit_Scope (Element);

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_For_Loop_Statement
                 | A_Block_Statement
                 | An_Accept_statement
                 =>
                  -- Plug rules below this line:

                  -- No more rules below this line:
                  Exit_Scope (Element);
              when others =>
                  null;
            end case;

         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Attribute_Reference =>
                  -- Plug rules below this line:

                  -- No more rules below this line:

                  -- Reset other identifier processing
                  State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level - 1;
               when others =>
                  null;
            end case;

         when A_Pragma =>
            -- Plug rules below this line:

            -- No more rules below this line:

            -- Reset other identifier processing
            State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level - 1;

         when others =>
            null;
      end case;

   exception
      when others =>
         Utilities.Trace ("Exception in Post_Procedure", Element, With_Source => True);
         raise;
   end Post_Procedure;

   -------------
   -- Process --
   -------------

   procedure Process (Unit_Name  : in     Wide_String;
                      Spec_Only  : in     Boolean;
                      My_Context : in out Asis.Context) is
      use Asis;
      use Asis.Elements;
      use Asis.Compilation_Units;
      use Utilities;

      --
      -- Do_Process
      --
      procedure Do_Process (My_Unit : Compilation_Unit) is
         The_Control : Traverse_Control := Continue;
         The_Info    : Info := (Pragma_Or_Attribute_Level => 0);
      begin
         if Is_Nil (My_Unit) then
            Trace ("Nil unit");
            return;
         end if;

         Enter_Unit (My_Unit);

      Process_Context_Clauses :
         declare
            My_CC_List : constant Context_Clause_List
              := Context_Clause_Elements (Compilation_Unit => My_Unit,
                                          Include_Pragmas  => True) ;
         begin
            for I in My_CC_List'Range loop
               Traverse (My_CC_List (I), The_Control, The_Info);
            end loop;
         end Process_Context_Clauses;

      Process_Unit :
         declare
            My_Declaration : constant Declaration := Unit_Declaration (My_Unit);
         begin
            Traverse (My_Declaration, The_Control, The_Info);
         end Process_Unit;

         Exit_Unit (My_Unit);
      end Do_Process;

      use Asis.Exceptions, Ada.Exceptions, Ada.Characters.Handling;
      Unit_Body : Asis.Compilation_Unit;
   begin
      User_Log ("Ruling " & Unit_Name & " specification");

      if not Spec_Only then
         -- Get the body before accessing the spec to avoid tree swapping
         -- (see Asis User's Guide about tree swapping)
         Unit_Body := Compilation_Unit_Body (Unit_Name, My_Context);
      end if;

      Do_Process (Library_Unit_Declaration (Unit_Name, My_Context));

      if not Spec_Only then
         User_Log ("Ruling " & Unit_Name & " body");
         Do_Process (Unit_Body);
      end if;

   exception
      when Occur : ASIS_Inappropriate_Context
        | ASIS_Inappropriate_Container
        | ASIS_Inappropriate_Compilation_Unit
        | ASIS_Inappropriate_Element
        | ASIS_Inappropriate_Line
        | ASIS_Inappropriate_Line_Number
        =>
         Failure_Occured := True;

         User_Message ("In rule : " & Framework.Rules_Manager.Last_Rule);
         User_Message ("For unit: " & Unit_Name);
         Asis_Exception_Messages (Occur);

         -- Clean-up:
         Stub_Nesting := 0;
         Framework.Scope_Manager.Reset;

         -- Propagate the exception only if Exit_Option set
         -- (otherwise, try to process other units)
         if Adactl_Options.Exit_Option then
            raise;
         end if;

      when Occur : others =>
         Failure_Occured := True;

         User_Message ("Internal error: " & To_Wide_String (Exception_Name (Occur)));
         User_Message ("       In rule: " & Framework.Rules_Manager.Last_Rule);
         User_Message ("      For unit: " & Unit_Name);
         User_Message ("       Message: " & To_Wide_String (Exception_Message (Occur)));

         -- Clean-up:
         Stub_Nesting := 0;
         Framework.Scope_Manager.Reset;

         -- Propagate the exception only if Exit_Option set
         -- (otherwise, try to process other units)
         if Adactl_Options.Exit_Option then
            raise;
         end if;
   end Process;

   -----------------
   -- Had_Failure --
   -----------------

   function Had_Failure return Boolean is
   begin
      return Failure_Occured;
   end Had_Failure;

end Ruler;

