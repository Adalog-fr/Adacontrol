----------------------------------------------------------------------
--  Ruler - Package body                                            --
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

-- Ada
with
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis,
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;

-- Adalog
with
  A4G_Bugs,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Plugs,
  Framework.Specific_Plugs,
  Framework.Rules_Manager,
  Framework.Scope_Manager;

-- Pragmas
pragma Elaborate_All (Asis.Iterator);

package body Ruler is

   Stub_Nesting : Natural := 0;
   --  Depth of stubs traversal of proper bodies
   --  (proper bodies are traversed at the place of the corresponding stub)
   --  If a rule does not want to be activated for proper bodies, include the
   --  the call in an:
   --     if Stub_Nesting = 0 then ...

   -- Info type for Traverse:
   type Info is record
      Pragma_Or_Attribute_Level : Natural;
   end record;
   -- Pragma_Or_Attribute_Level:
   -- Used to trace whether we are in a pragma or attribute (see procedure True_Identifer);
   -- We need a counter rather than a boolean, because attributes may have multiple levels
   -- (i.e. T'Base'First)

   type Inhibited_Rule is new Framework.Rule_Context with
      record
         Rule_Name : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      end record;
   Inhibited : Framework.Context_Store;

   -----------------
   -- Enter_Unit --
   -----------------

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      use Asis.Compilation_Units;
   begin
      Framework.Scope_Manager. Enter_Unit (Unit);
      Framework.Plugs.         Enter_Unit (Unit);
      Framework.Specific_Plugs.Enter_Unit (Unit);
   exception
      when others =>
         Utilities.Trace ("Exception in Enter_Unit for " & Unit_Full_Name (Unit)); --## rule line off No_Trace
         raise;
   end Enter_Unit;

   -----------------
   -- Exit_Unit --
   -----------------

   procedure Exit_Unit (Unit : in Asis.Compilation_Unit) is
      use Framework.Scope_Manager, Asis.Compilation_Units;

   begin
      Utilities.Assert (Current_Depth = 0, "Not null depth at unit exit");
      Framework.Plugs.         Exit_Unit (Unit);
      Framework.Specific_Plugs.Exit_Unit (Unit);
   exception
      when others =>
         Utilities.Trace ("Exception in Exit_Unit for " & Unit_Full_Name (Unit)); --## rule line off No_Trace
         raise;
   end Exit_Unit;

   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope (Element : in Asis.Element) is
   begin
      Framework.Scope_Manager. Enter_Scope (Element);
      Framework.Plugs.         Enter_Scope (Element);
      Framework.Specific_Plugs.Enter_Scope (Element);
   end Enter_Scope;

   ----------------
   -- Exit_Scope --
   ----------------

   procedure Exit_Scope (Element : in Asis.Element) is
   begin
      Framework.Plugs.         Exit_Scope (Element);
      Framework.Specific_Plugs.Exit_Scope (Element);
      Framework.Scope_Manager. Exit_Scope (Element);
   end Exit_Scope;

   ---------------------
   -- True_Identifier --
   ---------------------

   procedure True_Identifier (Element : in Asis.Expression; State : in Info) is
   begin
      if State.Pragma_Or_Attribute_Level /= 0 then
         return;
      end if;
      Framework.Specific_Plugs.True_Identifier (Element);
      Framework.Plugs.         True_Identifier (Element);
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
                            State   : in out Info)
   is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Ada.Strings.Wide_Fixed, Utilities;
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Declaration
                 | A_Procedure_Declaration
                 | An_Entry_Declaration
                 | A_Generic_Procedure_Declaration
                 | A_Generic_Function_Declaration
                 | A_Formal_Procedure_Declaration
                 | A_Formal_Function_Declaration
                 | A_Package_Declaration
                 | A_Package_Body_Declaration
                 | A_Generic_Package_Declaration
                 | A_Task_Type_Declaration
                 | A_Single_Task_Declaration
                 | A_Task_Body_Declaration
                 | A_Protected_Type_Declaration
                 | A_Single_Protected_Declaration
                 | A_Protected_Body_Declaration
                 | An_Entry_Body_Declaration
                 | A_Procedure_Body_Declaration
                 | A_Function_Body_Declaration
                 | A_Package_Renaming_Declaration
                 | A_Procedure_Renaming_Declaration
                 | A_Function_Renaming_Declaration
                 | A_Generic_Package_Renaming_Declaration
                 | A_Generic_Procedure_Renaming_Declaration
                 | A_Generic_Function_Renaming_Declaration
                 | A_Package_Instantiation
                 | A_Procedure_Instantiation
                 | A_Function_Instantiation
                 =>
                  Enter_Scope (Element);
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

               when A_Body_Stub =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

                  -- Process proper bodies at the place of the stub
                  Stub_Nesting := Stub_Nesting + 1;
                  User_Log (3 * Stub_Nesting * ' '
                            & "Controlling separate "
                            & Defining_Name_Image (Names(Element)(1)));
                  Traverse (Corresponding_Subunit (Element), Control, State);
                  User_Log (3 * Stub_Nesting * ' '
                            & "returning");
                  Stub_Nesting := Stub_Nesting - 1;

               when others =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
            end case;

         when An_Exception_Handler =>
            Enter_Scope (Element);
            Framework.Plugs.         Pre_Procedure (Element);
            Framework.Specific_Plugs.Pre_Procedure (Element);

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_For_Loop_Statement
                 | A_Block_Statement
                 | An_Accept_statement
                 =>
                  Enter_Scope (Element);
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
               when others =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
            end case;

         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Attribute_Reference =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

                  -- Traverse manually, because we want to inhibit True_Identifier for
                  -- the attribute designator, not for the prefix
                  Traverse (Prefix (Element), Control, State);
                  State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level + 1;
                  Traverse (A4G_Bugs.Attribute_Designator_Identifier (Element), Control, State);
                  State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level - 1;
                  Control := Abandon_Children;

               when An_Identifier
                 | An_Operator_Symbol
                 | An_Enumeration_Literal
                 =>
                  True_Identifier (Element, State);

                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

               when others =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
            end case;

         when A_Pragma =>
            -- Cancel other identifier processing
            State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level + 1;

            Framework.Plugs.         Pre_Procedure (Element);
            Framework.Specific_Plugs.Pre_Procedure (Element);

         when others =>
            Framework.Plugs.         Pre_Procedure (Element);
            Framework.Specific_Plugs.Pre_Procedure (Element);
      end case;

   exception
      when others =>
         Utilities.Trace ("Exception in Pre_Procedure", Element, With_Source => True); --## rule line off No_Trace
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
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Declaration
                 | A_Procedure_Declaration
                 | An_Entry_Declaration
                 | A_Generic_Procedure_Declaration
                 | A_Generic_Function_Declaration
                 | A_Formal_Procedure_Declaration
                 | A_Formal_Function_Declaration
                 | A_Package_Declaration
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
                 | A_Package_Renaming_Declaration
                 | A_Procedure_Renaming_Declaration
                 | A_Function_Renaming_Declaration
                 | A_Generic_Package_Renaming_Declaration
                 | A_Generic_Procedure_Renaming_Declaration
                 | A_Generic_Function_Renaming_Declaration
                 | A_Package_Instantiation
                 | A_Procedure_Instantiation
                 | A_Function_Instantiation
                 =>
                  Framework.Plugs.         Post_Procedure (Element);
                  Framework.Specific_Plugs.Post_Procedure (Element);
                 Exit_Scope (Element);

               when others =>
                  Framework.Plugs.         Post_Procedure (Element);
                  Framework.Specific_Plugs.Post_Procedure (Element);
            end case;

         when An_Exception_Handler =>
            Framework.Plugs.         Post_Procedure (Element);
            Framework.Specific_Plugs.Post_Procedure (Element);

            Exit_Scope (Element);

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_For_Loop_Statement
                 | A_Block_Statement
                 | An_Accept_statement
                 =>
                  Framework.Plugs.         Post_Procedure (Element);
                  Framework.Specific_Plugs.Post_Procedure (Element);

                  Exit_Scope (Element);

              when others =>
                 Framework.Plugs.         Post_Procedure (Element);
                 Framework.Specific_Plugs.Post_Procedure (Element);
            end case;

         when A_Pragma =>
            Framework.Plugs.         Post_Procedure (Element);
            Framework.Specific_Plugs.Post_Procedure (Element);

            -- Reset other identifier processing
            State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level - 1;

         when others =>
            Framework.Plugs.         Post_Procedure (Element);
            Framework.Specific_Plugs.Post_Procedure (Element);
      end case;

   exception
      when others =>
         Utilities.Trace ("Exception in Post_Procedure", Element, With_Source => True); --## rule line off No_Trace
         raise;
   end Post_Procedure;

   -------------
   -- Process --
   -------------

   procedure Process (Unit_Name  : in Wide_String;
                      Spec_Only  : in Boolean) is
      use Asis;
      use Asis.Elements;
      use Asis.Compilation_Units;
      use Utilities;

      --
      -- Do_Process
      --
      procedure Do_Process (My_Unit : Compilation_Unit) is
         use Framework.Rules_Manager;

         The_Control : Traverse_Control := Continue;
         The_Info    : Info := (Pragma_Or_Attribute_Level => 0);

         procedure Process_Inhibition (State : Rule_Action) is
            use Asis.Declarations, Framework, Ada.Strings.Wide_Unbounded;
            Context : constant Rule_Context'Class
              := Matching_Context (Inhibited, Names (Unit_Declaration (My_Unit))(1));
            use Framework.Rules_Manager;
         begin
            if Context /= No_Matching_Context then
               Rules_Manager.Command (To_Wide_String (Inhibited_Rule (Context).Rule_Name), State);
               loop
                  declare
                     New_Context : constant Rule_Context'Class := Next_Matching_Context (Inhibited);
                  begin
                     exit when New_Context = No_Matching_Context;
                     Rules_Manager.Command (To_Wide_String (Inhibited_Rule (New_Context).Rule_Name), State);
                  end;
               end loop;
            end if;
         end Process_Inhibition;

      begin
         if Is_Nil (My_Unit) then
            Trace ("Nil unit");    --## rule line off No_Trace
            return;
         end if;

         Enter_Unit (My_Unit);
         Process_Inhibition (Suspend);

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

         Process_Inhibition (Resume);

         Exit_Unit (My_Unit);
      exception
         when others =>
            -- Do not call exit_unit to not make things worse...
            Process_Inhibition (Resume);
            raise;
      end Do_Process;

      Unit_Body : Asis.Compilation_Unit;
   begin -- Process
      User_Log ("Controlling " & Unit_Name & " specification");

      if not Spec_Only then
         -- Get the body before accessing the spec to avoid tree swapping
         -- (see Asis User's Guide about tree swapping)
         Unit_Body := Compilation_Unit_Body (Unit_Name, My_Context);
      end if;

      Do_Process (Library_Unit_Declaration (Unit_Name, My_Context));

      if not Spec_Only then
         User_Log ("Controlling " & Unit_Name & " body");
         Do_Process (Unit_Body);
      end if;
   end Process;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Stub_Nesting := 0;
   end Reset;

   -------------
   -- Inhibit --
   -------------

   procedure Inhibit (Rule_Name : in Wide_String) is
      use Framework, Framework.Language, Ada.Strings.Wide_Unbounded;
   begin
      if not Parameter_Exists then
         Parameter_Error ("Missing unit names in ""Inhibit"" command");
      end if;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            -- Check that inhibition is not already specified
            -- (otherwise, the suspend/resume mechanism won't work since
            -- suspensions are not stacked)

            Associate (Inhibited,
                       Entity,
                       Inhibited_Rule'(Rule_Name =>To_Unbounded_Wide_String (Rule_Name)),
                       Additive => True);
         exception
            when Already_In_Store =>
               Parameter_Error ("Rule " & Rule_Name & " already inhibited for " & Image (Entity));
         end;
      end loop;
   end Inhibit;

end Ruler;

