----------------------------------------------------------------------
--  Framework.Ruler - Package body                                  --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005.         --
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
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis,
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Exceptions,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Text;

-- Adalog
with
  Thick_Queries,
  Units_List,
  Utilities;

-- Adactl
with
  Framework.Plugs,
  Framework.Queries,
  Framework.Reports,
  Framework.Rules_Manager,
  Scope_Manager,
  Framework.Specific_Plugs,
  Framework.Symbol_Table,
  Rules.Uncheckable;

-- Pragmas
pragma Elaborate_All (Asis.Iterator);

package body Framework.Ruler is

   -- This package is the main engine that drives the traversal of the unit.
   -- There are a number of delicate issues that are dealt with here, in order
   -- to save the burden to the rules.
   -- Make sure you understand the issues before making changes!

   Stub_Nesting : Natural := 0;
   --  Depth of stubs traversal of proper bodies
   --  (proper bodies are traversed at the place of the corresponding stub)

   -- Info type for Semantic_Traverse:
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

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      use Ada.Characters.Handling, Ada.Exceptions;
      use Asis, Asis.Compilation_Units;
   begin
      Scope_Manager.           Enter_Unit (Unit);
      Framework.Plugs.         Enter_Unit (Unit);
      Framework.Specific_Plugs.Enter_Unit (Unit);
   exception
      when Occur : others =>
         begin
            Utilities.Trace ("Exception "                                  --## rule line off No_Trace
                             & To_Wide_String (Exception_Name (Occur))
                             & " in Enter_Unit for "
                             & Unit_Full_Name (Unit)
                             & " (" & Unit_Kinds'Wide_Image (Unit_Kind (Unit)) & ')');
         exception
            when Occur : others =>
               -- If we are not even able to call Unit_Full_Name (presumably), trace and ignore the exception
               -- in order to propagate the original exception (more interesting)
               Utilities.Trace ("Exception in handler for enter_unit", Occur); --## rule line off No_Trace
         end;
         raise;
   end Enter_Unit;


   -----------------
   -- Exit_Unit --
   -----------------

   procedure Exit_Unit (Unit : in Asis.Compilation_Unit) is
      use Ada.Characters.Handling, Ada.Exceptions;
      use Asis, Asis.Compilation_Units;

   begin
      Framework.Plugs.         Exit_Unit (Unit);
      Framework.Specific_Plugs.Exit_Unit (Unit);
      Scope_Manager.           Exit_Unit (Unit);
   exception
      when Occur : others =>
         begin
            Utilities.Trace ("Exception "                                    --## rule line off No_Trace
                             & To_Wide_String (Exception_Name (Occur))
                             & " in Exit_Unit for "   --## rule line off No_Trace
                             & Unit_Full_Name (Unit)
                             & " (" & Unit_Kinds'Wide_Image (Unit_Kind (Unit)) & ')');
         exception
            when Occur : others =>
               -- If we are not even able to call Unit_Full_Name (presumably), trace and ignore the exception
               -- in order to propagate the original exception (more interesting)
               Utilities.Trace ("Exception in handler for exit_unit", Occur); --## rule line off No_Trace
         end;
         raise;
   end Exit_Unit;


   --------------------------
   -- Exit_Context_Clauses --
   --------------------------

   procedure Exit_Context_Clauses (Unit : in Asis.Compilation_Unit) is
   begin
      Framework.Plugs.         Exit_Context_Clauses (Unit);
      Framework.Specific_Plugs.Exit_Context_Clauses (Unit);
      Scope_Manager.           Exit_Context_Clauses;
   end Exit_Context_Clauses;


   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope (Element : in Asis.Element) is
   begin
      Scope_Manager.           Enter_Scope (Element);  -- Must stay first
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
      Framework.Symbol_Table.  Exit_Scope (Element);
      Scope_Manager.           Exit_Scope (Element);  -- Must stay last
   end Exit_Scope;

   ------------------------
   -- Is_True_Identifier --
   ------------------------

   function Is_True_Identifier (Element : in Asis.Expression; State : in Info) return Boolean is
      use Asis.Exceptions, Asis.Expressions;
      pragma Warnings (Off, "*Junk*"); -- Junk is never read, just set to test the exception
      Junk : Asis.Definition;
      pragma Warnings (On, "*Junk*");
   begin
      -- An identifier appearing in a pragma that is not a true identifier raises
      -- ASIS_Inappropriate_Element for its Corresponding_Name_Definition
      if State.Pragma_Or_Attribute_Level /= 0 then
         begin
            Junk := Corresponding_Name_Definition (Element);
            -- OK!
         exception
            when ASIS_Inappropriate_Element =>
               -- Not a true identifier!
               return False;
         end;
      end if;

      return True;
   end Is_True_Identifier;

   -----------------------
   -- Semantic_Traverse --
   -----------------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Info);
   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info);

   procedure Semantic_Traverse_Elements is new Asis.Iterator.Traverse_Element (Info, Pre_Procedure, Post_Procedure);

   procedure Semantic_Traverse (Unit : Asis.Compilation_Unit) is
      use Asis, Asis.Elements, Asis.Text;
      use Framework.Queries;

      The_Control : Traverse_Control := Continue;
      The_Info    : Info := (Pragma_Or_Attribute_Level => 0);
      Duplicate   : Boolean;

      My_Pragma_List : constant Pragma_Element_List := Compilation_Pragmas (Unit);
      My_CC_List     : constant Context_Clause_List := Context_Clause_Elements (Compilation_Unit => Unit,
                                                                                Include_Pragmas  => True) ;
      My_Declaration : constant Declaration := Unit_Declaration (Unit);
   begin
      Reports.Total_Lines := Reports.Total_Lines + Last_Line_Number (My_Declaration);

      Init_Standard (Unit);
      Enter_Unit (Unit);

      -- Process_Compilation_Pragmas :
      -- In the comments for Compilation_Pragmas, ASIS says:
      -- Pragmas from this query may be duplicates of some or all of the
      -- non-Elaborate pragmas available from the Context_Clause_Elements query.
      -- We eliminate duplicates from here since they will appear at their natural place if
      -- analysed as context clauses
      for I in My_Pragma_List'Range loop
         -- not a very efficient algorithm, but we assumes there are not many compilation pragmas nor
         -- context clauses
         Duplicate := False;
         for C in My_CC_List'Range loop
            if Is_Equal (My_Pragma_List (I), My_CC_List (C)) then
               Duplicate := True;
               exit;
            end if;
         end loop;

         if not Duplicate then
            Semantic_Traverse_Elements (My_Pragma_List (I), The_Control, The_Info);
         end if;
      end loop;

      -- Process_Context_Clauses
      for I in My_CC_List'Range loop
         Semantic_Traverse_Elements (My_CC_List (I), The_Control, The_Info);
      end loop;

      Exit_Context_Clauses (Unit);

      -- Process_Unit
      Semantic_Traverse_Elements (My_Declaration, The_Control, The_Info);

      Exit_Unit (Unit);
   end Semantic_Traverse;

   ---------------------
   -- Traverse_Pragma --
   ---------------------

   -- This is used to traverse manually pragma arguments, because we have to decide on a
   -- case-by-case basis whether the arguments are expressions (that should be traversed)
   -- or not (special names that should not be considered identifiers)
   -- This replaces the normal (recursive) traversal, any code that calls this procedure must set
   -- Control to Abandon_Children
   procedure Traverse_Pragma (Element : Asis.Element;
                              Control : in out Asis.Traverse_Control;
                              State   : in out Info)
   is
      use Asis, Asis.Elements;
      use Utilities;

      Associations : constant Asis.Association_List := Pragma_Argument_Associations (Element);
      Level_Delta  : Natural range 0..1;
   begin
      case Pragma_Kind (Element) is
         when A_Detect_Blocking_Pragma
            | A_Normalize_Scalars_Pragma
            | A_Page_Pragma
            | A_Reviewable_Pragma
            =>
            -- No parameters
            return;
         when An_All_Calls_Remote_Pragma
            | An_Asynchronous_Pragma
            | An_Atomic_Pragma
            | An_Atomic_Components_Pragma
            | An_Attach_Handler_Pragma
            | A_Controlled_Pragma
            | A_CPU_Pragma
            | A_Default_Storage_Pool_Pragma
            | A_Dispatching_Domain_Pragma
            | An_Elaborate_Pragma
            | An_Elaborate_All_Pragma
            | An_Elaborate_Body_Pragma
            | An_Independent_Pragma
            | A_Independent_Components_Pragma
            | An_Inspection_Point_Pragma
            | An_Interrupt_Handler_Pragma
            | An_Interrupt_Priority_Pragma
            | A_Linker_Options_Pragma
            | A_No_Return_Pragma
            | A_Pack_Pragma
            | A_Preelaborable_Initialization_Pragma
            | A_Preelaborate_Pragma
            | A_Priority_Pragma
            | A_Pure_Pragma
            | A_Relative_Deadline_Pragma
            | A_Remote_Call_Interface_Pragma
            | A_Remote_Types_Pragma
            | A_Shared_Passive_Pragma
            | A_Storage_Size_Pragma
            | An_Unchecked_Union_Pragma
            | A_Volatile_Pragma
            | A_Volatile_Components_Pragma
            =>
            -- All parameters are true expressions (or names), no named notation
            -- => It is safe to traverse normally
            Level_Delta := 0;
         when An_Assert_Pragma
            | A_Discard_Names_Pragma
            | An_Export_Pragma
            | An_Import_Pragma
            | An_Inline_Pragma
            | A_Priority_Specific_Dispatching_Pragma
            =>
            -- Named parameters allowed, or name may designate several entities.
            Level_Delta := 1;
         when An_Assertion_Policy_Pragma
            | A_Convention_Pragma
            | A_List_Pragma
            | A_Locking_Policy_Pragma
            | An_Optimize_Pragma
            | A_Partition_Elaboration_Policy_Pragma
            | A_Profile_Pragma
            | A_Queuing_Policy_Pragma
            | A_Restrictions_Pragma
            | A_Suppress_Pragma
            | A_Task_Dispatching_Policy_Pragma
            | An_Unsuppress_Pragma
            =>
            -- Some parameters are special names
            Level_Delta := 1;
         when An_Implementation_Defined_Pragma
            | An_Unknown_Pragma
            =>
            -- Who knows?
            Level_Delta := 1;
         when Not_A_Pragma =>
            Failure ("Not_A_Pragma in Traverse_Pragma", Element);
      end case;

      State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level + Level_Delta;
      for A in Associations'Range loop
         Semantic_Traverse_Elements (Associations (A), Control, State);
         case Control is
            when Continue =>
               null;
            when Terminate_Immediately =>
               State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level - Level_Delta;
               return;
            when Abandon_Children =>
               Failure ("Ruler: Semantic_Traverse returned Abandon_Children-2");
            when Abandon_Siblings =>
               Control := Continue;
               State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level - Level_Delta;
               return;
         end case;
      end loop;
      State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level - Level_Delta;

   end Traverse_Pragma;


   ---------------------------
   -- Traverse_With_Private --
   ---------------------------

   -- This is used to traverse manually the children of an element that includes a private part,
   -- in order to separate the private part from the visible one and call Enter_Private_Part in between.
   -- Note that Enter_Private_Part is called even if there is no private part.
   -- This replaces then normal (recursive) traversal, any code that calls this procedure must
   -- set Control to Abandon_Children
   procedure Traverse_With_Private (Visible_Part : in     Asis.Declarative_Item_List;
                                    Private_Part : in     Asis.Declarative_Item_List;
                                    Control      : in out Asis.Traverse_Control;
                                    State        : in out Info)
   is
      use Asis, Utilities;
   begin

      for I in Visible_Part'Range loop
         Semantic_Traverse_Elements (Visible_Part (I), Control, State);
         case Control is
            when Continue =>
               null;
            when Terminate_Immediately =>
               return;
            when Abandon_Children =>
               Failure ("Ruler: Semantic_Traverse returned Abandon_Children-1");
            when Abandon_Siblings =>
               Control := Continue;
               return;
         end case;
      end loop;

      Scope_Manager.Enter_Private_Part;

      for I in Private_Part'Range loop
         Semantic_Traverse_Elements (Private_Part (I), Control, State);
         case Control is
            when Continue =>
               null;
            when Terminate_Immediately =>
               return;
            when Abandon_Children =>
               Failure ("Ruler: Semantic_Traverse returned Abandon_Children-2");
            when Abandon_Siblings =>
               Control := Continue;
               return;
         end case;
      end loop;
   end Traverse_With_Private;


   -------------------
   -- Traverse_Body --
   -------------------

   -- This is used to traverse manually the children of an unit body, in order to be able to call
   -- Enter_Statement_List between the declarative parts and the statements.
   -- This replaces the normal (recursive) traversal, any code that calls this procedure must set
   -- Control to Abandon_Children
   procedure Traverse_Body (Decl         : Asis.Declaration;
                            Control      : in out Asis.Traverse_Control;
                            State        : in out Info )
   is
      use Asis, Asis.Declarations, Asis.Elements;
      use Thick_Queries, Utilities;

      Body_Kind : constant Declaration_Kinds := Declaration_Kind (Decl);
      -- can only be:
      -- A_Procedure_Body_Declaration, A_Function_Body_Declaration, A_Package_Body_Declaration,
      -- A_Task_Body_Declaration,      An_Entry_Body_Declaration
      -- All with subtly different parts to traverse...
      Elem : Asis.Element;
   begin

      -- Names: for everybody
      -- No need to be over-rigorous, we know damn well that bodies have only one name
      Semantic_Traverse_Elements (Names (Decl)(1), Control, State);
      case Control is
         when Continue =>
            null;
         when Terminate_Immediately =>
            return;
         when Abandon_Children =>
            Failure ("Ruler: Semantic_Traverse returned Abandon_Children-3");
         when Abandon_Siblings =>
            Control := Continue;
            return;
      end case;

      -- Entry_Index_Specification: only for entries
      -- Beware: optional, do not traverse Nil_Element!
      if Body_Kind = An_Entry_Body_Declaration then
         Elem := Entry_Index_Specification (Decl);
         if not Is_Nil (Elem) then
            Semantic_Traverse_Elements (Elem, Control, State);
            case Control is
               when Continue =>
                  null;
               when Terminate_Immediately =>
                  return;
               when Abandon_Children =>
                  Failure ("Ruler: Semantic_Traverse returned Abandon_Children-4");
               when Abandon_Siblings =>
                  Control := Continue;
                  return;
            end case;
         end if;
      end if;

      -- Parameter profile: only for procedures, functions and entries
      case Body_Kind is
         when A_Procedure_Body_Declaration | A_Function_Body_Declaration | An_Entry_Body_Declaration =>
            declare
               Parms : constant Asis.Parameter_Specification_List := Parameter_Profile (Decl);
            begin
               for P in Parms'Range loop
                  Semantic_Traverse_Elements (Parms (P), Control, State);
                  case Control is
                     when Continue =>
                        null;
                     when Terminate_Immediately =>
                        return;
                     when Abandon_Children =>
                        Failure ("Ruler: Semantic_Traverse returned Abandon_Children-5");
                     when Abandon_Siblings =>
                        Control := Continue;
                        return;
                  end case;
               end loop;
            end;
         when others =>
            null;
      end case;

      -- Entry_Barrier: only for entries
      if Body_Kind = An_Entry_Body_Declaration then
         Semantic_Traverse_Elements (Entry_Barrier (Decl), Control, State);
         case Control is
            when Continue =>
               null;
            when Terminate_Immediately =>
               return;
            when Abandon_Children =>
               Failure ("Ruler: Semantic_Traverse returned Abandon_Children-6");
            when Abandon_Siblings =>
               Control := Continue;
               return;
         end case;
      end if;

      -- Result profile: only for functions
      if Body_Kind = A_Function_Body_Declaration then
         Semantic_Traverse_Elements (Result_Profile (Decl), Control, State);
         case Control is
            when Continue =>
               null;
            when Terminate_Immediately =>
               return;
            when Abandon_Children =>
               Failure ("Ruler: Semantic_Traverse returned Abandon_Children-7");
            when Abandon_Siblings =>
               Control := Continue;
               return;
         end case;
      end if;

      -- Aspects: for everybody
      declare
         Aspects : constant Asis.Definition_List := Aspect_Specifications (Decl);
      begin
         for A in Aspects'Range loop
            Semantic_Traverse_Elements (Aspects (A), Control, State);
            case Control is
               when Continue =>
                  null;
               when Terminate_Immediately =>
                  return;
               when Abandon_Children =>
                  Failure ("Ruler: Semantic_Traverse returned Abandon_Children-8");
               when Abandon_Siblings =>
                  Control := Continue;
                  return;
            end case;
         end loop;
      end;

      -- Declarative part : for everybody
      declare
         Decls : constant Asis.Declaration_List := Declarative_Items (Decl, Include_Pragmas => True);
      begin
         for D in Decls'Range loop
            Semantic_Traverse_Elements (Decls (D), Control, State);
            case Control is
               when Continue =>
                  null;
               when Terminate_Immediately =>
                  return;
               when Abandon_Children =>
                  Failure ("Ruler: Semantic_Traverse returned Abandon_Children-8");
               when Abandon_Siblings =>
                  Control := Continue;
                  return;
            end case;
         end loop;
      end;

      Framework.Plugs.         Enter_Statement_List (Decl);
      Framework.Specific_Plugs.Enter_Statement_List (Decl);

      -- Statements : for everybody
      declare
         Stmts : constant Asis.Statement_List := Statements (Decl);
      begin
         for S in Stmts'Range loop
            Semantic_Traverse_Elements (Stmts (S), Control, State);
            case Control is
               when Continue =>
                  null;
               when Terminate_Immediately =>
                  return;
               when Abandon_Children =>
                  Failure ("Ruler: Semantic_Traverse returned Abandon_Children-9");
               when Abandon_Siblings =>
                  Control := Continue;
                  return;
            end case;
         end loop;
      end;

      -- Exception_Handlers : for everybody
      declare
         Handlers : constant Asis.Exception_Handler_List := Exception_Handlers (Decl);
      begin
         for H in Handlers'Range loop
            Semantic_Traverse_Elements (Handlers (H), Control, State);
            case Control is
               when Continue =>
                  null;
               when Terminate_Immediately =>
                  return;
               when Abandon_Children =>
                  Failure ("Ruler: Semantic_Traverse returned Abandon_Children-10");
               when Abandon_Siblings =>
                  Control := Continue;
                  return;
            end case;
         end loop;
      end;
   end Traverse_Body;


   ----------------------
   -- Textual_Traverse --
   ----------------------

   procedure Textual_Traverse (Unit : Asis.Compilation_Unit) is
      use Asis, Asis.Compilation_Units, Asis.Declarations, Asis.Elements, Asis.Text;
      use Ada.Strings.Wide_Fixed, Rules_Manager, Utilities;
      Element_From_Unit : constant Asis.Element := Unit_Declaration (Unit);
      Unit_Lines : constant Line_List := Lines (Element  => Element_From_Unit,
                                                The_Span => Compilation_Span (Element_From_Unit));
      File_Name : constant Wide_String := Get_File_Name (Get_Location (Element_From_Unit));
   begin
      if not Has_Active_Rules (Semantic) then
         Reports.Total_Lines := Reports.Total_Lines + Unit_Lines'Length;
      end if;

      Framework.Plugs.         Text_Enter_Unit (Unit);
      Framework.Specific_Plugs.Text_Enter_Unit (Unit);
      for I in Unit_Lines'Range loop
         declare
            Line : constant Asis.Program_Text := Line_Image (Unit_Lines (I));
            Loc  : constant Location          := Create_Location (File_Name, I, 1);
         begin
            Framework.Plugs.         Text_Analysis (Line, Loc);
            Framework.Specific_Plugs.Text_Analysis (Line, Loc);
         end;
      end loop;

      -- If there are no semantic rules, subunits have not been traversed, therefore we must do it here.
      -- Find possible stubs. Since stubs are always declared at first level, there is
      -- no need to do a full traversal.
      if not Has_Active_Rules (Semantic)
        and then Unit_Kind (Unit) in A_Procedure_Body .. A_Task_Body_Subunit
        -- All *_body and *_body_subunit, except a_protected_body_subunit, since it cannot have stubs
      then
         declare
            Declaration_List : constant Declarative_Item_List := Body_Declarative_Items (Unit_Declaration (Unit));
         begin
            for I in Declaration_List'Range loop
               if Declaration_Kind (Declaration_List (I)) in A_Body_Stub then
                  Stub_Nesting := Stub_Nesting + 1;
                  declare
                     Proper_Body : constant Asis.Declaration := Corresponding_Subunit (Declaration_List (I));
                     Stub_Name   : constant Wide_String      := Defining_Name_Image (Names (Declaration_List (I)) (1));
                     Stub_Unit   :  Asis.Compilation_Unit;
                  begin
                     if Is_Nil (Proper_Body) then
                        User_Log (3 * Stub_Nesting * ' '
                                  & "Controlling separate "
                                  & Stub_Name
                                  & " ... not found");
                        Rules.Uncheckable.Process_Missing_Unit ("missing proper body for " & Stub_Name);
                     else
                        Stub_Unit := Enclosing_Compilation_Unit (Proper_Body);
                        Process_Inhibition (Stub_Unit, Suspend);
                        User_Log (3 * Stub_Nesting * ' '
                                  & "Controlling separate "
                                  & Stub_Name);

                        Textual_Traverse  (Stub_Unit);

                        User_Log (3 * Stub_Nesting * ' ' & "returning");
                        Process_Inhibition (Stub_Unit, Resume);
                     end if;
                  end;
                  Stub_Nesting := Stub_Nesting - 1;
               end if;
            end loop;
         end;
      end if;
   end Textual_Traverse;

   -------------------
   -- Pre_Procedure --
   -------------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Info)
   is
      use Ada.Characters.Handling, Ada.Exceptions;
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;

      -- Declarations to be added in ASIS 2012:
      subtype A_Statement_Path   is Path_Kinds range An_If_Path .. A_Then_Abort_Path;
      subtype An_Expression_Path is Path_Kinds range A_Case_Expression_Path .. An_Else_Expression_Path;
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Declaration
                  | An_Expression_Function_Declaration   -- Ada 2012
                  | A_Procedure_Declaration
                  | A_Null_Procedure_Declaration
                  | An_Entry_Declaration
                  | A_Task_Type_Declaration
                  | A_Single_Task_Declaration
                  | A_Protected_Type_Declaration
                  | A_Single_Protected_Declaration

                  | A_Protected_Body_Declaration
                  | A_Body_Stub

                  | A_Generic_Procedure_Declaration
                  | A_Generic_Function_Declaration

                  | A_Formal_Procedure_Declaration
                  | A_Formal_Function_Declaration

                  | A_Procedure_Renaming_Declaration
                  | A_Function_Renaming_Declaration
                  | A_Package_Renaming_Declaration
                  | A_Generic_Procedure_Renaming_Declaration
                  | A_Generic_Function_Renaming_Declaration
                  | A_Generic_Package_Renaming_Declaration

                  | A_Package_Instantiation
                  | A_Procedure_Instantiation
                  | A_Function_Instantiation
                 =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
                  Enter_Scope (Element);

               when A_Procedure_Body_Declaration
                  | A_Function_Body_Declaration
                  | A_Package_Body_Declaration
                  | A_Task_Body_Declaration
                  | An_Entry_Body_Declaration
                  =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

                  Enter_Scope (Element);
                  Traverse_Body (Element, Control, State);

                  -- Post-procedure is not automatically called when exiting
                  -- with Control = Abandon_Children:
                  Post_Procedure (Element, Control, State);
                  Control := Abandon_Children;

               when A_Package_Declaration => -- Thing that can have a private part
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

                  Enter_Scope (Element);
                  Traverse_With_Private (Names(Element)(1)
                                         & Visible_Part_Declarative_Items (Element, Include_Pragmas => True),
                                         Private_Part_Declarative_Items (Element, Include_Pragmas => True),
                                         Control,
                                         State);

                  -- Post-procedure is not automatically called when exiting
                  -- with Control = Abandon_Children:
                  Post_Procedure (Element, Control, State);
                  Control := Abandon_Children;

               when A_Generic_Package_Declaration => -- Thing that can have a private part
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

                  Enter_Scope (Element);
                  Traverse_With_Private (Generic_Formal_Part (Element, Include_Pragmas => True)
                                         & Names(Element)(1)
                                         & Visible_Part_Declarative_Items (Element, Include_Pragmas => True),
                                         Private_Part_Declarative_Items (Element, Include_Pragmas => True),
                                         Control,
                                         State);

                  -- Post-procedure is not automatically called when exiting
                  -- with Control = Abandon_Children:
                  Post_Procedure (Element, Control, State);
                  Control := Abandon_Children;

               when others =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
            end case;

         when A_Definition =>
            case Definition_Kind (Element) is
               when A_Task_Definition   -- Things that can have a private part
                 | A_Protected_Definition
                 =>
                  Traverse_With_Private (Visible_Part_Items (Element, Include_Pragmas => True),
                                         Private_Part_Items (Element, Include_Pragmas => True),
                                         Control,
                                         State);

                  -- Post-procedure is not automatically called when exiting
                  -- with Control = Abandon_Children:
                  Post_Procedure (Element, Control, State);
                  Control := Abandon_Children;

               when An_Aspect_Specification =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

                  -- Traverse manually, because we want to inhibit True_Identifier for
                  -- the aspect mark; note that some identifiers of the aspect definition
                  -- may not be true identifiers (Convention => C)
                  State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level + 1;
                  Semantic_Traverse_Elements (Aspect_Mark (Element), Control, State);
                  if not Is_Nil (Aspect_Definition (Element)) then -- can be Nil for aspect such as "with Pack;"
                     Semantic_Traverse_Elements (Aspect_Definition (Element), Control, State);
                  end if;
                  State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level - 1;
                  if Control /= Terminate_Immediately then
                     Control := Abandon_Children;
                  end if;

               when others =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
            end case;

         when An_Exception_Handler =>
            Framework.Plugs.         Pre_Procedure (Element);
            Framework.Specific_Plugs.Pre_Procedure (Element);
            Enter_Scope (Element);

            Framework.Plugs.         Enter_Statement_List (Element);
            Framework.Specific_Plugs.Enter_Statement_List (Element);

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_For_Loop_Statement     -- Statements with declarations and statement list
                  | A_Block_Statement
                  | An_Extended_Return_Statement
                  | An_Accept_Statement
                  =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
                  Enter_Scope (Element);

                  Framework.Plugs.         Enter_Statement_List (Element);
                  Framework.Specific_Plugs.Enter_Statement_List (Element);

               when A_Loop_Statement       -- Statements with statement list only
                  | A_While_Loop_Statement
                    =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

                  Framework.Plugs.         Enter_Statement_List (Element);
                  Framework.Specific_Plugs.Enter_Statement_List (Element);

               when others =>     -- Statements with paths and non compound statements
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
            end case;

         when A_Path =>
            Framework.Plugs.         Pre_Procedure (Element);
            Framework.Specific_Plugs.Pre_Procedure (Element);

            if Path_Kind (Element) in A_Statement_Path then
               Framework.Plugs.         Enter_Statement_List (Element);
               Framework.Specific_Plugs.Enter_Statement_List (Element);
            end if;

         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Attribute_Reference =>
                  -- don't consider attributes like Pre'Class as "true" attributes
                  if Is_True_Identifier (Prefix (Element), State) then
                     Framework.Plugs.         Pre_Procedure (Element);
                     Framework.Specific_Plugs.Pre_Procedure (Element);

                     -- Traverse manually, because we want to inhibit True_Identifier for
                     -- the attribute designator, not for the prefix
                     Semantic_Traverse_Elements (Prefix (Element), Control, State);
                     State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level + 1;
                     Semantic_Traverse_Elements (Attribute_Designator_Identifier (Element), Control, State);
                     State.Pragma_Or_Attribute_Level := State.Pragma_Or_Attribute_Level - 1;
                     case Attribute_Kind (Element) is
                        when A_First_Attribute
                           | A_Last_Attribute
                           | A_Length_Attribute
                           | A_Range_Attribute
                           | An_Implementation_Defined_Attribute
                           | An_Unknown_Attribute
                           =>
                           declare
                              Expressions : constant Asis.Expression_List := Attribute_Designator_Expressions (Element);
                           begin
                              for E in Expressions'Range loop
                                 Semantic_Traverse_Elements (Expressions (E), Control, State);
                              end loop;
                           end;
                        when others =>
                           null;
                     end case;
                     if Control /= Terminate_Immediately then
                        Control := Abandon_Children;
                     end if;
                  end if;

               when An_Identifier
                 | An_Operator_Symbol
                 | An_Enumeration_Literal
                  =>
                  if Is_True_Identifier (Element, State) then
                     Framework.Plugs.         True_Identifier (Element);
                     Framework.Specific_Plugs.True_Identifier (Element);
                  end if;

                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);

               when others =>
                  Framework.Plugs.         Pre_Procedure (Element);
                  Framework.Specific_Plugs.Pre_Procedure (Element);
            end case;

         when A_Pragma =>
            Framework.Plugs.         Pre_Procedure (Element);
            Framework.Specific_Plugs.Pre_Procedure (Element);

            -- Traverse manually pragma arguments
            Traverse_Pragma (Element, Control, State);
            if Control /= Terminate_Immediately then
               Control := Abandon_Children;
            end if;

         when others =>
            Framework.Plugs.         Pre_Procedure (Element);
            Framework.Specific_Plugs.Pre_Procedure (Element);
      end case;

   exception
      when Utilities.User_Error =>             -- Call to Parameter_Error while traversing => propagate silently
         raise;
      when Framework.Reports.Cancellation =>   -- Too many messages while traversing => propagate silently
         raise;
      when Occur: others =>
         Utilities.Trace ("Exception "                                  --## rule line off No_Trace
                          & To_Wide_String (Exception_Name (Occur))
                          & " in Pre_Procedure at "
                          & Safe_Image (Get_Location (Element)),
                          Element,
                          With_Source => True);
         raise;
   end Pre_Procedure;

   --------------------
   -- Post_Procedure --
   --------------------

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info)
   is
      pragma Unreferenced (Control, State);
      use Ada.Characters.Handling, Ada.Exceptions, Ada.Strings.Wide_Fixed;
      use Asis, Asis.Declarations, Asis.Elements;
      use Rules_Manager, Utilities;
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Procedure_Declaration   -- Any change to this list must be reflected in Framework.Symbol_Table
                  | A_Null_Procedure_Declaration
                  | A_Function_Declaration
                  | An_Expression_Function_Declaration   -- Ada 2012
                  | An_Entry_Declaration
                  | A_Package_Declaration
                  | A_Task_Type_Declaration
                  | A_Single_Task_Declaration
                  | A_Protected_Type_Declaration
                  | A_Single_Protected_Declaration

                  | A_Procedure_Body_Declaration
                  | A_Function_Body_Declaration
                  | An_Entry_Body_Declaration
                  | A_Package_Body_Declaration
                  | A_Task_Body_Declaration
                  | A_Protected_Body_Declaration

                  | A_Generic_Procedure_Declaration
                  | A_Generic_Function_Declaration
                  | A_Generic_Package_Declaration

                  | A_Formal_Procedure_Declaration
                  | A_Formal_Function_Declaration

                  | A_Procedure_Renaming_Declaration
                  | A_Function_Renaming_Declaration
                  | A_Package_Renaming_Declaration
                  | A_Generic_Package_Renaming_Declaration
                  | A_Generic_Procedure_Renaming_Declaration
                  | A_Generic_Function_Renaming_Declaration

                  | A_Procedure_Instantiation
                  | A_Function_Instantiation
                  | A_Package_Instantiation
                    =>
                  Exit_Scope (Element);
                  Framework.Plugs.         Post_Procedure (Element);
                  Framework.Specific_Plugs.Post_Procedure (Element);

               when A_Body_Stub =>
                  Exit_Scope (Element);
                  Framework.Plugs.         Post_Procedure (Element);
                  Framework.Specific_Plugs.Post_Procedure (Element);

                  -- After processing of the stub itself, process the proper body at the place of the stub
                  Stub_Nesting := Stub_Nesting + 1;
                  declare
                     Proper_Body : constant Asis.Declaration := Corresponding_Subunit (Element);
                     Stub_Name   : constant Wide_String      := Defining_Name_Image (Names (Element) (1));
                  begin
                     if Is_Nil (Proper_Body) then
                        User_Log (3 * Stub_Nesting * ' '
                                  & "Controlling separate "
                                  & Stub_Name
                                  & " ... not found");
                        Rules.Uncheckable.Process_Missing_Unit ("missing proper body for " & Stub_Name);
                     else
                        User_Log (3 * Stub_Nesting * ' '
                                  & "Controlling separate "
                                  & Stub_Name);
                        declare
                           Stub_Unit : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Proper_Body);
                        begin
                           Process_Inhibition (Stub_Unit, Suspend);
                           Semantic_Traverse (Stub_Unit);
                           -- If we have both semantic and textual rules, check textual rules here
                           if Has_Active_Rules (Textual) then
                              Textual_Traverse (Stub_Unit);
                           end if;
                           Process_Inhibition (Stub_Unit, Resume);
                        end;
                        User_Log (3 * Stub_Nesting * ' ' & "returning");
                     end if;
                  end;
                  Stub_Nesting := Stub_Nesting - 1;

               when others =>
                  Framework.Plugs.         Post_Procedure (Element);
                  Framework.Specific_Plugs.Post_Procedure (Element);
            end case;

         when An_Exception_Handler =>
            Exit_Scope (Element);
            Framework.Plugs.         Post_Procedure (Element);
            Framework.Specific_Plugs.Post_Procedure (Element);

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_For_Loop_Statement
                  | A_Block_Statement
                  | An_Extended_Return_Statement
                  | An_Accept_Statement
                  =>
                  Exit_Scope (Element);
                  Framework.Plugs.         Post_Procedure (Element);
                  Framework.Specific_Plugs.Post_Procedure (Element);

              when others =>
                 Framework.Plugs.         Post_Procedure (Element);
                 Framework.Specific_Plugs.Post_Procedure (Element);
            end case;

         when others =>
            Framework.Plugs.         Post_Procedure (Element);
            Framework.Specific_Plugs.Post_Procedure (Element);
      end case;

   exception
      when Utilities.User_Error =>             -- Call to Parameter_Error while traversing => propagate silently
         raise;
      when Framework.Reports.Cancellation =>   -- Too many messages while traversing => propagate silently
         raise;
      when Occur: others =>
         Utilities.Trace ("Exception "                                  --## rule line off No_Trace
                          & To_Wide_String (Exception_Name (Occur))
                          & " in Post_Procedure at "   --## rule line off No_Trace
                          & Safe_Image (Get_Location (Element)),
                          Element,
                          With_Source => True);
         raise;
   end Post_Procedure;

   -------------
   -- Process --
   -------------

   procedure Process (Unit_Name  : in Wide_String;
                      Unit_Pos   : in Integer;
                      Spec_Only  : in Boolean;
                      Go_Count   : in Positive)
   is
      use Asis, Asis.Compilation_Units;
      use Utilities;

      --
      -- Do_Process
      --
      procedure Do_Process (My_Unit : Compilation_Unit) is
         use Framework.Rules_Manager;

      begin
         if Unit_Kind (My_Unit) in A_Subunit then
            -- Subunits are processed as part of the processing of their parent.
            -- We can find subunits here only if they have been explicitely specified,
            -- but in that case the parent has automatically been added.
            --   => do nothing, otherwise they would be processed twice.
            return;
         end if;

         Process_Inhibition (My_Unit, Suspend);

         -- Semantic rules
         if Has_Active_Rules (Semantic) then
            Semantic_Traverse (My_Unit);
         end if;

         -- Textual rules
         if Has_Active_Rules (Textual) then
            Textual_Traverse (My_Unit);
         end if;
         Enter (No_Rule);   -- Close timers

         Process_Inhibition (My_Unit, Resume);
      exception
         when others =>
            -- Do not call exit_unit to not make things worse...
            begin
               Process_Inhibition (My_Unit, Resume);
            exception
               when Occur: others =>
                  -- If we are not even able to call Process_Inhibition, trace and ignore the exception
                  -- in order to propagate the original exception (more interesting)
                  Utilities.Trace ("Exception in handler for Do_Process", Occur);    --## rule line off No_Trace
            end;
            raise;
      end Do_Process;

      function Progress_Indicator return Wide_String is
      begin
         if Units_List.Length = 1 then
            return "";
         end if;

         if Go_Count = 1 then
            return '(' & Integer_Img (Unit_Pos) & '/' & Integer_Img (Units_List.Length) & ") ";
         else
            return '(' & Integer_Img (Unit_Pos) & '/' & Integer_Img (Units_List.Length)
              & "):" & Integer_Img (Go_Count)
              & ' ';
         end if;
      end Progress_Indicator;

      Unit_Spec : Asis.Compilation_Unit;
      Unit_Body : Asis.Compilation_Unit;

   begin -- Process
      if not Spec_Only then
         -- Get the body before accessing the spec to avoid tree swapping
         -- (see Asis User's Guide about tree swapping)
         Unit_Body := Compilation_Unit_Body (Unit_Name, Framework.Adactl_Context);
      end if;

      -- Control specification
      -- If there is no explicit spec, no need to try and analyze the spec
      -- This saves useless messages about not found specifications
      case Unit_Class (Unit_Body) is
         when A_Public_Declaration_And_Body | A_Separate_Body =>
            Unit_Spec := Nil_Compilation_Unit;
         when others =>
            -- This covers Not_A_Class, which happens when there is a spec and no body
            -- (Unit_Body is a Nil_Compilation_Unit)
            Unit_Spec := Library_Unit_Declaration (Unit_Name, Framework.Adactl_Context);
            if Is_Nil (Unit_Spec) then
               User_Log (Progress_Indicator & "Controlling " & Unit_Name & " specification ... not found!");
               Rules.Uncheckable.Process_Missing_Unit ("missing specification for " & Unit_Name);
            else
               User_Log (Progress_Indicator & "Controlling " & Unit_Name & " specification");
               Do_Process (Unit_Spec);
            end if;
      end case;

      -- Control body
      if not Spec_Only
        and Unit_Kind (Unit_Spec) not in A_Generic_Unit_Instance
        and Unit_Kind (Unit_Spec) not in A_Renaming
      then
         if Is_Nil (Unit_Spec)
           or else (Unit_Kind (Unit_Spec) /= A_Package and Unit_Kind (Unit_Spec) /= A_Generic_Package)
           or else Is_Body_Required (Unit_Spec)
         then
            if Is_Nil (Unit_Body) then
               User_Log (Progress_Indicator & "Controlling " & Unit_Name & " body ... not found!");
               Rules.Uncheckable.Process_Missing_Unit ("missing body for " & Unit_Name);
            else
               User_Log (Progress_Indicator & "Controlling " & Unit_Name & " body");
               Do_Process (Unit_Body);
            end if;
         end if;
      end if;
   end Process;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Stub_Nesting := 0;
   end Reset;

end Framework.Ruler;
