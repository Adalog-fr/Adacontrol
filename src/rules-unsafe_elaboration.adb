----------------------------------------------------------------------
--  Rules.Unsafe_Elaboration - Package body                         --
--                                                                  --
--  This software is (c) Alstom and Adalog 2004-2013.               --
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
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Symbol_Table;

package body Rules.Unsafe_Elaboration is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- This rule controls only library unit (generic) packages.
   --
   -- The unit is traversed, excluding any inner subprogram.
   -- For every call or instantiation encountered:
   --    - if the called/instantiated element is outside the current unit, check that
   --      appropriate pragmas are provided for its enclosing unit
   --    - if the called subprogram is inside the current unit, traverse its body
   --    - if the called entry is inside the current unit, there is nothing to do
   --      (local tasks are traversed anyway)
   --    - if the instantiated generic is a package (forget SP), traverse the instantiated
   --      template.
   --
   -- The declaration of the current unit is passed to the traversal to allow the comparison of units.
   -- Note that the /specifications/ must be compared to avoid wrong messages, if elaboration calls a local
   -- subprogram, that is also exported by the package.
   --
   -- Single tasks are traversed normally, since they are started during elaboration.
   -- Task type specifications need to be traversed too, since they can contain expressions evaluated
   -- at elaboration time. Strictly speaking, it is necessary to traverse task type bodies only
   -- if an object of the type is created; however, this would include composite objects with
   -- a task subcomponent, possibly created by an allocator... Not worth the burden, therefore we
   -- traverse task bodies too. No big deal, since it can create only false positives in some unlikely
   -- cases.
   --
   -- Entry calls are treated like SP calls, since for dependencies, what counts is the place where
   -- the task type is declared, which is obviously the same unit as where the entry is declared.
   --
   -- Since all the interesting properties are static, there  is no point in traversing (or checking)
   -- the same element twice. A symbol table (containing nothing) keeps track of elements that have
   -- been already analyzed.
   --
   -- Note that it is /not/ necessary to check for banned units in case of recursive traversal,
   -- since we traverse only units inside the current unit.

   Rule_Used : Boolean := False;
   Save_Used : Boolean;
   Usage     : Basic_Rule_Context;

   package Analyzed is new Framework.Symbol_Table.Data_Access (Null_State);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls elaboration code of (generic) packages that may cause elaboration issues");
      User_Message;
      User_Message ("Parameter(s): none");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Framework.Language;
   begin
      if Parameter_Exists then
         Parameter_Error (Rule_Id, "No parameter allowed");
      end if;

      if Rule_Used then
         Parameter_Error (Rule_Id, "Rule can be specified only once");
      end if;

      Usage     := Basic.New_Context (Ctl_Kind, Ctl_Label);
      Rule_Used := True;
   end Add_Control;

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


   ----------------
   -- Check_Unit --
   ----------------

   procedure Check_Unit (Unit     : in Asis.Compilation_Unit;
                         For_Unit : in Asis.Compilation_Unit;
                         Name     : in Asis.Name)
   is
   -- Unit is an external unit used by elaboration calls or instantiations from For_Unit.
   -- Name is the name of the called or instantiated program unit
   -- Check appropriate pragmas.
      use Asis, Asis.Compilation_Units, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Thick_Queries;

      Has_Pragma : constant Pragma_Set := Corresponding_Pragma_Set (Names (Unit_Declaration (Unit)) (1));

      function Applicable_Context_Clauses return Context_Clause_List is
      -- For a spec: returns its context clauses
      -- For a body: returns the context clauses of the spec + the ones of the body
      begin
         if Unit_Kind (For_Unit) in A_Library_Unit_Body then
            return Context_Clause_Elements (Corresponding_Declaration (For_Unit), Include_Pragmas => True)
                 & Context_Clause_Elements (For_Unit,                             Include_Pragmas => True);
         else
            return Context_Clause_Elements (For_Unit, Include_Pragmas => True);
         end if;
      end Applicable_Context_Clauses;

   begin   -- Check_Unit
      if Unit_Origin (Unit) /= An_Application_Unit then --## rule line off Use_Ultimate_Origin ## we work on Unit here
         return;
      end if;

      if   Has_Pragma (A_Preelaborate_Pragma)
        or Has_Pragma (A_Pure_Pragma)
        or Has_Pragma (A_Shared_Passive_Pragma)
        or Has_Pragma (A_Remote_Types_Pragma)
        or Has_Pragma (A_Remote_Call_Interface_Pragma)
      then
         -- no elaboration control needed
         return;
      end if;

      for C : Asis.Context_Clause of Applicable_Context_Clauses loop
         if (        Pragma_Kind (C) = An_Elaborate_Pragma
             or else Pragma_Kind (C) = An_Elaborate_All_Pragma)
           and then
             Is_Equal (Enclosing_Compilation_Unit
                       (Corresponding_Name_Declaration
                          (Simple_Name
                             (Actual_Parameter
                                (Pragma_Argument_Associations (C) (1))))),
                       Unit)
         then
            return;
         end if;
      end loop;

      if Is_Part_Of_Instance (Name) then
         Report (Rule_Id,
                 Usage,
                 Get_Location (Unit_Declaration (For_Unit)),
                 Defining_Name_Image (Names (Unit_Declaration (Unit)) (1))
                 & " used in elaboration code through instantiation at "
                 & Image (Get_Location (Ultimate_Enclosing_Instantiation (Name)))
                 & ", needs pragma Elaborate or Elaborate_All");
      else
         Report (Rule_Id,
                 Usage,
                 Get_Location (Unit_Declaration (For_Unit)),
                 Defining_Name_Image (Names (Unit_Declaration (Unit)) (1))
                 & " used in elaboration code at " & Image (Get_Location (Name))
                 & ", needs pragma Elaborate or Elaborate_All");
      end if;
   end Check_Unit;


   --------------
   -- Traverse --
   --------------

   type Traverse_Info is
      record
         Force_Body : Boolean;
         Unit       : Asis.Compilation_Unit;
      end record;

   procedure Pre_Operation (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Traverse_Info);

   procedure Post_Operation (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Traverse_Info) is null;

   procedure Traverse is new Asis.Iterator.Traverse_Element (Traverse_Info,
                                                             Pre_Operation,
                                                             Post_Operation);


   -------------------
   -- Pre_Operation --
   -------------------

   procedure Pre_Operation (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Traverse_Info)
   is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      procedure Check_Name (Name : Asis.Name; Must_Traverse : out Boolean) is
      -- Checks that there is an elaboration pragma for the unit that contains Name
      -- Must_Traverse is true if further analysis of some body is necessary.
      -- Since what is to be traversed depends on the caller, it is better to have
      -- an "out" boolean than to traverse from inside this procedure
         use Asis.Compilation_Units;

         Name_Unit : Asis.Compilation_Unit;
      begin
         Must_Traverse := False;
         if Is_Nil (Name)                                          -- Pointer to subprogram...
           or else Expression_Kind (Name) = An_Attribute_Reference -- Attribute function
           or else Is_Nil (Corresponding_Name_Definition (Name))   -- Some predefined stuff
           or else Analyzed.Is_Present (Name)                      -- Already seen
         then
            return;
         end if;
         Analyzed.Store (Name, (null record));

         Name_Unit := Enclosing_Compilation_Unit (Corresponding_Name_Declaration (Name));

         if Is_Equal (Corresponding_Declaration (Name_Unit), Corresponding_Declaration (State.Unit)) then
            -- Internal call/instantiation
            if not Is_Part_Of_Instance (Corresponding_Name_Declaration (Name)) then
               -- Except those that are inside an expanded generic unit
               -- (dependences are checked on the instantiation) (Ticket #38)
               Must_Traverse := True;
            end if;
         else
            Check_Unit (Name_Unit, State.Unit, Name);
         end if;
      end Check_Name;

      procedure Check_Subprogram (Call : Asis.Element) is
      -- This procedure because procedure calls and function calls are treated the same
      -- (/not/ entry calls!)
         use Framework.Locations, Framework.Reports, Utilities;

         Must_Traverse : Boolean;
         Ignored       : Asis.Traverse_Control := Continue;
         Call_Descr    : Call_Descriptor;
         Called_Body   : Asis.Declaration;
      begin
         Check_Name (Called_Simple_Name (Call), Must_Traverse);
         if not Must_Traverse then
            return;
         end if;

         Call_Descr := Corresponding_Call_Description (Call);
         case Call_Descr.Kind is
            when A_Regular_Call =>
               -- Let's go to a real body (or expression)
               Called_Body := Call_Descr.Declaration;
               loop
                  case Declaration_Kind (Called_Body) is
                     when A_Procedure_Declaration
                        | A_Function_Declaration
                        | A_Generic_Procedure_Declaration
                        | A_Generic_Function_Declaration
                        | A_Procedure_Instantiation
                        | A_Function_Instantiation
                        =>
                        Called_Body := Corresponding_Body (Called_Body);
                     when An_Expression_Function_Declaration =>   -- Ada 2012
                        -- Like Analyze_Body, on the result expression
                        Traverse (Result_Expression (Called_Body), Ignored, State);
                        exit;
                     when A_Null_Procedure_Declaration =>
                        exit;
                     when A_Procedure_Body_Declaration
                        | A_Function_Body_Declaration
                        =>
                        -- A real body (at last!)
                        State.Force_Body := True;
                        Traverse (Called_Body, Ignored, State);
                        exit;
                     when A_Procedure_Body_Stub
                        | A_Function_Body_Stub
                        =>
                        Called_Body := Corresponding_Subunit (Called_Body);
                     when A_Procedure_Renaming_Declaration
                        | A_Function_Renaming_Declaration
                        =>
                        Called_Body := Simple_Name (Renamed_Entity (Called_Body));
                        if Expression_Kind (Called_Body) = An_Identifier then
                           Called_Body := Corresponding_Name_Declaration (Called_Body);
                        else
                           -- some weird construct, necessarily involving pointers to subprograms, renaming of entries..
                           Called_Body := Nil_Element;
                        end if;
                     when A_Formal_Function_Declaration
                        | A_Formal_Procedure_Declaration
                        =>
                        Called_Body := Nil_Element;
                     when Not_A_Declaration =>
                        -- this should happen only when the body is given by a pragma import
                        Assert (Element_Kind (Called_Body) = A_Pragma,
                                "Unsafe_Elaboration: not a declaration or pragma");
                        Called_Body := Nil_Element;
                     when others =>
                        Failure ("Unsafe_Elaboration: not a callable entity declaration", Called_Body);
                  end case;
                  exit when Is_Nil (Called_Body);
               end loop;

            when A_Predefined_Entity_Call
               | An_Attribute_Call
               | An_Enumeration_Literal
               =>
               null;
            when A_Dereference_Call | A_Dispatching_Call =>
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Call),
                            "Dispatching or dynamic call in elaboration code, can't check elaboration");
         end case;
      end Check_Subprogram;

      Must_Traverse : Boolean;
      Ignored       : Asis.Traverse_Control := Continue;
   begin   -- Pre_Operation
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               -- Ignore:
               -- All program units except tasks, since tasks can be started during elaboration
               -- (generic) formal parameters
               when A_Procedure_Declaration
                  | A_Generic_Procedure_Declaration

                  | A_Function_Declaration
                  | An_Expression_Function_Declaration
                  | A_Generic_Function_Declaration

                  | A_Parameter_Specification
                  | A_Formal_Declaration
                  =>
                  Control := Abandon_Children;
               when A_Protected_Body_Declaration =>
                  -- Nothing happens here at elaboration time (but specifications need to be
                  -- traversed, since there can be expressions in entry families or components)
                  Control := Abandon_Children;
               when  A_Procedure_Body_Declaration
                  | A_Function_Body_Declaration
                  =>
                  if State.Force_Body then
                     State.Force_Body := False;
                  else
                     Control := Abandon_Children;
                  end if;
               when A_Package_Instantiation =>
                  Check_Name (Simple_Name (Generic_Unit_Name (Element)), Must_Traverse);
                  if Must_Traverse then
                     Traverse (Corresponding_Declaration (Element), Ignored, State);
                     if not Is_Nil (Corresponding_Body (Element)) then
                        Traverse (Corresponding_Body (Element), Ignored, State);
                     end if;
                  end if;
               when A_Procedure_Instantiation
                  | A_Function_Instantiation
                  =>
                  Check_Name (Simple_Name (Generic_Unit_Name (Element)), Must_Traverse);
                  -- Nothing to traverse for instantiations of generic SP.
               when others =>
                  null;
            end case;

         when An_Expression =>
            case Expression_Kind (Element) is
               when A_Function_Call =>
                  Check_Subprogram (Element);
               when others =>
                  null;
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_Procedure_Call_Statement =>
                  Check_Subprogram (Element);
               when An_Entry_Call_Statement =>
                  -- Check the task [type] name,
                  Check_Name (Called_Simple_Name (Element), Must_Traverse);
                  -- But there is nothing  to traverse for entries
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end Pre_Operation;


   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Unit : in Asis.Compilation_Unit) is
      use Asis, Asis.Compilation_Units, Asis.Elements;

      Control : Traverse_Control := Continue;
      State   : Traverse_Info    := (False, Unit);
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Unit_Kind (Unit) is
         when A_Package
            | A_Generic_Package
            | A_Package_Body
            =>
            Traverse (Unit_Declaration (Unit), Control, State);
         when others =>
            null;
      end case;
   end Process_Unit;

begin  -- Rules.Unsafe_Elaboration
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Unsafe_Elaboration;
