----------------------------------------------------------------------
--  Rules.Potentially_Blocking_Operations - Package body            --
--                                                                  --
--  This module  is (c) BelgoControl and Adalog 2004-2005.          --
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
  Ada.Strings.Wide_Unbounded;

-- Asis
with
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;

-- Adalog
with
  Binary_Map,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Adactl_Options,
  Framework.Language,
  Framework.Element_Queues,
  Framework.Queries,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Potentially_Blocking_Operations is
   use Framework;

   -- Algorithm
   --
   -- The SP_Property map keeps whether a given subprogram is potentially blocking.
   -- It is initialized with SP from the standard library known to be potentially blocking.
   -- Since being potentially blocking is a property of the SP (independently of where it is
   -- called from) the map is never cleared, thus avoiding the same SP from being traversed
   -- twice (even from different runs).
   --
   -- In the case where a SP is an instantiation, we analyse the corresponding generic and keep it
   -- SP_Property too. This way we do not need to re-analyze every instantiation, but more
   -- importantly, this allows all instantiations from the generics in the IO library to be
   -- recognized as potentially blocking.
   --
   -- Note: PTO below is an abbreviation for Protected Type or Object

   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Rule_Kind  : Control_Kinds;
   Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type SP_Property_Record is
      record
         Is_Blocking    : Boolean;
         Referenced_PTO : Framework.Element_Queues.Queue;
         -- A queue of Protected Types or Objects referenced directly or indirectly
         -- from the SP
      end record;

   package SP_Property_Map is new Binary_Map (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
                                              SP_Property_Record,
                                              Ada.Strings.Wide_Unbounded."<",
                                              Ada.Strings.Wide_Unbounded.">");
   SP_Property : SP_Property_Map.Map;
   SP_Property_Initialized : Boolean := False;


   ----------------------------
   -- Initialize_SP_Property --
   ----------------------------

   procedure Initialize_SP_Property is
   -- Initialize SP_Property map with known potentially blocking subprograms
      use Ada.Strings.Wide_Unbounded;

      procedure Add_SP (Name : Unbounded_Wide_String) is
         use SP_Property_Map, Framework.Element_Queues;
      begin
         Add (SP_Property, Name, (Is_Blocking => True, Referenced_PTO => Empty_Queue));
      end Add_SP;

      procedure Add_Unit (Name : Wide_String) is
         use Asis.Declarations, Asis.Elements, Asis.Compilation_Units;
         Unit : constant Asis.Compilation_Unit := Library_Unit_Declaration (Name, Framework.Adactl_Context);

         function Is_Recognized (Decl : Asis.Declaration; SP_Name, First_Param : Wide_String) return Boolean is
            use Asis, Utilities;
         begin
            if To_Upper (Defining_Name_Image (Names (Decl)(1))) /= SP_Name then
               return False;
            end if;

            declare
               Formals : constant Asis.Parameter_Specification_List := Parameter_Profile (Decl);
            begin
               if Formals = Nil_Element_List then
                  return False;
               end if;

               return To_Upper (Defining_Name_Image (Names (Formals (1))(1))) = First_Param;
            end;
         end Is_Recognized;

         procedure Traverse_Declarations (Decls : in Asis.Declaration_List) is
            use Asis, Framework.Queries;

         begin  -- Traverse_Declarations
            for Decl : Asis.Declaration of Decls loop
               case Declaration_Kind (Decl) is
                  when A_Procedure_Declaration | A_Function_Declaration =>
                     -- All non potentially blocking operations from IO packages are either
                     -- Get with a first parameter named From, or Put with a first parameter named To
                     if    not Is_Recognized (Decl, SP_Name => "GET", First_Param => "FROM")
                       and not Is_Recognized (Decl, SP_Name => "PUT", First_Param => "TO")
                     then
                        Add_SP (To_Key_Upper (Names (Decl)(1)));
                     end if;
                  when A_Package_Declaration | A_Generic_Package_Declaration =>
                     Traverse_Declarations (Visible_Part_Declarative_Items (Decl));
                  when others =>
                     null;
               end case;
            end loop;
         end Traverse_Declarations;

      begin   -- Add_Unit
         if Is_Nil (Unit) then
            -- Unit can be Nil if it is not in the context.
            -- In GNAT, this happens for standard units that are not referenced
            -- For other compilers, this will (also) happen for the "false" child
            -- packages of Text_IO (see below).
            return;
         end if;

         Traverse_Declarations (Visible_Part_Declarative_Items (Unit_Declaration (Unit)));
      end Add_Unit;

   begin  -- Initialize_SP_Property
      -- Do not initialize if -C option (we have no ASIS context)
      declare
         use Adactl_Options;
      begin
         if Action = Check then
            return;
         end if;
      end;

      -- Individual subprograms defined as blocking
      Add_SP (To_Unbounded_Wide_String (
              "ADA.TASK_IDENTIFICATION.ABORT_TASK{ADA.TASK_IDENTIFICATION.TASK_ID}"));
      Add_SP (To_Unbounded_Wide_String (
              "ADA.SYNCHRONOUS_TASK_CONTROL.SUSPEND_UNTIL_TRUE{ADA.SYNCHRONOUS_TASK_CONTROL.SUSPENSION_OBJECT}"));

      -- Text_IO
      Add_Unit ("ADA.TEXT_IO");
      Add_Unit ("ADA.TEXT_IO.TEXT_STREAMS");
      -- GNAT implements the generic packages inside Text_IO (and cousins) as child units.
      -- We cannot retrieve them automatically, because Corresponding_Children does not work
      -- in dynamic compilation mode.
      -- Therefore, we have to give them explicitely. There is no harm if this is ported to
      -- a different implementation, because these units won't exist, and Add_Unit will then
      -- simply return immediately.
      Add_Unit ("ADA.TEXT_IO.INTEGER_IO");
      Add_Unit ("ADA.TEXT_IO.MODULAR_IO");
      Add_Unit ("ADA.TEXT_IO.FLOAT_IO");
      Add_Unit ("ADA.TEXT_IO.FIXED_IO");
      Add_Unit ("ADA.TEXT_IO.DECIMAL_IO");
      Add_Unit ("ADA.TEXT_IO.ENUMERATION_IO");

      -- Wide_Text_IO
      Add_Unit ("ADA.WIDE_TEXT_IO");
      Add_Unit ("ADA.WIDE_TEXT_IO.TEXT_STREAMS");
      -- Same thing here
      Add_Unit ("ADA.WIDE_TEXT_IO.INTEGER_IO");
      Add_Unit ("ADA.WIDE_TEXT_IO.MODULAR_IO");
      Add_Unit ("ADA.WIDE_TEXT_IO.FLOAT_IO");
      Add_Unit ("ADA.WIDE_TEXT_IO.FIXED_IO");
      Add_Unit ("ADA.WIDE_TEXT_IO.DECIMAL_IO");
      Add_Unit ("ADA.WIDE_TEXT_IO.ENUMERATION_IO");

      -- Wide_Wide_Text_IO
      Add_Unit ("ADA.WIDE_WIDE_TEXT_IO");
      Add_Unit ("ADA.WIDE_WIDE_TEXT_IO.TEXT_STREAMS");
      -- Same thing here
      Add_Unit ("ADA.WIDE_WIDE_TEXT_IO.INTEGER_IO");
      Add_Unit ("ADA.WIDE_WIDE_TEXT_IO.MODULAR_IO");
      Add_Unit ("ADA.WIDE_WIDE_TEXT_IO.FLOAT_IO");
      Add_Unit ("ADA.WIDE_WIDE_TEXT_IO.FIXED_IO");
      Add_Unit ("ADA.WIDE_WIDE_TEXT_IO.DECIMAL_IO");
      Add_Unit ("ADA.WIDE_WIDE_TEXT_IO.ENUMERATION_IO");

      -- Other IO packages
      Add_Unit ("ADA.SEQUENTIAL_IO");
      Add_Unit ("ADA.DIRECT_IO");
      Add_Unit ("ADA.STREAMS.STREAM_IO");

      -- Note that operations from Storage_IO do not operate on files, and are thus not potentially blocking

      -- Other non IO packages
      Add_Unit ("SYSTEM.RPC");
   end Initialize_SP_Property;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control calls to potentially blocking operations from protected operations");
      User_Message;
      User_Message ("Parameter(s): None");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if Rule_Used then
         Parameter_Error (Rule_Id, "this rule can be specified only once");
      end if;

      if Parameter_Exists then
         Parameter_Error (Rule_Id, "no parameter allowed");
      end if;

      Rule_Used  := True;
      Rule_Kind  := Ctl_Kind;
      Rule_Label := To_Unbounded_Wide_String (Ctl_Label);

      -- We cannot initialize SP_Property at package elaboration time,
      -- because the context is not yet open.
      -- Therefore, we do it when the rule is registered, with the (small)
      -- additional benefit that there is no useless initialization if the
      -- rule is not used.
      if not SP_Property_Initialized then
         Initialize_SP_Property;
         SP_Property_Initialized := True;
      end if;
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


   -------------
   -- Prepare --
   -------------

   procedure Prepare is
      use SP_Property_Map;
   begin
      Balance (SP_Property);
   end Prepare;

   -----------
   -- Check --
   -----------

   -- Forward declaration
   procedure Check (Entity_Decl    :     Asis.Declaration;
                    PTO_Def        :     Asis.Definition;
                    Is_Blocking    : out Boolean;
                    Referenced_PTO : out Framework.Element_Queues.Queue);

   type Info is
      record
         PTO_Def        : Asis.Definition;  -- See body of Check
         Is_Blocking    : Boolean;
         Referenced_PTO : Framework.Element_Queues.Queue;
      end record;
   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Info)
   is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Ada.Strings.Wide_Unbounded, Thick_Queries, Framework.Element_Queues, Framework.Locations, Framework.Reports;

      Is_Blocking    : Boolean;
      Referenced_PTO : Queue;

      procedure Set_State (To : Boolean; Message : Wide_String) is
      begin
         State.Is_Blocking := State.Is_Blocking or To;
         if To and not Is_Nil (State.PTO_Def) then
            Report (Rule_Id,
                    To_Wide_String (Rule_Label),
                    Rule_Kind,
                    Get_Location (Element),
                    Message);
         end if;
      end Set_State;

      procedure Add_External_Calls is
         use Utilities;
         Target  : constant Asis.Expression := External_Call_Target (Element);
         Current : Framework.Element_Queues.Cursor;
      begin
         -- Update referenced PTO
         Append (State.Referenced_PTO, Referenced_PTO);

         if not Is_Nil (Target) then
            -- This call is itself an external call
            Append (State.Referenced_PTO, Target);
            if Is_Equal (Ultimate_Expression_Type (Target), State.PTO_Def) then
               Report (Rule_Id,
                       To_Wide_String (Rule_Label),
                       Rule_Kind,
                       Get_Location (Element),
                       Choose (Declaration_Kind (Enclosing_Element (State.PTO_Def)) = A_Protected_Type_Declaration,
                               "possible ",
                               "")
                       & "external call or requeue to same object");
               State.Is_Blocking := True;
            end if;
        end if;

         -- Check if the enclosing PTO (if any) is part of the refenced PTOs, and thus
         -- potentially blocking due to an external call to itself
         if not Is_Nil (State.PTO_Def) then
            Current := First (Referenced_PTO);
            while Has_Element (Current) loop
               if Is_Equal (Ultimate_Expression_Type (Fetch (Current)), State.PTO_Def) then
                  Report (Rule_Id,
                          To_Wide_String (Rule_Label),
                          Rule_Kind,
                          Get_Location (Element),
                          Choose (Declaration_Kind (Enclosing_Element (State.PTO_Def)) = A_Protected_Type_Declaration,
                                  "possible ",
                                  "")
                          & "external call or requeue to same object from "
                          & Image (Get_Location (Fetch (Current))));
                  State.Is_Blocking := True;
               end if;
               Current := Next (Current);
            end loop;
         end if;

      end Add_External_Calls;

      function Pretty_Name (S : Asis.Statement) return Wide_String is
         Result : Wide_String := Statement_Kinds'Wide_Image (Statement_Kind (S));
         Start  : Positive;
         Stop   : Positive;
      begin
         -- Get rid of A_ or An_
         if Result (2) = '_' then
            Start := 3;
         else
            Start := 4;
         end if;

         -- Get rid of _STATEMENT
         Stop := Result'Last - 10;

         -- Get rid of underscores
         for C : Wide_Character of Result (Start .. Stop) loop
            if C = '_' then
               C := ' ';
            end if;
         end loop;
         return Result (Start .. Stop);
      end Pretty_Name;

      procedure Process_Call (Kind : Wide_String) is
         Called : constant Call_Descriptor := Corresponding_Call_Description (Element);
      begin
         case Called.Kind is
            when A_Regular_Call =>
               if Declaration_Kind (Called.Declaration)
                  in A_Formal_Procedure_Declaration .. A_Formal_Function_Declaration
               then
                  -- TBSL As long as we don't traverse instantiated bodies, consider these as dynamic => Uncheckable
                  Uncheckable (Rule_Id,
                               False_Negative,
                               Get_Location (Element),
                               "Call to formal generic subprogram; assuming not blocking");
               else
                  Check (Called.Declaration,
                         PTO_Def        => Nil_Element,
                         Is_Blocking    => Is_Blocking,
                         Referenced_PTO => Referenced_PTO);
                  Set_State (Is_Blocking,
                             "call of potentially blocking " & Kind & ' '
                             & Full_Name_Image (Names (Called.Declaration) (1)));
               end if;
            when An_Enumeration_Literal | A_Predefined_Entity_Call | An_Attribute_Call =>
               -- Assumed never potentially blocking
               null;
            when A_Dereference_Call | A_Dispatching_Call =>
               -- Assume not blocking short of knowing what it is
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Element),
                            "Dispatching call or call to dynamic entity; assuming not blocking");
         end case;
         Add_External_Calls;
      end Process_Call;

   begin  -- Pre_Procedure
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Single_Task_Declaration =>
                  Set_State (True, "task declaration");
                  Control := Abandon_Children;
               when A_Constant_Declaration | A_Variable_Declaration =>
                  declare
                     Def : constant Asis.Definition := Object_Declaration_View (Element);
                     St_Name : Asis.Expression;
                  begin
                     if Definition_Kind (Def) = A_Subtype_Indication then
                        St_Name := Subtype_Simple_Name (Def);
                        if Is_Class_Wide_Subtype (St_Name) then
                           -- We cannot know what is actually inside, but it can contain a task only if it is limited
                           if Is_Limited (St_Name) then
                              Uncheckable (Rule_Id,
                                           False_Positive,
                                           Get_Location (Element),
                                           "limited class-wide object");
                              Set_State (True, "possible task declaration");
                           end if;
                        elsif Contains_Type_Declaration_Kind (Corresponding_Name_Declaration (St_Name),
                                                           A_Task_Type_Declaration)
                        then
                           Set_State (True, "task declaration");
                        end if;
                     end if;
                  end;
               when others =>
                  null;
            end case;

         when An_Expression =>
            case Expression_Kind (Element) is
               when A_Function_Call =>
                  Process_Call ("function");
               when An_Allocation_From_Subtype =>
                  if Contains_Type_Declaration_Kind (Corresponding_Name_Declaration
                                                     (Subtype_Simple_Name (Allocator_Subtype_Indication (Element))),
                                                     A_Task_Type_Declaration)
                  then
                     Set_State (True, "task creation");
                  end if;
               when An_Allocation_From_Qualified_Expression =>
                  -- Since Ada 2005, initialization of a limited type (potentially containing a task)
                  -- is possible
                  declare
                     St_Name : constant Asis.Expression := Simple_Name (Converted_Or_Qualified_Subtype_Mark
                                                                        (Allocator_Qualified_Expression (Element)));
                  begin
                     if Is_Class_Wide_Subtype (St_Name) then
                        -- We cannot know what is actually inside, but it can contain a task only if it is limited
                        if Is_Limited (St_Name) then
                           Uncheckable (Rule_Id,
                                        False_Positive,
                                        Get_Location (Element),
                                        "limited class-wide object");
                           Set_State (True, "possible task declaration");
                        end if;
                     elsif Contains_Type_Declaration_Kind (Corresponding_Name_Declaration (St_Name),
                                                           A_Task_Type_Declaration)
                     then
                        Set_State (True, "task creation");
                     end if;
                  end;
               when An_Attribute_Reference =>
                  -- If the attribute is a function, it is a predefined function => not blocking
                  -- If the attribute is a value, we don't care
                  -- => In all cases, ignore.
                  Control := Abandon_Children;
               when others =>
                  null;
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_Procedure_Call_Statement =>
                  Process_Call ("procedure");
               when An_Entry_Call_Statement =>
                  -- Always blocking
                  Set_State(True, "potentially blocking statement: " & Pretty_Name (Element));
                  Add_External_Calls;
                  Control := Abandon_Children;
               when A_Requeue_Statement =>
                  Add_External_Calls;
               when An_Accept_Statement
                 | A_Delay_Until_Statement
                 | A_Delay_Relative_Statement
                 | A_Selective_Accept_Statement
                 | A_Timed_Entry_Call_Statement
                 | A_Conditional_Entry_Call_Statement
                 | An_Asynchronous_Select_Statement
                 | An_Abort_Statement
                 =>
                  -- Always blocking
                  Set_State(True, "potentially blocking statement: " & Pretty_Name (Element));
                  Control := Abandon_Children;
               when others =>
                  null;
            end case;

         when A_Pragma =>
            -- Nothing interesting for us in pragmas...
            Control := Abandon_Children;

         when others =>
            null;
      end case;
   end Pre_Procedure;

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info)
   is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure;

   procedure Traverse is new Asis.Iterator.Traverse_Element (Info, Pre_Procedure, Post_Procedure);

   procedure Check (Entity_Decl      :     Asis.Declaration;
                    PTO_Def          :     Asis.Definition;
                    Is_Blocking      : out Boolean;
                    Referenced_PTO   : out Framework.Element_Queues.Queue)
     -- Expected declaration kinds for Decl:
     --   Any declaration or body of a callable entity
     --
     -- PTO_Def is the protected definition of the immediately enclosing protected type/object,
     -- or Nil_Element if check is not called immediately from a PTO
   is
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Element_Queues, Framework.Queries, Framework.Rules_Manager, Thick_Queries;
      use SP_Property_Map;
      Decl : Asis.Declaration := Entity_Decl;
   begin
      -- Get rid of renamings
      while Declaration_Kind (Decl) in A_Renaming_Declaration loop
         Decl := Corresponding_Name_Declaration (Simple_Name (Renamed_Entity (Decl)));
      end loop;

      if Is_Banned (Decl, Rule_Id) then
         Is_Blocking    := False;
         Referenced_PTO := Empty_Queue;
         return;
      end if;

      declare
         Decl_Name : constant Unbounded_Wide_String := To_Key_Upper (Names (Decl)(1));
         Control   : Asis.Traverse_Control;
         Body_Info : Info;
         Decl_Body : Asis.Declaration;
      begin
         if Is_Present (SP_Property, Decl_Name) and then Is_Nil (PTO_Def) then
            -- If it is present, but PTO_Def is not nil, it is a protected SP that we already
            -- traversed because it was called by some previously analyzed protected operation.
            -- We must traverse it again to get the messages.

            Is_Blocking    := Fetch (SP_Property, Decl_Name).Is_Blocking;
            Referenced_PTO := Fetch (SP_Property, Decl_Name).Referenced_PTO;
            return;
         end if;

         -- Add the entry now, to block recursion of analysis if the subprogram is recursive
         -- It is OK to mark as non potentially blocking for the moment, it will be changed
         -- later if we find something potentially blocking
         Add (SP_Property, Decl_Name, (Is_Blocking => False, Referenced_PTO => Empty_Queue));

         if Is_Part_Of_Instance (Names (Decl)(1)) then
            -- Analyze the corresponding generic instead
            -- We do not want to analyze the expanded body for two reasons:
            --  1) It avoids analyzing several times instantiations of the same generic
            --  2) It allows recognizing instantiations from the generic IO packages that
            --     are already in SP_Property
            -- Drawback: if a SP calls a generic formal SP, it is considered uncheckable
            Check (Enclosing_Element (Corresponding_Generic_Element (Names (Decl)(1))),
                   PTO_Def          => Nil_Element,
                   Is_Blocking      => Is_Blocking,
                   Referenced_PTO   => Referenced_PTO);

            -- Note the instance too
            Add (SP_Property, Decl_Name, (Is_Blocking => Is_Blocking, Referenced_PTO => Referenced_PTO));

            return;
         end if;

         if Ultimate_Origin (Decl) /= An_Application_Unit then
            -- This is from the standard library and not marked => not potentially blocking
            Is_Blocking    := False;
            Referenced_PTO := Empty_Queue;
            return;
         end if;

         -- Not already analyzed, not from instance, not from standard library
         -- Here we must check the body

         Decl_Body := Corresponding_Body (Decl);
         if Is_Nil (Decl_Body)                         -- Predefined operations, f.e. ...
           or else Element_Kind (Decl_Body) = A_Pragma -- Imported operations are not defined as potentially blocking
         then
            Is_Blocking    := False;
            Referenced_PTO := Empty_Queue;
            return;
         end if;

         Control   := Asis.Continue;
         Body_Info := (PTO_Def          => PTO_Def,
                       Referenced_PTO   => Empty_Queue,
                       Is_Blocking      => False); -- Assume not blocking until proven wrong
         Traverse (Decl_Body, Control, Body_Info);

         Add (SP_Property,
              Decl_Name,
              (Is_Blocking => Body_Info.Is_Blocking, Referenced_PTO => Body_Info.Referenced_PTO));
         Is_Blocking    := Body_Info.Is_Blocking;
         Referenced_PTO := Body_Info.Referenced_PTO;
      end;
   end Check;

   ----------------------------
   -- Process_Protected_Body --
   ----------------------------

   procedure Process_Protected_Body (Protected_Body : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Utilities, Framework.Element_Queues;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Ignored        : Boolean;
         Referenced_PTO : Queue;
         PTO_Def        : Asis.Definition;
         Good_Body      : Asis.Declaration := Protected_Body;
      begin
         -- Corresponding_Declaration does not work on separate bodies, replace
         -- by the stub in that case
         if Is_Subunit (Protected_Body) then
            Good_Body := Corresponding_Body_Stub (Protected_Body);
         end if;

         case Declaration_Kind (Corresponding_Declaration (Good_Body)) is
            when A_Single_Protected_Declaration =>
               PTO_Def   := Object_Declaration_View (Corresponding_Declaration (Good_Body));
            when A_Protected_Type_Declaration =>
               PTO_Def   := Type_Declaration_View (Corresponding_Declaration (Good_Body));
            when others =>
               Failure ("Body is not protected", Good_Body);
         end case;

         for Item : Asis.Declaration of Protected_Operation_Items (Protected_Body) loop
            case Declaration_Kind (Item) is
               when A_Procedure_Declaration | A_Function_Declaration | A_Null_Procedure_Declaration =>
                  null;
               when A_Procedure_Body_Declaration
                  | A_Function_Body_Declaration
                  | An_Expression_Function_Declaration   -- Ada 2012
                  | An_Entry_Body_Declaration
                  =>
                  Check (Item,
                         PTO_Def        => PTO_Def,
                         Is_Blocking    => Ignored,
                         Referenced_PTO => Referenced_PTO);

               when Not_A_Declaration =>
                  -- Presumably a representation clause
                  null;
               when others =>
                  Failure ("Wrong protected item", Item);
            end case;
         end loop;
      end;

   end Process_Protected_Body;

begin  -- Rules.Potentially_Blocking_Operations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Potentially_Blocking_Operations;
