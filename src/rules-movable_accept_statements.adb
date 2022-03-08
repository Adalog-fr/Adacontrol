----------------------------------------------------------------------
--  Rules.Movable_Accept_Statements - Package body                  --
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

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

-- Adalog
with
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Movable_Accept_Statements  is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   --   The algorithm implemented here may be decomposed in 5 steps:
   --     - First of all, we create a fictive element that would be referenced only
   --       within unmovable statements. By default, this element is considered as
   --       a dependent object and it is used for user-defined dependent objects.
   --
   --     - The second step consists in retrieving all of the `accept' corresponding
   --       entry parameters, and thus, since every statement referencing a parameter
   --       of the `accept' cannot be moved to an outer scope.
   --
   --     - Then, we process each statement of the body differently depending
   --       on the statement kind using the following procedure:
   --         ~ for a RETURN or a REQUEUE statement:
   --           * set a value to tell that the next statements are unreachable
   --           * quit the procedure
   --         ~ for any other statement, we TRAVERSE it and store each referenced
   --           identifier since it may be a dependent object.
   --           This step is very important since we need to reference every unmovable
   --           statements and since this operation requires all identifiers to check
   --           whether a statement may be movable to an outer scope.
   --           The following procedure explains how this part of the algorithm works:
   --             * First, we check whether the encountered identifier is a user-defined
   --               dependent element. When we match a user-defined dependent element,
   --               we set the LAST STATEMENT INDEX and KIND to the appropriate values.
   --             * Then, when the identifier is a parameter of the `accept' structure
   --               or an object (variable, constant, renaming), we need to store it
   --               into a kind of dependency table since variables and constants may
   --               reference directly or indirectly a parameter of the `accept'.
   --               Since a statement referencing a parameter of the `accept' statement
   --               we are processing is unmovable, we need to make a special case for
   --               identifiers that reference a parameter by just setting the LAST
   --               STATEMENT INDEX and KIND to the appropriate values.
   --             * In any case, we need to store or replace the identifier within the
   --               objects queue whether the element has already been referenced.
   --
   --   At this point, we should know the INDEX and KIND of the LAST STATEMENT that
   --   may either reference a parameter, reference a user-defined dependent object
   --   or exclude any further statements.
   --   If the index is greater than the number of statements composing the `accept'
   --   structure, it tells us that any statement may be moved to an outer as none
   --   depends on any case described here above.
   --   The specific case of an empty body SHOULD NOT REPORT an error.
   --
   --   Past this point, at least one statement matches one of the cases described
   --   above. This statement can be called THE LAST STATEMENT.
   --   We can set all statements appearing after this LAST STATEMENT as
   --   movable as they WILL NOT AFFECT the program behavior if moved out of the
   --   accept scope.
   --
   --   From here, all identifiers that have been used within each statement
   --   are known and we need only consider the identifiers of the statements
   --   appearing before the LAST STATEMENT.
   --
   --   According to the user choice, we can then (not) report any statement
   --   not referencing, directly or indirectly, a parameter of the accept.
   --
   --   If the user asked to report all statements, we need to loop over each
   --   statement until no new dependency is found (i.e. stable state). The
   --   dependency can be from a statement or an identifier to a parameter or
   --   a user-defined dependency.
   --
   --
   --   As to understand this tricky part of the algorithm, we must define the
   --   notion of DEPENDENCY UPON PARAMETER and the notion of UNMOVABILITY.
   --   The further 3 rules explain how to spot/locate every element depending
   --   upon a parameter.
   --
   --   Rule #1:
   --      A parameter is a constant (in) or a variable (in out, out) depending
   --      on itself.
   --   Rule #2:
   --      Any statement referencing a parameter, directly or indirectly, can be
   --      declared as unmovable from the scope it appears in.
   --   Rule #3:
   --      Any constant or variable that appears in a unmovable statement can be
   --      declared dependent on the parameters referenced in the statement.
   --
   --
   --   The next rule is to explain the notion of INDIRECT REFERENCE used within
   --   the above 3 rules.
   --
   --   Rule #4:
   --       A statement references indirectly a parameter when the reference is
   --       done via any variable or constant dependent on the parameter.
   --
   --
   --   At this point of the algorithm, we should have spot any dependency upon
   --   parameters and any unmovable statement.
   --
   --   We can now proceed with the 5th and last step by reporting each statement
   --   set as movable in their order of appearance depending on the report kind
   --   the user defined.
   --


   type Rule_Detail is (K_Certain, K_Possible);
   package Detail_Flags_Utilities is new Framework.Language.Flag_Utilities (Rule_Detail, "K_");

   type Usage_Flags is array (Rule_Detail) of Boolean;
   Not_Used : constant Usage_Flags := (others => False);

   Rule_Used : Usage_Flags := Not_Used;
   Save_Used : Usage_Flags;
   Usage     : array (Rule_Detail) of Basic_Rule_Context;

   -- The context store (storing non-movable entities)
   Entities  : Context_Store;


   -- A type for statements dependencies upon parameters
   type Dependency_Array is array (Asis.List_Index range <>) of Boolean;
   type Dependency_Array_Access is access all Dependency_Array;
   -- A type for objects references within statements
   type Reference_Array  is array (Asis.List_Index range <>) of Boolean;
   -- A type for objects kinds
   type Object_Kind is (Independent, Dependent, Parameter);

   -- A type to store information about referenced objects
   type Object_Information (Nb_Refs : Asis.List_Index) is
      record
         Identifier : Asis.Defining_Name;
         Kind       : Object_Kind                  := Independent;
         References : Reference_Array (1..Nb_Refs) := (others => False);
         Checked    : Boolean                      := False;
      end record;

   -- A simple enumeration type indicating the last statement kind
   type Last_Statement_Kind is (None,
                                Exclusive, Inner_Exclusive,
                                Parameter_Reference,
                                Synchronization,
                                User_Defined_Dependency);
   -- Information about last statement
   type Last_Statement_Information is
      record
         Kind  : Last_Statement_Kind;
         Index : Asis.ASIS_Natural;
      end record;


   -- Instantiation of a Linear_Queue used to store all information about referenced objects
   package Object_Queue is new Linear_Queue (Object_Information);


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
      use Detail_Flags_Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control (re)movable statements from `accept' bodies.");
      User_Message  ("Movable statements might be moved to an outer scope.");
      User_Message;
      Help_On_Flags ("Parameter(1):");
      User_Message  ("Parameter(2..): <name>");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
      use Detail_Flags_Utilities;

      Detail : Rule_Detail;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least ""Certain"" or ""Possible"" expected");
      end if;

      Detail := Get_Flag_Parameter (Allow_Any => False);

      if Rule_Used (Detail) then
         Parameter_Error (Rule_Id, "rule can be called only once for ""Certain"", ""Possible""");
      end if;

      -- Retrieve all user-defined dependencies and associate them with the appropriate detail context
      -- In our case, we just merge all user-defined dependencies into a single context
      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Entities, Entity, Null_Context, Additive => True);
         exception
            when Already_In_Store =>
               Parameter_Error (Rule_Id, "Entity already given: " & Image (Entity));
         end;
      end loop;

      Rule_Used (Detail) := True;
      Usage     (Detail) := Basic.New_Context (Ctl_Kind, Ctl_Label);
   end Add_Control;


   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := Not_Used;
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
   begin
      -- If weaker checks have been specified, force them for stronger ones
      if Rule_Used (K_Possible) and not Rule_Used (K_Certain) then
         Rule_Used (K_Certain) := True;
         Usage     (K_Certain) := Usage (K_Possible);
      end if;
   end Prepare;


   ----------------------------------
   -- Add_Fictive_Object_Reference --
   ----------------------------------

   procedure Add_Fictive_Object_Reference
     (The_Queue       : in out Object_Queue.Queue;
      Statement_Index : in Asis.List_Index)
   is
      use Object_Queue;

      Fictive_Object_Cursor : constant Cursor    := First (The_Queue);
      Fictive_Object        : Object_Information := Fetch (Fictive_Object_Cursor);
   begin
      Fictive_Object.References (Statement_Index) := True;
      Replace (Fictive_Object_Cursor, Fictive_Object);
   end Add_Fictive_Object_Reference;


   --------------
   -- Traverse --
   --------------

   -- retrieve each identifier used within the statement
   -- check if there is a direct reference to a parameter,
   --  and set the Last_Statement value accordingly
   -- store every encountered identifier that matches an object definition

   type State_Information is
      record
         Current_Statement    : Asis.ASIS_Natural;
         Number_Of_Statements : Asis.ASIS_Natural;
         Last_Statement       : Last_Statement_Information;
         References_Queue     : Object_Queue.Queue;
         Movable_Statements   : Dependency_Array_Access;
      end record;

   -- forward declarations due to recursive traversal
   procedure Pre_Procedure  (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out State_Information);
   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out State_Information);
   -- Traverse --
   procedure Traverse is new Asis.Iterator.Traverse_Element
     (State_Information, Pre_Procedure, Post_Procedure);


   -- Pre_Procedure --
   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out State_Information)
   is
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
      use Object_Queue;
   begin
      case Element_Kind (Element) is
         when A_Pragma =>
            -- Nothing for us in pragmas, and traversing them creates troubles...
            Control := Abandon_Children;

         -- EXPRESSIONS
         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Identifier =>
                  -- Check if the identifier is a user-defined dependent object
                  -- The object may be
                  --    A_Function_Call, A_Procedure_Call_Statement,      (specified by their identifier)
                  --    A_Parameter_Specification, An_Object_Declaration,
                  --    An_Object_Renaming_Declaration, etc.
                  declare
                     Entity_Context : constant Root_Context'Class
                       := Matching_Context (Entities, Element, Extend_To => All_Extensions);
                  begin
                     if Entity_Context /= No_Matching_Context then
                        -- In the case we found a matching context,
                        -- the dependency has been defined by the user
                        State.Last_Statement :=
                          Last_Statement_Information'(Kind  => User_Defined_Dependency,
                                                      Index => State.Current_Statement);
                        Add_Fictive_Object_Reference (State.References_Queue,
                                                      State.Current_Statement);
                     end if;
                  end;

                  -- Check if the identifier is A_Parameter_Specification, An_Object_Declaration or
                  -- An_Object_Renaming_Declaration and process it
                  declare
                     use Framework.Locations, Framework.Reports;
                     Identifier             : Asis.Expression    := Element;
                     Identifier_Definition  : Asis.Defining_Name :=
                                                Corresponding_Name_Definition (Identifier);
                     Identifier_Declaration : Asis.Declaration   :=
                                                Enclosing_Element (Identifier_Definition);
                  begin
                     case Declaration_Kind (Identifier_Declaration) is
                        when A_Parameter_Specification
                          | An_Object_Declaration
                          | An_Object_Renaming_Declaration
                          =>
                           Identifier := Ultimate_Name (Identifier, No_Component => True);
                           if Is_Nil (Identifier) then
                              -- Dynamic renaming
                              Uncheckable (Rule_Id,
                                           False_Negative,
                                           Get_Location (Element),
                                           "Entity is not statically determinable");
                              return;
                           end if;
                           Identifier_Definition  := Corresponding_Name_Definition (Identifier);
                           Identifier_Declaration := Enclosing_Element (Identifier_Definition);

                           -- Insert the element into the queue if it does not exist in yet
                           declare
                              Current_Cursor    : Cursor;
                              Current_Object    : Object_Information (State.Number_Of_Statements);
                              Inserted_Object   : Object_Information (State.Number_Of_Statements);
                              Object_References : Reference_Array (1..State.Number_Of_Statements);
                           begin
                              -- Reset the cursor to the first element
                              Current_Cursor := First (State.References_Queue);
                              -- Loop until we find the element or the end of the queue
                              while Has_Element (Current_Cursor) loop
                                 Current_Object := Fetch (Current_Cursor);
                                 exit when Is_Equal (Current_Object.Identifier, Identifier_Definition);
                                 Current_Cursor := Next (Current_Cursor);
                              end loop;
                              -- Check whether the element is in the queue
                              if Has_Element (Current_Cursor) then
                                 -- The element is already in the queue
                                 -- Update its references
                                 Object_References := Current_Object.References;
                                 Object_References (State.Current_Statement) := True;
                                 -- Check if the object is a parameter
                                 if Current_Object.Kind = Parameter then
                                    State.Last_Statement :=
                                      Last_Statement_Information'(Kind  => Parameter_Reference,
                                                                  Index => State.Current_Statement);
                                 end if;
                                 -- Create the object to replace
                                 Inserted_Object :=
                                   Object_Information'(Nb_Refs    => State.Number_Of_Statements,        -- Discriminant
                                                       Identifier => Current_Object.Identifier,
                                                       Kind       => Current_Object.Kind,
                                                       References => Object_References,
                                                       Checked    => False);
                                 -- Replace the object with its updated values
                                 Replace (Current_Cursor, Inserted_Object);
                              else -- The element isn't yet in the queue
                                   -- Set references
                                 Object_References := (others => False);
                                 Object_References (State.Current_Statement) := True;
                                 -- Create the object to insert
                                 Inserted_Object :=
                                   Object_Information'(Nb_Refs    => State.Number_Of_Statements,        -- Discriminant
                                                       Identifier => Identifier_Definition,
                                                       Kind       => Independent,
                                                       References => Object_References,
                                                       Checked    => False);
                                 -- Insert the newly created object information
                                 Append (State.References_Queue, Inserted_Object);
                              end if;
                           end;
                        when others =>
                           null;
                     end case;
                  end;

                  -- Since we cannot match an attribute declaration, just check the prefix
               when An_Attribute_Reference =>
                  Traverse (Prefix (Element), Control, State);
                  Control := Abandon_Children;

               when others =>
                  null;
            end case;

            -- STATEMENTS
         when A_Statement =>
            case Statement_Kind (Element) is
               -- Never movable statements
               -- Note: extended return statements not allowed for accepts!
               when A_Return_Statement
                  | A_Requeue_Statement
                  | A_Requeue_Statement_With_Abort
                    =>
                  State.Last_Statement :=
                    Last_Statement_Information'(Kind  => Inner_Exclusive,
                                                Index => State.Current_Statement);
                  State.Movable_Statements (State.Current_Statement) := False;
                  Add_Fictive_Object_Reference (State.References_Queue,
                                                State.Current_Statement);

               -- Synchronization statements
               when An_Accept_Statement
                 | A_Delay_Until_Statement
                 | A_Delay_Relative_Statement
                 | An_Entry_Call_Statement
                 | An_Abort_Statement
                 | A_Terminate_Alternative_Statement
                 | A_Selective_Accept_Statement
                 | A_Timed_Entry_Call_Statement
                 | A_Conditional_Entry_Call_Statement
                 | An_Asynchronous_Select_Statement
                 =>
                  State.Last_Statement :=
                    Last_Statement_Information'(Kind  => Synchronization,
                                                Index => State.Current_Statement);
                  State.Movable_Statements (State.Current_Statement) := False;
                  Add_Fictive_Object_Reference (State.References_Queue,
                                                State.Current_Statement);

               when others =>
                  null;
            end case;

         when A_Definition =>
            case Definition_Kind (Element) is
               when An_Aspect_Specification =>
                  -- 2012, ignored for the moment
                  Control := Abandon_Children;
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end Pre_Procedure;

   -- Post_Procedure --
   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out State_Information)
   is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure;


   ------------------------------
   -- Process_Accept_Statement --
   ------------------------------

   procedure Process_Accept_Statement (Statement : in Asis.Statement) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports;
      use Object_Queue, Thick_Queries;

      Stable_State : Boolean := False;
   begin
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Body_Statements : constant Asis.Statement_List := Accept_Body_Statements (Statement);

         -- A set of values indicating the statements movability
         Movable_Statements : aliased Dependency_Array := (Body_Statements'Range => True);

         -- Information used for the traversal of statements
         The_Control : Asis.Traverse_Control := Continue;
         The_State   : State_Information :=
           State_Information'(Current_Statement    => 0,
                              Number_Of_Statements => Body_Statements'Length,
                              Last_Statement       =>
                                Last_Statement_Information'(Kind => None,
                                                            Index => Body_Statements'First - 1),
                              References_Queue     => Object_Queue.Empty_Queue,
                              Movable_Statements   => Movable_Statements'Unchecked_Access);

      begin

         -- Do not process the `accept' if it has no statements
         if The_State.Number_Of_Statements = 0 then
            return;
         end if;

         -- 1st step: insert a fictive object into the referenced objects queue
         Append (The_State.References_Queue, (Nb_Refs      => The_State.Number_Of_Statements,      -- Discriminant
                                              Identifier   => Nil_Element,
                                              Kind         => Dependent,
                                              References   => (others => False),
                                              Checked      => False));

         -- 2nd step: insert all of the `accept' parameters into the referenced objects queue
         for P : Asis.Parameter_Specification of Parameter_Profile (Corresponding_Entry (Statement)) loop
            for Id : Asis.Defining_Name of Names (P) loop
               Append (The_State.References_Queue, (Nb_Refs      => The_State.Number_Of_Statements,
                                                    Identifier   => First_Defining_Name (Id),
                                                    Kind         => Parameter,
                                                    References   => (others => False),
                                                    Checked      => False));
            end loop;
         end loop;

         -- 3rd step: retrieve all identifiers and set the last statement index and kind
     Identifiers_Retrieval:
         for Stmt_Index in Body_Statements'Range loop
            The_State.Current_Statement := Stmt_Index;
            case Statement_Kind (Body_Statements (Stmt_Index)) is
                  -- returning statements
               when A_Return_Statement
                  | A_Requeue_Statement
                  | A_Requeue_Statement_With_Abort
                 =>
                  -- Since further statements are unreachable, do not try to process them
                  The_State.Last_Statement :=
                    Last_Statement_Information'(Kind  => Exclusive,
                                                Index => Stmt_Index);
                  The_State.Movable_Statements (Stmt_Index) := False;
                  exit;
               -- Synchronization statements
               when An_Accept_Statement
                 | A_Delay_Until_Statement
                 | A_Delay_Relative_Statement
                 | An_Entry_Call_Statement
                 | An_Abort_Statement
                 | A_Terminate_Alternative_Statement
                 | A_Selective_Accept_Statement
                 | A_Timed_Entry_Call_Statement
                 | A_Conditional_Entry_Call_Statement
                 | An_Asynchronous_Select_Statement
                 =>
                  Traverse (Body_Statements (Stmt_Index), The_Control, The_State);
               -- any other statement kind
               when others =>
                  Traverse (Body_Statements (Stmt_Index), The_Control, The_State);
            end case;
         end loop Identifiers_Retrieval;


         -- At this point, we have a complete list of identifiers with values
         -- indicating dependency upon parameters and referencing statements.
         -- We also know the index and the kind of the last statement.

         -- 4th step: try to obtain a stable state for dependent identifiers and movable statements
         -- No need to check all statements if "Possible" has not been requested
         if not Rule_Used (K_Possible) then
            Stable_State := True;
         end if;

         -- Loop until obtaining a stable state
         while not Stable_State loop
            Stable_State := True;
            declare
               Current_Cursor : Cursor;
               Current_Object : Object_Information (The_State.Number_Of_Statements);
            begin
               -- find the first parameter or dependent object that has not been checked yet
               Current_Cursor := First (The_State.References_Queue);
               while Has_Element (Current_Cursor) loop
                  Current_Object := Fetch (Current_Cursor);
                  -- Check if the object is dependent and not yet checked
                  if Current_Object.Kind /= Independent
                    and not Current_Object.Checked
                  then
                     -- Current_Object is a dependent object that has not been checked yet
                     Stable_State := False;
                     -- Set each object referenced within the same statements as
                     -- the parameter dependent object we just found to dependent
                     declare
                        Dependent_Object    : Object_Information renames Current_Object;
                        References_Iterator : Cursor;
                        Referenced_Object   : Object_Information (The_State.Number_Of_Statements);
                     begin
                        References_Iterator := First (The_State.References_Queue);
                        while Has_Element (References_Iterator) loop
                           Referenced_Object := Fetch (References_Iterator);
                           -- Set all referencing statements as unmovable.
                           for S in List_Index range Body_Statements'First .. The_State.Last_Statement.Index loop
                              if Dependent_Object.References (S) then
                                 -- Current statement is referencing the parameter dependent object.
                                 -- Set it as unmovable / parameter dependent.
                                 The_State.Movable_Statements (S) := False;
                                 -- Set all referenced identifiers within the current statement as dependent.
                                 if Referenced_Object.Kind = Independent     -- avoid a replace if unnecessary
                                   and Referenced_Object.References (S)
                                 then
                                    Referenced_Object.Kind := Dependent;
                                    Replace (References_Iterator, Referenced_Object);
                                 end if;
                              end if;
                           end loop;
                           References_Iterator := Next (References_Iterator);
                        end loop;
                        -- Set the dependent object as checked
                        Dependent_Object.Checked := True;
                        Replace (Current_Cursor, Dependent_Object);
                     end;
                  end if;
                  Current_Cursor := Next (Current_Cursor);
               end loop;
               --
            end;
         end loop;


         -- From here on, every reference of an identifier or parameter within a statement is known.
         -- We also know which statements are movable/removable.
         -- We can then report each error we found within the `accept' statement

         -- 5th step: report errors
         if Rule_Used (K_Possible) then
            Report_Basic_Statements :
            for Stmt_Index in List_Index range Body_Statements'First .. The_State.Last_Statement.Index loop
               if The_State.Movable_Statements (Stmt_Index) then
                  Report (Rule_Id,
                          Usage (K_Possible),
                          Get_Location (Body_Statements (Stmt_Index)),
                          "statement may be moved to an outer scope");
               end if;
            end loop Report_Basic_Statements;
         end if;

     Report_Last_Statements:
         for Stmt : Asis.Statement of Body_Statements (The_State.Last_Statement.Index + 1 .. Body_Statements'Last) loop
            case The_State.Last_Statement.Kind is
               when Exclusive =>
                  Report (Rule_Id,
                          Usage (K_Certain),
                          Get_Location (Stmt),
                          "statement may be removed (dead code)");
               when Parameter_Reference =>
                  Report (Rule_Id,
                          Usage (K_Certain),
                          Get_Location (Stmt),
                          "statement may be moved to an outer scope (after last parameter reference)");
               when User_Defined_Dependency =>
                  Report (Rule_Id,
                          Usage (K_Certain),
                          Get_Location (Stmt),
                          "statement may be moved to an outer scope (after user-defined dependency)");
               when others =>
                  Report (Rule_Id,
                          Usage (K_Certain),
                          Get_Location (Stmt),
                          "statement may be moved to an outer scope");
            end case;
         end loop Report_Last_Statements;
      end;
   end Process_Accept_Statement;


begin  -- Rules.Movable_Accept_Statements
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Movable_Accept_Statements;
