----------------------------------------------------------------------
--  Rules.Exception_Propagation - Package body                      --
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
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  A4G_Bugs,
  Binary_Map,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

package body Rules.Exception_Propagation is
   use Framework, Ada.Strings.Wide_Unbounded;

   type Risk_Level is (No_Risk, Object_Declaration, Variable_In_Declaration, Call_In_Declaration, Always);

   -- Note that "interface" is a reserved work in Ada 2005
   type Subrules is (Kw_Interface, Kw_Parameter, Kw_Task, Kw_Declaration, Kw_Local_Exception);

   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "KW_");
   use Subrules_Flag_Utilities;

   type Usage is array (Subrules) of Boolean;
   No_Rule : constant Usage := (others => False);
   Rule_Used : Usage := No_Rule;
   Save_Used : Usage;

   type EP_Rule_Context is new Basic_Rule_Context with
      record
         Check_Level : Risk_Level;
      end record;
   package Convention_Map is new Binary_Map (Unbounded_Wide_String, EP_Rule_Context);
   use Convention_Map;

   Parameters          : Context_Store;
   Conventions         : Convention_Map.Map;
   Task_Context        : EP_Rule_Context;
   Declaration_Context : EP_Rule_Context;
   Local_Exc_Context   : Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      Help_On_Flags ("Parameter 1     : [<level>,]", Footer => "(<level> is required for declaration)");
      User_Message  ("Parameter 2 .. N: for interface: <convention name>");
      User_Message  ("                  for parameter: <full name of parameters known to expect call-backs>");
      User_Message  ("                  for task: nothing");
      User_Message  ("                  for declaration: nothing");
      User_Message  ("                  for local_exception: nothing, no <level> allowed");
      User_Message  ("Control that certain kinds of subprograms, tasks, or declarations cannot propagate exceptions");
      User_Message  ("and that local exceptions cannot propagate out of scope");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;

      Subrule     : Subrules;
      Int_Value   : Integer;
      Check_Level : Risk_Level := Always;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "parameters required");
      end if;

      if Is_Integer_Parameter then
         Int_Value := Get_Integer_Parameter (Min => 0, Max => 3);
         Check_Level := Risk_Level'Val (Risk_Level'Pos (Always) - Int_Value);
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);
      case Subrule is
         when Kw_Interface =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "at least two parameters required");
            end if;

            while Parameter_Exists loop
               declare
                  Convention : constant Wide_String := Get_Name_Parameter;
               begin
                  Add (Conventions,
                       To_Unbounded_Wide_String (Convention),
                       EP_Rule_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Check_Level));
               exception
                  when Already_In_Store =>
                     Parameter_Error (Rule_Id, "convention already given: " & Convention);
               end;
            end loop;
            Rule_Used (Kw_Interface) := True;

         when Kw_Parameter =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "at least two parameters required");
            end if;

            while Parameter_Exists loop
               declare
                  Entity : constant Entity_Specification := Get_Entity_Parameter;
               begin
                  Associate (Parameters,
                             Entity,
                             EP_Rule_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Check_Level));
               exception
                  when Already_In_Store =>
                     Parameter_Error (Rule_Id, "parameter already given: " & Image (Entity));
               end;
            end loop;
            Rule_Used (Kw_Parameter) := True;

         when Kw_Task =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "No parameter for ""task""");
            end if;

            if Rule_Used (Kw_Task) then
               Parameter_Error (Rule_Id, """task"" already given");
            end if;

            Task_Context        := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Check_Level);
            Rule_Used (Kw_Task) := True;

         when Kw_Declaration =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "No parameter for ""declaration""");
            end if;

            if Rule_Used (Kw_Declaration) then
               Parameter_Error (Rule_Id, """declaration"" already given");
            end if;

            if Check_Level = Always then
               Parameter_Error (Rule_Id, "non 0 level required for ""declaration""");
            end if;

            Declaration_Context        := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Check_Level);
            Rule_Used (Kw_Declaration) := True;

         when Kw_Local_Exception =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "No parameter for ""local_exception""");
            end if;

            if Rule_Used (Kw_Local_Exception) then
               Parameter_Error (Rule_Id, """local_exception"" already given");
            end if;

            if Check_Level /= Always then
               Parameter_Error (Rule_Id, "no level allowed for ""local_exception""");
            end if;

            Local_Exc_Context              := Basic.New_Context (Ctl_Kind, Ctl_Label);
            Rule_Used (Kw_Local_Exception) := True;
      end case;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := No_Rule;
            Clear (Parameters);
            Clear (Conventions);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Parameters);
      Balance (Conventions);
   end Prepare;


   --------------------------
   -- Traverse_Declaration --
   --------------------------

   -- Procedure to check the level of "danger" of declarations
   -- Never returns Always
   procedure Pre_Procedure_Declaration (Element : in     Asis.Element;
                                        Control : in out Asis.Traverse_Control;
                                        State   : in out Risk_Level);
   procedure Post_Procedure_Declaration (Element : in     Asis.Element;
                                         Control : in out Asis.Traverse_Control;
                                         State   : in out Risk_Level) is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure_Declaration;

   procedure Traverse_Declaration is new Asis.Iterator.Traverse_Element (Risk_Level,
                                                                         Pre_Procedure_Declaration,
                                                                         Post_Procedure_Declaration);

   procedure Pre_Procedure_Declaration (Element : in     Asis.Element;
                                        Control : in out Asis.Traverse_Control;
                                        State   : in out Risk_Level) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      procedure Process_Aliasing_Expr (Aliasing_Expr : Asis.Expression) is
         -- Handle expressions that are the name of something else:
         --   - Renamings
         --   - Prefixes of attributes
         use Utilities;
         Expr : Asis.Expression := Aliasing_Expr;
      begin
         loop
            case Expression_Kind (Expr) is
               when An_Identifier
                  | An_Enumeration_Literal =>
                  exit;
               when A_Selected_Component =>
                  Expr := Prefix (Expr);
               when An_Explicit_Dereference =>
                  -- consider it is an access to variable. Even if the access type is
                  -- access-to-constant, it could designate a variable.
                  State := Risk_Level'Max (State, Variable_In_Declaration);
                  Expr := Prefix (Expr);
               when An_Indexed_Component =>
                  declare
                     Indices : constant Asis.Expression_List := Index_Expressions (Expr);
                  begin
                     for I in Indices'Range loop
                        Traverse_Declaration (Indices (I), Control, State);
                     end loop;
                  end;
                  Expr := Prefix (Expr);
               when A_Slice =>
                  Traverse_Declaration (Slice_Range (Expr), Control, State);
                  Expr := Prefix (Expr);
               when A_Function_Call =>
                  Traverse_Declaration (Expr, Control, State);
                  exit;
               when An_Attribute_Reference =>
                  -- Can only be a renaming of a /value/ attribute here
                  -- (for /functions/ attributes, we would have found A_Function_Call)
                  exit;
               when A_Type_Conversion =>
                  -- Allowed for tagged types
                  -- There can be no variables or function calls in the prefix, but the
                  -- converted expression can be pretty much anything.
                  Expr := Converted_Or_Qualified_Expression (Expr);
               when others =>
                  Failure ("Exception_Propagate: unexpected expression in renaming (1)", Expr);
            end case;
         end loop;
      end Process_Aliasing_Expr;

   begin   -- Pre_Procedure_Declaration
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when An_Integer_Number_Declaration
                  | A_Real_Number_Declaration
                    =>
                  -- These are not allowed by the language to raise exceptions
                  Control := Abandon_Children;

               when An_Ordinary_Type_Declaration =>
                  case Type_Kind (Type_Declaration_View (Element)) is
                     when An_Enumeration_Type_Definition
                        | A_Signed_Integer_Type_Definition
                        | A_Modular_Type_Definition
                        | A_Floating_Point_Definition
                        | An_Ordinary_Fixed_Point_Definition
                        | A_Decimal_Fixed_Point_Definition
                          =>
                        -- Scalar type definitions are always static
                        Control := Abandon_Children;
                     when others =>
                        null;
                  end case;

               when A_Variable_Declaration | A_Constant_Declaration =>
                  State := Risk_Level'Max (State, Object_Declaration);
                  case Definition_Kind (Object_Declaration_View (Element)) is
                     when A_Task_Definition
                        | A_Protected_Definition
                          =>
                        -- Single task or protected object
                        -- Don't traverse the definition
                        Control := Abandon_Children;
                     when others =>
                        null;
                  end case;

               when An_Object_Renaming_Declaration =>
                  -- Only indexing (or slicing) and dereferences are evaluated here and can raise exceptions
                  -- Traverse manually just those.
                  Process_Aliasing_Expr (A4G_Bugs.Renamed_Entity (Element));
                  Control := Abandon_Children;

               when A_Procedure_Declaration
                  | A_Generic_Procedure_Declaration
                  | A_Procedure_Body_Declaration

                  | A_Function_Declaration
                  | A_Generic_Function_Declaration
                  | A_Function_Body_Declaration

                  | A_Generic_Package_Declaration

                  | A_Task_Type_Declaration
                  | A_Task_Body_Declaration
                 =>
                  -- Elaboration of these cannot raise exceptions
                  Control := Abandon_Children;

               when A_Package_Body_Declaration =>
                  -- Don't traverse generic bodies (but traverse regular ones)
                  if Is_Generic_Unit (Element) then
                     Control := Abandon_Children;
                  end if;

               when others =>
                  null;
            end case;

         when An_Expression =>
            case Expression_Kind (Element) is
               when A_Function_Call =>
                  -- There is no higher risk in declarations, no need to take the max (nor to analyze any further)
                  State   := Call_In_Declaration;
                  Control := Terminate_Immediately;
               when An_Identifier =>
                  case Declaration_Kind (A4G_Bugs.Corresponding_Name_Declaration (Element)) is
                     when A_Variable_Declaration =>
                        State := Risk_Level'Max (State, Variable_In_Declaration);
                     when An_Object_Renaming_Declaration =>
                        declare
                           use Utilities;
                           Expr : Asis.Expression
                             := A4G_Bugs.Renamed_Entity (A4G_Bugs.Corresponding_Name_Declaration (Element));
                        begin
                           loop
                              case Expression_Kind (Expr) is
                                 when An_Identifier =>
                                    Traverse_Declaration (Expr, Control, State);
                                    exit;
                                 when A_Selected_Component =>
                                    Traverse_Declaration (Expr, Control, State);
                                    Expr := Prefix (Expr);
                                 when An_Explicit_Dereference =>
                                    -- everything left of the dereference is "used" at the place of the renaming
                                    -- consider it is an access to variable. Even if the access type is
                                    -- access-to-constant, it could designate a variable.
                                    State := Risk_Level'Max (State, Variable_In_Declaration);
                                    exit;
                                 when A_Slice
                                    | An_Indexed_Component
                                      =>
                                    -- The indexing expression is "used" at the place of the renanming,
                                    -- not when using the renamed entity
                                    Expr := Prefix (Expr);
                                 when A_Function_Call =>
                                    -- The function is "used" at the place of the renanming,
                                    -- not when using the renamed entity
                                    exit;
                                 when An_Attribute_Reference =>
                                    Process_Aliasing_Expr (Prefix (Element));
                                    exit;
                                 when A_Type_Conversion =>
                                    -- Allowed for tagged types
                                    Expr := Converted_Or_Qualified_Expression (Expr);
                                 when others =>
                                    Failure ("Exception_Propagation: unexpected expression in renaming (2)", Expr);
                              end case;
                           end loop;
                        end;
                     when others =>
                        null;
                  end case;
               when An_Explicit_Dereference =>
                  -- consider it is an access to variable. Even if the access type is
                  -- access-to-constant, it could designate a variable.
                  State := Risk_Level'Max (State, Variable_In_Declaration);
               when An_Attribute_Reference =>
                  -- Traverse prefix only
                  Process_Aliasing_Expr (Prefix (Element));
                  Control := Abandon_Children;
               when others =>
                  null;
            end case;

         when A_Pragma =>
            -- Nothing interesting in pragmas
            Control := Abandon_Children;

         when others =>
            null;
      end case;
   end Pre_Procedure_Declaration;


   ----------------------
   -- Traverse_Handler --
   ----------------------

   -- Procedure to check if a raise statement (or equivalent) is encountered in a handler
   -- returns No_Risk or Always only
   type Handler_State is
      record
         Risk    : Risk_Level;
         Exc     : Asis.Defining_Name; -- Search this exception or all if nil_element
         Reraise : Boolean;            -- Search reraise if true
      end record;
   procedure Pre_Procedure_Handler (Element : in     Asis.Element;
                                    Control : in out Asis.Traverse_Control;
                                    State   : in out Handler_State);
   procedure Post_Procedure_Handler (Element : in     Asis.Element;
                                     Control : in out Asis.Traverse_Control;
                                     State   : in out Handler_State) is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure_Handler;

   procedure Traverse_Handler is new Asis.Iterator.Traverse_Element (Handler_State,
                                                                     Pre_Procedure_Handler,
                                                                     Post_Procedure_Handler);

   procedure Pre_Procedure_Handler (Element : in     Asis.Element;
                                    Control : in out Asis.Traverse_Control;
                                    State   : in out Handler_State) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Utilities, Thick_Queries;
      SP : Asis.Expression;
   begin
      case Statement_Kind (Element) is
         when A_Raise_Statement =>
            if Is_Nil (Raised_Exception (Element)) then
               if State.Reraise then
                  State.Risk := Always;
                  Control    := Terminate_Immediately;
               end if;
            elsif Is_Nil (State.Exc)
              or else Is_Equal (Corresponding_Name_Definition (Simple_Name (Raised_Exception (Element))),
                                State.Exc)
            then
               State.Risk := Always;
               Control    := Terminate_Immediately;
            end if;
         when A_Procedure_Call_Statement =>
            SP := Called_Simple_Name (Element);
            if not Is_Nil (SP) then
               -- It is nil for implicit or explicit dereference
               declare
                  SP_Name : constant Wide_String := To_Upper (Full_Name_Image (SP));
               begin
                  if SP_Name = "ADA.EXCEPTIONS.RAISE_EXCEPTION" then
                     if Is_Nil (State.Exc) then
                        State.Risk := Always;
                        Control    := Terminate_Immediately;
                     else
                        declare
                           Exc_Param : constant Asis.Expression := Ultimate_Expression
                                                                    (Actual_Parameter
                                                                     (Call_Statement_Parameters
                                                                      (Element, Normalized => True) (1)));
                        begin
                           if Expression_Kind (Exc_Param) = An_Attribute_Reference
                             and then A4G_Bugs.Attribute_Kind (Exc_Param) = An_Identity_Attribute
                             and then Is_Equal (Corresponding_Name_Definition (Ultimate_Name (Prefix (Exc_Param))),
                                                State.Exc)
                           then
                              State.Risk := Always;
                              Control    := Terminate_Immediately;
                           end if;
                        end;
                     end if;

                  elsif SP_Name = "ADA.EXCEPTIONS.RERAISE_OCCURRENCE" then
                     if State.Reraise then
                        State.Risk := Always;
                        Control    := Terminate_Immediately;
                     end if;
                  end if;
               end;
            end if;
         when others =>
            null;
      end case;
   end Pre_Procedure_Handler;


   --------------------------------
   -- Exception_Propagation_Risk --
   --------------------------------

   function Exception_Propagation_Risk (SP_Body : Asis.Declaration; Max_Level : Risk_Level) return Risk_Level is
      use Asis, Asis.Elements, Asis.Declarations, Asis.Statements;
      H_State     : Handler_State;
      Level       : Risk_Level;
      The_Control : Traverse_Control := Continue;

      Handlers    : constant Asis.Exception_Handler_List := Body_Exception_Handlers (SP_Body);
   begin -- Exception_Propagation_Risk
      -- Is there a handler ?
      if Handlers = Nil_Element_List then
         return Always;
      end if;

      -- Is there an others choice (it must be last and the only choice)?
      if Definition_Kind (Exception_Choices (Handlers (Handlers'Last)) (1)) /= An_Others_Choice then
         return Always;
      end if;

      -- Is there any raise statement in handler?
      H_State := (No_Risk, Exc => Nil_Element, Reraise => True);
      for I in Handlers'Range loop
         declare
            Stats : constant Asis.Statement_List := Handler_Statements (Handlers (I));
         begin
            for J in Stats'Range loop
               Traverse_Handler (Stats (J), The_Control, H_State);
               if H_State.Risk = Always then
                  return Always;
               end if;
            end loop;
         end;
      end loop;
      Level := No_Risk;

      -- No need to check declarations if not requested by the user
      if Max_Level < Always then
         declare
            Decls : constant Asis.Declaration_List := Body_Declarative_Items (SP_Body);
         begin
            The_Control := Continue;
            for I in Decls'Range loop
               Traverse_Declaration (Decls (I), The_Control, Level);
               exit when Level >= Max_Level;
               -- Highest risk detected, no need to go further
            end loop;
         end;
      end if;

      return Level;
   end Exception_Propagation_Risk;

   ------------------
   -- Risk_Message --
   ------------------

   function Risk_Message (Risk : Risk_Level) return Wide_String is
      use Utilities;
   begin
      if Risk = Always then
         return "";
      end if;

      return " (declaration at level " & Integer_Img (Risk_Level'Pos (Always) - Risk_Level'Pos (Risk)) & ')';
   end Risk_Message;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : Asis.Element) is
      use Asis, Asis.Statements, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Thick_Queries;

      -- Procedure to check if a 'Access or 'Address is encountered
      procedure Pre_Procedure_Parameter (Element : in     Asis.Element;
                                         Control : in out Asis.Traverse_Control;
                                         State   : in out EP_Rule_Context)
      is
         pragma Unreferenced (Control);
         use Asis.Declarations;
         SP_Declaration : Asis.Declaration;
         Risk           : Risk_Level;
         Good_Prefix    : Asis.Expression;
      begin
         if Expression_Kind (Element) = An_Attribute_Reference
           and then A4G_Bugs.Attribute_Kind (Element) in An_Access_Attribute .. An_Address_Attribute
         then
            Good_Prefix := Simple_Name (Prefix (Element));

            if Expression_Kind (Good_Prefix) = An_Explicit_Dereference or else Is_Access_Expression (Good_Prefix) then
               -- Explicit or implicit dereference: prefix subprogram is dynamic, nothing we can do
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Element),
                            "Prefix of attribute is not statically determinable");
               return;
            end if;
            Good_Prefix := Ultimate_Name (Good_Prefix);
            if Is_Nil (Good_Prefix) then
               -- Dynamic renaming
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Element),
                            "Prefix of attribute is not statically determinable");
               return;
            end if;

            SP_Declaration := A4G_Bugs.Corresponding_Name_Declaration (Good_Prefix);

            case Declaration_Kind (SP_Declaration) is
               when A_Procedure_Declaration
                  | A_Procedure_Body_Declaration
                  | A_Function_Declaration
                  | A_Function_Body_Declaration
                    =>
                  Risk := Exception_Propagation_Risk (Corresponding_Body (SP_Declaration), State.Check_Level);
                  if Risk >= State.Check_Level then
                     Report (Rule_Id,
                             State,
                             Get_Location (SP_Declaration),
                             "subprogram """ &  Defining_Name_Image (Names (SP_Declaration)(1))
                             & """ can propagate exceptions"
                             & Risk_Message (Risk)
                             & ", used as call-back at " & Image (Get_Location (Call)));
                  end if;
               when A_Procedure_Instantiation | A_Function_Instantiation =>
                  Risk := Exception_Propagation_Risk (Corresponding_Body
                                                      (A4G_Bugs.Corresponding_Name_Declaration
                                                       (Simple_Name
                                                        (Generic_Unit_Name
                                                         (SP_Declaration)))),
                                                     State.Check_Level);
                  if Risk >= State.Check_Level then
                     Report (Rule_Id,
                             State,
                             Get_Location (Corresponding_Body
                                           (A4G_Bugs.Corresponding_Name_Declaration
                                            (Simple_Name
                                             (Generic_Unit_Name
                                              (SP_Declaration))))),
                             "generic """ &  A4G_Bugs.Name_Image (Simple_Name (Generic_Unit_Name (SP_Declaration)))
                             & """ can propagate exceptions"
                             & Risk_Message (Risk)
                             & ", instance """ &  Defining_Name_Image (Names (SP_Declaration)(1))
                             & """ at " & Image (Get_Location (SP_Declaration))
                             & " used as call-back at " & Image (Get_Location (Call))
                            );
                  end if;
               when others =>
                  null;
            end case;
         end if;
      end Pre_Procedure_Parameter;

      procedure Post_Procedure_Parameter (Element : in     Asis.Element;
                                          Control : in out Asis.Traverse_Control;
                                          State   : in out EP_Rule_Context) is
         pragma Unreferenced (Element, Control, State);
      begin
         null;
      end Post_Procedure_Parameter;

      procedure Traverse_Parameter is new Asis.Iterator.Traverse_Element (EP_Rule_Context,
                                                                Pre_Procedure_Parameter,
                                                                Post_Procedure_Parameter);

   begin   -- Process_Call
      if not Rule_Used (Kw_Parameter) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Dispatching_Call (Call) then
         -- Improvement needed here, but it's quite difficult
         Uncheckable (Rule_Id, False_Negative, Get_Location (Call), "Dispatching call");
         return;
      end if;

      if Expression_Kind (Call) = A_Function_Call
        and then Is_Nil (A4G_Bugs.Corresponding_Called_Function (Call))
      then
         -- This is a call to a predefined operation without a proper declaration
         -- Certainly not something that could register a call-back!
         -- Eliminate that case that would otherwise cause trouble in the rest of this
         -- procedure.
         return;
      end if;

      if Expression_Kind (Called_Simple_Name (Call)) = An_Attribute_Reference then
         -- These are known to not have parameters that are access (or address) to SP
         -- Moreover, the rest of the algorithm wouldn't work since parameters of
         -- attributes SP have no "name"
         return;
      end if;

      declare
         Actuals : constant Asis.Association_List := Actual_Parameters (Call);
         The_Control : Asis.Traverse_Control := Asis.Continue;
      begin
         for I in Actuals'Range loop
            declare
               Current_Context : Root_Context'Class := Matching_Context (Parameters,
                                                                         Formal_Name (Call, I),
                                                                         Extend_To => All_Extensions);
            begin
               if Current_Context /= No_Matching_Context then
                  -- Parameter found
                  Traverse_Parameter (Actual_Parameter (Actuals (I)), The_Control, EP_Rule_Context (Current_Context));

                  -- The same formal parameter won't happen twice in the same call,
                  -- no need to check further
                  exit;
               end if;
            end;
         end loop;
      end;
   end Process_Call;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation  (Instantiation : Asis.Declaration) is
      use Framework.Reports, Thick_Queries, Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      Risk : Risk_Level;
   begin
      if not Rule_Used (Kw_Parameter) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Actuals : constant Asis.Association_List := Generic_Actual_Part (Instantiation);
      begin
         for I in Actuals'Range loop
            declare
               Current_Context : constant Root_Context'Class
                 := Matching_Context (Parameters, Formal_Name (Instantiation, I), Extend_To => All_Extensions);
               SP_Declaration : Asis.Declaration;
           begin
               if Current_Context /= No_Matching_Context then
                  -- Parameter found
                  -- Actual must be an identifier (or else it is not for us, dereference for example)
                  if Expression_Kind (Actual_Parameter (Actuals (I))) = An_Identifier then
                     SP_Declaration := A4G_Bugs.Corresponding_Name_Declaration (Ultimate_Name
                                                                       (Actual_Parameter
                                                                        (Actuals (I))));

                     case Declaration_Kind (SP_Declaration) is
                        when A_Procedure_Declaration
                           | A_Procedure_Body_Declaration
                           | A_Function_Declaration
                           | A_Function_Body_Declaration =>
                           Risk := Exception_Propagation_Risk (Corresponding_Body (SP_Declaration),
                                                               EP_Rule_Context (Current_Context).Check_Level);
                           if Risk >= EP_Rule_Context (Current_Context).Check_Level then
                              Report (Rule_Id,
                                      Current_Context,
                                      Get_Location (SP_Declaration),
                                      "subprogram """ &  A4G_Bugs.Name_Image (Actual_Parameter (Actuals (I)))
                                      & """ can propagate exceptions"
                                      & Risk_Message (Risk)
                                      & ", used as call-back in instantiation at "
                                      & Image (Get_Location (Instantiation)));
                           end if;

                        when A_Procedure_Instantiation | A_Function_Instantiation =>
                           Risk := Exception_Propagation_Risk (Corresponding_Body
                                                               (A4G_Bugs.Corresponding_Name_Declaration
                                                                (Generic_Unit_Name
                                                                 (SP_Declaration))),
                                                               EP_Rule_Context (Current_Context).Check_Level);
                           if Risk >= EP_Rule_Context (Current_Context).Check_Level then
                              Report (Rule_Id,
                                      Current_Context,
                                      Get_Location (Corresponding_Body
                                                    (A4G_Bugs.Corresponding_Name_Declaration
                                                     (Generic_Unit_Name
                                                      (SP_Declaration)))),
                                      "generic """ &  A4G_Bugs.Name_Image (Generic_Unit_Name (SP_Declaration))
                                      & """ can propagate exceptions"
                                      & Risk_Message (Risk)
                                      & ", instance """ &  Defining_Name_Image (Names (SP_Declaration)(1))
                                      & """ at " & Image (Get_Location (SP_Declaration))
                                      & " used as call-back in instantiation at " & Image (Get_Location (Instantiation))
                                     );
                           end if;

                        when others =>
                           null;
                     end case;

                     -- The same parameter won't happen twice in the same call,
                     -- no need to check further
                     exit;
                  end if;
               end if;
            end;
         end loop;
      end;
   end Process_Instantiation;


   ----------------------------
   -- Process_SP_Declaration --
   ----------------------------

   procedure Process_SP_Declaration (Element : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Utilities;
      use Framework.Reports;
      Spec_Declaration : Asis.Declaration;
   begin
      if not Rule_Used (Kw_Interface) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Spec_Declaration := Corresponding_Declaration (Element);
      if Is_Nil (Spec_Declaration) then
         -- If there is no explicit specification, it cannot have an Export or Convention pragma
         return;
      end if;

      declare
         All_Pragmas : constant Asis.Pragma_Element_List := Corresponding_Pragmas (Spec_Declaration);
         Risk        : Risk_Level;
      begin
         for I in All_Pragmas'Range loop
            case Pragma_Kind (All_Pragmas (I)) is
               when An_Export_Pragma | A_Convention_Pragma =>
                  -- The convention is always the first argument of the pragma
                  declare
                     Convention : constant Unbounded_Wide_String
                       := To_Unbounded_Wide_String (To_Upper (A4G_Bugs.Name_Image
                                                              (Actual_Parameter
                                                               (Pragma_Argument_Associations
                                                                (All_Pragmas (I))(1)))));
                  begin
                     if Is_Present (Conventions, Convention) then
                       Risk := Exception_Propagation_Risk (Element,  Fetch (Conventions, Convention).Check_Level);
                       if Risk >= Fetch (Conventions, Convention).Check_Level then
                          Report (Rule_Id,
                                  Fetch (Conventions, Convention),
                                  Get_Location (Element),
                                  "subprogram """ & Defining_Name_Image (Names (Element)(1))
                                  & """ can propagate exceptions"
                                  & Risk_Message (Risk)
                                  & ", interfaced to " & To_Wide_String (Convention));
                       end if;
                     end if;
                  end;

               when others =>
                  null;
            end case;
         end loop;
      end;
   end Process_SP_Declaration;

   -----------------------
   -- Process_Task_Body --
   -----------------------

   procedure Process_Task_Body (Task_Body : in Asis.Declaration) is
      use Framework.Reports, Asis.Declarations;
      Risk : Risk_Level;
   begin
      if not Rule_Used (Kw_Task) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Risk := Exception_Propagation_Risk (Task_Body, Task_Context.Check_Level);
      if Risk >= Task_Context.Check_Level then
         Report (Rule_Id,
                 Task_Context,
                 Get_Location (Task_Body),
                 "task """ & Defining_Name_Image (Names (Task_Body)(1))
                 & """ can propagate exceptions"
                 & Risk_Message (Risk));
      end if;
   end Process_Task_Body;


   -----------------------------
   -- Process_Local_Exception --
   -----------------------------

   procedure Process_Local_Exception (Exc : Asis.Definition) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Reports, Thick_Queries, Utilities;

      Scope : constant Asis.Declaration := Enclosing_Element (Enclosing_Element (Exc));
   begin
      case Declaration_Kind (Scope) is
         when A_Package_Declaration
            | A_Generic_Package_Declaration
            | A_Package_Body_Declaration
              =>
            return;
         when others =>
            null;
      end case;

      declare
         Handlers      : constant Asis.Exception_Handler_List := Exception_Handlers (Scope);
         H_State       : Handler_State;
         Handler_Found : Boolean := False;
         Reraise       : Boolean;
         The_Control   : Traverse_Control;
      begin
         Handlers_Loop : for H in Handlers'Range loop
            declare
               Choices : constant Asis.Element_List := Exception_Choices (Handlers (H));
            begin
               Reraise := False;
               for C in Choices'Range loop
                  if Element_Kind (Choices (C)) = A_Definition   -- "others"
                    or else Is_Equal (Corresponding_Name_Definition (Simple_Name (Choices (C))), Exc)
                  then
                     Handler_Found := True;
                     Reraise       := True;
                  end if;
               end  loop;
            end;
            H_State     := (No_Risk, Exc, Reraise);
            The_Control := Continue;
            Traverse_Handler (Handlers (H), The_Control, H_State);
            if H_State.Risk = Always then
               Report (Rule_Id,
                       Local_Exc_Context,
                       Get_Location (Handlers (H)),
                       "handler can propagate local exception at " & Image (Get_Location (Exc)));
            end if;
         end loop Handlers_Loop;
         if not Handler_Found then
            Report (Rule_Id,
                    Local_Exc_Context,
                    Get_Location (Exc),
                    "No handler for local exception in enclosing "
                    & Choose (Statement_Kind (Scope) = A_Block_Statement, "block", "body"));
         end if;
      end;
   end Process_Local_Exception;


   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Declaration   : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Framework.Reports;

      Risk        : Risk_Level := No_Risk;
      The_Control : Traverse_Control := Continue;
   begin
      if not Rule_Used (Kw_Declaration) and not Rule_Used (Kw_Local_Exception) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (Kw_Declaration) then
         Traverse_Declaration (Declaration, The_Control, Risk);
         if Risk >= Declaration_Context.Check_Level then
            Report (Rule_Id,
                    Declaration_Context,
                    Get_Location (Declaration),
                    "declaration can propagate exceptions" & Risk_Message (Risk));
         end if;
      end if;

      if Rule_Used (Kw_Local_Exception)
        and then Declaration_Kind (Declaration) = An_Exception_Declaration
      then
         declare
            Exc_Names : constant Asis.Name_List := Names (Declaration);
         begin
            for E in Exc_Names'Range loop
               Process_Local_Exception (Exc_Names (E));
            end loop;
         end;
      end if;
   end Process_Declaration;

begin  -- Rules.Exception_Propagation
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Exception_Propagation;
