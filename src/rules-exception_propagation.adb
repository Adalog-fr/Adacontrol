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

   type Risk_Level is (No_Risk, Object_In_Declaration, Variable_In_Declaration, Call_In_Declaration, Always);

   -- Note that "interface" is a reserved work in Ada 2005
   type Subrules is (Kw_Interface, Kw_Parameter, Kw_Task, Kw_Declaration);

   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "KW_");
   use Subrules_Flag_Utilities;

   type Usage is array (Subrules) of Boolean;
   Rule_Used : Usage := (others => False);
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
      User_Message  ("Control that certain kinds of subprograms, tasks, or declarations cannot propagate exceptions");
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
            Rule_Used := (others => False);
            Clear (Parameters);
            Clear (Conventions);
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
   begin
      Balance (Parameters);
      Balance (Conventions);
   end Prepare;

   -------------------------
   -- Post_Procedure_Null --
   -------------------------

   procedure Post_Procedure_Null (Element : in     Asis.Element;
                                  Control : in out Asis.Traverse_Control;
                                  State   : in out Risk_Level) is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure_Null;

   --------------------------
   -- Traverse_Declaration --
   --------------------------

   -- Procedure to check the level of "danger" of declarations
   -- Never returns Always
   procedure Pre_Procedure_Declaration (Element : in     Asis.Element;
                                        Control : in out Asis.Traverse_Control;
                                        State   : in out Risk_Level);

   procedure Traverse_Declaration is new Asis.Iterator.Traverse_Element (Risk_Level,
                                                                         Pre_Procedure_Declaration,
                                                                         Post_Procedure_Null);

   procedure Pre_Procedure_Declaration (Element : in     Asis.Element;
                                        Control : in out Asis.Traverse_Control;
                                        State   : in out Risk_Level) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
      Good_Name : Asis.Element;
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Variable_Declaration | A_Constant_Declaration =>
                  State := Risk_Level'Max (State, Object_In_Declaration);

               when A_Procedure_Body_Declaration
                 | A_Function_Body_Declaration
                 | A_Package_Body_Declaration
                 | A_Task_Body_Declaration
                 =>
                  -- These cannot raise exceptions, and will be traversed later if necessary
                  Control := Abandon_Children;

               when others =>
                  null;
            end case;

         when An_Expression =>
            case Expression_Kind (Element) is
               when A_Function_Call =>
                  -- There is no higher risk in declarations
                  State   := Call_In_Declaration;
               when An_Identifier =>
                  Good_Name := Ultimate_Name (Element);
                  if Is_Nil (Good_Name)  -- Dynamic renaming (not Uncheckable since we know it is a variable)
                    or else Declaration_Kind (Corresponding_Name_Declaration (Element))
                    = A_Variable_Declaration
                  then
                     State := Risk_Level'Max (State, Variable_In_Declaration);
                  end if;
               when An_Attribute_Reference =>
                  -- Traverse prefix only
                  Traverse_Declaration (Prefix (Element), Control, State);
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

   --------------------------------
   -- Exception_Propagation_Risk --
   --------------------------------


   function Exception_Propagation_Risk (SP_Body : Asis.Declaration; Max_Level : Risk_Level) return Risk_Level is

      -- Procedure to check if a raise statement (or equivalent) is encountered in a handler
      -- returns No_Risk or Always only
      procedure Pre_Procedure_Handler (Element : in     Asis.Element;
                                       Control : in out Asis.Traverse_Control;
                                       State   : in out Risk_Level) is
         use Asis, Asis.Elements, Utilities, Thick_Queries;
         SP : Asis.Expression;
      begin
         case Statement_Kind (Element) is
            when A_Raise_Statement =>
               State   := Always;
               Control := Terminate_Immediately;
            when A_Procedure_Call_Statement =>
               SP := Called_Simple_Name (Element);
               if not Is_Nil (SP) then
                  -- It is nil for implicit or explicit dereference
                  declare
                     SP_Name : constant Wide_String := To_Upper (Full_Name_Image (SP));
                  begin
                     if SP_Name = "ADA.EXCEPTIONS.RAISE_EXCEPTION"
                       or else SP_Name = "ADA.EXCEPTIONS.RERAISE_OCCURRENCE"
                     then
                        State   := Always;
                        Control := Terminate_Immediately;
                     end if;
                  end;
               end if;
            when others =>
               null;
         end case;
      end Pre_Procedure_Handler;

      procedure Traverse_Handler is new Asis.Iterator.Traverse_Element (Risk_Level,
                                                                        Pre_Procedure_Handler,
                                                                        Post_Procedure_Null);

      use Asis, Asis.Elements, Asis.Declarations, Asis.Statements;
      Level       : Risk_Level;
      The_Control : Traverse_Control := Continue;

      Handlers : constant Asis.Exception_Handler_List := Body_Exception_Handlers (SP_Body);
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
      Level := No_Risk;
      for I in Handlers'Range loop
         declare
            Stats : constant Asis.Statement_List := Handler_Statements (Handlers (I));
         begin
            for J in Stats'Range loop
               Traverse_Handler (Stats (J), The_Control, Level);
               if Level = Always then
                  return Always;
               end if;
            end loop;
         end;
      end loop;

      -- Level is No_Risk here
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
      procedure Pre_Procedure (Element : in     Asis.Element;
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
            Good_Prefix := Prefix (Element);
            if Expression_Kind (Good_Prefix) = A_Selected_Component then
               Good_Prefix := Selector (Good_Prefix);
            end if;

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

            SP_Declaration := Corresponding_Name_Declaration (Good_Prefix);

            case Declaration_Kind (SP_Declaration) is
               when A_Procedure_Body_Declaration | A_Function_Body_Declaration =>
                  Risk := Exception_Propagation_Risk (SP_Declaration, State.Check_Level);
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
                                                      (Corresponding_Name_Declaration
                                                       (Generic_Unit_Name
                                                        (SP_Declaration))),
                                                     State.Check_Level);
                  if Risk >= State.Check_Level then
                     Report (Rule_Id,
                             State,
                             Get_Location (Corresponding_Body
                                           (Corresponding_Name_Declaration
                                            (Generic_Unit_Name
                                             (SP_Declaration)))),
                             "generic """ &  A4G_Bugs.Name_Image (Generic_Unit_Name (SP_Declaration))
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
      end Pre_Procedure;

      procedure Post_Procedure (Element : in     Asis.Element;
                                Control : in out Asis.Traverse_Control;
                                State   : in out EP_Rule_Context) is
         pragma Unreferenced (Element, Control, State);
      begin
         null;
      end Post_Procedure;

      procedure Traverse is new Asis.Iterator.Traverse_Element (EP_Rule_Context, Pre_Procedure, Post_Procedure);

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
               Current_Context : Root_Context'Class := Extended_Matching_Context (Parameters,
                                                                                  Formal_Name (Call, I));
            begin
               if Current_Context /= No_Matching_Context then
                  -- Parameter found
                  Traverse (Actual_Parameter (Actuals (I)), The_Control, EP_Rule_Context (Current_Context));

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
                 := Extended_Matching_Context (Parameters, Formal_Name (Instantiation, I));
               SP_Declaration : Asis.Declaration;
           begin
               if Current_Context /= No_Matching_Context then
                  -- Parameter found
                  -- Actual must be an identifier (or else it is not for us, dereference for example)
                  if Expression_Kind (Actual_Parameter (Actuals (I))) = An_Identifier then
                     SP_Declaration := Corresponding_Name_Declaration (Ultimate_Name
                                                                       (Actual_Parameter
                                                                        (Actuals (I))));

                     case Declaration_Kind (SP_Declaration) is
                        when A_Procedure_Body_Declaration | A_Function_Body_Declaration =>
                           Risk := Exception_Propagation_Risk (SP_Declaration,
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
                                                               (Corresponding_Name_Declaration
                                                                (Generic_Unit_Name
                                                                 (SP_Declaration))),
                                                               EP_Rule_Context (Current_Context).Check_Level);
                           if Risk >= EP_Rule_Context (Current_Context).Check_Level then
                              Report (Rule_Id,
                                      Current_Context,
                                      Get_Location (Corresponding_Body
                                                    (Corresponding_Name_Declaration
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

   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Declaration   : in Asis.Declaration) is
      use Asis;
      use Framework.Reports;

      Risk        : Risk_Level := No_Risk;
      The_Control : Traverse_Control := Continue;
   begin
      if not Rule_Used (Kw_Declaration) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Traverse_Declaration (Declaration, The_Control, Risk);
      if Risk >= Declaration_Context.Check_Level then
         Report (Rule_Id,
                 Declaration_Context,
                 Get_Location (Declaration),
                 "declaration can propagate exceptions" & Risk_Message (Risk));
      end if;
   end Process_Declaration;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Exception_Propagation;
