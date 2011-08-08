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

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

package body Rules.Exception_Propagation is
   use Framework, Ada.Strings.Wide_Unbounded;

   -- Note that "interface" will be a reserved work in Ada 2006
   type Target_Kind is (Kw_Interface, Kw_Parameter, Kw_Task);

   type Usage is array (Target_Kind) of Boolean;
   Rule_Used : Usage := (others => False);
   Save_Used : Usage;

   package Convention_Map is new Binary_Map (Unbounded_Wide_String, Simple_Context);
   use Convention_Map;

   Parameters  : Context_Store;
   Conventions : Convention_Map.Map;
   Task_Label  : Unbounded_Wide_String;
   Task_Type   : Rule_Types;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter 1     : interface | parameter | task");
      User_Message ("Parameter 2 .. N: for interface: <convention name>");
      User_Message ("                  for parameter: <full name of parameters known to expect call-backs>");
      User_Message ("                  for task: nothing");
      User_Message ("Control that certain kinds of subprograms (or tasks) cannot propagate exceptions");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded, Framework.Language, Utilities;

      function Get_Target_Parameter is new Framework.Language.Get_Flag_Parameter (Flags     => Target_Kind,
                                                                                  Allow_Any => False,
                                                                                  Prefix    => "KW_");
      Target  : Target_Kind;
   begin
      if not Parameter_Exists then
         Parameter_Error ("Parameters required for rule " & Rule_Id);
      end if;

      Target := Get_Target_Parameter;
      case Target is
         when KW_Interface =>
            if not Parameter_Exists then
               Parameter_Error ("At least two parameters required for rule " & Rule_Id);
            end if;

            while Parameter_Exists loop
               declare
                  Convention : constant Wide_String := To_Upper (Get_String_Parameter);
               begin
                  Add (Conventions,
                       To_Unbounded_Wide_String (Convention),
                       Simple_Context'(Rule_Type, To_Unbounded_Wide_String (Label)));
               exception
                  when Already_In_Store =>
                     Parameter_Error ("Convention already given for rule " & Rule_Id
                                      & ": " & Convention);
               end;
            end loop;
            Rule_Used (KW_Interface) := True;

         when KW_Parameter =>
            if not Parameter_Exists then
               Parameter_Error ("At least two parameters required for rule " & Rule_Id);
            end if;

            while Parameter_Exists loop
               declare
                  Entity : constant Entity_Specification := Get_Entity_Parameter;
               begin
                  Associate (Parameters, Entity, Simple_Context'(Rule_Type,
                                                                 To_Unbounded_Wide_String (Label)));
               exception
                  when Already_In_Store =>
                     Parameter_Error ("Parameter already given for rule " & Rule_Id
                                      & ": " & Image (Entity));
               end;
            end loop;
            Rule_Used (KW_Parameter) := True;

         when Kw_Task =>
            if Parameter_Exists then
               Parameter_Error ("No parameter for ""task""");
            end if;

            if Rule_Used (KW_Task) then
               Parameter_Error ("""task"" already given for rule " & Rule_Id);
            end if;

            Task_Label          := To_Unbounded_Wide_String (Label);
            Task_Type           := Rule_Type;
            Rule_Used (KW_Task) := True;
      end case;
   end Add_Use;

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

   ------------------------------
   -- Can_Propagate_Exceptions --
   ------------------------------

   function Can_Propagate_Exceptions (SP_Body : Asis.Declaration) return Boolean is

      -- Procedure to check if a raise statement (or equivalent) is encountered
      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Asis.Traverse_Control;
                               State   : in out Boolean) is
         use Asis, Asis.Elements, Utilities, Thick_Queries;
         Callee : Asis.Expression;
      begin
         case Statement_Kind (Element) is
            when A_Raise_Statement =>
               State   := True;
               Control := Terminate_Immediately;
            when A_Procedure_Call_Statement =>
               Callee := Called_Simple_Name (Element);

               declare
                  SP_Name : constant Wide_String := To_Upper (Full_Name_Image (Callee));
               begin
                  if SP_Name = "ADA.EXCEPTIONS.RAISE_EXCEPTION"
                    or else SP_Name = "ADA.EXCEPTIONS.RERAISE_EXCEPTION"
                  then
                     State   := True;
                     Control := Terminate_Immediately;
                  end if;
               end;
            when others =>
               null;
         end case;
      end Pre_Procedure;

      procedure Post_Procedure (Element : in     Asis.Element;
                                Control : in out Asis.Traverse_Control;
                                State   : in out Boolean) is
         pragma Unreferenced (Element, Control, State);
      begin
         null;
      end Post_Procedure;

      procedure Traverse is new Asis.Iterator.Traverse_Element (Boolean, Pre_Procedure, Post_Procedure);

      use Asis, Asis.Elements, Asis.Declarations, Asis.Statements;
      Found       : Boolean;
      The_Control : Traverse_Control := Continue;

      Handlers : constant Asis.Exception_Handler_List := Body_Exception_Handlers (SP_Body);
   begin
      -- Is there a handler ?
      if Handlers = Nil_Element_List then
         return True;
      end if;

      -- Is there an others choice (it must be last)?
      declare
         Choices      : constant Asis.Element_List := Exception_Choices (Handlers (Handlers'last));
         Others_Found : Boolean := False;
      begin
         for Choice in Choices'Range loop
            if Definition_Kind (Choices (Choice)) = An_Others_Choice then
               Others_Found := True;
            end if;
         end loop;

         if not Others_Found then
            return True;
         end if;
      end;

      -- Is there any raise statement?
      Found := False;
      for I in Handlers'Range loop
         declare
            Statements : constant Asis.Statement_List := Handler_Statements (Handlers (I));
         begin
            for J in Statements'Range loop
               Traverse (Statements (J), The_Control, Found);
               if Found then
                  return True;
               end if;
            end loop;
         end;
      end loop;

      return False;
   end Can_Propagate_Exceptions;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : Asis.Statement) is
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Statements, Asis.Elements, Asis.Expressions, Thick_Queries;

      -- Procedure to check if a 'Access or 'Address is encountered
      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Asis.Traverse_Control;
                               State   : in out Simple_Context)
      is
         pragma Unreferenced (Control);
         use Asis, Asis.Declarations;
         use Framework.Reports;
         SP_Declaration : Asis.Declaration;
      begin
         if Expression_Kind (Element) = An_Attribute_Reference
           and then A4G_Bugs.Attribute_Kind (Element) in An_Access_Attribute .. An_Address_Attribute
         then
            case Expression_Kind (Prefix (Element)) is
               when An_Explicit_Dereference =>
                  -- Called subprogram is dynamic, nothing we can do
                  return;
               when A_Selected_Component =>
                  SP_Declaration := Corresponding_Name_Declaration (Ultimate_Name (Selector (Prefix (Element))));
               when others =>
                  SP_Declaration := Corresponding_Name_Declaration (Ultimate_Name (Prefix (Element)));
            end case;

            case Declaration_Kind (SP_Declaration) is
               when A_Procedure_Body_Declaration | A_Function_Body_Declaration =>
                  if Can_Propagate_Exceptions (SP_Declaration) then
                     Report (Rule_Id,
                             To_Wide_String (State.Rule_Label),
                             State.Rule_Type,
                             Get_Location (SP_Declaration),
                             "subprogram """
                             &  Defining_Name_Image (Names (SP_Declaration)(1))
                             & """ can propagate exceptions, used as call-back at "
                             & Image (Get_Location (Call)));
                  end if;
               when A_Procedure_Instantiation | A_Function_Instantiation =>
                  if Can_Propagate_Exceptions (Corresponding_Body
                                               (Corresponding_Name_Declaration
                                                (Generic_Unit_name
                                                 (SP_Declaration))))
                  then
                     Report (Rule_Id,
                             To_Wide_String (State.Rule_Label),
                             State.Rule_Type,
                             Get_Location (Corresponding_Body
                                           (Corresponding_Name_Declaration
                                            (Generic_Unit_name
                                             (SP_Declaration)))),
                             "generic """ &  Name_Image (Generic_Unit_Name (SP_Declaration))
                             & """ can propagate exceptions, "
                             & "instance """ &  Defining_Name_Image (Names (SP_Declaration)(1))
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
                                State   : in out Simple_Context) is
         pragma Unreferenced (Element, Control, State);
      begin
         null;
      end Post_Procedure;

      procedure Traverse is new Asis.Iterator.Traverse_Element (Simple_Context, Pre_Procedure, Post_Procedure);

   begin
      if not Rule_Used (KW_Parameter) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Dispatching_Call (Call) then
         -- Improvement needed here, but it's quite difficult
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

      declare
         Actuals : constant Asis.Association_List := Actual_Parameters (Call);
         The_Control : Asis.Traverse_Control := Asis.Continue;
      begin
         for I in Actuals'Range loop
            declare
               Current_Context : Rule_Context'Class := Extended_Matching_Context (Parameters,
                                                                                  Formal_Name (Call, I));
            begin
               if Current_Context /= No_Matching_Context then
                  -- Parameter found
                  Traverse (Actual_Parameter (Actuals (I)), The_Control, Simple_Context (Current_Context));

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
   begin
      if not Rule_Used (KW_Parameter) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Actuals : constant Asis.Association_List := Generic_Actual_Part (Instantiation);
      begin
         for I in Actuals'Range loop
            declare
               Current_Context : Rule_Context'Class := Extended_Matching_Context (Parameters,
                                                                                  Formal_Name (Instantiation, I));
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
                           if Can_Propagate_Exceptions (SP_Declaration) then
                              Report (Rule_Id,
                                      To_Wide_String (Simple_Context (Current_Context).Rule_Label),
                                      Simple_Context (Current_Context).Rule_Type,
                                      Get_Location (SP_Declaration),
                                      "subprogram """
                                      &  Name_Image (Actual_Parameter (Actuals (I)))
                                      & """ can propagate exceptions, used as call-back in instantiation at "
                                      & Image (Get_Location (Instantiation)));
                           end if;

                        when A_Procedure_Instantiation | A_Function_Instantiation =>
                           if Can_Propagate_Exceptions (Corresponding_Body
                                                        (Corresponding_Name_Declaration
                                                         (Generic_Unit_name
                                                          (SP_Declaration))))
                           then
                              Report (Rule_Id,
                                      To_Wide_String (Simple_Context (Current_Context).Rule_Label),
                                      Simple_Context (Current_Context).Rule_Type,
                                      Get_Location (Corresponding_Body
                                                    (Corresponding_Name_Declaration
                                                     (Generic_Unit_name
                                                      (SP_Declaration)))),
                                      "generic """
                                      &  Name_Image (Generic_Unit_Name (SP_Declaration))
                                      & """ can propagate exceptions, instance """
                                      &  Defining_Name_Image (Names (SP_Declaration)(1))
                                      & """ at "
                                      & Image (Get_Location (SP_Declaration))
                                      & " used as call-back in instantiation at "
                                      & Image (Get_Location (Instantiation))
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
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Utilities;
      use Framework.Reports;
      Spec_Declaration : Asis.Declaration;
   begin
      if not Rule_Used (KW_Interface) then
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
      begin
         for I in All_Pragmas'Range loop
            case Pragma_Kind (All_Pragmas (I)) is
               when An_Export_Pragma | A_Convention_Pragma =>
                  -- The convention is always the first argument of the pragma
                  declare
                     Convention : constant Unbounded_Wide_String
                       := To_Unbounded_Wide_String (To_Upper (Name_Image
                                                              (Actual_Parameter
                                                               (Pragma_Argument_Associations
                                                                (All_Pragmas (I))(1)))));
                  begin
                     if Is_Present (Conventions, Convention)
                       and then Can_Propagate_Exceptions (Element)
                     then
                        Report (Rule_Id,
                                To_Wide_String (Fetch (Conventions, Convention).Rule_Label),
                                Fetch (Conventions, Convention).Rule_Type,
                                Get_Location (Element),
                                "subprogram """
                                & Defining_Name_Image (Names (Element)(1))
                                & """ can propagate exceptions, interfaced to "
                                & To_Wide_String (Convention));
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
   begin
      if not Rule_Used (KW_Task) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Can_Propagate_Exceptions (Task_Body) then
         Report (Rule_Id,
                 To_Wide_String (Task_Label),
                 Task_Type,
                 Get_Location (Task_Body),
                 "task """
                 & Defining_Name_Image (Names (Task_Body)(1))
                 & """ can propagate exceptions");
      end if;
   end Process_Task_Body;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access,
                                     Prepare => Prepare'Access);
end Rules.Exception_Propagation;
