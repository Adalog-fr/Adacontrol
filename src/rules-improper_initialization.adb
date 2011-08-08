----------------------------------------------------------------------
--  Rules.Improper_Initialization - Package body                    --
--                                                                  --
--  This  software  is  (c)  CSEE  and Adalog  2004-2006.  The  Ada --
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

-- Asis
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

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

package body Rules.Improper_Initialization is
   use Framework;

   -- Algorithm
   --
   -- Bodies of things that can have a body are analyzed by Process_Structure
   -- The (local) map Object_Map is indexed by the names of local objects and out parameters
   -- It is initialized by Add_Out_Parameters and Add_Variables, all objects marked with a reference of
   -- None.
   --
   -- The statements are then traversed by Process_Statements. When a variable is written to, its status
   -- is changed to Assigned in Object_Map.
   -- When an "if" or "case" statement is encountered, a local map is initialized
   -- with the objects from Object_Map that are not yet known to be initialized.
   -- For each path, objects *not* written are removed from the local map; therefore, after traversing all
   -- paths, only objects written from all paths remain: they are marked as Assigned in the Object_Map
   --
   -- As a side effect, if an object is read while its Reference is None, it means it is read
   -- before being assigned; its Reference is then set to Read_Before_Assign

   type Object_Kind is (K_Out_Parameter, K_Variable, K_Initialized_Variable);
   package Object_Kind_Flag_Utilities is new Framework.Language.Flag_Utilities (Object_Kind, "K_");

   type Extension_Kind is (M_Access, M_Limited);
   package Extension_Kind_Modifier_Utilities is new Framework.Language.Modifier_Utilities (Extension_Kind, "M_");

   type Usage_Flags is array (Object_Kind) of Boolean;

   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;

   Extensions : array (Object_Kind) of Extension_Kind_Modifier_Utilities.Modifier_Set
     := (others => Extension_Kind_Modifier_Utilities.Empty_Set);

   type Usage_Contexts is array (Object_Kind) of Basic_Rule_Context;
   Usage : Usage_Contexts;

   type Reference_Kind is (None, Assigned, Read_Before_Assign);
   type Object_Information is
      record
         Identifier : Asis.Defining_Name;
         Kind       : Object_Kind;
         Reference  : Reference_Kind;
      end record;

   package Object_Info_Map is new Binary_Map (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
                                              Object_Information,
                                              Ada.Strings.Wide_Unbounded."<",
                                              Ada.Strings.Wide_Unbounded.">");


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Object_Kind_Flag_Utilities.Help_On_Flags ("Parameter(s): [access] [limited]");
      User_Message ("Control out parameters and local variables that are improperly initialized");
      User_Message ("(not initialized for all paths, or given an unnecessary initial value)");
   end Help;


   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Framework.Language;
      use Object_Kind_Flag_Utilities, Extension_Kind_Modifier_Utilities;

      Key : Object_Kind;
      Ext : Modifier_Set;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Ext := Get_Modifier_Set;
            Key := Get_Flag_Parameter (Allow_Any => False);

            if Rule_Used (Key) then
               Parameter_Error (Rule_Id, "rule can be specified only once for each parameter");
            end if;

            Rule_Used (Key)  := True;
            Usage (Key)      := Basic.New_Context (Rule_Use_Type, Label);
            Extensions (Key) := Ext;
         end loop;
      else
         if Rule_Used /= Usage_Flags'(others => False) then
            Parameter_Error (Rule_Id, "rule can be specified only once for each parameter");
         end if;

         Rule_Used := Usage_Flags'(others => True);
         Usage     := Usage_Contexts'(others => Basic.New_Context (Rule_Use_Type, Label));
      end if;
   end Add_Use;


   -------------
   -- Command --
   -------------

   procedure Command (Action : in Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager, Extension_Kind_Modifier_Utilities;
   begin
      case Action is
         when Clear =>
            Rule_Used  := (others => False);
            Extensions := (others => Empty_Set);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------------------------------
   -- Non_Array_Component_Declaration --
   -------------------------------------

   function Non_Array_Component_Declaration (Declaration : Asis.Declaration) return Asis.Declaration is
      -- If Declaration is not an array : return Declaration
      -- If Declaration is an array: return the declaration of the first component which is not itself an array
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      Temp       : Asis.Element;
      Definition : constant Asis.Definition := Type_Declaration_View (Declaration);
   begin
      case Definition_Kind (Definition) is
         when A_Type_Definition =>
            case Type_Kind (Definition) is
               when An_Unconstrained_Array_Definition
                  | A_Constrained_Array_Definition
                    =>
                  Temp := Subtype_Simple_Name (Component_Subtype_Indication
                                               (Array_Component_Definition (Definition)));
                  if Expression_Kind (Temp) = An_Attribute_Reference then
                     Temp := Prefix (Temp);
                     if Expression_Kind (Temp) = A_Selected_Component then
                        Temp := Selector (Temp);
                     end if;
                  end if;
                  return Non_Array_Component_Declaration (Corresponding_Name_Declaration (Temp));
               when others =>
                  return Declaration;
            end case;
         when A_Subtype_Indication =>
            Temp := Subtype_Simple_Name (Definition);
            if Expression_Kind (Temp) = An_Attribute_Reference then
               -- 'Class or 'Base, properties are the same as the prefix
               Temp := Prefix (Temp);
               if Expression_Kind (Temp) = A_Selected_Component then
                  Temp := Selector (Temp);
               end if;
            end if;
            -- Here, Temp is the simple name of the subtype
            return Non_Array_Component_Declaration
                    (Ultimate_Type_Declaration
                     (Corresponding_Name_Declaration (Temp)));
         when A_Private_Type_Definition =>
            return Non_Array_Component_Declaration (Corresponding_Type_Declaration
                                                    (Enclosing_Element (Definition)));
         when others =>
            -- Including Not_An_Element, case of a task type of the form "task T;"
            return Declaration;
      end case;
   end Non_Array_Component_Declaration;

   ----------------------
   -- Check_Object_Use --
   ----------------------

   procedure Check_Object_Use (Entity : Asis.Element; Object_Map : in out Object_Info_Map.Map) is
      use Asis, Asis.Iterator;

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out Null_State);
      procedure Traverse is new Traverse_Element (Null_State, Pre_Procedure, Null_State_Procedure);

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out Null_State)
      is
         use Asis.Elements, Asis.Expressions;
         use Ada.Strings.Wide_Unbounded;
         use Object_Info_Map, Framework.Reports, Thick_Queries, Utilities;

         Good_Name : Asis.Expression;
      begin
         case Element_Kind (Element) is
            when An_Expression =>
               case Expression_Kind (Element) is
                  when An_Identifier =>
                     Good_Name := Ultimate_Name (Element);
                     if Is_Nil (Good_Name) then
                        -- Renaming of something dynamic, ignore
                        Uncheckable (Rule_Id,
                                     False_Negative,
                                     Get_Location (Element),
                                     "Entity is not statically determinable");
                        return;
                     end if;
                     declare
                        Key  : constant Unbounded_Wide_String
                          := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Good_Name)));
                        Info : Object_Information;
                     begin
                        if Is_Present (Object_Map, Key) then
                           Info := Fetch (Object_Map, Key);
                           if Info.Reference = None then
                              case Info.Kind is
                                 when K_Out_Parameter =>
                                    Report (Rule_Id,
                                            Usage (K_Out_Parameter),
                                            Get_Location (Element),
                                            "use of uninitialized out parameter: " & Name_Image (Element));
                                 when K_Variable =>
                                    Report (Rule_Id,
                                            Usage (K_Variable),
                                            Get_Location (Element),
                                            "use of uninitialized variable: " & Name_Image (Element));
                                 when K_Initialized_Variable =>
                                    null;
                              end case;
                              Info.Reference := Read_Before_Assign;
                              Add (Object_Map, Key, Info);
                           end if;
                        end if;
                     end;

                  when An_Attribute_Reference =>
                     -- Do not traverse the attribute name
                     Traverse (Prefix (Element), Control, State);
                     Control := Abandon_Children;

                  when others =>
                     null;
               end case;

            when A_Pragma =>
               -- nothing interesting here
               Control := Abandon_Children;

            when others =>
               null;
         end case;
      end Pre_Procedure;

      Cont  : Traverse_Control := Continue;
      State : Null_State;
   begin -- Check_Object_Use
      Traverse (Entity, Cont, State);
   end Check_Object_Use;

   ------------------------
   -- Process_Statements --
   ------------------------

   Statements_Break : exception;
   -- Raised when a breaking statement (return, goto...) is encountered
   -- This terminates the analysis of all paths that are enclosing this statement, but not
   -- of paths parallel to the path that contains the breaking statement.
   -- Of course, you should think about this recursively...

   procedure Process_Statements (Object_Map     : in out Object_Info_Map.Map;
                                 Statement_List : in     Asis.Statement_List;
                                 Final_Location :    out Location)
   is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Reports, Thick_Queries, Utilities;
      use Object_Info_Map;

      procedure Process_Target (Name : Asis.Expression) is
         use Ada.Strings.Wide_Unbounded;

         Good_Name : Asis.Expression;
         Info      : Object_Information;
      begin
         -- Find variables that are read as part of the name
         Good_Name := Name;
         loop
            case Expression_Kind (Good_Name) is
               when An_Indexed_Component =>
                  declare
                     Exprs : constant Asis.Expression_List := Index_Expressions (Good_Name);
                  begin
                     for E in Exprs'Range loop
                        Check_Object_Use (Exprs (E), Object_Map);
                     end loop;
                  end;
                  Good_Name := Prefix (Good_Name);
               when A_Slice =>
                  Check_Object_Use (Slice_Range (Good_Name), Object_Map);
                  Good_Name := Prefix (Good_Name);
               when An_Explicit_Dereference =>
                  -- everything on the left of an explicit dereference is a "read"
                  -- traverse the whole thing and leave
                  Good_Name := Prefix (Good_Name);
                  Check_Object_Use (Good_Name, Object_Map);
                  exit;
               when A_Selected_Component =>
                  Good_Name := Prefix (Good_Name);
                  if Is_Access_Expression (Good_Name) then
                     -- This is an implicit dereference, treat like the explicit one
                     Check_Object_Use (Good_Name, Object_Map);
                     exit;
                  end if;
               when others =>
                  exit;
            end case;
         end loop;

         -- Find what the name is refering to, and mark as written
         Good_Name := Name;
         loop
            case Expression_Kind (Good_Name) is
               when An_Identifier =>
                  -- Retrieve the assigned variable definition
                  Good_Name := Ultimate_Name (Good_Name);
                  if Is_Nil (Good_Name) then
                     -- Renaming of something dynamic, ignore
                     Uncheckable (Rule_Id,
                                  False_Positive,
                                  Get_Location (Name),
                                  "Entity is not statically determinable");
                     return;
                  end if;
                  exit;

               when A_Selected_Component =>
                  Good_Name := Selector (Good_Name);

               when A_Type_Conversion =>
                  Good_Name := Converted_Or_Qualified_Expression (Good_Name);

               when A_Slice
                 | An_Indexed_Component
                 =>
                  -- Assignment to part of a variable, ignore
                  return;

               when An_Explicit_Dereference =>
                  Uncheckable (Rule_Id,
                               False_Positive,
                               Get_Location (Name),
                               "Entity is not statically determinable");
                  return;

               when others =>
                  Failure (Rule_Id & ": invalid expression kind", Good_Name);
            end case;
         end loop;

         case Declaration_Kind (Corresponding_Name_Declaration (Good_Name)) is
            when A_Variable_Declaration | A_Parameter_Specification =>
               declare
                  Name_Image : constant Unbounded_Wide_String
                    := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Good_Name)));
               begin
                  if Is_Present (Object_Map, Name_Image) then
                     Info := Fetch (Object_Map, Name_Image);
                     if Info.Reference = None then
                        Info.Reference := Assigned;
                        Add (Object_Map, Name_Image, Info);
                     end if;
                  end if;
               end;
            when others =>
               -- A record component or protected component...
               null;
         end case;
      end Process_Target;

      function Clean_Map (Source : Map) return Map is
         -- Returns a copy of Source with all undecided variables.
         Source_Copy : Map := Source;  -- Because we need a variable for iterate
         Result      : Map;
         procedure Make_One (Key   : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
                             Value : in out Object_Information)
         is
         begin
            if Value.Reference = None then
               Add (Result, Key, Value);
            end if;
         end Make_One;
         procedure Make_All is new Iterate (Make_One);
      begin
         Make_All (Source_Copy);
         return Result;
      end Clean_Map;

      procedure Refresh_One (Key   : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
                             Value : in out Object_Information)
      is
         pragma Unreferenced (Key);
      begin
         case Value.Reference is
            when None =>
               raise Delete_Current;
            when Assigned =>
               Value.Reference := None;
            when Read_Before_Assign =>
               -- Keep that state, to allow to propagate upwards
               null;
         end case;
      end Refresh_One;
      procedure Refresh is new Iterate (Refresh_One);
      -- Delete all entries in the None (not written) state, and returns others
      -- to None for the next round.

      procedure Update_One (Key   : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
                            Value : in out Object_Information)
      is
         Info : Object_Information;
      begin
         -- Since Refresh has been called, all Assigned have been turned to None.
         -- Propagate the (real) state upwards
         Info := Fetch (Object_Map, Key);
         case Info.Reference is
            when None =>
               if Value.Reference = Read_Before_Assign then
                  Info.Reference := Read_Before_Assign;
               else
                  Info.Reference := Assigned;
               end if;
               Add (Object_Map, Key, Info);
            when Read_Before_Assign | Assigned =>
               null;
         end case;
      end Update_One;
      procedure Update_Local is new Iterate (Update_One);
      -- Marks all remaining entries from the local map as Written in the Object_Map

   begin   -- Process_Statements
      Final_Location := Null_Location;
      for Stmt_Index in Statement_List'Range loop
         case Statement_Kind (Statement_List (Stmt_Index)) is
            when An_Assignment_Statement =>
               Check_Object_Use (Assignment_Expression (Statement_List (Stmt_Index)), Object_Map);
               Process_Target (Assignment_Variable_Name (Statement_List (Stmt_Index)));

            when An_Entry_Call_Statement | A_Procedure_Call_Statement =>
               declare
                  Called : constant Asis.Expression := Called_Name (Statement_List (Stmt_Index));
               begin
                  if Expression_Kind (Called) = An_Explicit_Dereference or else Is_Access_Expression (Called) then
                     -- Call is through implicit or explicit dereference
                     Check_Object_Use (Called, Object_Map);
                  end if;
               end;

               -- Check for out parameters in procedure and entry calls
               declare
                  Actuals : constant Asis.Association_List
                    := Call_Statement_Parameters (Statement_List (Stmt_Index));
                  Formal  : Asis.Defining_Name;
               begin
                  for Actual_Index in Actuals'Range loop
                     Formal := Formal_Name (Statement_List (Stmt_Index), Actual_Index);
                     -- Formal is nil for calls to a dispatching operation
                     -- We don't know the mode => pretend we do nothing
                     -- (consistent with the fact that dispatching calls are ignored)
                     if Is_Nil (Formal) then
                        Uncheckable (Rule_Id,
                                     False_Positive,
                                     Get_Location (Statement_List (Stmt_Index)),
                                     "Dispatching_Call");
                     else
                        case Mode_Kind (Enclosing_Element (Formal)) is
                           when Not_A_Mode =>
                              Failure (Rule_Id & ": Not_A_Mode");
                           when An_Out_Mode =>
                              Process_Target (Actual_Parameter (Actuals (Actual_Index)));
                           when others =>
                              Check_Object_Use (Actuals (Actual_Index), Object_Map);
                        end case;
                     end if;
                  end loop;
               end;

            when An_If_Statement
               | A_Case_Statement
                 =>
               if Statement_Kind (Statement_List (Stmt_Index)) = A_Case_Statement then
                  Check_Object_Use (Case_Expression (Statement_List (Stmt_Index)), Object_Map);
               end if;

               declare
                  Paths     : constant Asis.Path_List := Statement_Paths (Statement_List (Stmt_Index));
                  Local_Map : Map;
                  Ignored   : Location;
                  Had_Break : Boolean := False;
               begin
                  -- Don't consider it if it is an "if" without an else path
                  if Path_Kind (Paths (Paths'Last)) not in An_If_Path .. An_Elsif_Path then
                     Local_Map := Clean_Map (Object_Map);
                     for Path_Index in Paths'Range loop
                        begin
                           if Path_Kind (Paths (Path_Index)) in An_If_Path .. An_Elsif_Path then
                              Check_Object_Use (Condition_Expression (Paths (Path_Index)), Object_Map);
                           end if;
                           Process_Statements (Local_Map, Sequence_Of_Statements (Paths (Path_Index)), Ignored);
                        exception
                           when Statements_Break =>
                              Had_Break := True;
                        end;
                        Refresh (Local_Map);
                     end loop;
                     Update_Local (Local_Map);
                     Clear (Local_Map);
                     if Had_Break then
                        raise Statements_Break;
                     end if;
                  end if;
               end;

            when A_Null_Statement =>
               null; -- Precisely...

            when A_Goto_Statement
               | A_Return_Statement
                 =>
                 raise Statements_Break;

            when others =>
               -- End of intialization statements
               Final_Location := Get_Location (Statement_List (Stmt_Index));
               exit;
         end case;
      end loop;
   end Process_Statements;

   -----------------------
   -- Process_Structure --
   -----------------------

   procedure Process_Structure (Elem : in Asis.Element) is
      use Asis.Expressions;
      use Ada.Strings.Wide_Unbounded, Object_Info_Map;

      Object_Map : Object_Info_Map.Map;
      Final_Loc  : Location;

      procedure Add_Out_Parameters (Element : in Asis.Element) is
         use Asis, Asis.Declarations, Asis.Elements;
         use Utilities, Thick_Queries;

         function General_Parameter_Profile (Construct : Asis.Element) return Asis.Parameter_Specification_List is
            use Asis.Statements;
         begin
            if Statement_Kind (Construct) = An_Accept_Statement then
               return Accept_Parameters (Construct);
            else
               return Parameter_Profile (Construct);
            end if;
         end General_Parameter_Profile;

      begin
         if not Rule_Used (K_Out_Parameter) then
            return;
         end if;

         if Declaration_Kind (Element)        = A_Package_Body_Declaration
           or else Declaration_Kind (Element) = A_Task_Body_Declaration
           or else Statement_Kind (Element)   = A_Block_Statement
         then
            -- These have no parameters
            return;
         end if;

         declare
            Params_Profile : constant Asis.Parameter_Specification_List := General_Parameter_Profile (Element);
            Subtype_Decl   : Asis.Declaration;
            Component_Decl : Asis.Declaration;
         begin
            for Profile_Index in Params_Profile'Range loop
               case Mode_Kind (Params_Profile (Profile_Index)) is
                  when Not_A_Mode =>
                     Failure (Rule_Id & ": Not_A_Mode");
                  when An_Out_Mode =>
                     Subtype_Decl := Declaration_Subtype_Mark (Params_Profile (Profile_Index));
                     if Expression_Kind (Subtype_Decl) = An_Attribute_Reference then
                        Subtype_Decl := Prefix (Subtype_Decl);
                     end if;
                     if Expression_Kind (Subtype_Decl) = A_Selected_Component then
                        Subtype_Decl := Selector (Subtype_Decl);
                     end if;
                     -- Here, Subtype_Decl is the name of an appropriate subtype
                     Subtype_Decl   := Corresponding_Name_Declaration (Subtype_Decl);
                     Component_Decl := Non_Array_Component_Declaration (Subtype_Decl);

                     if (Extensions (K_Out_Parameter) (M_Access)  or else not Is_Access (Component_Decl))
                       and then
                        (Extensions (K_Out_Parameter) (M_Limited) or else not Is_Limited (Subtype_Decl))
                     then
                        declare
                           Param_Names : constant Asis.Defining_Name_List := Names (Params_Profile (Profile_Index));
                        begin
                           for Param_Index in Param_Names'Range loop
                              Add (Object_Map,
                                   To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Param_Names (Param_Index)))),
                                   (Identifier => Param_Names (Param_Index),
                                    Kind       => K_Out_Parameter, Reference => None));
                           end loop;
                        end;
                     end if;
                  when others =>
                     null;
               end case;
            end loop;
         end;
      end Add_Out_Parameters;

      procedure Process_Declarations (Element : in Asis.Element) is
         use Asis, Asis.Declarations, Asis.Elements;
         use Thick_Queries, Utilities;

         procedure Add_Variable (Decl : Asis.Declaration) is
            use Asis.Definitions;

            Var_Names      : constant Asis.Defining_Name_List := Names (Decl);
            Var_Kind       : Object_Kind;
            Subtype_Decl   : Asis.Declaration;
            Component_Decl : Asis.Declaration;
         begin
            if Is_Nil (Initialization_Expression (Decl)) then
               Var_Kind := K_Variable;
            else
               Var_Kind := K_Initialized_Variable;
            end if;
            if not Rule_Used (Var_Kind) then
               return;
            end if;

            Subtype_Decl := Object_Declaration_View (Decl);
            if Type_Kind (Subtype_Decl) in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition then
               -- Damn anonymous array
               -- Work on the components, but only one level (case of components of a limited private type)
               Subtype_Decl := Subtype_Simple_Name (Component_Subtype_Indication
                                                    (Array_Component_Definition (Subtype_Decl)));
            else
               -- A_Subtype_Indication
               Subtype_Decl := Subtype_Simple_Name (Subtype_Decl);
            end if;
            if Expression_Kind (Subtype_Decl) = An_Attribute_Reference then
               Subtype_Decl := Prefix (Subtype_Decl);
               if Expression_Kind (Subtype_Decl) = A_Selected_Component then
                  Subtype_Decl := Selector (Subtype_Decl);
               end if;
            end if;
            -- Here, Subtype_Decl is the name of an appropriate subtype
            Subtype_Decl := Corresponding_Name_Declaration (Subtype_Decl);
            -- Now, it is the real subtype declaration

            Component_Decl := Non_Array_Component_Declaration (Subtype_Decl);
            if (not Extensions (Var_Kind) (M_Access)  and then Is_Access (Component_Decl))
              or else
               (not Extensions (Var_Kind) (M_Limited) and then Is_Limited (Subtype_Decl))
            then
               return;
            end if;

            for Var_Index in Var_Names'Range loop
               Add (Object_Map,
                    To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Var_Names (Var_Index)))),
                    (Identifier => Var_Names (Var_Index), Kind => Var_Kind, Reference  => None));
            end loop;
         end Add_Variable;

         Expr : Asis.Expression;
      begin  -- Process_Declarations
         if Statement_Kind (Element) = An_Accept_Statement then
            -- No declarations
            return;
         end if;

         if Declaration_Kind (Element) = A_Package_Body_Declaration then
            -- Process declarations from the corresponding specification
            if Is_Subunit (Element) then
               Process_Declarations (Corresponding_Declaration (Corresponding_Body_Stub (Element)));
            else
               Process_Declarations (Corresponding_Declaration (Element));
            end if;
         end if;

         declare
            Decls : constant Asis.Declaration_List := Declarative_Items (Element);
         begin
            for Decl_Index in Decls'Range loop
               if Declaration_Kind (Decls (Decl_Index)) = An_Object_Renaming_Declaration then
                  -- The actual variable being renamed is not a use of the variable, however
                  -- any other use of variables within the renamed expression is.
                  Expr := A4G_Bugs.Renamed_Entity (Decls (Decl_Index));
                  loop
                     case Expression_Kind (Expr) is
                        when An_Identifier | An_Enumeration_Literal =>
                           -- It is the variable being renamed
                           exit;
                        when An_Explicit_Dereference =>
                           Check_Object_Use (Prefix (Expr), Object_Map);
                           exit;
                        when A_Function_Call =>
                           -- Renaming of the result of a function call
                           -- May include implicit dereference, but we don't care
                           Check_Object_Use (Expr, Object_Map);
                           exit;
                        when An_Indexed_Component =>
                           declare
                              Indexes : constant Asis.Expression_List := Index_Expressions (Expr);
                           begin
                              for I in Indexes'Range loop
                                 Check_Object_Use (Indexes (I), Object_Map);
                              end loop;
                           end;
                           Expr := Prefix (Expr);
                        when A_Slice =>
                           Check_Object_Use (Slice_Range (Expr), Object_Map);
                           Expr := Prefix (Expr);
                        when A_Selected_Component =>
                           if Declaration_Kind (Corresponding_Name_Declaration (Selector (Expr)))
                              not in A_Discriminant_Specification .. A_Component_Declaration
                           then
                              -- This is the object being renamed
                              Check_Object_Use (Prefix (Expr), Object_Map);
                              exit;
                           end if;

                           Expr := Prefix (Expr);
                           if Is_Access_Expression (Expr) then
                              -- Implicit dereference
                              Check_Object_Use (Expr, Object_Map);
                              exit;
                           end if;
                        when A_Type_Conversion =>
                           Expr := Converted_Or_Qualified_Expression (Expr);
                        when others =>
                           -- Remember, this is An_Object_Renaming_Declaration. Hence:
                           -- An_Enumeration_Literal and An_Operator_Symbol are also here rather than with An_Identifier
                           -- An_Attribute_Reference is here, because attributes can appear only for
                           --    attributes that are functions, and thus should have been caught as A_Function_Call
                           Failure ("Improper_Initialization: bad renaming", Expr);
                     end case;
                  end loop;
               else
                  if Declaration_Kind (Decls (Decl_Index)) = A_Variable_Declaration then
                     Add_Variable (Decls (Decl_Index));
                  end if;
                  Check_Object_Use (Decls (Decl_Index), Object_Map);
               end if;
            end loop;
         end;
      end Process_Declarations;

      procedure Report_One (Key : Unbounded_Wide_String; Info : in out Object_Information) is
         pragma Unreferenced (Key);
         use Asis.Declarations;
         use Framework.Reports, Utilities;
      begin
         case Info.Reference is
            when None =>
               case Info.Kind is
                  when K_Out_Parameter =>
                     Report (Rule_Id,
                             Usage (K_Out_Parameter),
                             Get_Location (Info.Identifier),
                             "out parameter """ & Defining_Name_Image (Info.Identifier)
                             & """ not safely initialized"
                             & Choose (Final_Loc = Null_Location,
                                       "",
                                       " before line " & Integer_Img (Get_First_Line (Final_Loc))));
                  when K_Variable =>
                     Report (Rule_Id,
                             Usage (K_Variable),
                             Get_Location (Info.Identifier),
                             "variable """ & Defining_Name_Image (Info.Identifier)
                             & """ not safely initialized"
                             & Choose (Final_Loc = Null_Location,
                                       "",
                                       " before line " & Integer_Img (Get_First_Line (Final_Loc))));
                  when K_Initialized_Variable =>
                     null;
               end case;
            when Read_Before_Assign =>
               case Info.Kind is
                  when K_Out_Parameter =>
                     Report (Rule_Id,
                             Usage (K_Out_Parameter),
                             Get_Location (Info.Identifier),
                             "out parameter """ & Defining_Name_Image (Info.Identifier)
                             & """ used before initialisation");
                  when K_Variable =>
                     Report (Rule_Id,
                             Usage (K_Variable),
                             Get_Location (Info.Identifier),
                             "variable """ & Defining_Name_Image (Info.Identifier)
                             & """ used before initialisation");
                  when K_Initialized_Variable =>
                     null;
               end case;
            when Assigned =>
               case Info.Kind is
                  when K_Initialized_Variable =>
                     Report (Rule_Id,
                             Usage (K_Initialized_Variable),
                             Get_Location (Info.Identifier),
                             "variable """ & Defining_Name_Image (Info.Identifier)
                             & """ unnecessarily initialized in declaration");
                  when others =>
                     null;
               end case;
         end case;
      end Report_One;

      procedure Report_All is new Iterate (Report_One);

   begin -- Process_Structure

      if Rule_Used = Usage_Flags'(others => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Add_Out_Parameters   (Elem);
      Process_Declarations (Elem);

      begin
         Process_Statements (Object_Map, Thick_Queries.Statements (Elem), Final_Loc);
      exception
         when Statements_Break =>
            null;
      end;
      Report_All (Object_Map);

      Clear (Object_Map);
   exception
      when others =>
         -- Prevent memory leak in case of problem
         Clear (Object_Map);
         raise;
   end Process_Structure;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB    => Help'Access,
                                     Add_Use_CB => Add_Use'Access,
                                     Command_CB => Command'Access);
end Rules.Improper_Initialization;
