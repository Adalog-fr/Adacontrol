----------------------------------------------------------------------
--  Rules.Improper_Initialization - Package body                    --
--                                                                  --
--  This software is (c) CSEE and Adalog 2004-2006.                 --
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
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

-- Adalog
with
  Binary_Map,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Queries;
pragma Elaborate (Framework.Language);

package body Rules.Improper_Initialization is
   use Framework, Framework.Control_Manager;

   -- Algorithm
   --
   -- Bodies of things that can have a body are analyzed by Process_Structure
   -- The map Global_Map is indexed by the names of local objects and out parameters
   -- It is initialized by Add_Out_Parameters and Add_Variables, all objects marked
   -- with a reference of None.
   --
   -- The statements are then traversed by Process_Statements. When a variable is written to,
   -- its status is changed to Assigned in Object_Map.
   -- When an "if" or "case" statement is encountered, a local map is initialized
   -- with the objects from Object_Map that are not yet known to be initialized.
   -- For each path, objects *not* written are removed from the local map; therefore, after
   -- traversing all paths, only objects written from all paths remain: they are marked as Assigned in the Object_Map
   --
   -- As a side effect, if an object is read while its Reference is None, it means it is read
   -- before being assigned; its Reference is then set to Read_Before_Assign

   -- Extended return statements are treated specially, since we want them to be trivial statements (it would be
   -- strange to allow normal return statements, and not extended return statements). But it is the only case of a
   -- trivial statement with declarations (the return object). Therefore, they are traversed normally (ignoring the
   -- return object) when encountered as part of a sequence of statements, and treated like a normal return statement
   -- unless they contain any non-trivial statement. In addition, they are traversed by Process_Structure to analyze
   -- use of the return object. Since they are quite rare, and normally not very long, we assume that the cost of
   -- processing them twice is acceptable.

   type Subrules is (K_Out_Parameter, K_Variable, K_Initialized_Variable);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "K_");

   type Extension_Kind is (M_Access, M_Limited, M_Package, M_Return);
   package Extension_Kind_Modifier_Utilities is new Framework.Language.Modifier_Utilities (Extension_Kind, "M_");

   type Usage_Flags is array (Subrules) of Boolean;

   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;

   Extensions : array (Subrules) of Extension_Kind_Modifier_Utilities.Modifier_Set
     := (others => Extension_Kind_Modifier_Utilities.Empty_Set);

   type Usage_Contexts is array (Subrules) of Basic_Rule_Context;
   Usage : Usage_Contexts;

   type Reference_Kind is (None, Assigned, Read_Before_Assign);
   type Object_Information is
      record
         Identifier : Asis.Defining_Name;
         Kind       : Subrules;
         Reference  : Reference_Kind;
      end record;

   package Object_Info_Map is new Binary_Map (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
                                              Object_Information,
                                              Ada.Strings.Wide_Unbounded."<",
                                              Ada.Strings.Wide_Unbounded.">");
   Global_Map : Object_Info_Map.Map;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control out parameters and local variables that are improperly initialized");
      User_Message ("(not initialized for all paths, or given an unnecessary initial value)");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags               ("Parameter(s): {<extras>}");
      Extension_Kind_Modifier_Utilities.Help_On_Modifiers ("    <extras>:");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
      use Subrules_Flag_Utilities, Extension_Kind_Modifier_Utilities;

      Subrule : Subrules;
      Ext     : Modifier_Set;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Ext := Get_Modifier_Set;
            Subrule := Get_Flag_Parameter (Allow_Any => False);

            if Rule_Used (Subrule) then
               Parameter_Error (Rule_Id, "rule can be specified only once for each parameter");
            end if;

            Rule_Used (Subrule)  := True;
            Usage (Subrule)      := Basic.New_Context (Ctl_Kind, Ctl_Label);
            Extensions (Subrule) := Ext;

            if Subrule = K_Out_Parameter and Extensions (Subrule) (M_Return) then
               Parameter_Error (Rule_Id, """return"" cannot be specified for ""out_parameter""");
            end if;
         end loop;
      else
         if Rule_Used /= Usage_Flags'(others => False) then
            Parameter_Error (Rule_Id, "rule can be specified only once for each parameter");
         end if;

         Rule_Used := Usage_Flags'(others => True);
         Usage     := Usage_Contexts'(others => Basic.New_Context (Ctl_Kind, Ctl_Label));
      end if;
   end Add_Control;


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
   -- Returns Nil_Element if the component is of an anonymous access type
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
                  Temp := Component_Definition_View (Array_Component_Definition (Definition));
                  if Definition_Kind (Temp) = An_Access_Definition then
                     -- anonymous access component
                     return Nil_Element;
                  end if;
                  Temp := Subtype_Simple_Name (Temp);
                  if Expression_Kind (Temp) = An_Attribute_Reference then
                     Temp := Simple_Name (Prefix (Temp));
                  end if;
                  return Non_Array_Component_Declaration (Corresponding_Name_Declaration (Temp));
               when others =>
                  return Declaration;
            end case;
         when A_Subtype_Indication =>
            Temp := Subtype_Simple_Name (Definition);
            if Expression_Kind (Temp) = An_Attribute_Reference then
               -- 'Class or 'Base, properties are the same as the prefix
               Temp := Simple_Name (Prefix (Temp));
            end if;
            -- Here, Temp is the simple name of the subtype
            return Non_Array_Component_Declaration
                    (Ultimate_Type_Declaration
                     (Corresponding_Name_Declaration (Temp)));
         when A_Private_Type_Definition =>
            return Non_Array_Component_Declaration (Corresponding_Full_Type_Declaration
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

      procedure Traverse_Attribute_Prefix  (Element : in     Asis.Name;
                                            Control : in out Traverse_Control;
                                            State   : in out Null_State)
      is
         use Asis.Elements, Asis.Expressions;
         use Utilities;
      begin
         case Expression_Kind (Element) is
            when An_Identifier | An_Enumeration_Literal =>
               null;
            when An_Attribute_Reference =>
               Traverse_Attribute_Prefix (Prefix (Element), Control, State);
            when A_Selected_Component =>
               Traverse_Attribute_Prefix (Prefix   (Element), Control, State);
               Traverse_Attribute_Prefix (Selector (Element), Control, State);
            when An_Indexed_Component =>
               for Ind : Asis.Expression of Index_Expressions (Element) loop
                  Traverse (Ind, Control, State);
               end loop;
            when A_Slice =>
               Traverse (Slice_Range (Element), Control, State);
            when A_Function_Call =>
               for P : Asis.Association of Function_Call_Parameters (Element) loop
                  Traverse (P, Control, State);
               end loop;
            when An_Explicit_Dereference =>
               Traverse (Prefix (Element), Control, State);
            when A_Type_Conversion =>
               Traverse_Attribute_Prefix (Converted_Or_Qualified_Expression (Element), Control, State);
            when others =>
               Failure ("Unexpected element in attribute prefix", Element);
         end case;
      end Traverse_Attribute_Prefix;

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out Null_State)
      is
         use Asis.Elements, Asis.Expressions, Asis.Statements;
         use Ada.Strings.Wide_Unbounded;
         use Object_Info_Map, Framework.Queries, Framework.Locations, Framework.Reports, Thick_Queries, Utilities;

         Good_Name : Asis.Expression;
      begin
         case Element_Kind (Element) is

            when A_Definition =>
               case Definition_Kind (Element) is
                  when An_Aspect_Specification =>
                     -- 2012, ignored for the moment
                     Control := Abandon_Children;
                  when others =>
                     null;
               end case;

            when An_Expression =>
               case Expression_Kind (Element) is
                  when An_Identifier =>
                     Good_Name := Ultimate_Name (Element, No_Component => True);
                     if Is_Nil (Good_Name) then
                        -- Renaming of something dynamic, ignore
                        Uncheckable (Rule_Id,
                                     False_Negative,
                                     Get_Location (Element),
                                     "Entity is not statically determinable");
                        return;
                     end if;
                     declare
                        Key  : constant Unbounded_Wide_String := To_Key (Good_Name);
                        Info : Object_Information;
                     begin
                        if Is_Present (Object_Map, Key) then
                           Info := Fetch (Object_Map, Key);
                           if Info.Reference /= Assigned then
                              case Info.Kind is
                                 when K_Out_Parameter =>
                                    Report (Rule_Id,
                                            Usage (K_Out_Parameter),
                                            Get_Location (Element),
                                            "use of uninitialized out parameter: " & Name_Image (Good_Name));
                                 when K_Variable =>
                                    Report (Rule_Id,
                                            Usage (K_Variable),
                                            Get_Location (Element),
                                            "use of uninitialized variable: " & Name_Image (Good_Name));
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

                     -- If the prefix is an explicit dereference, it is a Read
                     -- If the prefix is of an access type, and the attribute is one wich accepts implicit
                     -- dereferences, it is an implicit dereference, and thus a Read
                     -- Otherwise, we must special case the prefix, since its use is not a Read, but there
                     -- can be Reads of the components of the prefix, as in S(I,J)(K).Tab(L)'Access
                     if Is_Access_Expression (Prefix (Element)) then
                        case Attribute_Kind (Element) is
                           when A_Callable_Attribute
                              | A_Component_Size_Attribute
                              | A_Constrained_Attribute
                              | A_First_Attribute
                              | An_Identity_Attribute
                              | A_Last_Attribute
                              | A_Length_Attribute
                              | A_Range_Attribute
                              | A_Storage_Size_Attribute
                              | A_Tag_Attribute
                              | A_Terminated_Attribute
                              | A_Valid_Attribute
                                =>
                              -- Implicit dereference, treat as normal read
                              Traverse (Prefix (Element), Control, State);
                           when others =>
                              Traverse_Attribute_Prefix (Prefix (Element), Control, State);
                        end case;
                     else
                        Traverse_Attribute_Prefix (Prefix (Element), Control, State);
                     end if;
                     Control := Abandon_Children;

                  when others =>
                     null;
               end case;    -- Expression

            when A_Statement =>
               -- we can have statements when check_object_use is called on the statements part of a subpackage
               case Statement_Kind (Element) is
                  when An_Assignment_Statement =>
                     -- Traverse the RHS and only the appropriate parts of the LHS
                     Check_Object_Use (Assignment_Expression (Element), Global_Map);

                     -- We can have use of objects in the LHS, but only as part of indexing (or slices), or dereferences
                     declare
                        Lhs : Asis.Expression := Assignment_Variable_Name (Element);
                     begin
                        loop
                           case Expression_Kind (Lhs) is
                              when An_Identifier =>
                                 exit;
                              when An_Explicit_Dereference =>
                                 Lhs := Prefix (Lhs);
                              when A_Function_Call =>
                                 for P : Asis.Association of Actual_Parameters (Lhs)loop
                                    Check_Object_Use (P, Global_Map);
                                 end loop;
                                 Lhs := Prefix (Lhs);
                              when An_Indexed_Component =>
                                 for Ind : Asis.Expression of Index_Expressions (Lhs) loop
                                    Check_Object_Use (Ind, Global_Map);
                                 end loop;
                                 Lhs := Prefix (Lhs);
                              when A_Slice =>
                                 Check_Object_Use (Slice_Range (Lhs), Global_Map);
                                 Lhs := Prefix (Lhs);
                              when A_Selected_Component =>
                                 Lhs := Selector (Lhs);
                              when A_Type_Conversion =>
                                 Lhs := Converted_Or_Qualified_Expression (Lhs);
                              when others =>
                                 Failure ("Bad expression in LHS", Lhs);
                           end case;
                           if Is_Access_Expression (Lhs) then
                              -- at this point, lhs is an implicit (or even explicit) dereference
                              -- every variable appearing in the expression is a use
                              Check_Object_Use (Lhs, Global_Map);
                              exit;
                           end if;
                        end loop;
                     end;

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
      State : Null_State := (null record);
   begin -- Check_Object_Use
      Traverse (Entity, Cont, State);
   end Check_Object_Use;


   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Report_Loc : Locations.Location; Out_Params_Only : Boolean := False) is
      use Ada.Strings.Wide_Unbounded;
      use Object_Info_Map;

      procedure Report_One (Key : Unbounded_Wide_String; Info : in out Object_Information) is
         pragma Unreferenced (Key);
         use Asis.Declarations;
         use Framework.Locations, Framework.Reports, Utilities;
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
                             & Choose (Report_Loc = Null_Location,
                                       "",
                                       " before line " & ASIS_Integer_Img (Get_First_Line (Report_Loc))));
                  when K_Variable =>
                     if not Out_Params_Only then
                        Report (Rule_Id,
                                Usage (K_Variable),
                                Get_Location (Info.Identifier),
                                "variable """ & Defining_Name_Image (Info.Identifier)
                                & """ not safely initialized"
                                & Choose (Report_Loc = Null_Location,
                                          "",
                                          " before line " & ASIS_Integer_Img (Get_First_Line (Report_Loc))));
                     end if;
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
                     if not Out_Params_Only then
                        Report (Rule_Id,
                                Usage (K_Variable),
                                Get_Location (Info.Identifier),
                                "variable """ & Defining_Name_Image (Info.Identifier)
                                & """ used before initialisation");
                     end if;
                  when K_Initialized_Variable =>
                     null;
               end case;
            when Assigned =>
               case Info.Kind is
                  when K_Initialized_Variable =>
                     if not Out_Params_Only then
                        Report (Rule_Id,
                                Usage (K_Initialized_Variable),
                                Get_Location (Info.Identifier),
                                "variable """ & Defining_Name_Image (Info.Identifier)
                                & """ unnecessarily initialized in declaration");
                     end if;
                  when others =>
                     null;
               end case;
         end case;
      end Report_One;

      procedure Report_All is new Iterate (Report_One);

   begin  -- Do_Report
      Report_All (Global_Map);
   end Do_Report;


   ------------------------
   -- Process_Statements --
   ------------------------

   type Exit_Causes is (End_Of_Statements, Return_Statement, Non_Trivial_Statement);

   procedure Process_Statements (Object_Map     : in out Object_Info_Map.Map;
                                 Statement_List : in     Asis.Statement_List;
                                 Final_Location :    out Locations.Location;
                                 Exit_Cause     :    out Exit_Causes)
   is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Locations, Framework.Reports, Thick_Queries, Utilities;
      use Object_Info_Map;

      procedure Process_Target (Name : Asis.Expression) is
         use Ada.Strings.Wide_Unbounded;
         use Framework.Queries;

         Good_Name : Asis.Expression;
         Info      : Object_Information;
      begin
         -- Find variables that are read as part of the name
         Good_Name := Name;
         loop
            case Expression_Kind (Good_Name) is
               when An_Indexed_Component =>
                  for E : Asis.Expression of Index_Expressions (Good_Name) loop
                     Check_Object_Use (E, Object_Map);
                  end loop;
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
                  -- Can be a subcomponent in the case of a renaming, but then
                  -- it is not considered an assignmant to the variable
                  -- => No_Component must be false
                  Good_Name := Ultimate_Name (Good_Name, No_Component => False);
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
                               "Entity is not statically determinable (dereference)");
                  return;

               when A_Function_Call =>
                  -- Function call on the LHS => must be a user defined implicit dereference
                  Uncheckable (Rule_Id,
                               False_Positive,
                               Get_Location (Name),
                               "Entity is not statically determinable (user defined dereference)");
                  return;

               when others =>
                  Failure (Rule_Id & ": invalid expression kind", Good_Name);
            end case;
         end loop;
         case Declaration_Kind (Corresponding_Name_Declaration (Good_Name)) is
            when A_Variable_Declaration | A_Parameter_Specification | A_Return_Variable_Specification =>
               declare
                  Name_Image : constant Unbounded_Wide_String := To_Key (Good_Name);
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

      begin  -- Clean_Map
         Make_All (Source_Copy);
         return Result;
      end Clean_Map;

      procedure Refresh (On : in out Map; No_Delete : Boolean := False) is
         -- Delete all entries in the None (not written) state (unless no_delete),
         -- and returns others to None for the next round.
         procedure Refresh_One (Key   : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
                                Value : in out Object_Information)
         is
            pragma Unreferenced (Key);
         begin
            case Value.Reference is
               when None =>
                  if not No_Delete then
                     raise Delete_Current;
                  end if;
               when Assigned =>
                  Value.Reference := None;
               when Read_Before_Assign =>
                  -- Keep that state, to allow to propagate upwards
                  null;
            end case;
         end Refresh_One;
         procedure Refresh_All is new Iterate (Refresh_One);

      begin  -- Refresh
         Refresh_All (On);
      end Refresh;

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
      for Stmt : Asis.Statement of Statement_List loop
         case Statement_Kind (Stmt) is
            when An_Assignment_Statement =>
               Check_Object_Use (Assignment_Expression (Stmt), Object_Map);
               Process_Target (Assignment_Variable_Name (Stmt));

            when An_Entry_Call_Statement | A_Procedure_Call_Statement =>
               declare
                  Called : constant Asis.Expression := Called_Name (Stmt);
               begin
                  -- Handle calls to Raise_Exception, Reraise_Occurrence and non returning procedures
                  -- like a raise statement
                  if Expression_Kind (Called) /= An_Indexed_Component            -- Do not choke on entry families
                    and then not Is_Access_Expression (Called)                   -- or implicit dereferences
                    and then Expression_Kind (Called) /= An_Explicit_Dereference -- or explicit ones
                  then
                     declare
                        SP_Name : constant Wide_String := To_Upper (Full_Name_Image (Called));
                     begin
                        if SP_Name = "ADA.EXCEPTIONS.RAISE_EXCEPTION"
                          or else SP_Name = "ADA.EXCEPTIONS.RERAISE_OCCURRENCE"
                          or else Corresponding_Pragma_Set (Called) (A_No_Return_Pragma)
                        then
                           Exit_Cause := Return_Statement;
                           return;
                        end if;
                     end;
                  end if;

                  if Expression_Kind (Called) = An_Explicit_Dereference or else Is_Access_Expression (Called) then
                     -- Call is through implicit or explicit dereference
                     Check_Object_Use (Called, Object_Map);
                  end if;

                  -- Check for out parameters in procedure and entry calls
                  declare
                     Actuals : constant Asis.Association_List := Call_Statement_Parameters (Stmt);
                     Formal  : Asis.Defining_Name;
                  begin
                     for Actual_Index in Actuals'Range loop
                        Formal := Formal_Name (Stmt, Actual_Index);
                        -- Formal is nil for calls to a dispatching operation and for calls to attribute procedures
                        -- For attributes, only T'Read and T'Input have an out parameter (the second one).
                        -- For dispatching operations, we don't know the mode => pretend we do nothing
                        -- (consistent with the fact that dispatching calls are ignored)
                        if Is_Nil (Formal) then
                           if Expression_Kind (Called) = An_Attribute_Reference then
                              case Attribute_Kind (Called) is
                                 when A_Read_Attribute | An_Input_Attribute =>
                                    if Actual_Index = 2 then
                                       Process_Target (Actual_Parameter (Actuals (Actual_Index)));
                                    else
                                       Check_Object_Use (Actuals (Actual_Index), Object_Map);
                                    end if;
                                 when A_Write_Attribute | An_Output_Attribute =>
                                    Check_Object_Use (Actuals (Actual_Index), Object_Map);
                                 when others =>
                                    Failure ("Not a procedure call attribute", Called);
                              end case;
                           else
                              Uncheckable (Rule_Id, False_Positive, Get_Location (Stmt), "Dispatching_Call");
                           end if;
                        else -- Normal case
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
               end;

            when An_If_Statement
               | A_Case_Statement
                 =>
               if Statement_Kind (Stmt) = A_Case_Statement then
                  Check_Object_Use (Case_Expression (Stmt), Object_Map);
               end if;

               declare
                  Paths     : constant Asis.Path_List := Statement_Paths (Stmt);
                  Local_Map : Map;
                  Ignored   : Location;
               begin
                  -- If it is an "if" without an else path, leave the map empty since there is a (pseudo)
                  -- path that writes into no variable. We must traverse the statement however to deal with
                  -- return and non-trivial statements.
                  if Path_Kind (Paths (Paths'Last)) in An_If_Path .. An_Elsif_Path then
                     Clear (Local_Map);
                  else
                     Local_Map := Clean_Map (Object_Map);
                  end if;

                  for Path : Asis.Path of Paths loop
                     if Path_Kind (Path) in An_If_Path .. An_Elsif_Path then
                        Check_Object_Use (Condition_Expression (Path), Object_Map);
                     end if;
                     Process_Statements (Local_Map,
                                         Sequence_Of_Statements (Path),
                                         Final_Location => Ignored,
                                         Exit_Cause     => Exit_Cause);
                     case Exit_Cause is
                        when Non_Trivial_Statement =>
                           Final_Location := Get_Location (Stmt);
                           return;
                        when Return_Statement =>
                           -- This branch does not count for uninitialized variables
                           Refresh (Local_Map, No_Delete => True);
                        when End_Of_Statements =>
                           Refresh (Local_Map);
                     end case;
                  end loop;
                  Update_Local (Local_Map);
               end;

            when A_Null_Statement =>
               null; -- Precisely...

            when A_Return_Statement =>
               declare
                  Expr : constant Asis.Expression := Return_Expression (Stmt);
               begin
                  if not Is_Nil (Expr) then
                     Check_Object_Use (Expr, Object_Map);
                  end if;
               end;
               Final_Location := Get_Location (Stmt);

               -- Out parameters must be OK at this point
               Do_Report (Final_Location, Out_Params_Only => True);

               Exit_Cause := Return_Statement;
               return;

            when A_Raise_Statement =>
               -- This is somehow like a return statement, except that since out parameters are not guaranteed
               -- to be properly written back, there is no point in checking that they are initialized.

               Exit_Cause := Return_Statement;
               return;

            when An_Extended_Return_Statement =>
               -- Note: only out parameters (in 2012!) and variables are concerned here
               -- the case of the return object is checked by having Process_Structure invoked on
               -- the extended return statement

               declare
                  use Asis.Declarations;
                  Init_Expr : constant Asis.Expression := Initialization_Expression (Return_Object_Declaration (Stmt));
                  -- Local_Map is not really used, since there is only one path, but we need to call Process_Statements
                  -- to detect non trivial statements and uses before intialisation
                  Local_Map         : Map := Clean_Map (Object_Map);
                  Return_Statements : constant Asis.Statement_List
                    := Extended_Return_Statements (Stmt);
               begin
                  if not Is_Nil (Init_Expr) then
                     Check_Object_Use (Init_Expr, Object_Map);
                  end if;
                  Process_Statements (Local_Map,
                                      Return_Statements,
                                      Final_Location => Final_Location,
                                      Exit_Cause     => Exit_Cause);
                  case Exit_Cause is
                     when Non_Trivial_Statement =>
                        return;
                     when Return_Statement =>
                        null;
                     when End_Of_Statements =>
                        -- Note: end of statements is an implicit return, but Final_Location must point at "end"
                        -- Beware that the statement list (and "end") is optional!
                        if Is_Nil (Return_Statements) then
                           Final_Location := Get_End_Location (Stmt);
                        else
                           Final_Location := Get_Previous_Word_Location (Stmt,
                                                                         Matching => "END",
                                                                         Starting => From_Tail);
                        end if;
                  end case;
               end;

               -- Out parameters must be OK at this point (2012!)
               Do_Report (Final_Location, Out_Params_Only => True);

               Exit_Cause := Return_Statement;
               return;

            when others =>
               -- non trivial statements
               Final_Location := Get_Location (Stmt);
               Exit_Cause     := Non_Trivial_Statement;
               return;
         end case;
      end loop;

      Final_Location := Null_Location;
      Exit_Cause     := End_Of_Statements;
   end Process_Statements;

   -----------------------
   -- Process_Structure --
   -----------------------

   procedure Process_Structure (Elem : in Asis.Element) is
      use Object_Info_Map;
      use Framework.Locations, Framework.Queries;

      Final_Loc  : Location;
      Exit_Cause : Exit_Causes;

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

         use Asis.Expressions;
      begin  -- Add_Out_Parameters
         if not Rule_Used (K_Out_Parameter) then
            return;
         end if;

         if Declaration_Kind (Element)        = A_Package_Body_Declaration
           or else Declaration_Kind (Element) = A_Task_Body_Declaration
           or else Statement_Kind (Element)   = A_Block_Statement
           or else Statement_Kind (Element)   = An_Extended_Return_Statement
         then
            -- These have no parameters
            return;
         end if;

         declare
            Subtype_Decl   : Asis.Declaration;
            Component_Decl : Asis.Declaration;
         begin
            for Profile : Asis.Parameter_Specification of General_Parameter_Profile (Element) loop
               case Mode_Kind (Profile) is
                  when Not_A_Mode =>
                     Failure (Rule_Id & ": Not_A_Mode");
                  when An_Out_Mode =>
                     Subtype_Decl := Corresponding_Name_Declaration
                                     (Simple_Name
                                      (Strip_Attributes
                                       (Declaration_Subtype_Mark
                                        (Profile))));
                     Component_Decl := Non_Array_Component_Declaration (Subtype_Decl);
                     -- If Component_Decl is Nil_Element, the component is of an (anonymous) access type
                     if (Extensions (K_Out_Parameter) (M_Access)
                            or else not (Is_Nil (Component_Decl) or else Is_Access_Subtype (Component_Decl)))
                       and then
                        (Extensions (K_Out_Parameter) (M_Limited) or else not Is_Limited (Subtype_Decl))
                     then
                        for Param : Asis.Defining_Name of Names (Profile) loop
                           Add (Global_Map,
                                To_Key (Param),
                                (Identifier => Param, Kind => K_Out_Parameter, Reference  => None));
                        end loop;
                     end if;
                  when others =>
                     null;
               end case;
            end loop;
         end;
      end Add_Out_Parameters;

      procedure Process_Declarations (Element : in Asis.Element) is
         use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
         use Thick_Queries, Utilities;

         procedure Add_Variable (Decl : Asis.Declaration) is
            use Asis.Definitions;

            Var_Names      : constant Asis.Defining_Name_List := Names (Decl);
            Var_Kind       : Subrules;
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
            if Declaration_Kind (Decl) /= A_Return_Variable_Specification and then Extensions (Var_Kind) (M_Return) then
               return;
            end if;
            if not Extensions (Var_Kind) (M_Package) then
               case Declaration_Kind (Enclosing_Element (Decl)) is
                  when A_Package_Declaration
                     | A_Generic_Package_Declaration
                     | A_Package_Body_Declaration
                       =>
                     return;
                  when others =>
                     null;
               end case;
            end if;

            Subtype_Decl := Object_Declaration_View (Decl);
            if Definition_Kind (Subtype_Decl) = An_Access_Definition then
               -- 2005: anonymous access type
               -- It is an access type
               -- It is not a limited type
               -- Needs special processing since we have no type declaration
               if not Extensions (Var_Kind) (M_Access) then
                  return;
               end if;

               for Var : Asis.Defining_Name of Var_Names loop
                  Add (Global_Map,
                       To_Key (Var),
                       (Identifier => Var, Kind => Var_Kind, Reference  => None));
               end loop;
               return;
            elsif Type_Kind (Subtype_Decl) in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition then
               -- Damn anonymous array
               -- Work on the components, but only one level (case of components of a limited private type)
               Subtype_Decl := Component_Definition_View (Array_Component_Definition (Subtype_Decl));
               if Definition_Kind (Subtype_Decl) = An_Access_Definition then
                  -- Anonymous array of anonymous access! Those people should not be allowed to program...
                  -- Handle as above
                  if not Extensions (Var_Kind) (M_Access) then
                     return;
                  end if;

                  for Var : Asis.Defining_Name of Var_Names loop
                     Add (Global_Map,
                          To_Key (Var),
                          (Identifier => Var, Kind => Var_Kind, Reference  => None));
                  end loop;
                  return;
               end if;
               Subtype_Decl := Subtype_Simple_Name (Subtype_Decl);
            else
               -- A_Subtype_Indication
               Subtype_Decl := Subtype_Simple_Name (Subtype_Decl);
            end if;
            if Expression_Kind (Subtype_Decl) = An_Attribute_Reference then
               Subtype_Decl := Simple_Name (Prefix (Subtype_Decl));
            end if;
            -- Here, Subtype_Decl is the name of an appropriate subtype
            Subtype_Decl := Corresponding_Name_Declaration (Subtype_Decl);
            -- Now, it is the real subtype declaration

            Component_Decl := Non_Array_Component_Declaration (Subtype_Decl);
            -- If Component_Decl is Nil_Element, the component is of an (anonymous) access type
            if (not Extensions (Var_Kind) (M_Access)
                and then (Is_Nil (Component_Decl) or else Is_Access_Subtype (Component_Decl)))
              or else
               (not Extensions (Var_Kind) (M_Limited) and then Is_Limited (Subtype_Decl))
            then
               return;
            end if;

            for Var : Asis.Defining_Name of Var_Names loop
               Add (Global_Map,
                    To_Key (Var),
                    (Identifier => Var, Kind => Var_Kind, Reference  => None));
            end loop;
         end Add_Variable;

         function Generalized_Declarative_Items (E : Asis.Element) return Asis.Declaration_List is
         -- Return the declarative items of E, unless E is an extended return, in which case a list
         -- with the return object declaration as the only element is returned
         use Asis.Statements;
         begin
            if Statement_Kind (E) = An_Extended_Return_Statement then
               return (1 => Return_Object_Declaration (E));
            else
                return Declarative_Items (E);
            end if;
         end Generalized_Declarative_Items;

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

         for Decl : Asis.Declaration of Generalized_Declarative_Items (Element) loop
            case Element_Kind (Decl) is
               when A_Declaration =>
                  case Declaration_Kind (Decl) is
                     when A_Procedure_Declaration
                        | A_Null_Procedure_Declaration
                        | A_Function_Declaration
                        | A_Task_Type_Declaration
                        | A_Single_Task_Declaration
                        | A_Procedure_Body_Declaration
                        | A_Function_Body_Declaration
                        | A_Task_Body_Declaration
                        | A_Generic_Procedure_Declaration
                        | A_Generic_Function_Declaration
                        | A_Generic_Package_Declaration
                        =>
                        null;

                     when A_Package_Declaration =>
                        for Pack_Decl : Asis.Declaration of Declarative_Items (Decl) loop
                           Check_Object_Use (Pack_Decl, Global_Map);
                        end loop;

                     when A_Package_Body_Declaration =>
                        -- Beware: nothing to do for bodies of generic packages
                        if not Is_Generic_Unit (Decl) then
                           for Pack_Decl : Asis.Declaration of Declarative_Items (Decl) loop
                              Check_Object_Use (Pack_Decl, Global_Map);
                           end loop;

                           for Pack_Stmt : Asis.Statement of Thick_Queries.Statements (Decl) loop
                              Check_Object_Use (Pack_Stmt, Global_Map);
                           end loop;
                        end if;

                     when A_Procedure_Instantiation
                        | A_Function_Instantiation
                        | A_Package_Instantiation
                        =>
                        -- Objects may be used in actuals of instantiation
                        for Param : Asis.Association of Generic_Actual_Part (Decl) loop
                           Check_Object_Use (Param, Global_Map);
                        end loop;

                     when An_Object_Renaming_Declaration =>
                        -- The actual variable being renamed is not a use of the variable, however
                        -- any other use of variables within the renamed expression is.
                        Expr := Renamed_Entity (Decl);
                        loop
                           case Expression_Kind (Expr) is
                              when An_Identifier | An_Enumeration_Literal =>
                                 -- It is the variable being renamed
                                 exit;
                              when An_Explicit_Dereference =>
                                 Check_Object_Use (Prefix (Expr), Global_Map);
                                 exit;
                              when A_Function_Call =>
                                 -- Renaming of the result of a function call
                                 -- May include implicit dereference, but we don't care
                                 Check_Object_Use (Expr, Global_Map);
                                 exit;
                              when An_Indexed_Component =>
                                 for Index : Asis.Expression of Index_Expressions (Expr) loop
                                    Check_Object_Use (Index, Global_Map);
                                 end loop;
                                 Expr := Prefix (Expr);
                              when A_Slice =>
                                 Check_Object_Use (Slice_Range (Expr), Global_Map);
                                 Expr := Prefix (Expr);
                              when A_Selected_Component =>
                                 if Declaration_Kind (Corresponding_Name_Declaration (Selector (Expr)))
                                    not in A_Discriminant_Specification .. A_Component_Declaration
                                 then
                                    -- This is the object being renamed
                                    Check_Object_Use (Prefix (Expr), Global_Map);
                                    exit;
                                 end if;

                                 Expr := Prefix (Expr);
                                 if Is_Access_Expression (Expr) then
                                    -- Implicit dereference
                                    Check_Object_Use (Expr, Global_Map);
                                    exit;
                                 end if;
                              when A_Type_Conversion | A_Qualified_Expression =>
                                 Expr := Converted_Or_Qualified_Expression (Expr);
                              when others =>
                                 -- Remember, this is An_Object_Renaming_Declaration. Hence:
                                 -- An_Enumeration_Literal and An_Operator_Symbol are also here rather
                                 --    than with An_Identifier
                                 -- An_Attribute_Reference is here, because attributes can appear only for
                                 --    attributes that are functions, and thus should have been caught as
                                 --    A_Function_Call
                                 Failure ("Improper_Initialization: bad renaming", Expr);
                           end case;
                        end loop;

                     when A_Variable_Declaration | A_Return_Variable_Specification =>
                           Add_Variable (Decl);
                           Check_Object_Use (Decl, Global_Map);

                     when others =>
                        Check_Object_Use (Decl, Global_Map);
                  end case;  -- declarations

               when A_Pragma =>
                  -- Assume there is no "real" use of variables in pragmas
                  null;

               when A_Clause =>
                  -- Representation clauses are static, and hence cannot use variables
                  -- with and use clauses do not use variables.
                  null;

               when others =>
                  Failure ("Improper element in declarative part", Decl);
            end case;
         end loop;
      end Process_Declarations;

   begin -- Process_Structure

      if Rule_Used = Usage_Flags'(others => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Add_Out_Parameters   (Elem);
      Process_Declarations (Elem);

      Process_Statements (Global_Map, Thick_Queries.Statements (Elem), Final_Loc, Exit_Cause);

      -- we deliberately ignore exception handlers, no normal initialization should happen there!

      Do_Report (Final_Loc);
      Clear (Global_Map);
   exception
      when others =>
         -- Prevent memory leak in case of problem
         Clear (Global_Map);
         raise;
   end Process_Structure;

begin  -- Rules.Improper_Initialization
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Improper_Initialization;
