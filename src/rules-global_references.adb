----------------------------------------------------------------------
--  Rules.Global_References - Package body                          --
--                                                                  --
--  This software is (c) BELGOCONTROL and Adalog 2004-2005.         --
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
  Ada.Strings.Wide_Unbounded,
  Ada.Unchecked_Deallocation;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Exceptions,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Binary_Map,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language,
  Framework.Queries;
pragma Elaborate (Framework.Language);

package body Rules.Global_References is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- The algorithm for identifying globals is quite straightforward. The procedure Check_Body
   -- is in charge of identifying global variables usage for a given body, and it is called
   -- when an appropriate body is encountered. It traverses the body, and stores information
   -- about encountered variables in the (global) map "Usage". This variable serves also to
   -- note local declarations (not reported) and bodies that have already been traversed.
   --
   -- The difficulty comes from the "multiple" (or "multiple_non_atomic") option. Since we want
   -- to report global variables accessed from several entities, the reporting can be done only
   -- at the end of the analysis.
   -- Moreover, we wanted the messages to be sorted according to variables referenced (and not
   -- according to referencing bodies f.e.). Moreover, which bodies are part of "multiple" is
   -- done per - rule, i.e. :
   --   check Global_References (multiple, P1, P2);
   --   check Global_References (multiple, P1, P3);
   -- must report of global variables accessed by P1 and P2 on one side, and by P1 and P3 on the
   -- other side.
   -- Therefore, the following structures are used to maintain information about all variables:
   --   - Rules_Map is a map to keep information about each specified rule. The key is the rule
   --     index, and the value contains the rule label, type, and whether the rule is "all" ,
   --     "multiple" or "multiple_non_atomic".
   --   - Globals_Info keeps information about all global variables encountered. The key is the
   --     variable's Full_Name_Image, and the value holds a reference to the variable's
   --     declaration, and a map (Referencing_Rule_Map) of rules that mention at least one body
   --     that references the variable.
   --   - The Referencing_Rule_Map has the rule index as the key, and a linear list of the bodies
   --     that reference the variable as the value (plus a flag to deal with task and protected
   --     types).
   --
   -- At the end of the check of an appropriate construct, info gathered into "Usage" serves to
   -- update Rules_Map. At the end of the run, the report is printed by traversing Rules_Map.
   --
   -- Checked_Bodies is a multi-valued context store that identifies bodies that are to be
   -- traversed, and controls that mention this body.
   --
   -- There is a special tricky case for renamings, since usage of the renamed entity comes
   -- from usage of the renaming entity.
   -- Therefore, when an identifier is defined by a renaming declaration, we traverse the renamed
   -- expression, passing the read/write status of the identifier through the State parameter.
   -- Note that this parameter will be used only when Expression_Usage_Kind returns Untouched,
   -- which corresponds to the actual variable name in the renamed entity. Other elements used in
   -- the renaming (indexing...) will be handled normally.
   -- On the other hand, the renaming can be part of the traversed body, and would be traversed
   -- normally, but at that point we would not know whether the renaming entity is Read or Write.
   -- We therefore discard renamings during normal traversal.

   type Subrules is (K_All, K_Read, K_Written, K_Multiple, K_Multiple_Non_Atomic);
   package Subrules_Utilities is new Framework.Language.Flag_Utilities (Subrules, "K_");

   type Checked_Kind is (K_Name, K_Task, K_Protected, K_Function, K_Procedure);
   package Checked_Kind_Utilities is new Framework.Language.Flag_Utilities (Checked_Kind, "K_");

   Rules_Used : Control_Index := 0;
   Save_Used  : Control_Index;

   -- Management of checked bodies
   type Body_Context is new Root_Context with
      record
         Rule_Inx : Control_Index;
      end record;

   Checked_Bodies  : Context_Store;
   package Checked_Bodies_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Checked_Bodies);

   -- Information about encountered entities
   -- - Variables whose declaration is encountered during traversal are marked Non_Global
   --   to prevent them from being traced
   -- - Other variables are marked Read or Written according to usage
   -- - Callable entities and task are marked Checked, to avoid them being analyzed more than
   --   once (and especially to protect against recursion).
   type Usage_Value is (Read, Written, Non_Global, Checked);
   type Usage_Record is
      record
         Definition : Asis.Expression;
         Usage      : Usage_Value;
      end record;

   package Usage_Map is new Binary_Map (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
                                        Usage_Record,
                                        Ada.Strings.Wide_Unbounded."<",
                                        Ada.Strings.Wide_Unbounded.">");
   Usage : Usage_Map.Map;

   -- Management of rules
   type Rule_Info is new Basic_Rule_Context with
      record
         Reference : Subrules;
      end record;
   package Rules_Info_Map is new Binary_Map (Control_Index, Rule_Info);

   Rules_Map : Rules_Info_Map.Map;

   -- Management of globally accessed variables
   type Referencing_Body;
   type Body_List is access Referencing_Body;
   type Referencing_Body is
      record
         Decl  : Asis.Declaration;
         Usage : Usage_Value;
         Next  : Body_List;
      end record;
   procedure Free is new Ada.Unchecked_Deallocation (Referencing_Body, Body_List);

   type Write_Kind is (No_Write, Single_Write, Multiple_Writes);
   type Referencing_Info is
      record
         Includes_Type : Boolean;
         Bodies        : Body_List;
         Write_Count   : Write_Kind := No_Write;
      end record;
   procedure Clear_Referencing_Info (V : in out Referencing_Info);

   package Referencing_Rule_Map is new Binary_Map (Control_Index, Referencing_Info);
   procedure Clear_All is new Referencing_Rule_Map.Generic_Clear_And_Release (Clear_Referencing_Info);


   type Globals_Info_Record is
      record
         Definition : Asis.Expression;
         Rule_Map   : Referencing_Rule_Map.Map;
      end record;
   procedure Clear_Info_Record (V : in out Globals_Info_Record);

   package Globals_Info_Map is new Binary_Map (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
                                               Globals_Info_Record,
                                               Ada.Strings.Wide_Unbounded."<",
                                               Ada.Strings.Wide_Unbounded.">");
   procedure Clear_All is new Globals_Info_Map.Generic_Clear_And_Release (Clear_Info_Record);

   Globals_Info : Globals_Info_Map.Map;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control global (in a relative sense) variables referenced ");
      User_Message ("directly or indirectly from some specific constructs");
      User_Message;
      Subrules_Utilities.Help_On_Flags ("Parameter(1): ");
      Checked_Kind_Utilities.Help_On_Flags ("Parameter(2..): ", Extra_Value => "<name>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Utilities, Checked_Kind_Utilities;
      use Rules_Info_Map;

      Reference : Subrules;
      Check     : Checked_Kind;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "Parameters required");
      end if;
      Reference := Get_Flag_Parameter (Allow_Any => False);

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "Missing specification of checked bodies");
      end if;

      Rules_Used := Rules_Used + 1;
      while Parameter_Exists loop
         Check := Get_Flag_Parameter (Allow_Any => True);
         declare
            Entity : Entity_Specification;
         begin
            case Check is
               when K_Name =>
                  Entity := Get_Entity_Parameter;
               when K_Task =>
                  Entity := Value ("TASK");
               when K_Protected =>
                  Entity := Value ("PROTECTED");
               when K_Function =>
                  Entity := Value ("FUNCTION");
               when K_Procedure =>
                  Entity := Value ("PROCEDURE");
            end case;
            Add (Rules_Map,
                 Key => Rules_Used,
                 Value => (Basic.New_Context (Ctl_Kind, Ctl_Label) with Reference));
            Associate (Checked_Bodies,
                       Entity,
                       Body_Context'(Rule_Inx => Rules_Used),
                       Additive => True);
         end;
      end loop;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager, Rules_Info_Map;
   begin
      case Action is
         when Clear =>
            Rules_Used := 0;
            Clear (Checked_Bodies);
            Clear (Rules_Map);
         when Suspend =>
            Save_Used  := Rules_Used;
            Rules_Used := 0;
         when Resume =>
            Rules_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Checked_Bodies);
   end Prepare;

   -----------------------
   -- Clear_Info_Record --
   -----------------------

   procedure Clear_Info_Record (V : in out Globals_Info_Record) is
   begin
      Clear_All (V.Rule_Map);
   end Clear_Info_Record;

   ----------------------------
   -- Clear_Referencing_Info --
   ----------------------------

   procedure Clear_Referencing_Info (V : in out Referencing_Info) is
      Current : Body_List := V.Bodies;
      Next    : Body_List;
   begin
      while Current /= null loop
         Next := Current.Next;
         Free (Current);
         Current := Next;
      end loop;
   end Clear_Referencing_Info;

   ----------------
   -- Check_Body --
   ----------------

   -- Forward declarations:
   procedure Pre_Procedure  (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Usage_Value);
   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Usage_Value);
   procedure Traverse is new Asis.Iterator.Traverse_Element (Usage_Value, Pre_Procedure, Post_Procedure);
   procedure Check_Body (Body_Decl : Asis.Declaration);

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Usage_Value)
   is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Ada.Strings.Wide_Unbounded, Thick_Queries, Utilities, Usage_Map;
      use Framework.Queries, Framework.Locations, Framework.Reports;

      New_State : Usage_Value;

      procedure Process_Call is
         Called : constant Call_Descriptor := Corresponding_Call_Description (Element);
      begin
         case Called.Kind is
            when A_Regular_Call =>
               if Declaration_Kind (Called.Declaration)
                  not in A_Formal_Procedure_Declaration .. A_Formal_Function_Declaration
               then
                  begin
                     Check_Body (Corresponding_Body (Called.Declaration));
                  exception
                     when Asis.Exceptions.ASIS_Inappropriate_Element =>
                     -- ASIS bug: the subprogram is declared within a formal package
                       A4G_Bugs.Trace_Bug ("Global_References: call of SP from formal package");
                  end;
               end if;
            when An_Enumeration_Literal | A_Predefined_Entity_Call | An_Attribute_Call =>
               null;
            when A_Dereference_Call | A_Dispatching_Call =>
               -- Assume no global references, short of knowing what it does
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Element),
                            "Dispatching call or call to dynamic entity; assuming no global references");
         end case;
      end Process_Call;

  begin  -- Pre_Procedure
      case Element_Kind (Element) is
         when A_Defining_Name =>
            if Declaration_Kind (Enclosing_Element (Element)) = A_Variable_Declaration then
               Add (Usage,
                    To_Key (Element),
                    (Nil_Element, Non_Global));
            end if;

         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Renaming_Declaration =>
                  -- If the renaming entity is used, the declaration will be traversed manually
                  -- and we cannot traverse it at this point since we don't know how the renaming
                  -- entity is used.
                  -- if the renaming entity is not used, we don't care...
                  Control := Abandon_Children;
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

         when An_Expression =>
            case Expression_Kind (Element) is
               when A_Function_Call =>
                  Process_Call;
               when An_Identifier =>
                  begin
                     case Declaration_Kind (Corresponding_Name_Declaration (Element)) is
                        when A_Variable_Declaration =>
                           declare
                              Key : constant Unbounded_Wide_String := To_Key (Element);
                              U   : Usage_Record;
                           begin
                              if Is_Present (Usage, Key) then
                                 U := Fetch (Usage, Key);
                                 case U.Usage is
                                    when Non_Global | Written =>
                                       null;
                                    when Read =>
                                       case Expression_Usage_Kind (Element) is
                                          when Read =>
                                             null;
                                          when Write | Read_Write =>
                                             U.Usage := Written;
                                          when Untouched =>
                                             -- Usage comes from the renaming identifier
                                             U.Usage := State;
                                          when Unknown =>
                                             -- Assume no write, short of knowing what it does
                                             Uncheckable (Rule_Id,
                                                          False_Negative,
                                                          Get_Location (Element),
                                                          "Dispatching call or call to dynamic entity; "
                                                          & "assuming not an [in] out parameter");
                                       end case;
                                    when Checked =>
                                       Failure ("Variable marked as checked or unknown", Element);
                                 end case;
                              else
                                 case Expression_Usage_Kind (Element) is
                                    when Read =>
                                       U := (Corresponding_Name_Definition (Element), Read);
                                    when Write | Read_Write =>
                                       U := (Corresponding_Name_Definition (Element), Written);
                                    when Untouched =>
                                       -- Usage comes from the renaming identifier
                                       U := (Corresponding_Name_Definition (Element), State);
                                    when Unknown =>
                                       -- Assume simple read, short of knowing what it does
                                       Uncheckable (Rule_Id,
                                                    False_Negative,
                                                    Get_Location (Element),
                                                    "Dispatching call or call to dynamic entity; "
                                                    & "assuming not an [in] out parameter");
                                       U := (Corresponding_Name_Definition (Element), Read);
                                 end case;
                              end if;
                              Add (Usage, Key, U);
                           end;

                        when An_Object_Renaming_Declaration =>
                           -- we must process every element of the renamed entity in the context of
                           -- the current call.
                           case Expression_Usage_Kind (Element) is
                              when Untouched =>
                                 -- chain of renamings...
                                 New_State := State;
                              when Read =>
                                 New_State := Read;
                              when Write | Read_Write =>
                                 New_State := Written;
                              when Unknown =>
                                 -- Assume simple read, short of knowing what it does
                                 Uncheckable (Rule_Id,
                                              False_Negative,
                                              Get_Location (Element),
                                              "Dispatching call or call to dynamic entity; "
                                              & "assuming not an [in] out parameter");
                                 New_State := Read;
                           end case;
                           Traverse (Renamed_Entity (Corresponding_Name_Declaration (Element)),
                                     Control,
                                     New_State);
                        when others =>
                           -- Not a variable
                           null;
                     end case;
                  exception
                     when Asis.Exceptions.ASIS_Failed =>
                        -- KLUDGE FOR ASIS BUG: Constraint_Error in Corresponding_Name_Declaration.
                        -- This happens only if Element is a generic formal "in" parameter
                        -- (but not "in out"). We can thus safely ignore it (it is not a variable)
                        A4G_Bugs.Trace_Bug ("Global_References.Pre_Procedure: CE for generic formal in parameter");
                  end;

               when An_Attribute_Reference =>
                  -- If the attribute is a function, it is a predefined function => not analyzed
                  -- If the attribute is a value, the reference to the prefix is not an access
                  -- => In all cases, ignore.
                  Control := Abandon_Children;
               when others =>
                  null;
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_Procedure_Call_Statement =>
                  Process_Call;
               when An_Entry_Call_Statement =>
                  -- Process only protected entries (not task entries)
                  declare
                     Called : constant Asis.Declaration := Corresponding_Called_Entity (Element);
                  begin
                     if not Is_Nil (Called)
                       and then Definition_Kind (Enclosing_Element (Called)) = A_Protected_Definition
                     then
                        Check_Body (Corresponding_Body (Called));
                     end if;
                  end;
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
                             State   : in out Usage_Value)
   is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure;

   procedure Check_Body (Body_Decl : Asis.Declaration) is
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Declarations, Asis.Elements;
      use Framework.Queries, Framework.Rules_Manager, Thick_Queries, Usage_Map;
   begin
      if Is_Nil (Body_Decl) -- Predefined operations, dispatching call, f.e. ...
        or else Is_Banned (Body_Decl, Rule_Id)
        or else Element_Kind (Body_Decl) = A_Pragma                     -- case of pragma Import
        or else Definition_Kind (Body_Decl) = An_Aspect_Specification   -- or aspect Import
      then
         return;
      end if;

      if Ultimate_Origin (Body_Decl) /= An_Application_Unit then
         -- Do not check the standard library (nor the RTL!)
         return;
      end if;

      declare
         Body_Name : constant Unbounded_Wide_String := To_Key (Names (Body_Decl)(1));
         Control   : Asis.Traverse_Control := Continue;
         Ignored   : Usage_Value := Checked;  -- This value will cause failure if used not from renaming
      begin
         if Is_Present (Usage, Body_Name) then
            return;
         end if;

         Add (Usage, Body_Name, (Nil_Element, Checked));
         Traverse (Body_Decl, Control, Ignored);
      end;
   end Check_Body;

   --------------------
   -- Update_Globals --
   --------------------

   procedure Update_Globals (Decl : Asis.Declaration; Rule_Inx : Control_Index) is
      procedure Update_One_Variable (Key   : in     Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
                                     Value : in out Usage_Record)
      is
         use Asis, Asis.Declarations, Asis.Elements;
         use Globals_Info_Map, Referencing_Rule_Map;
      begin
         if Value.Usage in Read .. Written then
            declare
               Globals_Entry  : Globals_Info_Record := Fetch (Globals_Info,
                                                              Key,
                                                              Default_Value => (Value.Definition,
                                                                                Referencing_Rule_Map.Empty_Map));
               Ref_Info       : Referencing_Info := Fetch (Globals_Entry.Rule_Map,
                                                           Rule_Inx,
                                                           Default_Value => (False, null, No_Write));
               Spec_Decl_Kind : constant Declaration_Kinds := Declaration_Kind (Corresponding_Declaration (Decl));
            begin
               Ref_Info := (Includes_Type => Ref_Info.Includes_Type
                                             or Spec_Decl_Kind = A_Task_Type_Declaration
                                             or Spec_Decl_Kind = A_Protected_Type_Declaration,
                            Bodies        => new Referencing_Body'(Decl, Value.Usage, Ref_Info.Bodies),
                            Write_Count   => Ref_Info.Write_Count);
               if Value.Usage = Written then
                  -- The global is written, increment the count of write accesses differently
                  -- whether the global has been written from a task, a protected object or any
                  -- object kind.
                  case Spec_Decl_Kind is
                     when A_Task_Type_Declaration
                       | A_Protected_Type_Declaration =>
                        -- When the written from a task or a protected object, the global may be
                        -- accessed from multiple instances of the object
                        Ref_Info.Write_Count := Multiple_Writes;
                     when others =>
                        -- In any other case, we got a simple write access
                        if Ref_Info.Write_Count = No_Write then
                           Ref_Info.Write_Count := Single_Write;
                        else
                           Ref_Info.Write_Count := Multiple_Writes;
                        end if;
                  end case;
               end if;
               Add (Globals_Entry.Rule_Map, Rule_Inx, Ref_Info);
               Add (Globals_Info, Key, Globals_Entry);
            end;
         end if;
      end Update_One_Variable;

      procedure Update_All_Variables is new Usage_Map.Iterate (Update_One_Variable);

   begin  -- Update_Globals
      Update_All_Variables (Usage);
   end Update_Globals;

   ------------------
   -- Process_Body --
   ------------------

   procedure Process_Body (The_Body : in Asis.Declaration) is

      Iter : Context_Iterator := Checked_Bodies_Iterator.Create;

      procedure General_Iter_Reset is
         use Asis, Asis.Declarations, Asis.Elements;
         Name           : Asis.Defining_Name;
         From_Protected : Boolean;
      begin
         -- If this is a protected operation, the associated context is the one of the
         -- enclosing protected object
         if Declaration_Kind (Enclosing_Element (The_Body)) = A_Protected_Body_Declaration then
            Name           := Names (Enclosing_Element (The_Body))(1);
            From_Protected := True;
         else
            Name := Names (The_Body) (1);
            From_Protected := False;
         end if;

         Reset (Iter, Name);
         if not Is_Exhausted (Iter) then
            return;
         end if;

         if From_Protected then
            Reset (Iter, Value ("PROTECTED"));
            return;
         end if;

         case Declaration_Kind (The_Body) is
            when A_Task_Body_Declaration =>
               Reset (Iter, Value ("TASK"));
            when A_Function_Body_Declaration =>
               Reset (Iter, Value ("FUNCTION"));
            when A_Procedure_Body_Declaration =>
               Reset (Iter, Value ("PROCEDURE"));
            when others =>
               null;  -- Iter is exhausted
         end case;
     end General_Iter_Reset;

   begin                       -- Process_Body
      if Rules_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      General_Iter_Reset;
      if Is_Exhausted (Iter) then
         return;
      end if;

      Check_Body (The_Body);

      while not Is_Exhausted (Iter) loop
         Update_Globals (The_Body, Body_Context (Value (Iter)).Rule_Inx);
         Next (Iter);
      end loop;

      Usage_Map.Clear (Usage);
   end Process_Body;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      use Ada.Strings.Wide_Unbounded;

      procedure Report_One_Variable (Var_Key : in Unbounded_Wide_String; Var_Value : in out Globals_Info_Record) is
         pragma Unreferenced (Var_Key);

         procedure Report_One_Rule (Rule_Key : in Control_Index; Rule_Value : in out Referencing_Info) is
            use Asis, Asis.Declarations, Asis.Elements;
            use Framework.Locations, Framework.Reports, Rules_Info_Map, Thick_Queries, Utilities;

            Rule_Params   : constant Rule_Info        := Fetch (Rules_Map, Rule_Key);  -- Must be there
            Ref_Info      : constant Referencing_Info := Rule_Value;                   -- Ref_Info.Bodies /= null
            Body_Ptr      : Body_List;
            Different_POs : Boolean;
            PO_Decl       : Asis.Declaration := Nil_Element;
         begin
            -- Avoid cases when we must not print results
            case Rule_Params.Reference is
               when K_All | K_Read | K_Written =>
                  null;

               when K_Multiple | K_Multiple_Non_Atomic =>
                  if not Ref_Info.Includes_Type and Ref_Info.Bodies.Next = null then -- Only one body, not K_All
                     return;
                  end if;

                  -- We have several bodies, but we must check:
                  --   - that we are not in the case of Multiple_Non_Atomic with an Atomic variable
                  --   - that they are not all from the same protected object
                  --     (protected types have already been dealt with by flag Includes_Type)
                  if Rule_Params.Reference = K_Multiple_Non_Atomic
                  -- no more than one writer to the variable
                    and then Ref_Info.Write_Count /= Multiple_Writes
                  -- `pragma Atomic' or `pragma Atomic_Components' set upon declaration or its type
                    and then (Corresponding_Pragma_Set (Var_Value.Definition)
                              and Pragma_Set'(An_Atomic_Pragma | An_Atomic_Components_Pragma => True,
                                              others                                         => False))
                    /= Pragma_Set'(others => False)
                  then
                     return;
                  elsif not Ref_Info.Includes_Type then
                     -- We have several bodies, but we must check that they are not all from the same protected object
                     -- (protected types have already been dealt with by flag Includes_Type)
                     Body_Ptr      := Ref_Info.Bodies;
                     Different_POs := False;
                     while Body_Ptr /= null loop
                        if Declaration_Kind (Enclosing_Element (Body_Ptr.Decl)) /= A_Protected_Body_Declaration then
                           Different_POs := True;
                           exit;
                        end if;

                        if Is_Nil (PO_Decl) then
                           PO_Decl := Enclosing_Element (Body_Ptr.Decl);
                        elsif not Is_Equal (Enclosing_Element (Body_Ptr.Decl), PO_Decl) then
                           Different_POs := True;
                           exit;
                        end if;

                        Body_Ptr := Body_Ptr.Next;
                     end loop;
                     -- When all bodies are from the same protected object, we must not print results
                     if not Different_POs then
                        return;
                     end if;
                  end if;
            end case;

            -- Cases when we must not print results are avoided, so we can print results
            Body_Ptr := Ref_Info.Bodies;
            while Body_Ptr /= null loop
               if    (Rule_Params.Reference /= K_Read    or else Body_Ptr.Usage = Read)
                 and (Rule_Params.Reference /= K_Written or else Body_Ptr.Usage = Written)
               then
                  if Is_Part_Of_Instance (Var_Value.Definition) then
                     -- Let's refer to the instantiation
                     Report (Rule_Id,
                             Rule_Params,
                             Get_Location (Ultimate_Enclosing_Instantiation (Var_Value.Definition)),
                             '"' & To_Title (Defining_Name_Image (Var_Value.Definition)) & '"'
                             & Choose (Body_Ptr.Usage = Read, " read", " written")
                             & " (in instance) from " & Full_Name_Image (Names (Body_Ptr.Decl) (1))
                             & " at " & Image (Get_Location (Body_Ptr.Decl))
                            );
                  else
                     Report (Rule_Id,
                             Rule_Params,
                             Get_Location (Var_Value.Definition),
                             '"' & To_Title (Defining_Name_Image (Var_Value.Definition)) & '"'
                             & Choose (Body_Ptr.Usage = Read, " read", " written")
                             & " from " & Full_Name_Image (Names (Body_Ptr.Decl) (1))
                             & " at " & Image (Get_Location (Body_Ptr.Decl))
                            );
                  end if;
               end if;
               Body_Ptr := Body_Ptr.Next;
            end loop;
         end Report_One_Rule;

         procedure Report_All_Rules is new Referencing_Rule_Map.Iterate (Report_One_Rule);

      begin  -- Report_One_Variable
         Report_All_Rules (Var_Value.Rule_Map);
      end Report_One_Variable;

      procedure Report_All_Variables is new Globals_Info_Map.Iterate (Report_One_Variable);

   begin  -- Finalize
      if Rules_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Report_All_Variables (Globals_Info);
      Clear_All (Globals_Info);
   end Finalize;

begin  -- Rules.Global_References
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB         => Help'Access,
                                     Add_Control_CB  => Add_Control'Access,
                                     Command_CB      => Command'Access,
                                     Prepare_CB      => Prepare'Access,
                                     Finalize_CB     => Finalize'Access);
end Rules.Global_References;
