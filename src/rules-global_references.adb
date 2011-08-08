----------------------------------------------------------------------
--  Rules.Global_References - Package body                          --
--                                                                  --
--  This software is (c) BELGOCONTROL and Adalog 2004-2005. The Ada --
--  Controller  is free  software; you  can redistribute  it and/or --
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
  Ada.Strings.Wide_Unbounded,
  Ada.Unchecked_Deallocation;

-- ASIS
with
  Asis.Expressions;

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

-- ASIS
with
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Exceptions,
  Asis.Iterator;

package body Rules.Global_References is
   use Framework;

   -- Algorithm:
   --
   -- The algorithm for identifying globals is quite straightforward. The procedure Check is in charge
   -- of identifying global variables usage for a given body, and it is called when an appropriate body
   -- is encountered. It traverses the body, and stores information about encountered variables in the
   -- (global) map "Usage". This variable serves also to note local declarations (not reported) and bodies
   -- that have already been traversed.
   --
   -- The difficulty comes from the "multiple" (or "multiple_non_atomic") option. Since we want to report
   -- global variables accessed from several entities, the reporting can be done only at the end of the analysis.
   -- Moreover, we wanted the messages to be sorted according to variables referenced (and not according to
   -- referencing bodies f.e.). Moreover, which bodies are part of "multiple" is done per-rule, i.e.:
   --   check Global_References (multiple, P1, P2);
   --   check Global_References (multiple, P1, P3);
   -- must report of global variables accessed by P1 and P2 on one side, and by P1 and P3 on the other side.
   -- Therefore, the following structures are used to maintain information about all variables:
   --   - Rules_Map is a map to keep information about each specified rule. The key is the rule index, and the
   --     value contains the rule label, type, and whether the rule is "all" , "multiple" or "multiple_non_atomic".
   --   - Globals_Info keeps information about all global variables encountered. The key is the variable's
   --     Full_Name_Image, and the value holds a reference to the variable's declaration, and a map
   --     (Referencing_Rule_Map) of rules that mention at least one body that references the variable.
   --   - The Referencing_Rule_Map has the rule index as the key, and a linear list of the bodies that
   --     reference the variable as the value (plus a flag to deal with task and protected types).
   --
   -- At the end of the check of an appropriate construct, info gathered into "Usage" serves to update
   -- Rules_Map. At the end of the run, the report is printed by traversing Rules_Map.
   --
   -- Checked_Bodies is a multi-valued context store that identifies bodies that are to be traversed, and
   -- rules that mention this body.

   type Reference_Kind is (K_All, K_Multiple, K_Multiple_Non_Atomic);
   package Reference_Kind_Utilities is new Framework.Language.Flag_Utilities (Reference_Kind, "K_");

   type Checked_Kind is (K_Name, K_Task, K_Protected);
   package Checked_Kind_Utilities is new Framework.Language.Flag_Utilities (Checked_Kind, "K_");

   Rules_Used : Rule_Index := 0;
   Save_Used  : Rule_Index;

   -- Management of checked bodies
   type Body_Context is new Root_Context with
      record
         Rule_Inx : Rule_Index;
      end record;

   Checked_Bodies  : Context_Store;

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
         Reference : Reference_Kind;
      end record;
   package Rules_Info_Map is new Binary_Map (Rule_Index, Rule_Info);

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

   package Referencing_Rule_Map is new Binary_Map (Rule_Index, Referencing_Info);
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
      Reference_Kind_Utilities.Help_On_Flags ("Parameter(1): ");
      Checked_Kind_Utilities.Help_On_Flags ("Parameter(2..N): ", Extra_Value => "<name>");
      User_Message ("Control global (in a relative sense) variables referenced ");
      User_Message ("directly or indirectly from some specific constructs");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language, Reference_Kind_Utilities, Checked_Kind_Utilities;
      use Rules_Info_Map;

      Reference : Reference_Kind;
      Check     : Checked_Kind;
   begin
      if not Parameter_Exists then
         Parameter_Error ("Parameters required for rule " & Rule_Id);
      end if;
      Reference := Get_Flag_Parameter (Allow_Any => False);

      if not Parameter_Exists then
         Parameter_Error ("Missing specification of checked bodies");
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
            end case;
            Add (Rules_Map,
                 Key => Rules_Used,
                 Value => (Basic.New_Context (Rule_Type, Label) with Reference));
            Associate (Checked_Bodies,
                       Entity,
                       Body_Context'(Rule_Inx => Rules_Used),
                       Additive => True);
         end;
      end loop;
   end Add_Use;

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

   -----------
   -- Check --
   -----------

   -- Forward declarations:
   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Null_State);
   procedure Traverse is new Asis.Iterator.Traverse_Element (Null_State, Pre_Procedure, Null_State_Procedure);
   procedure Check (Body_Decl : Asis.Declaration);


   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Null_State) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Ada.Strings.Wide_Unbounded, Thick_Queries, Utilities, Usage_Map;

   begin
      case Element_Kind (Element) is
         when A_Defining_Name =>
            if Declaration_Kind (Enclosing_Element (Element)) = A_Variable_Declaration then
               Add (Usage,
                    To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Element, With_Profile => True))),
                    (Element, Non_Global));
            end if;

         when An_Expression =>
            case Expression_Kind (Element) is
               when A_Function_Call =>
                  declare
                     Called : constant Asis.Declaration := A4G_Bugs.Corresponding_Called_Function (Element);
                  begin
                     if not Is_Nil (Called)
                       and then Declaration_Kind (Called) /= A_Formal_Function_Declaration
                     then
                        Check (Corresponding_Body (Called));
                     end if;
                  end;
               when An_Identifier =>
                  begin
                     case Declaration_Kind (Corresponding_Name_Declaration (Element)) is
                        when A_Variable_Declaration =>
                           declare
                              Key : constant Unbounded_Wide_String
                                := To_Unbounded_Wide_String (To_Upper
                                                             (Full_Name_Image (Element, With_Profile => True)));
                              U   : Usage_Record;
                           begin
                              if Is_Present (Usage, Key) then
                                 U := Fetch (Usage, Key);
                                 case U.Usage is
                                    when Non_Global | Written =>
                                       null;
                                    when Read =>
                                       if Expression_Usage_Kind (Element) = Read then
                                          U.Usage :=  Read;
                                       else
                                          U.Usage := Written;
                                       end if;
                                    when Checked =>
                                       Failure ("Variable marked as checked", Element);
                                 end case;
                              else
                                 if Expression_Usage_Kind (Element) = Read then
                                    U := (Corresponding_Name_Definition (Element), Read);
                                 else
                                    U := (Corresponding_Name_Definition (Element), Written);
                                 end if;
                              end if;
                              Add (Usage, Key, U);
                           end;

                        when An_Object_Renaming_Declaration =>
                           Traverse (A4G_Bugs.Renamed_Entity (Corresponding_Name_Declaration (Element)),
                                     Control,
                                     State);

                        when others =>
                           -- Not a variable
                           null;
                     end case;
                  exception
                     when Asis.Exceptions.ASIS_Failed =>
                        -- KLUDGE FOR ASIS BUG: Constraint_Error in Corresponding_Name_Declaration.
                        -- This happens only if Element is a generic formal "in" parameter
                        -- (but not "in out"). We can thus safely ignore it (it is not a variable)
                        null;
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
                  declare
                     Called : constant Asis.Declaration := A4G_Bugs.Corresponding_Called_Entity (Element);
                  begin
                     -- Called is Nil for predefined stuff and access to SP, not interesting for us
                     if not Is_Nil (Called)
                       and then Declaration_Kind (Called) /= A_Formal_Procedure_Declaration
                     then
                        Check (Corresponding_Body (Called));
                     end if;
                  end;
               when An_Entry_Call_Statement =>
                  -- Process only protected entries (not task entries)
                  declare
                     Called : constant Asis.Declaration := A4G_Bugs.Corresponding_Called_Entity (Element);
                  begin
                     if not Is_Nil (Called)
                       and then Definition_Kind (Enclosing_Element (Called)) = A_Protected_Definition
                     then
                        Check (Corresponding_Body (Called));
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

   procedure Check (Body_Decl : Asis.Declaration) is
      use Asis, Asis.Compilation_Units, Asis.Declarations, Asis.Elements;
      use Thick_Queries, Utilities, Usage_Map, Ada.Strings.Wide_Unbounded;
   begin
      if Is_Nil (Body_Decl) -- Predefined operations, f.e. ...
        or else Is_Banned (Body_Decl, Rule_Id)
        or else Element_Kind (Body_Decl) = A_Pragma -- case of pragma Import
      then
         return;
      end if;

      if Unit_Origin (Enclosing_Compilation_Unit (Body_Decl)) /= An_Application_Unit then
         -- Do not check the standard library (nor the RTL!)
         return;
      end if;

      declare
         Body_Name : constant Unbounded_Wide_String := To_Unbounded_Wide_String (To_Upper
                                                                                 (Full_Name_Image
                                                                                    (Names (Body_Decl)(1),
                                                                                     With_Profile => True)));
         Control   : Asis.Traverse_Control := Continue;
         Ignored   : Null_State;
      begin
         if Is_Present (Usage, Body_Name) then
            return;
         end if;

         Add (Usage, Body_Name, (Nil_Element, Checked));
         Traverse (Body_Decl, Control, Ignored);
      end;
   end Check;

   --------------------
   -- Update_Globals --
   --------------------

   procedure Update_Globals (Decl : Asis.Declaration; Rule_Inx : Rule_Index) is
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
   begin
      Update_All_Variables (Usage);
   end Update_Globals;

   ------------------
   -- Process_Body --
   ------------------

   procedure Process_Body (The_Body : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;

      function General_Matching_Context return Root_Context'Class is
         Name           : Asis.Defining_Name := Names (The_Body)(1);
         From_Protected : Boolean := False;
      begin
         -- If this is a protected operation, the associated context is the one of the
         -- enclosing protected object
         if Declaration_Kind (Enclosing_Element (The_Body)) = A_Protected_Body_Declaration then
            Name           := Names (Enclosing_Element (The_Body))(1);
            From_Protected := True;
         end if;

         declare
            Result : constant Root_Context'Class := Matching_Context (Checked_Bodies, Name);
         begin
            if Result /= No_Matching_Context then
               return Result;
            end if;
         end;

         if From_Protected then
            return Framework.Association (Checked_Bodies, Value ("PROTECTED"));
         elsif Declaration_Kind (The_Body) = A_Task_Body_Declaration then
            return Framework.Association (Checked_Bodies, Value ("TASK"));
         else
            return No_Matching_Context;
         end if;
     end General_Matching_Context;

   begin                       -- Process_Body
      if Rules_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Current_Context : constant Root_Context'Class := General_Matching_Context;
      begin
         if Current_Context = No_Matching_Context then
            return;
         end if;

         Check (The_Body);

         Update_Globals (The_Body, Body_Context (Current_Context).Rule_Inx);
         loop
            declare
               Next_Context : constant Root_Context'Class := Next_Matching_Context (Checked_Bodies);
            begin
               exit when Next_Context = No_Matching_Context;
               Update_Globals (The_Body, Body_Context (Next_Context).Rule_Inx);
            end;
         end loop;
      end;

      Usage_Map.Clear (Usage);
   end Process_Body;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      use Ada.Strings.Wide_Unbounded;

      procedure Report_One_Variable (Key : in Unbounded_Wide_String; Var_Value : in out Globals_Info_Record) is
         pragma Unreferenced (Key);
         use Framework.Reports, Thick_Queries, Utilities;

         procedure Report_One_Rule (Rule_Key : in Rule_Index; Rule_Value : in out Referencing_Info) is
            use Asis, Asis.Declarations, Asis.Elements;
            use Rules_Info_Map;
            Rule_Params   : constant Rule_Info        := Fetch (Rules_Map, Rule_Key);  -- Must be there
            Ref_Info      : constant Referencing_Info := Rule_Value;                   -- Ref_Info.Bodies /= null
            Body_Ptr      : Body_List;
            Different_POs : Boolean;
            PO_Decl       : Asis.Declaration := Nil_Element;
         begin
            -- Avoid cases when we must not print results
            if Rule_Params.Reference /= K_All then
               if not Ref_Info.Includes_Type and Ref_Info.Bodies.Next = null then -- Only one body, not K_All
                  return;
               else
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
                                              others => False))
                             /= Pragma_Set'(others => False)
                  then
                     return;
                  elsif not Ref_Info.Includes_Type then
                     -- We have several bodies, but we must check that they are not all from the same protected object
                     -- (protected types have already been dealt with by flag Includes_Type)
                     Body_Ptr      := Ref_Info.Bodies;
                     Different_POs := False;
                     while Body_Ptr /= null loop
                        if Declaration_Kind (Enclosing_Element (Body_Ptr.Decl)) = A_Protected_Body_Declaration then
                           if Is_Nil (PO_Decl) then
                              PO_Decl := Enclosing_Element (Body_Ptr.Decl);
                           elsif not Is_Equal (Enclosing_Element (Body_Ptr.Decl), PO_Decl) then
                              Different_POs := True;
                              exit;
                           end if;
                        else
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
               end if;
            end if;

            -- Cases when we must not print results are avoided, so we can print results
            Body_Ptr := Ref_Info.Bodies;
            while Body_Ptr /= null loop
               Report (Rule_Id,
                       Rule_Params,
                       Get_Location (Var_Value.Definition),
                       Choose (Body_Ptr.Usage = Read, "Read", "Write")
                       & " of global variable """ & To_Title (Defining_Name_Image (Var_Value.Definition)) & '"'
                       & " from " & Full_Name_Image (Names (Body_Ptr.Decl)(1))
                       & " at " & Image (Get_Location (Body_Ptr.Decl))
                       );
               Body_Ptr := Body_Ptr.Next;
            end loop;
         end Report_One_Rule;

         procedure Report_All_Rules is new Referencing_Rule_Map.Iterate (Report_One_Rule);
      begin
         Report_All_Rules (Var_Value.Rule_Map);
      end Report_One_Variable;

      procedure Report_All_Variables is new Globals_Info_Map.Iterate (Report_One_Variable);

   begin
      if Rules_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Report_All_Variables (Globals_Info);
      Clear_All (Globals_Info);
   end Finalize;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help     => Help'Access,
                                              Add_Use  => Add_Use'Access,
                                              Command  => Command'Access,
                                              Prepare  => Prepare'Access,
                                              Finalize => Finalize'Access);
end Rules.Global_References;
