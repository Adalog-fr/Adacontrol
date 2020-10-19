----------------------------------------------------------------------
--  Rules.Default_Parameter - Package body                          --
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

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;

-- Adalog
with
  Binary_Map,
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Actual_Parameters is
   use Ada.Strings.Wide_Unbounded;
   use Framework, Framework.Control_Manager, Utilities;

   type Subrules is (SR_Default_Used, SR_Default_Not_Used, SR_Default_Positional, SR_Entity);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "SR_");
   type Subrules_Set is array (Subrules) of Boolean;
   No_Rule : constant Subrules_Set := (others => False);

   Rule_Used : Subrules_Set := No_Rule;
   Save_Used : Subrules_Set;

   ----------------------------------------------
   -- Declarations for subrule SR_Default

   subtype Default_Usage_Kind is Subrules range SR_Default_Used .. SR_Default_Positional;

   type Entity_Kind is (E_Name, E_Calls, E_Instantiations);
   package Entity_Kind_Utilities is new Framework.Language.Flag_Utilities (Entity_Kind, Prefix => "E_");

   type Default_Usage_Rec is new Basic_Rule_Context with
      record
         Active : Boolean;
      end record;
   type Default_Usage_Tab is array (Default_Usage_Kind) of Default_Usage_Rec;
   Empty_Default_Usage : constant Default_Usage_Tab := (others => (Basic_Rule_Context with Active => False));


   ----------------------------------------------
   -- Declarations for subrule SR_Entity

   type Entity_Context is new Basic_Rule_Context with
      record
         Entity      : Entity_Specification;
         Deep_Search : Boolean;
      end record;

   package Entity_Queue is new Linear_Queue (Entity_Context);

   ----------------------------------------------
   -- Declarations for all subrules

   type Parameter_Data is
      record
         Default_Data : Default_Usage_Tab;
         Entity_Data  : Entity_Queue.Queue;
      end record;
   Empty_Parameter_Data : constant Parameter_Data := (Default_Data => Empty_Default_Usage,
                                                      Entity_Data  => Entity_Queue.Empty_Queue);

   package Parameter_Tree is new Binary_Map (Key_Type   => Unbounded_Wide_String,
                                             Value_Type => Parameter_Data);

   type Controlled_Entity_Context is new Root_Context with
      record
         Formals_Map : Parameter_Tree.Map;
      end record;
   procedure Clear (Context : in out Controlled_Entity_Context);

   Controlled_Entities : Context_Store;

   Key_All               : constant Unbounded_Wide_String := To_Unbounded_Wide_String ("ALL");
   Entity_Calls          : constant Entity_Specification  := Value ("CALLS");
   Entity_Instantiations : constant Entity_Specification  := Value ("INSTANTIATIONS");

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control properties of actual parameters used in calls or instantiations");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags ("Parameter(1):");
      Entity_Kind_Utilities.Help_On_Flags (Header      => "Parameter(2):",
                                           Extra_Value => "<Subprogram or generic name>");
      User_Message ("Parameter(3): <Formal parameter name> | all");
      User_Message ("Parameter(4..) : [all] <Searched entity> (entity subrule only)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Entity_Kind_Utilities;
      use Subrules_Flag_Utilities;
      Subrule_Name : Subrules;
      Entity       : Entity_Specification;
      E_Kind       : Entity_Kind;
      Is_Deep      : Boolean;

      procedure Update_Default (Ent : Entity_Specification; Form : Wide_String; Usg : Default_Usage_Kind) is
         Formals_Map : Parameter_Tree.Map;
      begin
         begin
            Associate (Controlled_Entities, Ent, Controlled_Entity_Context'(Formals_Map => Formals_Map));
         exception
            when Already_In_Store =>
               null;
         end;

         -- Note: Maps have object (reference) semantics
         Formals_Map := Controlled_Entity_Context (Association (Controlled_Entities, Ent)).Formals_Map;
         declare
            Formal_Data : Parameter_Data := Parameter_Tree.Fetch (Formals_Map,
                                                                  To_Unbounded_Wide_String (Form),
                                                                  Default_Value => Empty_Parameter_Data);
            Val : Default_Usage_Tab renames Formal_Data.Default_Data;
         begin
            if Val (Usg).Active then
               Parameter_Error (Rule_Id, "this combination of parameters already specified");
            end if;
            Val (Usg) := (Basic.New_Context (Ctl_Kind, Ctl_Label) with True);
            Parameter_Tree.Add (Formals_Map, To_Unbounded_Wide_String (Form), Formal_Data);
            Update (Controlled_Entities, Controlled_Entity_Context'(Formals_Map => Formals_Map));
         end;
      end Update_Default;

      procedure Update_Entity (Ent       : Entity_Specification;
                               Formal    : Wide_String;
                               Param_Ent  : Entity_Specification;
                               Param_Deep : Boolean)
      is
         use Entity_Queue;
         Formals_Map : Parameter_Tree.Map;
      begin
         begin
            Associate (Controlled_Entities, Ent, Controlled_Entity_Context'(Formals_Map => Formals_Map));
         exception
            when Already_In_Store =>
               null;
         end;

         -- Note: Maps have object (reference) semantics
         Formals_Map := Controlled_Entity_Context (Association (Controlled_Entities, Ent)).Formals_Map;
         declare
            Formal_Data : Parameter_Data := Parameter_Tree.Fetch (Formals_Map,
                                                                  To_Unbounded_Wide_String (Formal),
                                                                  Default_Value => Empty_Parameter_Data);

         begin
            Append (Formal_Data.Entity_Data, (Basic.New_Context (Ctl_Kind, Ctl_Label) with Param_Ent, Param_Deep));
            Parameter_Tree.Add (Formals_Map, To_Unbounded_Wide_String (Formal), Formal_Data);
            Update (Controlled_Entities, Controlled_Entity_Context'(Formals_Map => Formals_Map));
         end;
      end Update_Entity;

   begin  -- Add_Control
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "parameters required");
      end if;
      Subrule_Name := Get_Flag_Parameter (Allow_Any => False);

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "missing subprogram or generic name");
      end if;
      E_Kind := Get_Flag_Parameter (Allow_Any => True);
      if E_Kind = E_Name then
         Entity := Get_Entity_Parameter;
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "missing formal name");
      end if;
      declare
         Formal : constant Wide_String := To_Upper (Get_Name_Parameter);
         -- Note: "ALL" is handled as a regular name
      begin
         case Subrule_Name is
            when Default_Usage_Kind =>
               case E_Kind is
                  when E_Name =>
                     Update_Default (Entity, Formal, Subrule_Name);
                  when E_Calls =>
                     Update_Default (Entity_Calls, Formal, Subrule_Name);
                  when E_Instantiations =>
                     Update_Default (Entity_Instantiations, Formal, Subrule_Name);
               end case;
            when SR_Entity =>
               if Parameter_Exists then
                  loop
                     Is_Deep := Get_Modifier ("ALL");
                     case E_Kind is
                        when E_Name =>
                           Update_Entity (Entity, Formal, Get_Entity_Parameter, Is_Deep);
                        when E_Calls =>
                           Update_Entity (Entity_Calls, Formal, Get_Entity_Parameter, Is_Deep);
                        when E_Instantiations =>
                           Update_Entity (Entity_Instantiations, Formal, Get_Entity_Parameter, Is_Deep);
                     end case;
                     exit when not Parameter_Exists;
                  end loop;
               else
                  Parameter_Error (Rule_Id, "entity expected");
               end if;
         end case;
      end;

      Rule_Used (Subrule_Name) := True;
   end Add_Control;

   -------------
   -- Or_Else --
   -------------

   function Or_Else (L, R : Default_Usage_Tab) return Default_Usage_Tab is
      Result : Default_Usage_Tab;
   begin
      for U in Default_Usage_Kind loop
         if L (U).Active then
            Result (U) := L (U);
         else
            Result (U) := R (U);
         end if;
      end loop;
      return Result;
   end Or_Else;

   -----------
   -- Clear --
   -----------

   procedure Clear (Context : in out Controlled_Entity_Context) is
      use Parameter_Tree;
   begin
      Clear (Context.Formals_Map);
   end Clear;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := No_Rule;
            Clear (Controlled_Entities);
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
      Balance (Controlled_Entities);
      -- We do not balance the trees for each formal.
      -- Since we do not expect more than 1 or 2 entries in each...
   end Prepare;

   -----------------------------------
   -- Process_Call_Or_Instantiation --
   -----------------------------------

   procedure Process_Call_Or_Instantiation (Call_Inst : in Asis.Element) is
      use Asis, Asis.Elements, Asis.Declarations;
      use Thick_Queries;

      Name : Asis.Expression;

      function Get_Formals_List return Asis.Element_List is
         use Asis.Expressions;
      begin
         if Expression_Kind (Name) = An_Attribute_Reference then
            -- Calls to attributes must be ignored, since they have no formal name (and no default value)
            return Nil_Element_List;
         elsif Expression_Kind (Call_Inst) = A_Function_Call then
            return Called_Profile (Call_Inst);
         elsif Statement_Kind (Call_Inst) = A_Procedure_Call_Statement or
           Statement_Kind (Call_Inst) = An_Entry_Call_Statement
         then
            return Called_Profile (Call_Inst);
         elsif Declaration_Kind (Call_Inst) in A_Generic_Instantiation
           or Declaration_Kind (Call_Inst) = A_Formal_Package_Declaration
         then
            return Generic_Formal_Part (Corresponding_Name_Declaration
                                        (Simple_Name
                                         (Ultimate_Name
                                          (Generic_Unit_Name (Call_Inst)))));
         else
            Failure ("Default_Parameter: Unexpected element in Get_Formals_List", Call_Inst);
         end if;
      end Get_Formals_List;

      procedure Check_Default (Formal : Asis.Expression; Name_Context, All_Context : Root_Context'Class)
      is
         use Asis.Expressions;
         use Framework.Locations, Framework.Reports, Parameter_Tree;

         Formal_Key : constant Unbounded_Wide_String
           := To_Unbounded_Wide_String (To_Upper (Defining_Name_Image (Formal)));
         Usage         :          Default_Usage_Tab := Empty_Default_Usage;
         Actual        : constant Asis.Expression   := Actual_Expression (Call_Inst, Formal, Return_Default => False);
         Is_Defaulted  : constant Boolean           := Is_Nil (Actual);
         Is_Positional : constant Boolean           := not Is_Defaulted
                                                       and then Is_Nil (Formal_Parameter (Enclosing_Element (Actual)));
      begin
         -- Build Usage in order of preferences:
         --    Sp_Name, Formal_Name
         --    Sp_Name, Positional
         --    Sp_Name, All
         --    All,     Formal_Name
         --    All,     Positional
         --    All,     All
         if Name_Context /= No_Matching_Context then
            if Is_Present (Controlled_Entity_Context (Name_Context).Formals_Map, Formal_Key) then
               Usage := Fetch (Controlled_Entity_Context (Name_Context).Formals_Map, Formal_Key).Default_Data;
            end if;
            if Is_Present (Controlled_Entity_Context (Name_Context).Formals_Map, Key_All) then
               Usage := Or_Else (Usage,
                                 Fetch (Controlled_Entity_Context (Name_Context).Formals_Map, Key_All).Default_Data);
            end if;
         end if;
         if All_Context /= No_Matching_Context then
            if Is_Present (Controlled_Entity_Context (All_Context).Formals_Map, Formal_Key) then
               Usage := Or_Else (Usage,
                                 Fetch (Controlled_Entity_Context (All_Context).Formals_Map, Formal_Key).Default_Data);
            end if;
            if Is_Present (Controlled_Entity_Context (All_Context).Formals_Map, Key_All) then
               Usage := Or_Else (Usage,
                                 Fetch (Controlled_Entity_Context (All_Context).Formals_Map, Key_All).Default_Data);
            end if;
         end if;

         if Usage (SR_Default_Used).Active then
            if Is_Defaulted then
               Report (Rule_Id,
                       Usage (SR_Default_Used),
                       Get_Location (Call_Inst),
                       "default use of formal """ & Defining_Name_Image (Formal) & '"');
            end if;
         end if;

         if Usage (SR_Default_Positional).Active then
            if Is_Positional then
               Report (Rule_Id,
                       Usage (SR_Default_Positional),
                       Get_Location (Call_Inst),
                       "use of defaulted formal """ & Defining_Name_Image (Formal) & """ with positional association");
            end if;
         end if;

         if Usage (SR_Default_Not_Used).Active then
            if not Is_Defaulted then
               Report (Rule_Id,
                       Usage (SR_Default_Not_Used),
                       Get_Location (Call_Inst),
                       "non default use of formal """ & Defining_Name_Image (Formal) & '"');
            end if;
         end if;
      end Check_Default;

      procedure Check_Entity (Formal : Asis.Expression; Name_Context, All_Context : Root_Context'Class) is
         use Parameter_Tree;

         Formal_Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (To_Upper
                                                                                  (Defining_Name_Image (Formal)));
         procedure Do_Entity_Report (Searched_Entities : Entity_Queue.Queue) is
            use Entity_Queue, Framework.Locations, Framework.Reports;
            use Asis.Expressions, Asis.Iterator;
            Curs          : Cursor := First (Searched_Entities);
            Context       : Entity_Context;
            Ctrl          : Traverse_Control;
            Actual        : constant Asis.Expression  := Actual_Expression (Call_Inst, Formal, Return_Default => True);
            Simple_Actual : Asis.Expression := Actual;

            procedure Pre  (Element : Asis.Element; Control : in out Traverse_Control; State : in out Entity_Context);
            procedure Post (Element : Asis.Element; Control : in out Traverse_Control; State : in out Entity_Context)
            is null;
            procedure Traverse_Expression is new Traverse_Element (Entity_Context, Pre, Post);
            procedure Pre (Element  : Asis.Element; Control : in out Traverse_Control; State : in out Entity_Context) is
            begin
               case Expression_Kind (Element) is
                  when An_Identifier =>
                     if Matches (State.Entity, Element, Extend_To => All_Extensions) then
                        if Is_Nil (Actual_Expression (Call_Inst, Formal, Return_Default => False)) then
                           -- A default value, attach message to the call (or instanciation)
                           -- since the entity does not appear
                           Report (Rule_Id,
                                   Context,
                                   Get_Location (Process_Call_Or_Instantiation.Call_Inst),
                                   "Entity " & Adjust_Image (Image (State.Entity))
                                   & " used in expression for parameter "  & Defining_Name_Image (Formal)
                                   & " (default value)");
                        else
                           -- Normal case
                           Report (Rule_Id,
                                   Context,
                                   Get_Location (Element),
                                   "Entity " & Adjust_Image (Image (State.Entity))
                                   & " used in expression for parameter "  & Defining_Name_Image (Formal));
                        end if;
                     end if;
                  when An_Attribute_Reference =>
                     -- Don't traverse the attribute designator
                     Traverse_Expression (Prefix (Element), Control, State);
                     Control := Abandon_Children;
                  when others =>
                     null;
               end case;
            end Pre;

         begin  -- Do_Entity_Report
            loop -- Get a clean name
               case Expression_Kind (Simple_Actual) is
                  when An_Identifier | An_Attribute_Reference =>
                     exit;
                  when A_Selected_Component =>
                     Simple_Actual := Selector (Simple_Actual);
                  when An_Indexed_Component =>
                     Simple_Actual := Prefix (Simple_Actual);
                  when A_Function_Call =>
                     Simple_Actual := Prefix (Simple_Actual);
                  when A_Parenthesized_Expression =>
                     Simple_Actual := Expression_Parenthesized (Simple_Actual);
                  when A_Type_Conversion | A_Qualified_Expression =>
                     Simple_Actual := Converted_Or_Qualified_Expression (Simple_Actual);
                  when others =>
                     Simple_Actual := Nil_Element;
                     exit;
               end case;
            end loop;

            while Has_Element (Curs) loop
               Context := Fetch (Curs);
               if Matches (Context.Entity, Simple_Actual, Extend_To => All_Extensions) then
                  if Is_Nil (Actual_Expression (Call_Inst, Formal, Return_Default => False)) then
                     -- A default value, attach message to the call (or instanciation) since the entity does not appear
                     Report (Rule_Id,
                             Context,
                             Get_Location (Call_Inst),
                             "Entity " & Adjust_Image (Image (Context.Entity))
                             & " used as actual for parameter "  & Defining_Name_Image (Formal)
                             & " (default value)");
                  else
                     -- Normal case
                     Report (Rule_Id,
                             Context,
                             Get_Location (Simple_Actual),
                             "Entity " & Adjust_Image (Image (Context.Entity))
                             & " used as actual for parameter "  & Defining_Name_Image (Formal));
                  end if;

               elsif Context.Deep_Search then
                  Ctrl := Continue;
                  Traverse_Expression (Actual, Ctrl, Context);
               end if;
               Curs := Next (Curs);
            end loop;
         end Do_Entity_Report;

      begin  -- Check_Entity

         -- Check Sp_Name, Formal_Name
         --       Sp_Name, All
         --       All,     Formal_Name
         --       All,     All
         if Name_Context /= No_Matching_Context then
            if Is_Present (Controlled_Entity_Context (Name_Context).Formals_Map, Formal_Key) then
               Do_Entity_Report (Fetch (Controlled_Entity_Context (Name_Context).Formals_Map, Formal_Key).Entity_Data);
            end if;
            if Is_Present (Controlled_Entity_Context (Name_Context).Formals_Map, Key_All) then
               Do_Entity_Report (Fetch (Controlled_Entity_Context (Name_Context).Formals_Map, Key_All).Entity_Data);
            end if;
         end if;
         if All_Context /= No_Matching_Context then
            if Is_Present (Controlled_Entity_Context (All_Context).Formals_Map, Formal_Key) then
               Do_Entity_Report (Fetch (Controlled_Entity_Context (All_Context).Formals_Map, Formal_Key).Entity_Data);
            end if;
            if Is_Present (Controlled_Entity_Context (All_Context).Formals_Map, Key_All) then
               Do_Entity_Report (Fetch (Controlled_Entity_Context (All_Context).Formals_Map, Key_All).Entity_Data);
            end if;
         end if;
      end Check_Entity;

      Entity_All : Entity_Specification;
   begin  -- Process_Call_Or_Instantiation
      if Rule_Used = No_Rule then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Element_Kind (Call_Inst) is
         when A_Declaration =>
            -- Must be A_Generic_Instantiation or A_Formal_Package_Declaration
            Name       := Generic_Unit_Name (Call_Inst);
            Entity_All := Entity_Instantiations;
         when An_Expression =>
            -- Must be A_Function_Call
            Name       := Called_Simple_Name (Call_Inst);
            Entity_All := Entity_Calls;
         when others =>
            -- Must be a procedure or entry call
            Name       := Called_Simple_Name (Call_Inst);
            Entity_All := Entity_Calls;
      end case;

      declare
         Name_Context : constant Root_Context'Class := Matching_Context (Controlled_Entities,
                                                                         Name,
                                                                         Extend_To => All_Extensions);
         All_Context  : constant Root_Context'Class := Control_Manager.Association (Controlled_Entities, Entity_All);
      begin
         if Name_Context = No_Matching_Context and All_Context = No_Matching_Context then
            return;
         end if;

         for Formal : Asis.Element of Get_Formals_List loop
            case Element_Kind (Formal) is
               when A_Clause =>
                  -- Use clause in generic formal part
                  null;
               when A_Declaration =>
                  case Declaration_Kind (Formal) is
                     when A_Parameter_Specification
                        | A_Formal_Object_Declaration
                        =>
                        if Rule_Used (SR_Entity) or not Is_Nil (Initialization_Expression (Formal)) then
                           for N : Asis.Defining_Name of Names (Formal) loop
                              if Rule_Used (Default_Usage_Kind) /= (Default_Usage_Kind => False) then
                                 Check_Default (N, Name_Context, All_Context);
                              end if;
                              if Rule_Used (SR_Entity) then
                                 Check_Entity (N, Name_Context, All_Context);
                              end if;
                           end loop;
                        end if;
                     when A_Formal_Procedure_Declaration
                        | A_Formal_Function_Declaration
                        =>
                        if  Rule_Used (Default_Usage_Kind) /= (Default_Usage_Kind => False) then
                           case Default_Kind (Formal) is
                              when Not_A_Default =>
                                 Failure ("Not_A_Default");
                              when A_Name_Default
                                 | A_Box_Default
                                 | A_Null_Default
                                 =>
                                 Check_Default (Names (Formal) (1), Name_Context, All_Context);
                              when A_Nil_Default =>
                                 null;
                           end case;
                        end if;
                        if Rule_Used (SR_Entity) then
                           Check_Entity (Names (Formal) (1), Name_Context, All_Context);
                        end if;

                     when others =>
                        -- Others cases have no possible default value
                        null;
                  end case;
               when others =>
                  Failure ("Bad formal", Formal);
            end case;
         end loop;
      end;
   end Process_Call_Or_Instantiation;

begin  -- Rules.Actual_Parameters
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Actual_Parameters;
