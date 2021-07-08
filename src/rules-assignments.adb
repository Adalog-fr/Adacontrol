----------------------------------------------------------------------
--  Rules.Assignments - Package body                                --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2021.           --
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
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  A4G_Bugs,
  Adactl_Constants,
  Binary_Map,
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Queries,
  Framework.Reports.Fixes;
  pragma Elaborate (Framework.Language);
  pragma Elaborate (Framework.Language.Shared_Keys);

package body Rules.Assignments is
   use Ada.Strings.Wide_Unbounded;
   use Framework, Framework.Control_Manager, Framework.Language.Shared_Keys;

   -- Algorithm:
   --
   -- Subrule Type:
   -- -------------
   -- Easy. Since the same type can be used as full type and as component, we need two different context_store.
   --
   -- Subrule Sliding:
   -- ----------------
   -- Easy
   --
   -- Subrule Access_Duplication:
   -- ---------------------------
   -- Traverse the assignment expression recursively, skipping anything that is not part of the value
   -- (like aggregate choices, subprogram parameters, prefixes of qualified expressions), for access variables.
   -- Note that functions could return the value of one of their parameters, hence their duplication status is
   -- "possible"
   -- Each reference to a variable (including dereferences) is checked for subcomponents containing access types,
   -- thanks to an instance of Traverse_Data_Structure. Note that we must find ALL references, since filters may
   -- target various types.
   -- The issue of whether a type is in a controlled structure is not easy. Here, we note in the state associated to
   -- the traversal the nesting depth of the first encountered controlled type. This is done in the pre_procedure, and
   -- undone (when we go through the same level) in the post_procedure.
   --
   -- Subrules repeated and groupable:
   -- --------------------------------
   -- Since this rule is about sequences of assignments, it is plugged on any construct
   -- that can contain statements. The sequence of statements is scanned for consecutive
   -- assignments.
   --
   -- A map holds information for all LHS (Left Hand Side) of a sequence of assignments.
   -- The name (key to the map) is made of the full name of the variable, plus selectors
   -- (for record components) or the value of static indices (for arrays). If anything
   -- not static is encountered, processing is stopped by raising Dynamic_LHS.
   -- The information associated to a LHS contains:
   --    - the total number of subcomponents this LHS can have
   --    - the number of subcomponents currently assigned.
   --
   -- The parent of a LHS is the expression with one less selector (or indexing) than
   -- the current LHS. It is also considered a LHS.
   --
   -- The first LHS (the one to the left of ":=") is marked as "full" assignment, others
   -- (during recursion) are marked as "component".
   --
   -- The Repeated subrule is triggered if the LHS or any of its parents is already in the
   -- map.
   --
   -- The Groupable subrule is triggered on various comparisons between the total number of
   -- subcomponents and the number of assigned subcomponents. The difficulty is that, for a
   -- subcomponent that is itself of a composite type, it has to be counted as "assigned" not
   -- only if it has been assigned in full, but also if it should have been assigned in full
   -- (i.e. if the rule would trigger on the subcomponent). And since it depends on the parameters
   -- of the control, it has to be recomputed dynamically for each control.
   --
   -- The processing of a "full" LHS simply increments its parent's count of (full) subcomponents.
   -- A "component" LHS is chained to its parent through Child/Brother links, and when evaluating
   -- the parent, the number of assigned components is recomputed by following this chain.
   --
   -- At the end of a sequence of assignments, all LHS in the map are traversed, and
   -- messages are issued according to the values assigned/total subcomponents.
   --
   -- Subrule Possible_Target_Name:
   -- -----------------------------
   -- Traverse the RHS and compare each subexpression to LHS, all the work is done by Are_Equivalent_Expressions.

   type Subrules is (Sr_Type, Sr_Access_Duplication, Sr_Sliding, Sr_Repeated, Sr_Groupable, Sr_Possible_Target_Name);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "Sr_");

   type Criteria is (Crit_Given, Crit_Missing, Crit_Ratio, Crit_Total);
   package Criteria_Utilities is new Framework.Language.Modifier_Utilities (Criteria, Prefix => "Crit_");

   type Usage_Flags is array (Subrules) of Boolean;
   Not_Used : constant Usage_Flags := (others => False);
   Rule_Used : Usage_Flags := Not_Used;
   Save_Used : Usage_Flags;


   -- Data for subrule Access_Duplication:
   type Filter_Kind is (Include, Exclude);
   type Duplication_Context is new Basic_Rule_Context with
      record
         Filter  : Filter_Kind;
      end record;

   Entities : array (Boolean) of Context_Store;
   -- True : context for controlled
   -- False: context for not controlled


   -- Data for subrule Type:
   type Type_Contexts is new Basic_Rule_Context with
      record
         Search_Ancestor : Boolean;
      end record;
   type LHS_Kinds is (Direct, Component);
   Target_Types :  array (LHS_Kinds) of Context_Store;
   -- True : context for component
   -- False: context for type


   -- Data for subrule Sliding:
   Sliding_Context : Basic_Rule_Context;


   -- Data for subrule Repeated:
   Repeated_Context : Basic_Rule_Context;


   -- Data for subrule Groupable
   subtype Percentage is Asis.ASIS_Natural range 0 .. 100;
   type Groupable_Context is new Basic_Rule_Context with
      record
         Given   : Thick_Queries.Biggest_Natural;
         Missing : Thick_Queries.Biggest_Natural;
         Ratio   : Percentage;
         Total   : Thick_Queries.Biggest_Natural;
      end record;

   package Context_Queue is new Linear_Queue (Groupable_Context);
   Groupable_Contexts : Context_Queue.Queue;

   type Coverage_Kind is (Full, Component);
   type LHS_Descriptor (Coverage : Coverage_Kind := Full) is
      record
         Loc : Framework.Locations.Location;
         case Coverage is
            when Full =>
               null;
            when Component =>
               Subcomp_Total      : Thick_Queries.Extended_Biggest_Natural;
               Full_Subcomp_Count : Thick_Queries.Biggest_Natural;
               First_Child        : Unbounded_Wide_String;
               Brother            : Unbounded_Wide_String;
         end case;
      end record;
   package LHS_Map is new Binary_Map (Unbounded_Wide_String, LHS_Descriptor);

   Duplication_Expected_Categories : constant Categories_Set := Basic_Set + Cat_Private;

   -- Data for subrule Possible_Target_Name:
   type Target_Context is new Basic_Rule_Context with
      record
         Allowed_Count : Adactl_Constants.ID_Count;  -- Largely sufficient for element count too
      end record;
   Trivial_Given              : Boolean := False;
   Trivial_Target_Context     : Target_Context;
   Not_Trivial_Given          : Boolean := False;
   Not_Trivial_Target_Context : Target_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Criteria_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control various issues related to assignments:");
      User_Message ("assignment to a given type, obvious array slidings, duplication of access value,");
      User_Message ("repeated assignments in a sequence to a same variable, sequences of assignments");
      User_Message ("to components of a structured variable that could be replaced by an aggregate,");
      User_Message ("or subexpressions that could be replaced by ""@""");
      User_Message;
      -- Since some subrules have various modifiers, we cannot use Help_On_Flags here.
      -- Update manually if new subrules are added
      User_Message ("Parameter(1): type    | [[not] controlled] access_duplication |");
      User_Message ("              sliding |repeated | groupable | [[not] trivial] possible_target name");
      User_Message;
      User_Message ("For type:");
      User_Message ("Parameter(2): [component] [ancestor] <Type name>");
      User_Message;
      User_Message ("For access_duplication:");
      User_Message ("Parameter(2..): [not] <Entity name> | <category> | procedure | function");
      Help_On_Categories (Expected => Duplication_Expected_Categories);
      User_Message;
      User_Message ("For groupable:");
      User_Message ("Parameter(2..): <criterion> <value>");
      Help_On_Modifiers (Header => "<criterion>:");
      User_Message;
      User_Message ("For trivial possible_target_name:");
      User_Message ("Parameter (2): Maximum length of acceptable identifier");
      User_Message;
      User_Message ("For not trivial possible_target_name:");
      User_Message ("Parameter (2): Maximum components of acceptable name");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Adactl_Constants, Context_Queue, Criteria_Utilities, Subrules_Flag_Utilities,
          Framework.Language, Thick_Queries;
      Given          : Biggest_Natural := 0;
      Missing        : Biggest_Natural := Biggest_Natural'Last;
      Ratio          : Percentage      := 0;
      Total          : Biggest_Natural := 0;
      Count          : Id_Count        := 0;
      Crit           : Criteria;
      Subrule        : Subrules;
      Has_Not        : Boolean;
      Has_Controlled : Boolean;
      Has_Ancestor   : Boolean;
      Has_Trivial    : Boolean;
      LHS_Kind       : LHS_Kinds;
      Filter         : Filter_Kind;
      Entity         : Entity_Specification;
      All_Exclude    : Boolean := True;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "subrule expected");
      end if;

      Has_Not        := Get_Modifier ("NOT");
      Has_Controlled := Get_Modifier ("CONTROLLED");
      Has_Trivial    := Get_Modifier ("TRIVIAL");
      Subrule        := Get_Flag_Parameter (Allow_Any => False);
      if Has_Not and not (Has_Controlled or Has_Trivial) then
         Parameter_Error (Rule_Id, """not"" must be followed by ""Controlled"" or ""Trivial""");
      end if;
      if Has_Controlled then
         if Subrule /= Sr_Access_Duplication then
            Parameter_Error (Rule_Id, """[not] controlled"" modifier applies only to subrule Access_Duplication");
         end if;
      end if;
      if Has_Trivial then
         if Subrule /= Sr_Possible_Target_Name then
            Parameter_Error (Rule_Id, """[not] trivial"" modifier applies only to subrule Possible_Target_Name");
         end if;
      end if;

      case Subrule is
         when Sr_Access_Duplication =>
            if Parameter_Exists then
               while Parameter_Exists loop
                  if Get_Modifier ("NOT") then
                     Filter := Exclude;
                  else
                     Filter      := Include;
                     All_Exclude := False;
                  end if;
                  Entity := Get_Entity_Parameter (Allow_Extended => Parens_OK);
                  Check_Category (Rule_Id, Entity, Duplication_Expected_Categories);

                  begin
                     if not Has_Controlled or else not Has_Not then
                        Associate (Entities (True),
                                   Entity,
                                   Duplication_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Filter));
                     end if;
                     if not Has_Controlled or else Has_Not then
                        Associate (Entities (False),
                                   Entity,
                                   Duplication_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Filter));
                     end if;
                  exception
                     when Already_In_Store =>
                        Parameter_Error (Rule_Id,
                                         "subrule ""access_duplication"" already specified with these parameters");
                  end;
               end loop;
               if All_Exclude then
                  Associate (Entities (not Has_Not),
                             Value ("ALL"),
                             Duplication_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Include));
               end if;
            else
               Associate (Entities (not Has_Not),
                          Value ("ALL"),
                          Duplication_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Include));
            end if;

         when Sr_Type =>
            if Parameter_Exists then
               while Parameter_Exists loop
                  if Get_Modifier ("COMPONENT") then
                     LHS_Kind := Component;
                  else
                     LHS_Kind := Direct;
                  end if;
                  Has_Ancestor := Get_Modifier ("ANCESTOR");
                  begin
                     Entity := Get_Entity_Parameter;
                     Associate (Target_Types (LHS_Kind),
                                Entity,
                                Type_Contexts'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Has_Ancestor));
                  exception
                     when Already_In_Store =>
                        Parameter_Error (Rule_Id,
                                         "subrule ""type"" already specified for type " & Image (Entity));
                  end;
               end loop;
            else
               Parameter_Error (Rule_Id, "missing type name for subrule ""type""");
            end if;

         when Sr_Sliding =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "No parameter for subrule ""sliding""");
            end if;

            if Rule_Used (Sr_Sliding) then
               Parameter_Error (Rule_Id, "subrule ""sliding"" already specified");
            end if;

            Sliding_Context := Basic.New_Context (Ctl_Kind, Ctl_Label);

         when Sr_Repeated =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "No parameter for subrule ""repeated""");
            end if;
            if Rule_Used (Sr_Repeated) then
               Parameter_Error (Rule_Id, "subrule ""repeated"" already specified");
            end if;

            Repeated_Context := Basic.New_Context (Ctl_Kind, Ctl_Label);

         when Sr_Groupable =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "criterion of acceptable component assignments required");
            end if;

            while Parameter_Exists loop
               Crit := Get_Modifier (Required => True);
               case Crit is
                  when Crit_Given =>
                     Given := Get_Integer_Parameter (Min => 1);
                  when Crit_Missing =>
                     Missing := Get_Integer_Parameter (Min => 0);
                  when Crit_Ratio =>
                     Ratio := Get_Integer_Parameter (Min => 1, Max => 100);
                  when Crit_Total =>
                     Total := Get_Integer_Parameter (Min => 1);
               end case;
            end loop;
            Append (Groupable_Contexts, (Basic.New_Context (Ctl_Kind, Ctl_Label) with Given, Missing, Ratio, Total));

         when Sr_Possible_Target_Name =>
            if Parameter_Exists and then Is_Integer_Parameter then
               Count := Id_Count (Biggest_Int'(Get_Integer_Parameter (Min => 1, Max => Biggest_Int (Id_Count'Last))));
            end if;
            if Has_Not then  -- can happen only as Not Trivial here
               if Not_Trivial_Given then
                  Parameter_Error (Rule_Id, "Not trivial Possible_Target_Name already given");
               end if;
               Not_Trivial_Target_Context := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Allowed_Count => Count);
               Not_Trivial_Given := True;
            else
               if Trivial_Given then
                  Parameter_Error (Rule_Id, "Trivial Possible_Target_Name already given");
               end if;
               Trivial_Target_Context := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Allowed_Count => Count);
               Trivial_Given := True;

               if not Has_Trivial then -- Control is for both
                  if Count /= 0 then
                     Parameter_Error (Rule_Id, "Count can be given only with ""(not] trivial""");
                  end if;
                  if Not_Trivial_Given then
                     Parameter_Error (Rule_Id, "Not trivial Possible_Target_Name already given");
                  end if;
                  Not_Trivial_Target_Context := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Allowed_Count => 1);
                  Not_Trivial_Given := True;
               end if;
            end if;
      end case;
      if Parameter_Exists then
         Parameter_Error (Rule_Id, "Too many parameters for subrule " & Image (Subrule));
      end if;
      Rule_Used (Subrule) := True;
   end Add_Control;


   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used         := Not_Used;
            Trivial_Given     := False;
            Not_Trivial_Given := False;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Not_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   ---------------------
   -- Process_Sliding --
   ---------------------

   procedure Process_Sliding (LHS : Asis.Element; RHS : Asis.Expression) is
      use Framework.Locations, Framework.Reports, Thick_Queries, Utilities;
   begin
      if Type_Category (LHS) /= An_Array_Type then
         return;
      end if;

      declare
         LH_Bounds : constant Extended_Biggest_Int_List := Discrete_Constraining_Values (LHS);
         RH_Bounds : constant Extended_Biggest_Int_List := Discrete_Constraining_Values (RHS);
         Inx       : Asis.List_Index;
      begin
         if LH_Bounds = Nil_Extended_Biggest_Int_List or RH_Bounds = Nil_Extended_Biggest_Int_List then
            -- Bounds cannot be determined (dynamic or determined by context)
            return;
         end if;
         for Dim in Asis.List_Index range 1 .. LH_Bounds'Last / 2 loop
            Inx := 2 * Dim - 1;
            if         LH_Bounds (Inx) /= Not_Static
              and then RH_Bounds (Inx) /= Not_Static
              and then LH_Bounds (Inx) /= RH_Bounds (Inx)
            then
               if LH_Bounds'Length = 2 then
                  Report (Rule_Id,
                          Sliding_Context,
                          Get_Location (RHS),
                          "Lower bound ("
                          & Biggest_Int_Img (RH_Bounds (Inx))
                          & ") does not match assigned variable ("
                          & Biggest_Int_Img (LH_Bounds (Inx))
                          & ')'
                         );
               else
                  Report (Rule_Id,
                          Sliding_Context,
                          Get_Location (RHS),
                          "Lower bound of dimension " & ASIS_Integer_Img (Dim) & " ("
                          & Biggest_Int_Img (RH_Bounds (Inx))
                          & ") does not match assigned variable ("
                          & Biggest_Int_Img (LH_Bounds (Inx))
                          & ')'
                         );
               end if;
            end if;
         end loop;
      end;
   end Process_Sliding;


   -------------------------
   -- Process_Target_Name --
   -------------------------

   procedure Process_Target_Name (LHS : Asis.Element; RHS : Asis.Expression) is
      use Asis, Asis.Iterator;

      procedure Target_Pre (Sub_Expr   :        Asis.Element;
                            Control    : in out Traverse_Control;
                            Target_Var : in out Asis.Expression);
      procedure Target_Post (Sub_Expr   :        Asis.Element;
                             Control    : in out Traverse_Control;
                             Target_Var : in out Asis.Expression)
      is null;
      procedure Target_Traverse is new Traverse_Element (Asis.Expression, Target_Pre, Target_Post);

      procedure Target_Pre (Sub_Expr   :        Asis.Element;
                            Control    : in out Traverse_Control;
                            Target_Var : in out Asis.Expression)
      is
         use Asis.Elements, Asis.Expressions;
         use Adactl_Constants, Framework.Locations, Framework.Reports, Thick_Queries;
         function Target_Count return Id_Count is
            use Asis.Text;
            use Utilities;

            The_Span  : constant Span := A4G_Bugs.Element_Span (Target_Var);
            Result    : ID_Count := 1;
            Remaining : Asis.Expression := Target_Var;
         begin
            if Expression_Kind (Target_Var) = An_Identifier then
               -- Trivial
               return Id_Count (The_Span.Last_Column - The_Span.First_Column + 1);
            end if;

            -- Not trivial
            loop
               case Expression_Kind (Remaining) is
                  when A_Selected_Component | An_Indexed_Component | An_Explicit_Dereference =>
                     Result := Result + 1;
                     Remaining := Prefix (Remaining);
                  when An_Identifier =>
                     return Result;
                  when others =>
                     -- A_Function_Call...
                     -- Should have been eliminated before we arrive here
                     Failure ("Unexpected target", Target_Var);
               end case;
            end loop;
         end Target_Count;
      begin   -- Target_Pre
         case Expression_Kind (Sub_Expr) is
            when An_Attribute_Reference =>
               -- don't traverse the attribute name
               Target_Traverse (Prefix (Sub_Expr), Control, Target_Var);
               Control := Abandon_Children;
            when A_Function_Call =>
               -- don't traverse the function name
               for P : Asis.Association of Function_Call_Parameters (Sub_Expr) loop
                  Target_Traverse (Actual_Parameter (P), Control, Target_Var);
               end loop;
               Control := Abandon_Children;
            when An_Allocation_From_Subtype =>
               -- Certainly not replaceable by target name!
               Control := Abandon_Children;
            when An_Allocation_From_Qualified_Expression =>
               -- Certainly not replaceable by target name, but traverse the qualified expression
               Target_Traverse (Allocator_Qualified_Expression (Sub_Expr), Control, Target_Var);
               Control := Abandon_Children;
            when A_Type_Conversion | A_Qualified_Expression =>
               Target_Traverse (Converted_Or_Qualified_Expression (Sub_Expr), Control, Target_Var);
               Control := Abandon_Children;
            when Not_An_Expression =>
               -- F.e. a definition/a_range from an array aggregate
               null;
            when others =>
               if Are_Equivalent_Expressions (Target_Var, Sub_Expr, RM_Static => True) then
                  -- We need RM_Static here, otherwise two variables known to have the same value would
                  -- be considered equivalent. While it would not be wrong to make the replacement in that case,
                  -- it is not desirable.
                  if Expression_Kind (Target_Var) = An_Identifier then
                     if Trivial_Given and then Target_Count > Trivial_Target_Context.Allowed_Count then
                        Report (Rule_Id,
                                Trivial_Target_Context,
                                Get_Location (Sub_Expr),
                                "Possible use of trivial target name");
                        Fixes.Replace (Sub_Expr, "@");
                     end if;
                  else
                     if Not_Trivial_Given and then Target_Count > Not_Trivial_Target_Context.Allowed_Count then
                        Report (Rule_Id,
                                Not_Trivial_Target_Context,
                                Get_Location (Sub_Expr),
                                "Possible use of not trivial target name");
                        Fixes.Replace (Sub_Expr, "@");
                     end if;
                  end if;
                  Control := Abandon_Children;  -- No sub-subexpression can be identical to Sub_Expr
               end if;
         end case;
      end Target_Pre;

      Control : Traverse_Control := Continue;
      LHS_Var : Asis.Expression  := LHS;   -- Because traverse requires a variable (in out parameter)
   begin  -- Process_Target_Name
      Target_Traverse (RHS, Control, LHS_Var);
   end Process_Target_Name;


   --------------------------------
   -- Process_Access_Duplication --
   --------------------------------

   procedure Process_Access_Duplication (Expr                 : Asis.Expression;
                                         Enclosing_Assignment : in Asis.Statement := Asis.Nil_Element)
   is
   -- Enclosing Assignment is Nil when called for an initialization ("@" not allowed there)
      use Thick_Queries, Utilities;
      use Asis, Asis.Iterator;
      Ignored         : Traverse_Control         := Continue;
      Controlled_Expr : Boolean                  := Is_Controlled (Expr);

      procedure Report_Possible (Element : Asis.Element; Controlled : Boolean) is
         use Framework.Locations, Framework.Reports;

         Cont : constant Root_Context'Class := Control_Manager.Association (Entities (Controlled), Value ("ALL"));
      begin   -- Report_Possible
         if Cont = No_Matching_Context then
            return;
         end if;

         Report (Rule_Id,
                 Cont,
                 Get_Location (Element),
                 "Possible duplication of " & Choose (Controlled, "", "not ") & "controlled access value");
      end Report_Possible;

      type Report_State is
         record
            In_Controlled     : Boolean;
            First_Inner_Ctrld : Asis.ASIS_Natural;
            Duplication_Root  : Asis.Element;
         end record;
      procedure Pre_Operation (Def     : in     Asis.Definition;
                               Control : in out Asis.Traverse_Control;
                               State   : in out Report_State;
                               Depth   : in     Asis.Asis_Positive)
      is
         use Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
         Good_Def    : Asis.Definition := Def;
         Target_Type : Asis.Declaration;

         function Is_In_Controlled return Boolean is
         begin
            return State.In_Controlled or State.First_Inner_Ctrld /= 0;
         end Is_In_Controlled;

         procedure Do_Report (Cont : Root_Context'Class) is
            use Framework.Locations, Framework.Reports;
         begin
            if Is_Part_Of_Instance (Good_Def) then
               Report (Rule_Id,
                       Cont,
                       Get_Location (State.Duplication_Root),
                       "Duplication of " & Choose (Is_In_Controlled, "", "not ") & "controlled access"
                       & " defined at " & Image (Get_Location
                                                 (Corresponding_Generic_Element
                                                  (Names (Enclosing_Element (Good_Def)) (1))))
                       &  " instantiated at " & Image (Get_Location (Ultimate_Enclosing_Instantiation (Good_Def))));
            else
               Report (Rule_Id,
                       Cont,
                       Get_Location (State.Duplication_Root),
                       "Duplication of " & Choose (Is_In_Controlled, "", "not ") & "controlled access"
                       & " defined at " & Image (Get_Location (Good_Def)));
            end if;
         end Do_Report;
      begin  -- Pre_Operation
         if State.First_Inner_Ctrld = 0 and then Is_Controlled (Good_Def) then
            State.First_Inner_Ctrld := Depth;
         end if;

         if not Is_Access_Subtype (Good_Def) then
            return;
         end if;

         if Definition_Kind (Good_Def) = A_Subtype_Indication then
            -- Get rid of subtypes
            Good_Def := Type_Declaration_View (A4G_Bugs.Corresponding_First_Subtype
                                               (Corresponding_Name_Declaration
                                                (Strip_Attributes
                                                 (Subtype_Simple_Name (Good_Def)))));
         end if;

         if Type_Kind (Good_Def) = A_Derived_Type_Definition then
            -- get rid of derived types
            Good_Def := Type_Declaration_View (Corresponding_Root_Type (Good_Def));
         end if;

         if        Access_Type_Kind (Good_Def)       = An_Access_To_Procedure
           or else Access_Type_Kind (Good_Def)       = An_Access_To_Protected_Procedure
           or else Access_Definition_Kind (Good_Def) = An_Anonymous_Access_To_Procedure
           or else Access_Definition_Kind (Good_Def) = An_Anonymous_Access_To_Protected_Procedure
         then
            -- access to procedure
            declare
               Cont : constant Root_Context'Class := Control_Manager.Association (Entities (Is_In_Controlled),
                                                                                  "PROCEDURE");
            begin
               if Cont /= No_Matching_Context then
                  if Duplication_Context (Cont).Filter = Include then
                     Do_Report (Cont);
                  end if;
                  Control := Abandon_Children;
                  return;
               end if;
            end;
         elsif     Access_Type_Kind (Good_Def)       = An_Access_To_Function
           or else Access_Type_Kind (Good_Def)       = An_Access_To_Protected_Function
           or else Access_Definition_Kind (Good_Def) = An_Anonymous_Access_To_Function
           or else Access_Definition_Kind (Good_Def) = An_Anonymous_Access_To_Protected_Function
         then
            -- access to function
            declare
               Cont : constant Root_Context'Class := Control_Manager.Association (Entities (Is_In_Controlled),
                                                                                  "FUNCTION");
            begin
               if Cont /= No_Matching_Context then
                  if Duplication_Context (Cont).Filter = Include then
                     Do_Report (Cont);
                  end if;
                  Control := Abandon_Children;
                  return;
               end if;
            end;
         else
            -- A regular access to object
            Target_Type := Access_Target_Type (Good_Def);

            -- search type itself
            declare
               Cont : constant Root_Context'Class := Matching_Context (Entities (Is_In_Controlled),
                                                                       Names (Target_Type)(1),
                                                                       Extend_To => All_Extensions);
            begin
               if Cont /= No_Matching_Context then
                  if Duplication_Context (Cont).Filter = Include then
                     Do_Report (Cont);
                  end if;
                  Control := Abandon_Children;
                  return;
               end if;
            end;

            -- search category
            declare
               use Framework.Language.Shared_Keys.Categories_Utilities;
               Cont : constant Root_Context'Class
                 := Control_Manager.Association (Entities (Is_In_Controlled),
                                                 Image (Matching_Category (Target_Type,
                                                                           From_Cats          => Full_Set,
                                                                           Follow_Derived     => True,
                                                                           Privacy            => Follow_Private,
                                                                           Separate_Extension => False)));
            begin
               if Cont /= No_Matching_Context then
                  if Duplication_Context (Cont).Filter = Include then
                     Do_Report (Cont);
                  end if;
                  Control := Abandon_Children;
                  return;
               end if;
            end;
         end if;

         -- For all cases, if nothing else matched, search "ALL"
         declare
            Cont : constant Root_Context'Class := Control_Manager.Association (Entities (Is_In_Controlled),
                                                                               "ALL");
         begin
            if Cont /= No_Matching_Context then
               Do_Report (Cont);
               return;
            end if;
         end;
      end Pre_Operation;

      procedure Post_Operation (Def     : in     Asis.Definition;
                                Control : in out Asis.Traverse_Control;
                                State   : in out Report_State;
                                Depth   : in     Asis.Asis_Positive)
      is
         pragma Unreferenced (Def, Control);
      begin
         if State.First_Inner_Ctrld = Depth then
            State.First_Inner_Ctrld := 0;
         end if;
      end Post_Operation;

      procedure Check_Included_Types is new Traverse_Data_Structure (Report_State, Pre_Operation, Post_Operation);

      procedure Check_Type_Def (In_Controlled : in Boolean;
                                Expr_Elem     : in Asis.Expression;
                                Type_Def      : in Asis.Definition)
      is
         State   : Report_State := (In_Controlled, 0, Expr_Elem);
         Control : Asis.Traverse_Control := Continue;
      begin
         Check_Included_Types (Type_Def, Control, State);
      end Check_Type_Def;

      procedure Pre_Operation  (Element       :        Asis.Element;
                                Control       : in out Traverse_Control;
                                In_Controlled : in out Boolean);
      procedure Post_Operation (Element       :        Asis.Element;
                                Control       : in out Traverse_Control;
                                In_Controlled : in out Boolean) is null;
      procedure Traverse is new Traverse_Element (Boolean, Pre_Operation, Post_Operation);
      -- In_Controlled is True while traversing subcomponents of a controlled aggregate

      procedure Pre_Operation (Element       :        Asis.Element;
                               Control       : in out Traverse_Control;
                               In_Controlled : in out Boolean)
      is
         procedure Traverse_Instead (Element : Asis.Element) is             --## rule line off Local_Hiding
            Local_Controlled : Boolean := In_Controlled;
            -- We need a local variable because the traversal can change it
         begin
            Traverse (Element, Control, Local_Controlled);
            if Control /= Terminate_Immediately then
               Control := Abandon_Children;
            end if;
         end Traverse_Instead;

         use Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions, Asis.Statements;
      begin  -- Pre_Operation
         case Expression_Kind (Element) is
            when Not_An_Expression =>
               Failure ("Process_Access_Duplication: not an expression", Element);

            when A_Box_Expression =>
               -- This cannot come from a record aggregate, since we traverse these as normalized associations
               -- For array aggregates, aspect Default_Component_Value (or Default_Value of the component) are
               -- defined only for scalar types => not access or composite.
               -- Therefore, this can make an access duplication only if the component type contains a record with
               -- a component of a type that has a default value that makes an access duplication...
               -- Too hard to check for the moment, mark this (unlikely) case as "possible" if the component type
               -- contains a record.
               --
               -- The box expression is in an association in an aggregate
               declare
                  Compo_Def  : Asis.Definition := Enclosing_Element (Enclosing_Element (Element));
                  Compo_Type : Asis.Declaration;
               begin
                  -- We must skip subaggregates of multimensional array aggregates, since they have no type definition
                  -- of their own. The only way we found to differentiate these from -say- an array of arrays is
                  -- that their Corresponding_Expression_Type_Definition returns Nil_Element. If anyone knows a
                  -- better solution...
                  --
                  -- Bad luck: Corresponding_Expression_Type returns Nil_Element on these aggregates, therefore
                  -- fooling Thick_Queries.Corresponding_Expression_Type_Definition... but
                  -- Asis.Expressions.Corresponding_Expression_Type_Definition seems to work!
                  -- So, exceptionnally use the one from A4G...
                  while Is_Nil (Asis.Expressions.Corresponding_Expression_Type_Definition (Compo_Def)) loop
                     Compo_Def := Enclosing_Element (Enclosing_Element (Compo_Def));
                  end loop;
                  Compo_Def := Component_Definition_View (Array_Component_Definition
                                                          (Asis.Expressions.Corresponding_Expression_Type_Definition
                                                           (Compo_Def)));
                  if Definition_Kind (Compo_Def) = An_Access_Definition then
                     Check_Type_Def (In_Controlled, Element, Compo_Def);
                  else -- necessarily a component_definition
                     Compo_Type := Corresponding_Name_Declaration (Subtype_Simple_Name (Compo_Def));
                     if        Contains_Type_Declaration_Kind (Compo_Type,
                                                               An_Ordinary_Type_Declaration,
                                                               A_Record_Type_Definition)
                       or else Contains_Type_Declaration_Kind (Compo_Type,
                                                               An_Ordinary_Type_Declaration,
                                                               A_Tagged_Record_Type_Definition)
                     then
                        Report_Possible (Element, In_Controlled or else Is_Controlled (Compo_Type));
                     end if;
                  end if;
               end;
            when An_Integer_Literal
               | A_Real_Literal
               | A_String_Literal
               | An_Operator_Symbol
               | A_Character_Literal
               | An_Enumeration_Literal
               =>
               -- Final, nothing special
               null;

            when A_Raise_Expression =>
               -- A raise expression can contain various stuff, but it's not assigned
               Control := Abandon_Children;

            when An_Identifier =>
               Check_Type_Def (In_Controlled,
                               Element,
                               Thick_Queries.Corresponding_Expression_Type_Definition (Element));

            when An_Explicit_Dereference =>
               -- Since we never traverse prefixes of qualified names, this is a plain X.all (not X.all.Comp)
               Control := Abandon_Children;
               Check_Type_Def (In_Controlled,
                               Element,
                               Type_Declaration_View
                                (Access_Target_Type
                                 (Ultimate_Expression_Type
                                  (Simple_Name (Prefix (Element))))));

            when A_Function_Call =>
               Control := Abandon_Children;
               declare
                  Result_Type : constant Definition := Thick_Queries.Corresponding_Expression_Type_Definition (Element);
               begin
                  if not Is_Nil (Result_Type)  -- Predefined, universal stuff...
                    and then Contains_Type_Declaration_Kind (Result_Type,
                                                             An_Ordinary_Type_Declaration,
                                                             An_Access_Type_Definition)
                  then
                     Report_Possible (Element, In_Controlled or else Is_Controlled (Result_Type));
                  end if;
               end;

            when An_Indexed_Component | A_Slice =>
               -- Since we check for composite structures containing access types,
               -- it equivalent to check the whole array (and it saves traversing the indexes)
               Traverse_Instead (Prefix (Element));

            when A_Selected_Component =>
               -- traverse selector only
               Traverse_Instead (Selector (Element));

            when An_Attribute_Reference =>
               -- The only potentially interesting one is '(Unchecked_)Access, however it would be
               -- treated like an allocator, which is ignored
               Control := Abandon_Children;

            when A_Parenthesized_Expression =>
               -- let recurse normally
               null;

            when A_Type_Conversion | A_Qualified_Expression =>
               -- Don't traverse the type name
               Traverse_Instead (Converted_Or_Qualified_Expression (Element));

            when A_Record_Aggregate | An_Extension_Aggregate =>
               -- Traverse components only (not choices)
               declare
                  Controlled_Rec : Boolean := In_Controlled;
               begin
                  if Expression_Kind (Element) = An_Extension_Aggregate then
                     -- fixed part of extension aggregates
                     Traverse (Extension_Aggregate_Expression (Element), Control, Controlled_Rec);
                     if Control = Terminate_Immediately then
                        return;
                     end if;
                     -- Traverse may have destroyed Controlled_Rec + an extension of a controlled type is controlled
                     Controlled_Rec := In_Controlled or else Is_Controlled (Extension_Aggregate_Expression (Element));
                  end if;

                  declare
                     Compo   : Asis.Expression;
                     Temp_CR : Boolean;
                  begin
                     for Assoc : Asis.Association of Record_Component_Associations (Element, Normalized => True) loop
                        Compo := Component_Expression (Assoc);
                        if Is_Nil (Compo) then
                           -- Compo is nil for the normalized form of a box expression with no default
                           -- Do like A_Box_Expression above
                           -- The aggregate could have "others => <>" covering different types, but since we
                           -- use a normalized association, ASIS will sort this up for us. Retrieve the component's
                           -- type starting from the choices. We know there is only one choice per association.
                           -- Reminder: since it is a normalized association, the choices are the defining names
                           --           of the components
                           declare
                              Compo_Def  : Asis.Definition ;
                              Compo_Type : Asis.Declaration;
                           begin
                              Compo_Def := Object_Declaration_View (Enclosing_Element
                                                                    (Record_Component_Choices (Assoc) (1)));
                              if Definition_Kind (Compo_Def) = A_Component_Definition then
                                 Compo_Def := Component_Definition_View (Compo_Def);
                                 if Definition_Kind (Compo_Def) = An_Access_Definition then
                                    Check_Type_Def (In_Controlled, Element, Compo_Def);
                                    Compo_Type := Nil_Element;
                                 else
                                    Compo_Type := Corresponding_Name_Declaration (Strip_Attributes
                                                                                  (Subtype_Simple_Name
                                                                                   (Compo_Def)));
                                 end if;
                              else
                                 -- The component was a discriminant, Compo_Def is the discriminant's type name
                                 Compo_Type := Corresponding_Name_Declaration
                                                   (Strip_Attributes (Simple_Name (Compo_Def)));
                              end if;

                              if not Is_Nil (Compo_Type)
                                and then (       Contains_Type_Declaration_Kind (Compo_Type,
                                                                                 An_Ordinary_Type_Declaration,
                                                                                 A_Record_Type_Definition)
                                          or else Contains_Type_Declaration_Kind (Compo_Type,
                                                                                  An_Ordinary_Type_Declaration,
                                                                                  A_Tagged_Record_Type_Definition))
                              then
                                 Report_Possible (Element, In_Controlled or else Is_Controlled (Compo_Type));
                              end if;
                           end;
                        else
                           Temp_CR := Controlled_Rec;
                           Traverse (Compo, Control, Temp_CR);
                           if Control = Terminate_Immediately then
                              return;
                           end if;
                        end if;
                     end loop;
                     Control := Abandon_Children;
                  end;
               end;

            when A_Positional_Array_Aggregate | A_Named_Array_Aggregate =>
               -- Traverse components only (not choices)
               -- No need to check controlledness, arrays can't be controlled
               declare
                  procedure Traverse_Components (Aggr : Asis.Expression) is
                  begin
                     for Assoc : Asis.Association of Array_Component_Associations (Aggr) loop
                        declare
                           Compo            : constant Asis.Expression := Component_Expression (Assoc);
                           Controlled_Comps : Boolean := In_Controlled;
                        begin
                           if Expression_Kind (Compo) in A_Positional_Array_Aggregate .. A_Named_Array_Aggregate then
                              -- Multidimensional array or array of array: traverse only components
                              Traverse_Components (Compo);
                           else
                              Traverse (Compo, Control, Controlled_Comps);
                           end if;
                           if Control = Terminate_Immediately then
                              return;
                           end if;
                        end;
                     end loop;
                  end Traverse_Components;
               begin
                  Traverse_Components (Element);
                  if Control = Terminate_Immediately then
                     return;
                  end if;
                  Control := Abandon_Children;
               end;

            when An_And_Then_Short_Circuit
               | An_Or_Else_Short_Circuit
               | An_In_Membership_Test
               | A_Not_In_Membership_Test
               | A_For_All_Quantified_Expression
               | A_For_Some_Quantified_Expression
               =>
               -- These always return Boolean
               Control := Abandon_Children;

            when A_Null_Literal
               | An_Allocation_From_Subtype
               =>
               -- Purposedly ignored
               Control := Abandon_Children;

            when An_Allocation_From_Qualified_Expression =>
               -- There might be duplication if the initial value contains access types
               Traverse_Instead (Allocator_Qualified_Expression (Element));

            when A_Case_Expression | An_If_Expression =>
               -- Recurse through data only (not choices)
               declare
                  Controlled_Path : Boolean;
               begin
                  for P : Asis.Path of Expression_Paths (Element) loop
                     Controlled_Path := In_Controlled;
                     Traverse (Dependent_Expression (P), Control, Controlled_Path);
                     if Control = Terminate_Immediately then
                        return;
                     end if;
                  end loop;
                  Control := Abandon_Children;
               end;
            when others => -- A_Target_Name, given as "when others" for compatibility
               if Expression_Kinds'Wide_Image (Expression_Kind (Element)) = "A_TARGET_NAME" then
                  -- The type is the one of the LHS of the assignment
                  Check_Type_Def (In_Controlled,
                                  Assignment_Variable_Name (Enclosing_Assignment),
                                  Thick_Queries.Corresponding_Expression_Type_Definition (Element));
               else
                  Failure ("Process_Access_Duplication: Wrong expressions kind", Element);
               end if;
         end case;
      end Pre_Operation;

   begin   -- Process_Access_Duplication
      Traverse (Expr, Ignored, Controlled_Expr);
   end Process_Access_Duplication;

   ------------------
   -- Process_Type --
   ------------------

   procedure Process_Type (LHS : Asis.Element) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
      Decl : Asis.Declaration;

      procedure Check_Type (Type_Decl : Asis.Declaration; LHS_Kind : LHS_Kinds) is
         use Asis.Declarations, Asis.Definitions;
         use Framework.Locations, Framework.Reports;

         Store : Context_Store renames Target_Types (LHS_Kind);
         Cur_Decl : Asis.Declaration := Type_Decl;
         Cont : constant Root_Context'Class := Matching_Context (Store, Names (Cur_Decl)(1));
      begin
         if Cont /= No_Matching_Context then
            Report (Rule_Id,
                    Cont,
                    Get_Location (LHS),
                    "Assignment to "
                    & (if LHS_Kind = Component then "component of " else "")
                    & "variable of type " & Last_Matching_Name (Store));
         end if;

         while Type_Kind (Type_Declaration_View (Cur_Decl))
               in A_Derived_Type_Definition | A_Derived_Record_Extension_Definition
         loop
            -- Be careful if type derived from T'Base:
            Cur_Decl := Corresponding_Name_Declaration (Strip_Attributes
                                                        (Subtype_Simple_Name
                                                         (Parent_Subtype_Indication
                                                          (Type_Declaration_View (Cur_Decl)))));
            declare
               Der_Cont : constant Root_Context'Class := Matching_Context (Store, Names (Cur_Decl) (1));
            begin
               if Der_Cont /= No_Matching_Context and then Type_Contexts (Der_Cont).Search_Ancestor then
                  Report (Rule_Id,
                          Der_Cont,
                          Get_Location (LHS),
                          "Assignment to "
                          & (if LHS_Kind = Component then "component of " else "")
                          & "variable of type derived from " & Last_Matching_Name (Store));
               end if;
            end;
         end loop;
      end Check_Type;

   begin  -- Process_Type
      -- Check normal case
      Decl := Enclosing_Element (Thick_Queries.Corresponding_Expression_Type_Definition (LHS));
      -- Decl can be an object declaration in case of an anonymous type
      if Declaration_Kind (Decl) not in A_Type_Declaration | A_Subtype_Declaration then
         return;
      end if;
      Check_Type (Decl, Direct);

      -- Check component case
      case Expression_Kind (LHS) is
         when A_Selected_Component =>
            -- Make sure the prefix is a record (not a program unit). Fortunately, there are no anonymous record types!
            Decl := A4G_Bugs.Corresponding_Expression_Type (Simple_Name (Prefix (LHS)));
            -- Decl is nil for package names, and also anonymous types (which is OK)
            if not Is_Nil (Decl) then
                  Check_Type (Decl, Component);
            end if;
         when An_Indexed_Component =>
            Decl := Enclosing_Element (Thick_Queries.Corresponding_Expression_Type_Definition (Prefix (LHS)));
            -- Decl can be an object declaration in case of an anonymous type
            if Declaration_Kind (Decl) not in A_Type_Declaration | A_Subtype_Declaration then
               return;
            end if;
            Check_Type (Decl, Component);
         when others =>
            null;
      end case;
   end Process_Type;

   ------------------------
   -- Process_Assignment --
   ------------------------

   procedure Process_Assignment (Statement : in Asis.Statement) is
      use Asis.Statements;
   begin   -- Process_Assignment
      if (Rule_Used and Usage_Flags'(Sr_Sliding | Sr_Access_Duplication | Sr_Type | Sr_Possible_Target_Name => True,
                                     others => False)) = Not_Used
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (Sr_Sliding) then
         Process_Sliding (LHS => Assignment_Variable_Name (Statement),
                          RHS => Assignment_Expression    (Statement));
      end if;

      if Rule_Used (Sr_Access_Duplication) then
         Process_Access_Duplication (Assignment_Expression (Statement), Statement);
      end if;

      if Rule_Used (Sr_Type) then
         Process_Type (Assignment_Variable_Name (Statement));
      end if;

      if Rule_Used (Sr_Possible_Target_Name) then
         Process_Target_Name (LHS => Assignment_Variable_Name (Statement),
                              RHS => Assignment_Expression    (Statement));
      end if;
   end Process_Assignment;

   --------------------------------
   -- Process_Object_Declaration --
   --------------------------------

   procedure Process_Object_Declaration  (Declaration : in Asis.Declaration) is
      use Asis.Declarations, Asis.Elements;
   begin
      if not Rule_Used (Sr_Sliding) and not Rule_Used (Sr_Access_Duplication) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Init_Expr : constant Asis.Expression := Initialization_Expression (Declaration);
      begin
         if not Is_Nil (Init_Expr) then
            if Rule_Used (Sr_Sliding) then
               Process_Sliding (LHS => Names (Declaration) (1),  -- Even if there are several names
                                RHS => Init_Expr);
            end if;

            if Rule_Used (Sr_Access_Duplication) then
               Process_Access_Duplication (Init_Expr);
            end if;
         end if;
      end;
   end Process_Object_Declaration;

   ----------------------------------
   -- Process_Statement_Container  --
   ----------------------------------

   procedure Process_Statement_Container (Element : in Asis.Element) is
      use LHS_Map, Thick_Queries;
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;

      LHS_Infos   : LHS_Map.Map;
      Dynamic_LHS : exception;
      -- Raised when the LHS is not statically determinable, the whole assignment
      -- is abandonned.
      Reason      : Unbounded_Wide_String;


      function Total_Fields (Struct : Asis.Element) return Extended_Biggest_Natural is
      -- Total number of components of a record or array definition.
      -- By extension: works also on an expresion of a record type.
         use Utilities;
         use Asis.Declarations, Asis.Definitions;

         Def : Asis.Definition;
      begin
         case Element_Kind (Struct) is
            when An_Expression =>
               Def := Thick_Queries.Corresponding_Expression_Type_Definition (Struct);
            when A_Definition =>
               Def := Struct;
            when others =>
               Failure ("Total_Fields: unexpected element", Struct);
         end case;

         -- Among other things, this loop unwinds subtypes and visible derivations
         -- We cannot use Ultimate_Expression_Type because we do not want to unwind
         -- private type declarations and record extensions.
         loop
            case Definition_Kind (Def) is
               when A_Type_Definition =>
                  case Type_Kind (Def) is
                     when An_Unconstrained_Array_Definition
                        | A_Constrained_Array_Definition
                        =>
                        return Result  : Extended_Biggest_Natural := 1 do
                           for L : Extended_Biggest_Natural of Discrete_Constraining_Lengths (Struct) loop
                              if L = Not_Static then
                                 Result := Not_Static;
                                 return;
                              end if;
                              Result := Result * L;
                           end loop;
                           return;
                        end return;
                     when A_Record_Type_Definition
                        | A_Tagged_Record_Type_Definition
                        =>
                        Def := Asis.Definitions.Record_Definition (Def);
                     when A_Derived_Type_Definition =>
                        Def := Parent_Subtype_Indication (Def);
                     when A_Derived_Record_Extension_Definition =>
                        declare
                           Parent_Fields    : constant Extended_Biggest_Natural
                             := Total_Fields (Parent_Subtype_Indication (Def));
                           Extension_Fields : constant Extended_Biggest_Natural
                             := Total_Fields (Asis.Definitions.Record_Definition (Def));
                        begin
                           if Parent_Fields = Not_Static or Extension_Fields = Not_Static then
                              return Not_Static;
                           end if;
                           return Parent_Fields + Extension_Fields;
                        end;
                     when An_Interface_Type_Definition => --2005
                        -- Interfaces have no data...
                        return 0;
                     when others =>
                        Failure ("Total_Fields: unexpected type kind " & Type_Kinds'Wide_Image (Type_Kind (Def)), Def);
                  end case;

               when A_Record_Definition =>
                  declare
                     -- The Record_Type_Definition is in A_Type_Definition which is in A_Type_Declaration...
                     Decl       : constant Asis.Declaration := Enclosing_Element (Enclosing_Element (Def));
                     Components : constant Asis.Record_Component_List := Record_Components (Def);
                     Result     : Biggest_Natural := 0;
                  begin
                     if Definition_Kind (Components (Components'Last)) = A_Variant_Part then
                        return Not_Static;
                     end if;
                     for C : Asis.Element of Components loop
                        if Definition_Kind (C) /= A_Null_Component then
                           Result := Result + Names (C)'Length;
                        end if;
                     end loop;
                     if not Is_Nil (Discriminant_Part (Decl)) then
                        for D : Asis.Declaration of Discriminants (Discriminant_Part (Decl)) loop
                           Result := Result + Names (D)'Length;
                        end loop;
                     end if;
                     return Result;
                  end;

               when A_Protected_Definition =>
                  declare
                     Decl   : constant Asis.Declaration := Enclosing_Element (Def);
                     Result : Biggest_Natural := 0;
                  begin
                     for Compo : Asis.Element of Private_Part_Items (Def) loop
                        if Declaration_Kind (Compo) = A_Component_Declaration then
                           Result := Result + Names (Compo)'Length;
                        end if;
                     end loop;
                     if Declaration_Kind (Decl) = A_Protected_Type_Declaration  -- no discriminants for single PO
                       and then not Is_Nil (Discriminant_Part (Decl))
                     then
                        for D : Asis.Declaration of Discriminants (Discriminant_Part (Decl)) loop
                           Result := Result + Names (D)'Length;
                        end loop;
                     end if;
                     return Result;
                  end;

               when A_Subtype_Indication =>  -- No 'Base for record and array types
                  Def := Type_Declaration_View (Corresponding_Name_Declaration (Subtype_Simple_Name (Def)));

               when A_Private_Type_Definition
                  | A_Tagged_Private_Type_Definition
                  | A_Private_Extension_Definition =>
                  return Not_Static;

               when A_Null_Record_Definition =>
                  return 0;

               when A_Formal_Type_Definition =>
                  return Not_Static;

               when others =>
                  Failure ("Total_Fields: unexpected definition", Def);
            end case;
         end loop;
      end Total_Fields;

      function Exceeds_Thresholds (Value         : LHS_Descriptor;
                                   Limits        : Groupable_Context;
                                   With_Messages : Boolean := False) return Boolean

      is
         use Utilities;

         Subcomp_Count : Thick_Queries.Biggest_Natural := Value.Full_Subcomp_Count;
         Subcomp_Descr : LHS_Descriptor;
         Subcomp_Key   : Unbounded_Wide_String;
         Matched       : Boolean := True;

      begin
         if Value.Coverage = Full then
            return True;  -- Since aggregate required
         end if;

         Reason := Null_Unbounded_Wide_String;

         Subcomp_Key := Value.First_Child;
         while Subcomp_Key /= Null_Unbounded_Wide_String loop
            Subcomp_Descr := Fetch (LHS_Infos, Subcomp_Key);
            if Exceeds_Thresholds (Subcomp_Descr, Limits, With_Messages => False) then
               Subcomp_Count := Subcomp_Count + 1;
            end if;
            Subcomp_Key := Subcomp_Descr.Brother;
         end loop;

         --
         -- Given
         --
         if Limits.Given > 0 then
            if Subcomp_Count < Limits.Given then
               Matched := False;
            elsif With_Messages then
               Append (Reason, ", given: " & Biggest_Int_Img (Subcomp_Count)
                       & " (>=" & Biggest_Int_Img (Limits.Given) & ')');
            end if;
         end if;

         --
         -- Missing
         --
         if Limits.Missing < Biggest_Int'Last then
            if Value.Subcomp_Total = Not_Static
              or else Value.Subcomp_Total - Subcomp_Count > Limits.Missing
            then
               Matched := False;
            elsif With_Messages then
               Append (Reason, ", missing: " & Biggest_Int_Img (Value.Subcomp_Total - Subcomp_Count)
                       & " (<=" & Biggest_Int_Img (Limits.Missing) & ')');
            end if;
         end if;

         --
         -- Ratio
         --
         if Limits.Ratio > 0 then
            if Value.Subcomp_Total = Not_Static
              or else Subcomp_Count * 100 / Value.Subcomp_Total < Biggest_Int (Limits.Ratio)
            then
               Matched := False;
            elsif With_Messages then
               Append (Reason, ", ratio: " & Biggest_Int_Img (Subcomp_Count * 100 / Value.Subcomp_Total)
                       & " (>=" & ASIS_Integer_Img (Limits.Ratio) & ')');
            end if;
         end if;

         --
         -- Total
         --
         if Limits.Total > 0 then
            if Value.Subcomp_Total = Not_Static
              or else Value.Subcomp_Total > Limits.Total
            then
               Matched := False;
            elsif With_Messages then
               Append (Reason, ", total: " & Biggest_Int_Img (Value.Subcomp_Total)
                       & " (<=" & Biggest_Int_Img (Limits.Total) & ')');
            end if;
         end if;

         return Matched;
      end Exceeds_Thresholds;

      procedure Process_Assignment (LHS      : in Asis.Expression;
                                    Coverage : in Coverage_Kind;
                                    Key      : out Unbounded_Wide_String)
      is
         use Asis.Declarations;
         use Framework.Locations, Framework.Queries, Framework.Reports, Utilities;
         Target       : Asis.Expression := LHS;
         Target_Descr : LHS_Descriptor;
         Parent       : Asis.Expression;
         Parent_Key   : Unbounded_Wide_String;
         Parent_Descr : LHS_Descriptor;

         -- Adaptor function for expressions (rather than subtypes)
         function Is_Class_Wide (E : Asis.Element) return Boolean is
            Simple_E : constant Asis.Expression := Simple_Name (E);
            Decl     : Asis.Declaration;
         begin
            case Expression_Kind (Simple_E) is
               when A_Type_Conversion | A_Qualified_Expression =>
                  if Expression_Kind (Simple_E) = An_Attribute_Reference then
                     -- Can only be 'Base or 'Class, and 'Base is not applicable to class-wide types
                     return Attribute_Kind (Prefix (Simple_E)) = A_Class_Attribute;
                  end if;
                  return Is_Class_Wide_Subtype (Type_Declaration_View (Corresponding_Name_Declaration
                                                                       (Simple_Name
                                                                        (Converted_Or_Qualified_Subtype_Mark
                                                                         (Simple_E)))));
               when An_Indexed_Component | A_Slice =>
                  -- array elements cannot be class-wide
                  return False;
               when An_Explicit_Dereference =>
                  return Is_Class_Wide_Subtype (Thick_Queries.Corresponding_Expression_Type_Definition (Simple_E));
               when An_Identifier =>
                  Decl := Corresponding_Name_Declaration (Simple_E);
                  case Declaration_Kind (Decl) is
                     when An_Element_Iterator_Specification =>
                        -- Certainly not class-wide...
                        return False;
                     when A_Protected_Type_Declaration | A_Task_Type_Declaration =>
                        -- This happens when the prefix is the name of a protected or task type within its own body
                        -- designating the current object
                        return False;
                     when others =>
                        return Is_Class_Wide_Subtype (Object_Declaration_View (Decl));
                  end case;
               when A_Function_Call =>
                  return Is_Class_Wide_Subtype (Thick_Queries.Corresponding_Expression_Type_Definition (Simple_E));
               when others =>
                    Failure ("Is_Class_Wide: unexpected prefix", E);
            end case;
         end Is_Class_Wide;

      begin  -- Process_Assignment
         loop
            case Expression_Kind (Target) is
               when A_Selected_Component =>
                  if Declaration_Kind (Corresponding_Name_Declaration (Selector (Target)))
                     in A_Discriminant_Specification .. A_Component_Declaration
                  then
                     Parent := Prefix (Target);
                     if Is_Access_Expression (Parent) or else Is_Class_Wide (Parent) then
                        -- Implicit dereference, or class-wide parent (no aggregate allowed)
                        raise Dynamic_LHS;
                     end if;
                     Process_Assignment (Parent, Component, Key);
                     Parent_Key := Key;
                     Append (Key, '.' & To_Upper (Name_Image (Selector (Target))));
                     exit;
                  end if;

                  -- Not a record component (Fully named variable from a package, f.e.)
                  Target := Selector (Target);

               when An_Indexed_Component =>
                  Parent := Prefix (Target);
                  if Is_Access_Expression (Parent) or else Is_Class_Wide (Parent) then
                     -- Implicit dereference, or class-wide parent (no aggregate allowed)
                     raise Dynamic_LHS;
                  end if;
                  Process_Assignment (Parent, Component, Key);
                  Parent_Key := Key;
                  Append (Key, '(');
                  for E : Asis.Expression of Index_Expressions (Target) loop
                     declare
                        Value : constant Wide_String := Static_Expression_Value_Image (E);
                     begin
                        if Value = "" then
                           raise Dynamic_LHS;
                        end if;
                        Append (Key, Value & ',');
                     end;
                  end loop;
                  Replace_Element (Key, Length (Key), ')');
                  exit;
               when A_Slice  =>
                  -- Strictly speaking, we should handle that as as many LHS as elements
                  -- in the slice (provided it is static of course). But this could make
                  -- thousands of entries...
                  -- Not worth the complication, just consider it dynamic in all cases.
                  raise Dynamic_LHS;
               when A_Type_Conversion =>
                  Target := Converted_Or_Qualified_Expression (Target);
               when An_Explicit_Dereference
                  | A_Function_Call
                  =>
                  raise Dynamic_LHS;
               when others =>
                  if Declaration_Kind (Corresponding_Name_Declaration (Target))
                    /= An_Object_Renaming_Declaration
                  then
                     -- A truly global assignment, not groupable
                     Key    := To_Key (Target);
                     Parent := Nil_Element;
                     exit;
                  end if;

                  -- A renaming: start over with the renamed expression
                  Target := Renamed_Entity (Corresponding_Name_Declaration (Target));
            end case;
         end loop;

         if Is_Present (LHS_Infos, Key) then
            -- Variable already assigned
            Target_Descr := Fetch (LHS_Infos, Key);
            if Rule_Used (Sr_Repeated)
              and then (Coverage = Full or Target_Descr.Coverage = Full)
            then
               Report (Rule_Id,
                       Repeated_Context,
                       Get_Location (LHS),
                       "variable already assigned in same group at " & Image (Target_Descr.Loc));
            end if;
            return;
         end if;

         -- Only "new" (not already assigned) components past this point
         case Coverage is
            when Full =>
               Target_Descr := (Coverage => Full, Loc => Get_Location (LHS));
            when Component =>
               Target_Descr := (Coverage           => Component,
                                Loc                => Get_Location (LHS),
                                Subcomp_Total      => Total_Fields (LHS),
                                Full_Subcomp_Count => 0,  -- will be incremented by full children
                                First_Child        => Null_Unbounded_Wide_String,
                                Brother            => Null_Unbounded_Wide_String);
         end case;

         if Is_Nil (Parent) or else Is_Limited (Parent) then
            Add (LHS_Infos, Key, Target_Descr);
            return;
         end if;

         -- True field, not already seen: Increment parent count if full child, chain otherwise
         Parent_Descr := Fetch (LHS_Infos, Parent_Key);
         if Parent_Descr.Coverage = Full then  -- Parent previously assigned in full
            if Rule_Used (Sr_Repeated) then
               Report (Rule_Id,
                       Repeated_Context,
                       Get_Location (LHS),
                       "variable already assigned in same group at " & Image (Parent_Descr.Loc));
            end if;
         else
            Parent_Descr.Loc := Get_Location (LHS);
            case Coverage is
               when Full =>
                  Parent_Descr.Full_Subcomp_Count := Parent_Descr.Full_Subcomp_Count + 1;
               when Component =>
                  if Parent_Descr.First_Child /= Null_Unbounded_Wide_String then
                     Target_Descr.Brother := Parent_Descr.First_Child;
                  end if;
                  Parent_Descr.First_Child := Key;
            end case;
            Add (LHS_Infos, Parent_Key, Parent_Descr);
         end if;

         Add (LHS_Infos, Key, Target_Descr);
      end Process_Assignment;

      procedure Report_One (Key : Unbounded_Wide_String; Value : in out LHS_Descriptor) is
         use Context_Queue, Framework.Reports, Utilities;
         Current : Cursor;
         Context : Groupable_Context;
      begin
         if Value.Coverage = Full   --A global assignment
           or else Value.Subcomp_Total = 0  --A null record
         then
            return;
         end if;

         -- We do not use Assert for the following sanity check, to avoid evaluating the error string
         -- (would raise Constraint_Error if Value.Total = Not_Static)
         if Value.Subcomp_Total /= Not_Static and then Value.Full_Subcomp_Count > Value.Subcomp_Total then
            Failure ("More assigned fields than possible for " & To_Wide_String (Key) & ": "
                     & Biggest_Int_Img (Value.Full_Subcomp_Count) & "/" & Biggest_Int_Img (Value.Subcomp_Total));
         end if;

         Current := First (Groupable_Contexts);
         while Has_Element (Current) loop
            Context := Fetch (Current);

            if Exceeds_Thresholds (Value, Limits => Context, With_Messages => True)  then
               Report (Rule_Id,
                       Context,
                       Value.Loc,
                       "too many assignments to components of "
                       & To_Title (Strip_Profile (To_Wide_String (Key)))
                       & To_Wide_String (Reason));
            end if;

            Current := Next (Current);
         end loop;
      end Report_One;

      procedure Do_Report is new LHS_Map.Iterate (Report_One);

   begin   -- Process_Statement_Container
      if not Rule_Used (Sr_Groupable) and not Rule_Used (Sr_Repeated) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Stmts   : constant Asis.Statement_List := Thick_Queries.Statements (Element);
         Ignored : Unbounded_Wide_String;
      begin
         if Stmts'Length = 0 then
            -- Package body without statements
            return;
         end if;

         for S : Asis.Statement of Stmts loop
            case Statement_Kind (S) is
               when An_Assignment_Statement =>
                  begin
                     Process_Assignment (LHS      => Assignment_Variable_Name (S),
                                         Coverage => Full,
                                         Key      => Ignored);
                  exception
                     when Dynamic_LHS =>
                        null;
                  end;
               when A_Null_Statement =>
                  -- This one is harmless...
                  null;
               when others =>
                  Do_Report (LHS_Infos);
                  Clear (LHS_Infos);
            end case;
         end loop;

         if Statement_Kind (Stmts (Stmts'Last)) = An_Assignment_Statement
           or else Statement_Kind (Stmts (Stmts'Last)) = A_Null_Statement
         then
            Do_Report (LHS_Infos);
            Clear (LHS_Infos);
         end if;
      end;
   end Process_Statement_Container;

begin  -- Rules.Assignments
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Assignments;
