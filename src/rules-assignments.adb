----------------------------------------------------------------------
--  Rules.Assignments - Package body                                --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005.         --
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
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Assignments is
   use Ada.Strings.Wide_Unbounded;
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- Subrule Sliding:
   -- Easy
   --
   -- Subrule Access_Duplication:
   -- Traverse the assignment expression recursively, skipping anything that is not part of the value
   -- (like aggregate choices, subprogram parameters, prefixes of qualified expressions), for access variables.
   -- Note that functions could return the value of one of their parameters, hence their duplication status is
   -- "possible"
   --
   -- Subrules repeated and groupable:
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

   type Subrules is (Sliding, Access_Duplication, Repeated, Groupable);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Criteria is (Crit_Given, Crit_Missing, Crit_Ratio, Crit_Total);
   package Criteria_Utilities is new Framework.Language.Modifier_Utilities (Criteria, Prefix => "Crit_");

   type Usage_Flags is array (Subrules) of Boolean;
   Not_Used : constant Usage_Flags := (others => False);
   Rule_Used : Usage_Flags := Not_Used;
   Save_Used : Usage_Flags;

   -- Data for subrule Sliding:
   Sliding_Context : Basic_Rule_Context;

   -- Data for subrule Access_Duplication:
   Access_Duplication_Context : array (Boolean) of Basic_Rule_Context;
   Access_Duplication_Used    : array (Boolean) of Boolean := (others => False);
   -- False: context/used for uncontrolled
   -- True : context/used for controlled

   -- Data for subrule Repeated:
   Repeated_Context : Basic_Rule_Context;

   -- Data for subrule Groupable
   subtype Percentage is Asis.ASIS_Natural range 0 .. 100;
   type Rule_Context is new Basic_Rule_Context with
      record
         Given   : Thick_Queries.Biggest_Natural;
         Missing : Thick_Queries.Biggest_Natural;
         Ratio   : Percentage;
         Total   : Thick_Queries.Biggest_Natural;
      end record;

   package Context_Queue is new Linear_Queue (Rule_Context);
   Groupable_Contexts : Context_Queue.Queue;

   type Coverage_Kind is (Full, Component);
   type LHS_Descriptor (Coverage : Coverage_Kind := Full) is
      record
         Loc : Location;
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


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Criteria_Utilities, Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control various issues in relation to assignments:");
      User_Message ("obvious array slidings, duplication of access value,");
      User_Message ("repeated assignments in a sequence to a same variable, or sequences of assignments");
      User_Message ("to components of a structured variable that could be replaced by an aggregate");
      User_Message;
      Help_On_Flags ("Parameter(1): [[not] controlled] ");
      User_Message ("For groupable:");
      User_Message ("Parameter(2..): <criterion> <value>");
      Help_On_Modifiers (Header => "<criterion>:");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Context_Queue, Criteria_Utilities, Subrules_Flag_Utilities, Framework.Language, Thick_Queries, Utilities;
      Given          : Biggest_Natural := 0;
      Missing        : Biggest_Natural := Biggest_Natural'Last;
      Ratio          : Percentage      := 0;
      Total          : Biggest_Natural := 0;
      Crit           : Criteria;
      Subrule        : Subrules;
      Has_Not        : Boolean;
      Has_Controlled : Boolean;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "subrule expected");
      end if;

      Has_Not        := Get_Modifier ("NOT");
      Has_Controlled := Get_Modifier ("CONTROLLED");
      Subrule        := Get_Flag_Parameter (Allow_Any => False);
      if Subrule /= Access_Duplication and (Has_Not or Has_Controlled) then
         Parameter_Error (Rule_Id, """[not] controlled"" modifier applies only to Access_Duplication");
      elsif Has_Not and not Has_Controlled then
         Parameter_Error (Rule_Id, "missing ""Controlled"" modifier for Access_Duplication");
      end if;
      case Subrule is
         when Sliding =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "No parameter for subrule ""sliding""");
            end if;

            if Rule_Used (Sliding) then
               Parameter_Error (Rule_Id, "subrule ""sliding"" already specified");
            end if;

            Sliding_Context := Basic.New_Context (Ctl_Kind, Ctl_Label);

         when Access_Duplication =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "No parameter for subrule ""access_duplication""");
            end if;

            if Rule_Used (Access_Duplication) and not Has_Controlled then
               Parameter_Error (Rule_Id, "subrule ""access_duplication"" already specified");
            elsif Access_Duplication_Used (not Has_Not) then
               Parameter_Error (Rule_Id, "subrule ""access_duplication"" already specified for"
                                         & Choose (Has_Not, "not ", "")
                                         & """controlled""");
            end if;

            if Has_Controlled then
               Access_Duplication_Context (not Has_Not) := Basic.New_Context (Ctl_Kind, Ctl_Label);
               Access_Duplication_Used    (not Has_Not) := True;
            else
               Access_Duplication_Context := (others => Basic.New_Context (Ctl_Kind, Ctl_Label));
               Access_Duplication_Used    := (others => True);
            end if;

         when Repeated =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "No parameter for subrule ""repeated""");
            end if;
            if Rule_Used (Repeated) then
               Parameter_Error (Rule_Id, "subrule ""repeated"" already specified");
            end if;

            Repeated_Context := Basic.New_Context (Ctl_Kind, Ctl_Label);

         when Groupable =>
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
      end case;
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
            Rule_Used := Not_Used;
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
      use Framework.Reports, Thick_Queries, Utilities;
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


   --------------------------------
   -- Process_Access_Duplication --
   --------------------------------

   procedure Process_Access_Duplication (Expr : in Asis.Expression) is
      use Thick_Queries, Utilities;
      use Asis, Asis.Iterator;
      type Duplication_State is (Possible, Certain);
      Ignored         : Traverse_Control := Continue;
      Controlled_Expr : Boolean          := Is_Controlled (Expr);

      function Contains_Access_Type (Def : Asis.Definition) return Boolean is
         -- Pre: Def is a type definition
         use  Asis.Definitions, Asis.Elements, Asis.Expressions;
         Good_Def : Asis.Definition := Def;
      begin
         if Is_Nil (Good_Def) then
            -- some predefined or universal stuff...
            return False;
         end if;

         -- The essential analysis is done by Contains_Type_Declaration_Kind, but it needs a type declaration
         -- Get rid of anonymous types, and other annoying cases, until we get a good type
         loop
            case Definition_Kind (Good_Def) is
               when A_Type_Definition =>
                  case Type_Kind (Good_Def) is
                     when An_Unconstrained_Array_Definition | A_Constrained_Array_Definition =>
                        Good_Def := Array_Component_Definition (Good_Def);
                     when others =>
                        return Contains_Type_Declaration_Kind (Enclosing_Element (Good_Def),
                                                               An_Ordinary_Type_Declaration,
                                                               An_Access_Type_Definition);
                  end case;
               when An_Access_Definition =>
                  return True;
               when A_Private_Type_Definition =>
                  return Contains_Type_Declaration_Kind (Enclosing_Element (Good_Def),
                                                         An_Ordinary_Type_Declaration,
                                                         An_Access_Type_Definition);
               when A_Component_Definition =>
                  return Contains_Type_Declaration_Kind (Corresponding_Name_Declaration
                                                         (Strip_Attributes
                                                          (Subtype_Simple_Name
                                                           (Component_Definition_View (Good_Def)))),
                                                         An_Ordinary_Type_Declaration,
                                                         An_Access_Type_Definition);
               when A_Formal_Type_Definition =>
                  return Contains_Type_Declaration_Kind (Enclosing_Element (Good_Def),
                                                         An_Ordinary_Type_Declaration,
                                                         An_Access_Type_Definition);
               when others =>
                  Failure ("Contains_Access_Type: bad definition", Good_Def);
            end case;
         end loop;
      end Contains_Access_Type;

      procedure Do_Report (Element : Asis.Element; State : Duplication_State; Controlled : Boolean) is
         use Reports;
      begin
         if not Access_Duplication_Used (Controlled) then
            return;
         end if;

         case State is
            when Possible =>
               Report (Rule_Id,
                       Access_Duplication_Context (Controlled),
                       Get_Location (Element),
                       "Possible duplication of " & Choose (Controlled, "", "un") & "controlled access value");
            when Certain =>
               Report (Rule_Id,
                       Access_Duplication_Context (Controlled),
                       Get_Location (Element),
                       "Duplication of " & Choose (Controlled, "", "un") & "controlled access value");
         end case;
      end Do_Report;

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

         use Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
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
                  Compo_Type : constant Asis.Declaration := Corresponding_Name_Declaration
                                                             (Subtype_Simple_Name
                                                              (Component_Definition_View
                                                               (Array_Component_Definition
                                                                (Thick_Queries.Corresponding_Expression_Type_Definition
                                                                 (Enclosing_Element (Enclosing_Element (Element)))))));
               begin
                  if        Contains_Type_Declaration_Kind (Compo_Type,
                                                            An_Ordinary_Type_Declaration,
                                                            A_Record_Type_Definition)
                    or else Contains_Type_Declaration_Kind (Compo_Type,
                                                            An_Ordinary_Type_Declaration,
                                                            A_Tagged_Record_Type_Definition)
                  then
                     Do_Report (Element, Possible, In_Controlled or else Is_Controlled (Compo_Type));
                  end if;
               end;
            when An_Integer_Literal
               | A_Real_Literal
               | A_String_Literal
               | An_Operator_Symbol
               | A_Character_Literal
               | An_Enumeration_Literal
               | A_Raise_Expression
               =>
               -- Final, nothing special
               null;

            when An_Identifier =>
               if  Contains_Access_Type (Thick_Queries.Corresponding_Expression_Type_Definition (Element)) then
                  Do_Report (Element, Certain, In_Controlled or else Is_Controlled (Element));
               end if;

            when An_Explicit_Dereference =>
               -- Since we never traverse prefixes of qualified names, this is a plain X.all
               Control := Abandon_Children;
               declare
                  Target_Type : constant Asis.Declaration := Access_Target_Type
                                                              (Thick_Queries.Corresponding_Expression_Type_Definition
                                                               (Prefix (Element)));
               begin
                  if Contains_Access_Type (Type_Declaration_View (Target_Type)) then
                     Do_Report (Element, Certain, Is_Controlled (Target_Type));
                  end if;
               end;

            when A_Function_Call =>
               Control := Abandon_Children;
               declare
                  Result_Type : constant Definition := Thick_Queries.Corresponding_Expression_Type_Definition (Element);
               begin
                  if Contains_Access_Type (Result_Type) then
                     Do_Report (Element, Possible, In_Controlled or else Is_Controlled (Result_Type));
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
                     Assocs  : constant Association_List := Record_Component_Associations (Element, Normalized => True);
                     Compo   : Asis.Expression;
                     Temp_CR : Boolean;
                  begin
                     for A in Assocs'Range loop
                        Compo := Component_Expression (Assocs (A));
                        if Is_Nil (Compo) then
                           -- Compo is nil for the normalized form of a box expression with no default
                           -- Do like A_Box_Expression above
                           -- The aggregate could have "others => <>" covering different types, but since we
                           -- use a normalized association, ASIS will sort this up for us. Retrieve the component's
                           -- type starting from the choices. We know there is only one choice per association.
                           declare
                              Compo_Type : constant Asis.Declaration := Corresponding_Name_Declaration
                                                                         (Subtype_Simple_Name
                                                                          (Component_Definition_View
                                                                           (Object_Declaration_View
                                                                            (Enclosing_Element
                                                                             (Record_Component_Choices (Assocs (A)) (1)
                                                                            )))));
                           begin
                              if        Contains_Type_Declaration_Kind (Compo_Type,
                                                                        An_Ordinary_Type_Declaration,
                                                                        A_Record_Type_Definition)
                                or else Contains_Type_Declaration_Kind (Compo_Type,
                                                                        An_Ordinary_Type_Declaration,
                                                                        A_Tagged_Record_Type_Definition)
                              then
                                 Do_Report (Element, Possible, In_Controlled or else Is_Controlled (Compo_Type));
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
                     Assocs           : constant Asis.Association_List := Array_Component_Associations (Aggr);
                  begin
                     for A in Assocs'Range loop
                        declare
                           Compo            : constant Asis.Expression := Component_Expression (Assocs (A));
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
                  Paths : constant Asis.Element_List := Expression_Paths (Element);
                  Controlled_Path : Boolean;
               begin
                  for P in Paths'Range loop
                     Controlled_Path := In_Controlled;
                     Traverse (Dependent_Expression (Paths (P)), Control, Controlled_Path);
                     if Control = Terminate_Immediately then
                        return;
                     end if;
                  end loop;
                  Control := Abandon_Children;
               end;
         end case;
      end Pre_Operation;

   begin   -- Process_Access_Duplication
      Traverse (Expr, Ignored, Controlled_Expr);
   end Process_Access_Duplication;

   ------------------------
   -- Process_Assignment --
   ------------------------

   procedure Process_Assignment (Statement : in Asis.Statement) is
      use Asis.Statements;
   begin   -- Process_Assignment
      if not Rule_Used (Sliding) and not Rule_Used (Access_Duplication) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (Sliding) then
         Process_Sliding (LHS => Assignment_Variable_Name (Statement),
                          RHS => Assignment_Expression (Statement));
      end if;

      if Rule_Used (Access_Duplication) then
         Process_Access_Duplication (Assignment_Expression (Statement));
      end if;
   end Process_Assignment;

   --------------------------------
   -- Process_Object_Declaration --
   --------------------------------

   procedure Process_Object_Declaration  (Declaration : in Asis.Declaration) is
      use Asis.Declarations, Asis.Elements;
   begin
      if not Rule_Used (Sliding) and not Rule_Used (Access_Duplication) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Init_Expr : constant Asis.Expression := Initialization_Expression (Declaration);
      begin
         if not Is_Nil (Init_Expr) then
            if Rule_Used (Sliding) then
               Process_Sliding (LHS => Names (Declaration) (1),  -- Even if there are several names
                                RHS => Init_Expr);
            end if;

            if Rule_Used (Access_Duplication) then
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
                        declare
                           Lengths : constant Extended_Biggest_Natural_List := Discrete_Constraining_Lengths (Struct);
                           Result  : Biggest_Natural := 1;
                        begin
                           for I in Lengths'Range loop
                              if Lengths (I) = Not_Static then
                                 return Not_Static;
                              end if;
                              Result := Result * Lengths (I);
                           end loop;
                           return Result;
                        end;
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
                     for I in Components'Range loop
                        if Definition_Kind (Components (I)) /= A_Null_Component then
                           Result := Result + Names (Components (I))'Length;
                        end if;
                     end loop;
                     if not Is_Nil (Discriminant_Part (Decl)) then
                        declare
                           Discrs : constant Asis.Discriminant_Specification_List
                             := Discriminants (Discriminant_Part (Decl));
                        begin
                           for I in Discrs'Range loop
                              Result := Result + Names (Discrs (I))'Length;
                           end loop;
                        end;
                     end if;
                     return Result;
                  end;

               when A_Protected_Definition =>
                  declare
                     Decl       : constant Asis.Declaration := Enclosing_Element (Def);
                     Components : constant Asis.Declarative_Item_List := Private_Part_Items (Def);
                     Result     : Biggest_Natural := 0;
                  begin
                     for I in Components'Range loop
                        if Declaration_Kind (Components (I)) = A_Component_Declaration then
                           Result := Result + Names (Components (I))'Length;
                        end if;
                     end loop;
                     if Declaration_Kind (Decl) = A_Protected_Type_Declaration  -- no discriminants for single PO
                       and then not Is_Nil (Discriminant_Part (Decl))
                     then
                        declare
                           Discrs : constant Asis.Discriminant_Specification_List
                             := Discriminants (Discriminant_Part (Decl));
                        begin
                           for I in Discrs'Range loop
                              Result := Result + Names (Discrs (I))'Length;
                           end loop;
                        end;
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
                                   Limits        : Rule_Context;
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
         use Framework.Reports, Utilities;
         Target       : Asis.Expression := LHS;
         Target_Descr : LHS_Descriptor;
         Parent       : Asis.Expression;
         Parent_Key   : Unbounded_Wide_String;
         Parent_Descr : LHS_Descriptor;
      begin
         loop
            case Expression_Kind (Target) is
               when A_Selected_Component =>
                  if Declaration_Kind (Corresponding_Name_Declaration (Selector (Target)))
                  in A_Discriminant_Specification .. A_Component_Declaration
                  then
                     Parent := Prefix (Target);
                     if Is_Access_Expression (Parent) then
                        -- Implicit dereference
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
                  if Is_Access_Expression (Parent) then
                     -- Implicit dereference
                     raise Dynamic_LHS;
                  end if;
                  Process_Assignment (Parent, Component, Key);
                  Parent_Key := Key;
                  declare
                     Indices : constant Asis.Expression_List := Index_Expressions (Target);
                  begin
                     Append (Key, '(');
                     for I in Indices'Range loop
                        declare
                           Value : constant Wide_String := Static_Expression_Value_Image (Indices (I));
                        begin
                           if Value = "" then
                              raise Dynamic_LHS;
                           end if;
                           Append (Key, Value & ',');
                        end;
                     end loop;
                     Replace_Element (Key, Length (Key), ')');
                  end;
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
                     Key := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Target)));
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
            if Rule_Used (Repeated)
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
            if Rule_Used (Repeated) then
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
         Context : Rule_Context;
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
                       & To_Title (To_Wide_String (Key))
                       & To_Wide_String (Reason));
            end if;

            Current := Next (Current);
         end loop;
      end Report_One;

      procedure Do_Report is new LHS_Map.Iterate (Report_One);

   begin   -- Process_Statement_Container
      if not Rule_Used (Groupable) and not Rule_Used (Repeated) then
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

         for I in Stmts'Range loop
            case Statement_Kind (Stmts (I)) is
               when An_Assignment_Statement =>
                  begin
                     Process_Assignment (LHS      => Assignment_Variable_Name (Stmts (I)),
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
