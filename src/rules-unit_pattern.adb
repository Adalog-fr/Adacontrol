----------------------------------------------------------------------
--  Rules.Unit_Pattern - Package body                               --
--                                                                  --
--  This software is (c) Alstom and Adalog 2004-2014.               --
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

-- ASIS
with
   Asis.Compilation_Units,
   Asis.Declarations,
   Asis.Definitions,
   Asis.Elements,
   Asis.Expressions;

-- Adalog
with
  Scope_Manager,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Ordering_Machine,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);
pragma Elaborate (Framework.Ordering_Machine);

package body Rules.Unit_Pattern is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- Easy for Single_Tagged_Type, Tagged_Type_Hierarchy.
   -- Context_Clauses_Order and Declarations_Order simply need the state machine provided by
   -- Framework.Ordering_Machine.
   --
   -- For Declarations_Order, the *_body declarations are further split according to the place where the corresponding
   -- spec is declared (public, private, own). In order to simplify syntax (for the user), there is a modifier to
   -- specify the place, but internally each *_body comes in three flavours. The enumerations without the place is
   -- used only for user syntax, the ones with the place are used internally (and not visible to the user). See the
   -- declaration of type Declarations_Group.


   type Subrules is (Single_Tagged_Type, Tagged_Type_Hierarchy, Context_Clauses_Order, Declarations_Order);
   type Subrules_Set is array (Subrules) of Boolean;
   No_Rules : constant Subrules_Set := (others => False);

   package Patterns_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules);
   use Patterns_Flags_Utilities;

   Rule_Used : Subrules_Set := No_Rules;
   Save_Used : Subrules_Set;
   Contexts  : Context_Store;

   ---- Declarations for Single_Tagged type
   --                    ******************

   package Declarations_Store is new Scope_Manager.Scoped_Store (Asis.Declaration,
                                                                 Equivalent_Keys => Asis.Elements.Is_Equal);


   ---- Declarations for Tagged_Type_Hierarchy
   --                    *********************

   -- (none)


   ---- Declarations for Context_Clauses_Order
   --                    *********************

   type Context_Clause_Element is (CC_With, CC_Use, CC_Use_Type, CC_Use_All_Type, CC_Pragma);
   package Context_Clause_Element_Utilities is new Framework.Language.Modifier_Utilities
                                                     (Modifiers => Context_Clause_Element,
                                                      Prefix    => "CC_");
   package Context_Clause_Ordering_Machine is new Framework.Ordering_Machine
     (Rule_Id, Context_Clause_Element, Context_Clause_Element_Utilities.Modifier_Set);
   Context_Clauses_Ordering : Context_Clause_Ordering_Machine.Instance;


   ---- Declarations for Declarations_Order
   --                    ******************
   use Framework.Language.Shared_Keys;
   type Declarations_Group is (DG_Use,                             DG_Use_Type,      DG_Use_All_Type,

                               DG_Number,                          DG_Constant,      DG_Variable,

                               DG_Private_Type,                    DG_Full_Type,     DG_Subtype,

                               DG_Subprogram_Spec,                 DG_Package_Spec,
                               DG_Generic_Subprogram_Spec,         DG_Generic_Package_Spec,
                               DG_Task_Spec,                       DG_Protected_Spec,

                               DG_Subprogram_Body,                 DG_Package_Body,          -- Body_Declarations_Group
                               DG_Generic_Subprogram_Body,         DG_Generic_Package_Body,
                               DG_Task_Body,                       DG_Protected_Body,

                               DG_Object_Renaming,                 DG_Subprogram_Renaming,
                               DG_Package_Renaming,                DG_Exception_Renaming,

                               DG_Subprogram_Instantiation,        DG_Package_Instantiation,

                               DG_Exception,

                               DG_Others,

                               DG_Own_Subprogram_Body,             DG_Own_Package_Body,
                               DG_Own_Generic_Subprogram_Body,     DG_Own_Generic_Package_Body,
                               DG_Own_Task_Body,                   DG_Own_Protected_Body,

                               DG_Private_Subprogram_Body,         DG_Private_Package_Body,
                               DG_Private_Generic_Subprogram_Body, DG_Private_Generic_Package_Body,
                               DG_Private_Task_Body,               DG_Private_Protected_Body,

                               DG_Public_Subprogram_Body,          DG_Public_Package_Body,
                               DG_Public_Generic_Subprogram_Body,  DG_Public_Generic_Package_Body,
                               DG_Public_Task_Body,                DG_Public_Protected_Body
                              );
   subtype User_Declarations_Group is Declarations_Group range Declarations_Group'First .. DG_Others;
   subtype Body_Declarations_Group is Declarations_Group range DG_Subprogram_Body       .. DG_Protected_Body;
   DG_Placed_Mapping : constant array (Body_Declarations_Group, Visibility_Places) of Declarations_Group
     := (DG_Subprogram_Body         => (S_Own     => DG_Own_Subprogram_Body,
                                        S_Private => DG_Private_Subprogram_Body,
                                        S_Public  => DG_Public_Subprogram_Body),
         DG_Package_Body            => (S_Own     => DG_Own_Package_Body,
                                        S_Private => DG_Private_Package_Body,
                                        S_Public  => DG_Public_Package_Body),
         DG_Generic_Subprogram_Body => (S_Own     => DG_Own_Generic_Subprogram_Body,
                                        S_Private => DG_Private_Generic_Subprogram_Body,
                                        S_Public  => DG_Public_Generic_Subprogram_Body),
         DG_Generic_Package_Body    => (S_Own     => DG_Own_Generic_Package_Body,
                                        S_Private => DG_Private_Generic_Package_Body,
                                        S_Public  => DG_Public_Generic_Package_Body),
         DG_Task_Body               => (S_Own     => DG_Own_Task_Body,
                                        S_Private => DG_Private_Task_Body,
                                        S_Public  => DG_Public_Task_Body),
         DG_Protected_Body          => (S_Own     => DG_Own_Protected_Body,
                                        S_Private => DG_Private_Protected_Body,
                                        S_Public  => DG_Public_Protected_Body));

   package Declarations_Group_Utilities is new Framework.Language.Modifier_Utilities
                                                  (Modifiers => Declarations_Group,
                                                   Prefix    => "DG_");

   package Declarations_Ordering_Machine is new Framework.Ordering_Machine (Rule_Id,
                                                                            Declarations_Group,
                                                                            Declarations_Group_Utilities.Modifier_Set);

   type Unit_Parts is (Package_Public, Package_Private, Package_Body, Subprogram);
   -- More expected later.
   package Parts_Flags_Utilities is new Framework.Language.Flag_Utilities (Unit_Parts);

   type Declaration_Ordering_Context is new Basic_Rule_Context with
      record
         Machine : Declarations_Ordering_Machine.Instance;
      end record;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls various usage patterns of units and entities in them");
      User_Message;
      Help_On_Flags ("Parameter(1):");
      User_Message;
      User_Message ("For Context_Clauses_Order:");
      Context_Clause_Element_Utilities.Help_On_Modifiers(Header => "parameter(2..): list of");
      User_Message;
      User_Message ("For Declarations_Order:");
      Parts_Flags_Utilities.Help_On_Flags ("parameter(2):");
      User_Message ("parameter(3..): ");
      Declarations_Group_Utilities.Help_On_Modifiers (Header   => "   list of: {<place>}",
                                                      Footer   => "(separated by '|')",
                                                      Expected => (User_Declarations_Group => True, others => False));
      Help_On_Scope_Places (Header   => "<place>:",
                            Expected => (Visibility_Places => True, others => False),
                            With_Not => False);
   end Help;


   ----------------------------
   -- Extended_Syntax_Getter --
   ----------------------------

   -- This is used only by Add_Control, but accessibility rules require it to be global
   procedure Extended_Syntax_Getter (Modifier : out Declarations_Group_Utilities.Modifier_Set;
                                     Found    : out Boolean;
                                     Expected : in  Declarations_Group_Utilities.Modifier_Set)
   is
      use Framework.Language, Scope_Places_Utilities, Declarations_Group_Utilities;
      Visib : Scope_Places_Utilities.Modifier_Set := Get_Modifier_Set (Expected => (Visibility_Places => True,
                                                                                    others            => False));
      DG    : Declarations_Group;
   begin
      Get_Modifier (DG, Found => Found, Expected => Expected);
      if not Found then
         return;
      end if;

      Modifier := Declarations_Group_Utilities.Empty_Set;

      if DG in Body_Declarations_Group then
         if Visib = Scope_Places_Utilities.Empty_Set then
            Visib := (Visibility_Places => True, others => False);
         end if;
         for V in Visibility_Places loop
            Modifier (DG_Placed_Mapping (DG, V)) := Visib (V);
         end loop;

      else
         if Visib /= Scope_Places_Utilities.Empty_Set then
            Parameter_Error (Rule_Id, "places can be specified only for ""body"" declarations");
         end if;
         Modifier (DG) := True;
      end if;
   end Extended_Syntax_Getter;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Parts_Flags_Utilities;

      Subrule : Subrules;
      Part    : Unit_Parts;

   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "This rule requires at least one parameter");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);
      case Subrule is
         when Single_Tagged_Type | Tagged_Type_Hierarchy =>
            Associate (Contexts,
                       Value (Subrules'Wide_Image (Subrule)),
                       Basic.New_Context (Ctl_Kind, Ctl_Label));

         when Context_Clauses_Order =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "At least one clause set must be specified");
            end if;

            declare
               use Context_Clause_Element_Utilities, Context_Clause_Ordering_Machine;
            begin
               loop
                  Add_State (Context_Clauses_Ordering, Get_Modifier_Set (No_Parameter => True));
                  exit when not Parameter_Exists;
               end loop;
            end;
            Associate (Contexts,
                       Value (Subrules'Wide_Image (Context_Clauses_Order)),
                       Basic.New_Context (Ctl_Kind, Ctl_Label));

         when Declarations_Order =>
            Part := Get_Flag_Parameter (Allow_Any => False);

            declare
               use Declarations_Group_Utilities, Declarations_Ordering_Machine;
               Machine       : Declarations_Ordering_Machine.Instance;
               State         : Modifier_Set;
               Not_Specified : Modifier_Set := Full_Set;
            begin
               loop
                  State  := Get_Modifier_Set (No_Parameter => True,
                                              Expected     => (User_Declarations_Group => True, others => False),
                                              Getter       => Extended_Syntax_Getter'Access);

                  if State (DG_Others) then
                     if (State and Modifier_Set'(DG_Others => False, others => True)) /= Empty_Set then
                        Parameter_Error (Rule_Id, """others"" must appear alone in parameter");
                     end if;
                     if Parameter_Exists then
                        Parameter_Error (Rule_Id, """others"" must appear as the last parameter");
                     end if;
                     if Not_Specified /= Empty_Set then
                        Add_State (Machine, Not_Specified);
                     end if;
                  else
                     Not_Specified := Not_Specified and not State;
                     Add_State (Machine, State);
                  end if;
                  exit when not Parameter_Exists;
               end loop;
               Associate (Contexts,
                          Value (Subrules'Wide_Image (Declarations_Order) & Unit_Parts'Wide_Image (Part)),
                          Declaration_Ordering_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Machine));
            end;
      end case;

      Rule_Used (Subrule) := True;
   exception
      when Already_In_Store =>
         Parameter_Error (Rule_Id, "Subrule already specified");
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager, Context_Clause_Ordering_Machine;
   begin
      case Action is
         when Clear =>
            Rule_Used := No_Rules;
            Reset (Context_Clauses_Ordering);
            Clear (Contexts);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rules;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Declarations_Store.Activate;
   end Prepare;


   ------------------------------
   -- Check_Single_Tagged_Type --
   ------------------------------

   procedure Check_Single_Tagged_Type (Declaration : in Asis.Declaration) is
      -- Precondition: Declaration is for a tagged type
      use Asis, Asis.Elements;
      use Framework.Locations, Framework.Reports, Scope_Manager;
   begin
      if not Rule_Used (Single_Tagged_Type) then
         return;
      end if;

      case Declaration_Kind (Current_Scope) is
         when A_Package_Declaration
            | A_Generic_Package_Declaration
            | A_Package_Body_Declaration
            =>
            Declarations_Store.Reset (Current_Scope_Only);
            if Declarations_Store.Data_Available then
               Report (Rule_Id,
                       Control_Manager.Association (Contexts, Value (Subrules'Wide_Image (Single_Tagged_Type))),
                       Get_Location (Declaration),
                       "Tagged type already declared at " & Image (Get_Location (Declarations_Store.Current_Data)));
            else
               Declarations_Store.Push (Declaration);
            end if;
         when others =>
            null;
      end case;
   end Check_Single_Tagged_Type;


   ---------------------------------
   -- Check_Tagged_Type_Hierarchy --
   ---------------------------------

   procedure Check_Tagged_Type_Hierarchy (Declaration : in Asis.Declaration) is
   -- Precondition: Declaration is for a (full) type extension
      use Asis, Asis.Compilation_Units, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Thick_Queries;
   begin
      if not Rule_Used (Tagged_Type_Hierarchy) then
         return;
      end if;

      declare
         This_Unit   : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Declaration);
         Parent_Unit : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Corresponding_Name_Declaration
                                                                                     (Subtype_Simple_Name
                                                                                      (Parent_Subtype_Indication
                                                                                       (Type_Declaration_View
                                                                                        (Declaration)))));
      begin
         if Unit_Kind (This_Unit) in A_Subunit   -- Certainly not an appropriate child
           or else not Is_Equal (Corresponding_Parent_Declaration (This_Unit), Parent_Unit)
         then
            Report (Rule_Id,
                    Control_Manager.Association (Contexts, Value (Subrules'Wide_Image (Tagged_Type_Hierarchy))),
                    Get_Location (Declaration),
                    "Type extension not in child unit of its parent's unit");
         end if;
      end;
   end Check_Tagged_Type_Hierarchy;


   ------------------------------
   -- Process_Type_Declaration --
   ------------------------------

   procedure Process_Type_Declaration (Declaration : in Asis.Declaration) is
      -- Precondition: Declaration_Kind (Declaration) = An_Ordinary_Type_Declaration
      use Asis, Asis.Declarations, Asis.Elements;
   begin   -- Process_Type_Declaration
      if Rule_Used = No_Rules then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Type_Kind (Type_Declaration_View (Declaration)) is
         when A_Tagged_Record_Type_Definition =>
            Check_Single_Tagged_Type(Declaration);
         when A_Derived_Record_Extension_Definition =>
            Check_Single_Tagged_Type    (Declaration);
            Check_Tagged_Type_Hierarchy (Declaration);
         when others =>
            null;
      end case;
   end Process_Type_Declaration;


   ----------------
   -- Enter_Unit --
   ----------------

   procedure Process_Compilation_Unit (Unit : in Asis.Compilation_Unit) is
      use Asis, Asis.Elements;
      use Framework.Locations, Framework.Reports, Utilities,
          Context_Clause_Ordering_Machine, Context_Clause_Element_Utilities;
   begin
      if not Rule_Used (Context_Clauses_Order) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Clause : Context_Clause_Element;
      begin
         Set_Initial (Context_Clauses_Ordering);
         for Cont_Elem : Asis.Context_Clause of Context_Clause_Elements (Unit, Include_Pragmas => True) loop
            case Element_Kind (Cont_Elem) is
               when A_Clause =>
                  case Clause_Kind (Cont_Elem) is
                     when A_Use_Package_Clause =>
                        Clause := CC_Use;
                     when A_Use_Type_Clause =>
                        Clause := CC_Use_Type;
                     when A_Use_All_Type_Clause =>
                        Clause := CC_Use_All_Type;
                     when A_With_Clause =>
                        Clause := CC_With;
                     when others =>
                        Failure ("Enter_Unit: unexpected clause", Cont_Elem);
                  end case;

               when A_Pragma =>
                  Clause := CC_Pragma;

               when others =>
                  Failure ("Enter_Unit: unexpected clause", Cont_Elem);
            end case;

            Set_State (Context_Clauses_Ordering, Clause);
            if not Is_Allowed (Context_Clauses_Ordering) then
               Report (Rule_Id,
                       Control_Manager.Association (Contexts, Value (Subrules'Wide_Image (Context_Clauses_Order))),
                       Get_Location (Cont_Elem),
                       "clause (or pragma) out of order"
                       & " (""" & Image (Clause, Title_Case)
                       & """ found in state " & Integer_Img (Current_State (Context_Clauses_Ordering)) & ')');
            end if;
         end loop;
      end;
   end Process_Compilation_Unit;


   --------------------------
   -- Process_Program_Unit --
   --------------------------

   procedure Process_Program_Unit (Unit : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Utilities;

      procedure Check (Part : Unit_Parts; Part_Declarations : Asis.Declaration_List) is
         use Framework.Locations, Framework.Reports, Declarations_Ordering_Machine, Declarations_Group_Utilities;
         use Thick_Queries;
         Context : Root_Context'Class := Control_Manager.Association (Contexts,
                                                                      Subrules'Wide_Image (Declarations_Order)
                                                                      & Unit_Parts'Wide_Image (Part));

         function Placed_State (Body_Decl  : Asis.Declaration;
                                Body_State : Body_Declarations_Group)
                                return Declarations_Group
         is
         -- Computes the real state, given a (user) body state and the current location
            Decl      : constant Asis.Declaration := Corresponding_Declaration (Body_Decl);
            Decl_Unit : Asis.Declaration;
         begin
            if Is_Nil (Decl) then
               -- the (subprogram) body has no explicit specification
               return DG_Placed_Mapping (Body_State, S_Own);
            end if;

            Decl_Unit := Enclosing_Element (Enclosing_Program_Unit (Decl));
            if  not Is_Equal (Decl_Unit, Corresponding_Declaration (Unit)) then
               return DG_Placed_Mapping (Body_State, S_Own);
            end if;

            if Is_Part_Of (Decl, Inside => Private_Part_Declarative_Items (Decl_Unit)) then
               return DG_Placed_Mapping (Body_State, S_Private);
            else
               return DG_Placed_Mapping (Body_State, S_Public);
            end if;
         end Placed_State;

      begin   -- Check
         if Context = No_Matching_Context then
            return;
         end if;

         declare
            Machine : Declarations_Ordering_Machine.Instance renames Declaration_Ordering_Context (Context).Machine;
            New_State : Declarations_Group;
         begin
            Set_Initial (Machine);
            for Decl : Asis.Declaration of Part_Declarations loop
               -- Actually, we may encounter pragmas and clauses as well as declarations...
               -- We use DG_Others to indicate an element not handled by the rule (and thus ignored)
               case Element_Kind (Decl) is
                  when A_Pragma =>
                     New_State := DG_Others;

                  when A_Clause =>
                     case Clause_Kind (Decl) is
                        when A_Use_Package_Clause =>
                           New_State := DG_Use;
                        when A_Use_Type_Clause =>
                           New_State := DG_Use_Type;
                        when A_Use_All_Type_Clause =>
                           New_State := DG_Use_All_Type;
                        when others =>
                           New_State := DG_Others;
                     end case;

                  when A_Declaration =>
                     case Declaration_Kind (Decl) is
                        when Not_A_Declaration =>
                           Failure ("Process_Program_Unit.Check: not_a_declaration", Decl);
                        when An_Enumeration_Literal_Specification   -- Things whose placement is not free
                           | A_Discriminant_Specification
                           | A_Component_Declaration
                           | A_Loop_Parameter_Specification
                           | A_Generalized_Iterator_Specification
                           | An_Element_Iterator_Specification
                           | A_Parameter_Specification
                           | A_Return_Variable_Specification
                           | A_Return_Constant_Specification
                           | A_Choice_Parameter_Specification
                           | An_Entry_Declaration
                           | An_Entry_Index_Specification
                           | A_Formal_Declaration
                           =>
                           New_State := DG_Others;

                        when A_Number_Declaration =>               -- Objects
                           New_State := DG_Number;
                        when A_Constant_Declaration
                           | A_Deferred_Constant_Declaration
                           =>
                           New_State := DG_Constant;
                        when A_Variable_Declaration =>
                           New_State := DG_Variable;

                        when A_Private_Type_Declaration           -- (sub)types
                           | A_Private_Extension_Declaration
                           =>
                           New_State := DG_Private_Type;
                        when An_Ordinary_Type_Declaration
                           | An_Incomplete_Type_Declaration
                           | A_Tagged_Incomplete_Type_Declaration
                           =>
                           New_State := DG_Full_Type;
                        when A_Subtype_Declaration =>
                           New_State := DG_Subtype;

                        when A_Procedure_Declaration              -- Specifications
                           | A_Function_Declaration
                           =>
                           New_State := DG_Subprogram_Spec;
                        when A_Package_Declaration =>
                           New_State := DG_Package_Spec;
                        when A_Generic_Package_Declaration =>
                           New_State := DG_Generic_Package_Spec;
                        when A_Generic_Procedure_Declaration
                           | A_Generic_Function_Declaration
                           =>
                           New_State := DG_Generic_Subprogram_Spec;
                        when A_Task_Type_Declaration
                           | A_Single_Task_Declaration
                           =>
                           New_State := DG_Task_Spec;
                        when A_Protected_Type_Declaration
                           | A_Single_Protected_Declaration
                           =>
                           New_State := DG_Protected_Spec;

                        when A_Null_Procedure_Declaration         -- Null procedure and expression function
                           | An_Expression_Function_Declaration
                           =>
                           if Is_Nil (Corresponding_Declaration (Decl)) then
                              New_State := DG_Subprogram_Spec;
                           else
                              New_State := Placed_State (Decl, DG_Subprogram_Body);
                           end if;

                        when A_Procedure_Body_Declaration         -- Bodies
                           | A_Procedure_Body_Stub
                           | A_Function_Body_Declaration
                           | A_Function_Body_Stub
                           | An_Entry_Body_Declaration
                           =>
                           if Is_Generic_Unit (Decl) then
                              New_State := Placed_State (Decl, DG_Generic_Subprogram_Body);
                           else
                              New_State := Placed_State (Decl, DG_Subprogram_Body);
                           end if;

                        when A_Package_Body_Declaration
                           | A_Package_Body_Stub
                           =>
                           if Is_Generic_Unit (Decl) then
                              New_State := Placed_State (Decl, DG_Generic_Package_Body);
                           else
                              New_State := Placed_State (Decl, DG_Package_Body);
                           end if;
                        when A_Task_Body_Declaration
                           | A_Task_Body_Stub
                           =>
                           New_State := Placed_State (Decl, DG_Task_Body);
                        when A_Protected_Body_Declaration
                           | A_Protected_Body_Stub
                           =>
                           New_State := Placed_State (Decl, DG_Protected_Body);

                        when An_Object_Renaming_Declaration =>    -- Renamings
                           New_State := DG_Object_Renaming;
                        when A_Procedure_Renaming_Declaration
                           | A_Function_Renaming_Declaration
                           | A_Generic_Procedure_Renaming_Declaration
                           | A_Generic_Function_Renaming_Declaration
                           =>
                           New_State := DG_Subprogram_Renaming;
                        when A_Package_Renaming_Declaration
                           | A_Generic_Package_Renaming_Declaration
                           =>
                           New_State := DG_Package_Renaming;
                        when An_Exception_Renaming_Declaration =>
                           New_State := DG_Exception_Renaming;

                        when A_Procedure_Instantiation            --  Instantiations
                           | A_Function_Instantiation
                           =>
                           New_State := DG_Subprogram_Instantiation;
                        when A_Package_Instantiation =>
                           New_State := DG_Package_Instantiation;

                        when An_Exception_Declaration =>          -- Exceptions
                           New_State := DG_Exception;
                     end case;

                  when others =>
                     Failure ("Process_Program_Unit.Check: unexpected element", Decl);
               end case;

               if New_State /= DG_Others then
                  Set_State (Machine, New_State);
                  if not Is_Allowed (Machine) then
                     Report (Rule_Id,
                             Context,
                             Get_Location (Decl),
                             "Declaration out of order for " & Set_Casing (Unit_Parts'Wide_Image (Part), Title_Case)
                             & " (""" & Image (New_State, Title_Case)
                             & """ found in state " & Integer_Img (Current_State (Machine)) & ')');
                  end if;
               end if;
            end loop;
         end;
      end Check;

   begin   -- Process_Program_Unit
      if not Rule_Used (Declarations_Order) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Unit) is
         when A_Package_Declaration
            | A_Generic_Package_Declaration
            =>
            Check (Package_Public,  Visible_Part_Declarative_Items (Unit, Include_Pragmas => True));
            Check (Package_Private, Private_Part_Declarative_Items (Unit, Include_Pragmas => True));
         when A_Function_Body_Declaration
            | A_Procedure_Body_Declaration
            | An_Entry_Body_Declaration
            =>
            Check (Subprogram, Body_Declarative_Items (Unit, Include_Pragmas => True));
         when A_Null_Procedure_Declaration =>
            null;    -- Nothing to check here
         when A_Package_Body_Declaration =>
            Check (Package_Body, Body_Declarative_Items (Unit, Include_Pragmas => True));
         when others =>
            Failure ("Process_Program_Unit: unexpected unit", Unit);
      end case;
   end Process_Program_Unit;

begin  -- Rules.Unit_Pattern
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Unit_Pattern;
