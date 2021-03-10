----------------------------------------------------------------------
--  Rules.Object_Declarations - Package body                        --
--                                                                  --
--  This software is (c) CSEE and Adalog 2004-2007.                 --
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
  Asis.Clauses,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Queries,
  Framework.Symbol_Table;
pragma Elaborate (Framework.Language);

-- Child unit
with
  Rules.Object_Declarations.NRT_Utilities;
package body Rules.Object_Declarations is
   use Framework, Framework.Control_Manager;

   -- Algorithm
   -- Note: NRT is used in identifiers as a shorthand for Not_Required_Type
   --
   -- subrules Type and Min_Integer_Span are fairly simple.
   --
   --
   -- for subrule Not_Required_Type, the difficulty comes when a variable is used to compute the value of another
   -- variable : whether the first variable is required depends on whether the second variable is required.
   -- And if the second variable is from an outer scope, it may be discovered that the first variable is required
   -- after leaving its own scope (see tests).
   -- Another tricky issue is that there may be circularities in dependencies, f.e. if V1 is used to compute V2, then
   -- later V2 is used to compute V1.
   --
   -- A suspect variable is a variable whose type is given to the subrule, until it becomes required.
   -- A variable is known to be required when it is used as an indexing expression, or within a subexpression (connected
   -- with predefined operators) and involving 'First and 'Last attributes whose prefix is of an array type.
   -- This can happen only while within the scope of the variable.
   -- When a suspect variable is used within the RHS of an assignment, if the LHS is known to be required, the
   -- variable becomes required; otherwise, it becomes dependent on the LHS variable.
   -- When a variable becomes required, all variables that depend on it become required (recursively). Note that the
   -- dependent variables may be out of scope at that point!
   --
   -- When exiting the declaration scope of a variable, the variable is reported if it is not required and
   -- it does not depend on any variable, or all variables it depends on are from the exited scope.
   -- For all variables that depend on this variable, the variable is removed
   -- from the set of variables the dependent variable depends on. If the dependent variable does not depend on
   -- any other variable, then the other variable is reported.
   --
   --
   -- Because this algorithm depends on scopes and also has to keep information indepently of scoping, two data
   -- structures are necessary. All management of these data structures is provided in the child package Nrt_Utilities.
   -- This way, this package is only in charge of making high level decisions (when does a variable become dependent...)
   -- See package Nrt_Utilities for the management of the data structures.
   --
   --
   --
   -- for subrule Volatile_No_Address, we avoid relying on Corresponding_Representation_Clauses
   -- and Corresponding_Pragmas, because they are not properly defined (since they take a
   -- declaration as parameter, it is not clear what happens if a declaration has several
   -- names, and the pragma or representation clause applies only to part of them).
   --
   -- We therefore use a Scoped_Store where every declared variable is added, and updated
   -- whenever we find a pragma volatile or address clause. At scope exit, we just need to
   -- browse the current level. Of course, it works because representation items must be
   -- declared in the same scope as the variable.

   type Subrules is (S_Type, S_Not_Required_Type, S_Min_Integer_Span, S_Volatile_No_Address, S_Address_Not_Volatile);
   subtype Vol_Addr_Rules is Subrules range S_Volatile_No_Address .. S_Address_Not_Volatile;
   subtype Type_Rules     is Subrules range S_Type .. S_Not_Required_Type;
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "S_");

   type Subrule_Set is array (Subrules) of Boolean;
   No_Rule : constant Subrule_Set := (others => False);
   Rule_Used : Subrule_Set := No_Rule;
   Save_Used : Subrule_Set;

   type Object_Kinds is (K_All, K_Variable, K_Constant);
   package Object_Kinds_Utilities is new Framework.Language.Modifier_Utilities (Object_Kinds, "K_");

   --------------------------------------------------
   -- Data for subrules Type and Not_Required_Type

   type Obj_Selector is (Var, Const);
   Type_Contexts : array (Type_Rules, Obj_Selector) of Context_Store;


   --------------------------------------------------
   -- Data for subrule Min_Integer_Span

   type Value_Context is new Basic_Rule_Context with
      record
         Min_Values : Thick_Queries.Biggest_Natural := 0;
      end record;
   Span_Context : array (Object_Kinds, Control_Kinds) of Value_Context;


   --------------------------------------------------
   -- Data for subrule Volatile_No_Address and Address_Not_Volatile:

   type Repr_Rec is
      record
         Volatile : Boolean;
         Address  : Boolean;
      end record;
   package Repr_Table is new Framework.Symbol_Table.Data_Access (Repr_Rec);

   Vno_Context  : array (Vol_Addr_Rules) of Basic_Rule_Context;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Subrules_Flag_Utilities, Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control allowed forms of object declarations");
      User_Message;
      Help_On_Flags ("Parameter(1):");
      User_Message;
      User_Message ("for type and not_required_type:");
      User_Message ("Parameter(2..): [constant|variable] <entity>");
      User_Message;
      User_Message("for Min_Integer_Span:");
      User_Message ("Parameter(2..): [constant|variable] <value>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Subrules_Flag_Utilities, Object_Kinds_Utilities, Thick_Queries, Framework.Language;
      Subrule : Subrules;
      Ok      : Object_Kinds;
      Vc      : Value_Context;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "missing subrule name");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);

      case Subrule is
         when S_Type | S_Not_Required_Type =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "missing type entity specification");
            end if;

            declare
               Entity  : Entity_Specification;
            begin
               loop
                  Ok := Get_Modifier (Required => False,
                                      Expected => (K_All => False, others => True),
                                      Default  => K_All);
                  Entity := Get_Entity_Parameter;
                  case Ok is
                     when K_All =>
                        Associate (Into          => Type_Contexts (Subrule, Var),
                                   Specification => Entity,
                                   Context       => Basic.New_Context (Ctl_Kind, Ctl_Label));
                        Associate (Into          => Type_Contexts (Subrule, Const),
                                   Specification => Entity,
                                   Context       => Basic.New_Context (Ctl_Kind, Ctl_Label));
                     when K_Variable =>
                        Associate (Into          => Type_Contexts (Subrule, Var),
                                   Specification => Entity,
                                   Context       => Basic.New_Context (Ctl_Kind, Ctl_Label));
                     when K_Constant =>
                        Associate (Into          => Type_Contexts (Subrule, Const),
                                   Specification => Entity,
                                   Context       => Basic.New_Context (Ctl_Kind, Ctl_Label));
                  end case;

                  exit when not Parameter_Exists;
               end loop;
            exception
               when Already_In_Store =>
                  Parameter_Error (Rule_Id, "entity already given: " & Image (Entity));
            end;

         when S_Min_Integer_Span =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "missing number of allowed values");
            end if;
            loop
               Ok := Get_Modifier (Required => False,
                                   Expected => (K_All => False, others => True),
                                   Default  => K_All);
               Vc := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Get_Integer_Parameter (Min => 1));
               if Ok in K_All | K_Constant then
                  if Span_Context (K_Constant, Ctl_Kind).Min_Values /= 0 then
                     Parameter_Error (Rule_Id, "subrule already given for constants");
                  end if;
                  Span_Context (K_Constant, Ctl_Kind) := Vc;
               end if;
               if Ok in K_All | K_Variable then
                  if Span_Context (K_Variable, Ctl_Kind).Min_Values /= 0 then
                     Parameter_Error (Rule_Id, "subrule already given for variables");
                  end if;
                  Span_Context (K_Variable, Ctl_Kind) := Vc;
               end if;
               exit when not Parameter_Exists;
            end loop;
         when S_Volatile_No_Address
            | S_Address_Not_Volatile
            =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "subrule has no parameters");
            end if;

            if Rule_Used (Subrule) then
               Parameter_Error (Rule_Id, "subrule already given");
            end if;

            Vno_Context (Subrule) := Basic.New_Context (Ctl_Kind, Ctl_Label);
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
            Rule_Used := No_Rule;

            for Cont : Value_Context of Span_Context loop
               Cont.Min_Values := 0;
            end loop;

            for Store : Context_Store of Type_Contexts  loop
               Clear (Store);
            end loop;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Decl : in Asis.Declaration) is
      use Thick_Queries;
      use Asis, Asis.Declarations, Asis.Elements;

      procedure Process_Type (Rule : Type_Rules) is
         use Asis.Definitions;
         use Framework.Locations, Framework.Reports, NRT_Utilities, Utilities;
         Temp : Asis.Element;
      begin   -- Process_Type
         case Declaration_Kind (Decl) is
            when A_Constant_Declaration =>
               if Definition_Kind (Object_Declaration_View (Decl)) /= A_Subtype_Indication then
                  -- anonymous array, task, protected
                  return;
               end if;

               declare
                  Cont : constant Root_Context'Class := Subtype_Or_Type_Context (Type_Contexts (Rule, Const),
                                                                                 Subtype_Simple_Name
                                                                                   (Object_Declaration_View (Decl)));
               begin
                  case Rule is
                     when S_Type =>
                        Report (Rule_Id,
                                Cont,
                                Get_Location (Decl),
                                "Constant declaration of type " & Last_Matching_Name (Type_Contexts (Rule, Const)));
                     when S_Not_Required_Type =>
                        if Cont /= No_Matching_Context then
                           for Name : Asis.Name of Names (Decl) loop
                              Make_Suspect (Name,
                                            Basic_Rule_Context (Cont),
                                            Last_Matching_Name (Type_Contexts (Rule, Const)));
                           end loop;
                        end if;
                  end case;
               end;
            when A_Variable_Declaration =>
               if Definition_Kind (Object_Declaration_View (Decl)) /= A_Subtype_Indication then
                  -- anonymous array, task, protected
                  return;
               end if;

               declare
                  Cont : constant Root_Context'Class := Subtype_Or_Type_Context (Type_Contexts (Rule, Var),
                                                                                 Subtype_Simple_Name
                                                                                   (Object_Declaration_View (Decl)));
               begin
                  case Rule is
                     when S_Type =>
                        Report (Rule_Id,
                                Cont,
                                Get_Location (Decl),
                                "Variable declaration of type " & Last_Matching_Name (Type_Contexts (Rule, Var)));
                     when S_Not_Required_Type =>
                        if Cont /= No_Matching_Context then
                           for Name : Asis.Name of Names (Decl) loop
                              Make_Suspect (Name,
                                            Basic_Rule_Context (Cont),
                                            Last_Matching_Name (Type_Contexts (Rule, Var)));
                           end loop;
                        end if;
                  end case;
               end;
            when A_Parameter_Specification =>
               -- only for Not_Required_Type
               Temp := Object_Declaration_View (Decl);
               if Definition_Kind (Temp) = An_Access_Definition then -- anonymous access type
                  return;
               end if;

               declare
                  Cont : constant Root_Context'Class := Subtype_Or_Type_Context (Type_Contexts (Rule, Var),Temp);
               begin
                  if Cont /= No_Matching_Context then
                     for Name : Asis.Name of Names (Decl) loop
                        Make_Suspect (Name, Basic_Rule_Context (Cont), Last_Matching_Name(Type_Contexts (Rule, Var)));
                     end loop;
                  end if;
               end;
            when A_Loop_Parameter_Specification =>
               Temp := Range_Ultimate_Name (Specification_Subtype_Definition (Decl));
               if Is_Nil (Temp) then -- Implicit Integer
                  Temp := Names (Framework.Queries.Standard_Value ("INTEGER"))(1);
               end if;
               declare
                  Cont : constant Root_Context'Class := Subtype_Or_Type_Context (Type_Contexts (Rule, Var), Temp);
                  Name : Asis.Expression;
               begin
                  if Cont /= No_Matching_Context then
                     Name := Names (Decl) (1);
                     case Rule is
                        when S_Type =>
                           Report (Rule_Id,
                                   Cont,
                                   Get_Location (Decl),
                                   "Loop parameter declaration of type "
                                   & Last_Matching_Name (Type_Contexts (Rule, Var)));
                        when S_Not_Required_Type =>
                           Make_Suspect (Name,
                                         Basic_Rule_Context (Cont),
                                         Last_Matching_Name (Type_Contexts (Rule, Var)));
                     end case;
                  end if;
               end;

            when An_Element_Iterator_Specification =>
               Temp := Thick_Queries.Corresponding_Expression_Type_Definition (Iteration_Scheme_Name (Decl));
               -- Temp is either an array object or an object of an iterable type
               if Is_Array_Subtype (Temp) then
                  Temp := Subtype_Simple_Name (Component_Definition_View (Array_Component_Definition (Temp)));
               else
                  -- Generalized element iterators
                  Temp := A4G_Bugs.Corresponding_Expression_Type (Iteration_Scheme_Name (Decl));
                  Temp := Simple_Name (Aspect_Definition (Corresponding_Aspects (Temp, "ITERATOR_ELEMENT") (1)));
               end if;
               declare
                  Cont : constant Root_Context'Class := Subtype_Or_Type_Context (Type_Contexts (Rule, Var), Temp);
                  Name : Asis.Expression;
               begin
                  if Cont /= No_Matching_Context then
                     Name := Names (Decl) (1);
                     case Rule is
                        when S_Type =>
                           Report (Rule_Id,
                                   Cont,
                                   Get_Location (Decl),
                                   "Loop parameter declaration of type "
                                   & Last_Matching_Name (Type_Contexts (Rule, Var)));
                        when S_Not_Required_Type =>
                           Make_Suspect (Name,
                                         Basic_Rule_Context (Cont),
                                         Last_Matching_Name (Type_Contexts (Rule, Var)));
                     end case;
                  end if;
               end;
            when others =>
               Failure ("Object_Declarations: Unexpected declaration", Decl);
         end case;
      end Process_Type;

      procedure Process_Min_Integer_Span is
         use Framework.Locations, Framework.Reports;

         Val      : Extended_Biggest_Natural;
         Type_Decl : Asis.Declaration;
         Obj_Kind  : Object_Kinds;

         function Decl_Type_Declaration return Asis.Declaration is
         -- returns the declaration of the type of Decl,
         -- nil_element for anonymous type declarations
            use Asis.Expressions;
            use Utilities;
            Def      : Asis.Definition;
            St_Name  : Asis.Expression;
         begin
            Def := Object_Declaration_View (Decl);
            if Definition_Kind (Def) /= A_Subtype_Indication then
               -- anonymous array, task, protected
               return Nil_Element;
            end if;
            St_Name := Subtype_Simple_Name (Def);
            if Expression_Kind (St_Name) = An_Attribute_Reference then
               case Attribute_Kind (St_Name) is
                  when A_Base_Attribute =>
                     -- for our purpose, the prefix will do as well
                     St_Name := Simple_Name (Prefix (St_Name));
                  when A_Class_Attribute =>
                     -- Certainly not an integer type...
                     -- For volatile, we have no way of retrieving Corresponding_Pragmas
                     --   => give up
                     return Nil_Element;
                  when others =>
                     Failure ("Bad attribute", St_Name);
               end case;
            end if;
            return Corresponding_Name_Declaration (St_Name);
         end Decl_Type_Declaration;

      begin  -- Process_Min_Integer_Span
         -- Check we have an object of an integer type
         Type_Decl := Decl_Type_Declaration;
         if Is_Nil (Type_Decl) then
            return;
         end if;
         if Type_Kind (Type_Declaration_View (Type_Decl)) not in Integer_Type_Kinds then
            return;
         end if;

         if Declaration_Kind (Decl) = A_Constant_Declaration then
            Obj_Kind := K_Constant;
         else
            Obj_Kind := K_Variable;
         end if;

         -- Check values

         declare
            Lengths : constant Extended_Biggest_Natural_List := Discrete_Constraining_Lengths (Decl);
         begin
            if Lengths'Length = 0 then
               -- The type is a 'base f.e. => treat like dynamic
               return;
            end if;
            Val := Lengths (1);
         end;

         if Val = Not_Static then
            return;
         end if;

         -- Note: Unspecified values of Range/Obj_Kind/Control contain 0, and Val is >= 0
         --       No problem in the following tests
         if Val < Span_Context (Obj_Kind, Check).Min_Values  then
            Report (Rule_Id,
                    Span_Context (Obj_Kind, Check),
                    Get_Location (Decl),
                    "integer object declaration has too few values ("
                    & Biggest_Int_Img (Val)
                    & ')');
         elsif Val < Span_Context (Obj_Kind, Search).Min_Values  then
            Report (Rule_Id,
                    Span_Context (Obj_Kind, Search),
                    Get_Location (Decl),
                    "integer object declaration has too few values ("
                    & Biggest_Int_Img (Val)
                    & ')');
         end if;

         if Val < Span_Context (Obj_Kind, Count).Min_Values  then
            Report (Rule_Id,
                    Span_Context (Obj_Kind, Count),
                    Get_Location (Decl),
                    "");
         end if;
      end Process_Min_Integer_Span;

      procedure Process_Volatile_Address is
      begin
         for N : Asis.Defining_Name of Names (Decl) loop
            Repr_Table.Store (N,
                              (Volatile => Corresponding_Pragma_Set (N) (A_Volatile_Pragma),
                               Address  => False));
         end loop;
      end Process_Volatile_Address;

   begin  -- Process_Declaration
      if Rule_Used = No_Rule then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (S_Not_Required_Type) then
         Process_Type (S_Not_Required_Type);
      end if;

      if Declaration_Kind (Decl) in
            A_Parameter_Specification
          | A_Loop_Parameter_Specification
          | An_Element_Iterator_Specification
      then
         -- Only for Not_Required_Type
         return;
      end if;

      if Rule_Used (S_Type) then
         Process_Type (S_Type);
      end if;

      if Rule_Used (S_Min_Integer_Span) then
         Process_Min_Integer_Span;
      end if;

      if (Rule_Used (S_Volatile_No_Address) or Rule_Used (S_Address_Not_Volatile))
        and then Declaration_Kind (Decl) = A_Variable_Declaration
      then
         Process_Volatile_Address;
      end if;
   end Process_Declaration;

   --------------------
   -- Process_Pragma --
   --------------------

   procedure Process_Pragma (Prgma : in Asis.Pragma_Element) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Framework.Locations, Framework.Reports;

      Name : Asis.Expression;
   begin
      if not (Rule_Used (S_Volatile_No_Address) or Rule_Used (S_Address_Not_Volatile)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Pragma_Kind (Prgma) /= A_Volatile_Pragma then
         return;
      end if;

      Name := Actual_Parameter (Pragma_Argument_Associations (Prgma) (1));
      if Attribute_Kind (Name) = A_Class_Attribute then  -- Excludes case when name is not an attribute
         if Rule_Used (S_Volatile_No_Address) then
            Uncheckable (Rule_Id,
                         False_Negative,
                         Get_Location (Name),
                         "pragma ignored for types covered by " & Name_Image (Simple_Name (Prefix (Name)))
                         & " in subrule Volatile_No_Address");
         end if;
         if Rule_Used (S_Address_Not_Volatile) then
            Uncheckable (Rule_Id,
                         False_Positive,
                         Get_Location (Name),
                         "pragma ignored for types covered by " & Name_Image (Simple_Name (Prefix (Name)))
                         & " in subrule Address_Not_Volatile");
         end if;
         return;
      end if;
   end Process_Pragma;

   -----------------------------------
   -- Process_Representation_Clause --
   -----------------------------------

   procedure Process_Representation_Clause (Clause : in Asis.Representation_Clause) is
      use Asis, Asis.Clauses, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      Name      : Asis.Expression;
      Repr_Data : Repr_Rec;
   begin
      if not (Rule_Used (S_Volatile_No_Address) or Rule_Used (S_Address_Not_Volatile)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Representation_Clause_Kind (Clause) is
         when An_Attribute_Definition_Clause =>
            Name := Representation_Clause_Name (Clause);
            if Attribute_Kind (Name) /= An_Address_Attribute then
               return;
            end if;
            Name := Simple_Name (Prefix (Name));
         when An_At_Clause =>
            Name := Representation_Clause_Name (Clause);
         when others =>
            return;
      end case;
      if Declaration_Kind (Corresponding_Name_Declaration (Name)) /= A_Variable_Declaration then
         return;
      end if;

      -- Here, we have an address repr_clause on a variable
      Repr_Data := Repr_Table.Fetch (Name);
      Repr_Data.Address := True;
      Repr_Table.Store (Name, Repr_Data);
   end Process_Representation_Clause;


   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Ident  : in Asis.Name) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use NRT_Utilities, Thick_Queries;

      Previous   : Asis.Element := Ident;
      Current    : Asis.Element;
   begin
      if not Rule_Used (S_Not_Required_Type) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if not Is_Suspect (Ident) then
         return;
      end if;

      -- Find out how this identifier is used
      Current := Enclosing_Element (Ident);
      loop
         case Element_Kind (Current) is
            when A_Declaration =>
               case Declaration_Kind (Current) is
                  when A_Variable_Declaration | A_Constant_Declaration =>
                     if not Is_Equal (Previous, Initialization_Expression (Current)) then
                        return;
                     end if;
                     for Name : Asis.Name of Names (Current) loop
                        Make_Dependent (Ident, On => Name);
                     end loop;
                     return;
                  when A_Loop_Parameter_Specification =>
                     Make_Dependent (Ident, On => Names (Current)(1));
                  when others =>
                     return;
               end case;

            when A_Definition =>
               case Definition_Kind (Current) is
                  when A_Discrete_Range | A_Discrete_Subtype_Definition =>
                     -- maybe used to slice an array, definition of a for loop...
                     null;
                  when others =>
                     return;
               end case;

            when An_Expression =>
               case Expression_Kind (Current) is
                  when A_Parenthesized_Expression | A_Qualified_Expression =>
                     null;
                  when An_Indexed_Component =>
                     for Inx : Asis.Expression of Index_Expressions (Current) loop
                        if Is_Equal (Inx, Previous) then
                           Make_Required (Ident);
                        end if;
                     end loop;
                     return;
                  when A_Slice =>
                     if Is_Equal (Slice_Range (Current), Previous) then
                        Make_Required (Ident);
                     end if;
                     return;
                  when A_Function_Call =>
                     declare
                        Actuals  : constant Expression_List := Actual_Parameters (Current);
                        Called_F : Asis.Declaration;
                     begin
                        if Corresponding_Call_Description (Current).Kind = A_Predefined_Entity_Call then
                           if Expression_Kind (Prefix (Current)) = An_Operator_Symbol and Actuals'Length = 2 then
                              -- A binary predefined operator => check the other operand
                              if        (Is_Equal (Previous, Actuals (1))
                                         and then Is_Requiring (Actuals (2),
                                                                A4G_Bugs.Corresponding_Expression_Type (Ident)))
                                or else (Is_Equal (Previous, Actuals (2))
                                         and then Is_Requiring (Actuals (1),
                                                                A4G_Bugs.Corresponding_Expression_Type (Ident)))
                              then
                                 Make_Required (Ident);
                                 return;
                              end if;
                           end if;
                        else
                           for Inx in Actuals'Range loop
                              if Is_Equal (Previous, Actuals (Inx)) then
                                 Called_F := Corresponding_Called_Function (Current);
                                 -- Called_F is Nil_Element for a dynamic (access or dispatching) call
                                 -- We can't make it dependent on anything, since the formal name is unknow
                                 -- (and depends on the target).
                                 if Is_Nil (Called_F) then
                                    return;
                                 end if;
                                 if Ultimate_Origin (Called_F) = An_Application_Unit then
                                    Make_Dependent (Ident, On => Formal_Name (Current, Inx));
                                 else
                                    -- Language defined, implementation defined...
                                    Make_Required (Ident);
                                 end if;
                                 return;
                              end if;
                           end loop;
                        Utilities.Failure ("Object_Declarations: function actual not found", Current);
                        end if;
                     end;
                  when others =>
                     return;
               end case;

            when A_Statement =>
               case Statement_Kind (Current) is
                  when An_Assignment_Statement =>
                     if Is_Equal (Assignment_Variable_Name (Current), Ident) then
                        -- Ident is the LHS variable
                        if Is_Requiring (Assignment_Expression (Current),
                                         A4G_Bugs.Corresponding_Expression_Type (Ident))
                        then
                           Make_Required (Ident);
                        end if;
                        return;
                     else
                        -- Ident is within the RHS
                        Make_Dependent (Ident, On => Assignment_Variable_Name (Current));
                        return;
                     end if;
                  when A_Procedure_Call_Statement | An_Entry_Call_Statement =>
                     -- find where we are in the call...
                     declare
                        Called  : constant Asis.Declaration := Corresponding_Called_Entity (Current);
                        Actuals : constant Expression_List  := Actual_Parameters (Current);
                     begin
                        -- Called is Nil_Element for a dynamic (access or dispatching) call
                        -- We can't make it dependent on anything, since the formal name is unknow
                        -- (and depends on the target).
                        if Is_Nil (Called) then
                           return;
                        end if;

                        for Inx in Actuals'Range loop
                           if Is_Equal (Previous, Actuals (Inx)) then
                              if Ultimate_Origin (Called) = An_Application_Unit then
                                 Make_Dependent (Ident, On => Formal_Name (Current, Inx));
                              else
                                 -- Language defined, implementation defined...
                                 Make_Required (Ident);
                              end if;
                              return;
                           end if;
                        end loop;
                     end;
                     Utilities.Failure ("Object_Declarations: procedure actual not found", Current);
                  when others =>
                     return;
               end case;

            when An_Association =>
               null;

            when others =>
               return;
         end case;

         -- Nothing interesting here, move up
         Previous := Current;
         Current  := Enclosing_Element (Current);
      end loop;
   end Process_Identifier;


   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Process_Scope_Exit is
      use Framework.Symbol_Table, NRT_Utilities;

      procedure Process_One_Volatile_Address (Entity : Asis.Defining_Name; Repr_Data : in out Repr_Rec) is
         use Framework.Locations, Framework.Reports;
      begin
         if Rule_Used (S_Volatile_No_Address) and Repr_Data.Volatile and not Repr_Data.Address then
            Report (Rule_Id,
                    Vno_Context (S_Volatile_No_Address),
                    Get_Location (Entity),
                    "variable is volatile and has no address clause");
         end if;
         if Rule_Used (S_Address_Not_Volatile) and Repr_Data.Address and not Repr_Data.Volatile then
            Report (Rule_Id,
                    Vno_Context (S_Address_Not_Volatile),
                    Get_Location (Entity),
                    "variable has address clause and is not volatile");
         end if;
      end Process_One_Volatile_Address;

      procedure Process_All_Volatile_Address is
        new Repr_Table.On_Every_Entity_From_Scope (Process_One_Volatile_Address);

      procedure Process_One_Not_Required_Type (Entity : Asis.Defining_Name; State : in out Variable_State) is
      begin   -- Process_One_Not_Required_Type
         case State is
            when Required =>
               null;  -- There are no dependents at this point
            when Suspect =>
               Check_Not_Required (Entity);
         end case;
      end Process_One_Not_Required_Type;
      procedure Process_All_Not_Required_Type is
        new Active_Suspect_Variables.On_Every_Entity_From_Scope (Process_One_Not_Required_Type);

   begin  -- Process_Scope_Exit
      if not (   Rule_Used (S_Volatile_No_Address)
              or Rule_Used (S_Address_Not_Volatile)
              or Rule_Used (S_Not_Required_Type))
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (S_Volatile_No_Address) or Rule_Used (S_Address_Not_Volatile) then
         Process_All_Volatile_Address (Declaration_Scope);
      end if;

      if Rule_Used (S_Not_Required_Type) then
         Process_All_Not_Required_Type (Visibility_Scope);
      end if;
   end Process_Scope_Exit;

begin  -- Rules.Object_Declarations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Object_Declarations;
