----------------------------------------------------------------------
--  Framework.Object_Tracker - Package body                         --
--  Copyright (C) 2020 Adalog - Eurocontrol                         --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                          Tel: +33 1 45 29 21 52          --
--  2 rue du Docteur Lombard        Fax: +33 1 45 29 25 00          --
--  92441 ISSY LES MOULINEAUX CEDEX E-m: info@adalog.fr             --
--  FRANCE                          URL: http://www.adalog.fr       --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
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
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
----------------------------------------------------------------------

-- Asis
with
   Asis.Declarations,
   Asis.Definitions,
   Asis.Elements,
   Asis.Exceptions,
   Asis.Expressions,
   Asis.Statements;

-- Adalog
with
   Linear_Queue,
   Utilities;

-- Adactl
with
  Framework.Element_Queues,
  Framework.Symbol_Table;

package body Framework.Object_Tracker is
   use Asis;
   use Thick_Queries;

   -- Algorithm
   --
   -- In this unit, the term "path" designates any construct that contains a sequence of statements, i.e. an
   -- ASIS path, but also a loop, a subprogram, an exception handler... but not an expression path.
   --
   -- When a variable is declared, it is entered in a symbol table, provided it is "appropriate" (i.e. of an
   -- elementary type). A variable present in the symbol table is said to be "tracked".
   --
   -- A variable is removed from the symbol table when it is no more appropriate (Untrack_Variable).
   -- This happens when:
   --   - the variable is accessed from a nested subprogram.
   --   - the variable is the actual parameter corresponding to an in out parameter of a generic instantiation
   --
   -- The value associated to a tracked variable is a stack (LIFO) of Value_Descr. The Value_Descr keeps info
   -- about:
   --   - The path associated to this state
   --   - The constraint associated to the declaration of the object (always obeyed)
   --   - The constraint associated to the current path (obeyed until the variable is assigned within the path)
   --   - The value of the variable (if it has been assigned in this path)
   --   - Whether the variable has been assigned in this path
   --
   -- When a variable is entered in the symbol table, the current constraint and value are
   -- inherited from the declaration constraint.
   --
   -- When a path with a condition is entered (if path, case path, while loop statement), a new value is pushed
   -- with the current condition. The value is intersected with the current condition.
   --
   -- When a variable is assigned (including [in] out parameters and initializatin), the assigned value is
   -- intersected with the declaration condition. If this is the first assignment in this path, other assigned
   -- values in enclosing paths become "unknown", since a path has other parallel paths that may (or not)
   -- change the variable.
   --
   -- There is a special issue with loops, since any variable modified in the body of the loop becomes "unknown"
   -- from the start of the loop. This is also true for exception handlers for all tracked variables, or statements
   -- that follow a label. Therefore, Intrinsically_Unknown_Paths is a LIFO of such paths; if the current path
   -- is at the top, any variable will be evaluated as "Unknow", until it has been assigned within the path.
   -- This is a bit pessimistic, since only variables actually modified somewhere in the path are really
   -- unknown, but it avoids a complex analysis.
   --
   -- Note that variables are invalidated before the loop (if a variable is part of the expression, its value
   -- must be unknown), but the constraint is effective only within the loop. Therefore, the pre-procedure for
   -- statements is split in two, Process_Outer_Statement is called before rules plug-ins for the statement,
   -- and Process_Statement is called after.


   -- Descriptor of the possible values of a discrete type:
   type Value_Descr is
      record
         Kind             : Content_Kinds;
         Declaration_Min  : Extended_Biggest_Int;
         Declaration_Max  : Extended_Biggest_Int;
         Constraint_Min   : Extended_Biggest_Int;
         Constraint_Max   : Extended_Biggest_Int;
         Assigned_Min     : Extended_Biggest_Int;
         Assigned_Max     : Extended_Biggest_Int;
      end record;
   Untracked_Descriptor : constant Value_Descr := (Kind   => Untracked, others => 0);

   -- For discrete variables:
   type Simple_Variable_Descr is
      record
         Attached_Path    : Asis.Element;  -- The path or statement or body that contains the assignment or constraint
         Assigned_In_Path : Boolean;
         Value            : Value_Descr;
      end record;
   package Simple_Descr_List   is new Linear_Queue (Simple_Variable_Descr);
   package Simple_Object_Table is new Framework.Symbol_Table.Data_Access (Simple_Descr_List.Queue);

   -- For discriminated variables:
   type Discriminant_Descr is
      record
         Discrim_Name  : Asis.Defining_Name;
         Value         : Value_Descr;
      end record;
   type Discriminant_Descr_Table  is array (ASIS_Integer range <>) of Discriminant_Descr;
   type Discriminated_Variable_Descr (Nb_Discr : Asis.ASIS_Positive) is
      record
         Attached_Path    : Asis.Element;  -- The path or statement or body that contains the assignment or constraint
         Assigned_In_Path : Boolean;
         Discriminants    : Discriminant_Descr_Table (1 .. Nb_Discr);
      end record;
   package Discriminated_Descr_List   is new Linear_Queue (Discriminated_Variable_Descr);
   package Discriminated_Object_Table is new Framework.Symbol_Table.Data_Access (Discriminated_Descr_List.Queue);

   -- For type declaration:
   package Type_Table is new Framework.Symbol_Table.Data_Access (Object_Value_Set);

   Intrinsically_Unknown_Paths : Element_Queues.Queue;

   ------------------
   -- Is_Trackable --
   ------------------

   function Is_Trackable (Var : Asis.Expression) return Boolean is
      use Asis.Elements, Asis.Expressions;
   -- An indexed component can be static, but we don't track it
   -- => no indexing in any part of Var
   begin
      case Expression_Kind (Var) is
         when An_Indexed_Component =>
            return False;
         when A_Selected_Component =>
            return Is_Trackable (Prefix (Var));
         when others =>
            return Is_Static_Object (Var);
      end case;
   end Is_Trackable;

   ------------------------
   -- Discriminant_Index --
   ------------------------

   function Discriminant_Index (In_Table : Discriminant_Descr_Table; Discr : Asis.Name) return Asis.ASIS_Integer is
      use Asis.Elements;
      use Utilities;
      Discr_Def : constant Asis.Defining_Name := First_Defining_Name (Discr);
   begin
      for Discr_Inx in In_Table'Range loop
         if Is_Equal (Discr_Def, In_Table (Discr_Inx).Discrim_Name) then
            return Discr_Inx;
         end if;
      end loop;
      Failure ("Discriminant_Index: not found", Discr);
   end Discriminant_Index;

   ---------------------
   -- Update_Variable --
   ---------------------

   type Value_Target is (Constraint, Assigned);
   -- The Declaration_Min/Max are never updated

   procedure Update_Variable (For_Path : Asis.Element;
                              Var      : Asis.Element;
                              Discr    : Asis.Name := Nil_Element; -- Nil_Element if no discriminant
                              Target   : Value_Target;
                              Min      : Extended_Biggest_Int;
                              Max      : Extended_Biggest_Int;
                              Offset   : Thick_Queries.Biggest_Int := 0)
     -- Registers that the value of Var is known to be in range Min..Max at the current place in path For_Path
     -- If any of Min and Max is Nil_Element, it is taken to be Not_Static
     -- If Discr /= Nil_Element, it is an update of discriminant Discr of variable Var, and Target is necessarily
     --    Constraint, since assigning to discriminants is not allowed.
     -- OTOH, if Discr = Nil_Element, Var may be the name of a discriminated variable, and all its discriminants
     --    must be updated.
   is
      use Simple_Descr_List, Discriminated_Descr_List;
      use Asis.Elements, Asis.Expressions;

      Simple_Queue        : Simple_Descr_List.Queue;
      Discriminated_Queue : Discriminated_Descr_List.Queue;

      Good_Var            : Asis.Element := Ultimate_Name (Var);
      Good_Discr          : Asis.Name    := Discr;
      Value               : Value_Descr;
      Var_Path            : Asis.Element;
      Path_Assigned       : Boolean;
      Discr_Index         : Asis.ASIS_Positive;
      Imin                : Extended_Biggest_Int := Min;
      Imax                : Extended_Biggest_Int := Max;
   begin
      if Is_Nil (Good_Discr)
        and then Expression_Kind (Var) = A_Selected_Component
        and then Declaration_Kind (Corresponding_Name_Declaration (Selector (Var))) = A_Discriminant_Specification
      then
         Good_Discr := Selector (Var);
         Good_Var   := Ultimate_Name (Prefix (Var));
      end if;

      if Is_Nil (Good_Discr) then
         Simple_Queue := Simple_Object_Table.Fetch (Good_Var,
                                                    Default => Simple_Descr_List.Empty_Queue);
         if Is_Empty (Simple_Queue) then
            -- Untracked variable
            return;
         end if;
         declare
            Element : constant Simple_Variable_Descr := Fetch (First (Simple_Queue));
         begin
            Value         := Element.Value;
            Var_Path      := Element.Attached_Path;
            Path_Assigned := Element.Assigned_In_Path and Is_Equal (For_Path, Var_Path);
         end;
      else
         Discriminated_Queue := Discriminated_Object_Table.Fetch (Good_Var,
                                                                  Default => Discriminated_Descr_List.Empty_Queue);
         if Is_Empty (Discriminated_Queue) then
            -- Untracked variable
            return;
         end if;
         declare
            Element : constant Discriminated_Variable_Descr := Fetch (First (Discriminated_Queue));
         begin
            Discr_Index   := Discriminant_Index (Element.Discriminants, Good_Discr);
            Value         := Element.Discriminants (Discr_Index).Value;
            Var_Path      := Element.Attached_Path;
            Path_Assigned := Element.Assigned_In_Path and Is_Equal (For_Path, Var_Path);
         end;
      end if;

      -- There are never two updates of constraints for the same path.
      -- => if the target is "Constraint", Var_Descr comes from some upper path
      -- => if the target is "Assigned", Var_Descr can come from the same path (in which case it is replaced)
      --    or some upper path
      case Target is
         when Constraint =>
            if Imin = Not_Static then
               Imin := Value.Constraint_Min;
            else
               Imin := Biggest_Int'Max (Imin + Offset, Value.Constraint_Min);
            end if;

             if Imax = Not_Static then
               Imax := Value.Constraint_Max;
            else
               Imax := Biggest_Int'Min (Imax + Offset, Value.Constraint_Max);
            end if;
            Value.Constraint_Min := Imin;
            Value.Constraint_Max := Imax;

        when Assigned =>
            if Imin = Not_Static then
               Imin := Value.Declaration_Min;
            else
               Imin := Biggest_Int'Max (Imin + Offset, Value.Declaration_Min);
            end if;

            if Imax = Not_Static then
               Imax := Value.Declaration_Max;
            else
               Imax := Biggest_Int'Min (Imax + Offset, Value.Declaration_Max);
            end if;
            Value.Assigned_Min   := Imin;
            Value.Assigned_Max   := Imax;
            -- Assignment destroys previously known constraints of path:
            Value.Constraint_Min := Value.Declaration_Min;
            Value.Constraint_Max := Value.Declaration_Max;
      end case;

      if Is_Nil (Good_Discr) then
         if Is_Equal (For_Path, Var_Path) then
            Replace (First (Simple_Queue), (For_Path, Path_Assigned or Target = Assigned, Value));
         else
            Prepend (Simple_Queue, (For_Path, Target = Assigned, Value));
         end if;
      else
         declare
            Var_Descr : Discriminated_Variable_Descr := Fetch (First (Discriminated_Queue));
         begin
            Var_Descr.Discriminants (Discr_Index).Value := Value;
            Var_Descr.Attached_Path                     := For_Path;
            Var_Descr.Assigned_In_Path                  := Path_Assigned or Target = Assigned;
            if Is_Equal (For_Path, Var_Path) then
               Replace (First (Discriminated_Queue),  Var_Descr);
            else
               Prepend (Discriminated_Queue, Var_Descr);
            end if;
         end;
      end if;

      if Target = Assigned and not Path_Assigned then
         -- First assignment to this variable in this path => assignments in all enclosing paths become unknown
         -- We have updated the queue above, therefore First (Simple_Queue/Discriminated_Queue) is the current path
         if Is_Nil (Good_Discr) then
            declare
               Curs        : Simple_Descr_List.Cursor := Next (First (Simple_Queue));
               Current_Var : Simple_Variable_Descr;
            begin
               while Has_Element (Curs) loop
                  Current_Var := Fetch (Curs);

                  Current_Var.Value.Assigned_Min   := Not_Static;
                  Current_Var.Value.Assigned_Max   := Not_Static;
                  Current_Var.Value.Constraint_Min := Current_Var.Value.Declaration_Min;
                  Current_Var.Value.Constraint_Max := Current_Var.Value.Declaration_Max;

                  Replace (Curs, Current_Var);
                  Curs := Next (Curs);
               end loop;
            end;
         else
            declare
               Curs        : Discriminated_Descr_List.Cursor := Next (First (Discriminated_Queue));
               Current_Var : Discriminated_Variable_Descr    := Fetch (Curs);
               -- Get the first element to dimension Current_Var.Discriminants, all tables have the same size
            begin
               loop
                  declare
                     Current_Discr_Val : Value_Descr renames Current_Var.Discriminants (Discr_Index).Value;
                  begin
                     Current_Discr_Val.Assigned_Min   := Not_Static;
                     Current_Discr_Val.Assigned_Max   := Not_Static;
                     Current_Discr_Val.Constraint_Min := Current_Discr_Val.Declaration_Min;
                     Current_Discr_Val.Constraint_Max := Current_Discr_Val.Declaration_Max;
                  end;

                  Replace (Curs, Current_Var);
                  Curs := Next (Curs);
                  exit when not Has_Element (Curs);
                  Current_Var := Fetch (Curs);
               end loop;
            end;
         end if;
      end if;

      if Is_Nil (Good_Discr) then
         Simple_Object_Table.Store (Good_Var, Simple_Queue);
      else
         Discriminated_Object_Table.Store (Good_Var, Discriminated_Queue);
      end if;
   end Update_Variable;

   ----------------------
   -- Update_Condition --
   ----------------------

   procedure Update_Condition (Cond : Asis.Expression) is
      use Asis.Elements, Asis.Expressions;

      Op     : Asis.Name;
      Var    : Asis.Expression;
      Discr  : Asis.Expression := Nil_Element;
      Expr   : Asis.Expression;
      Params : Asis.Expression_List (1 .. 2);
   begin
      case Expression_Kind (Cond) is
         when A_Function_Call =>
            if Corresponding_Call_Description (Cond).Kind /= A_Predefined_Entity_Call then
               return;
            end if;
            Op := Simple_Name (Prefix (Cond));
            if Operator_Kind (Op) not in Relational_Operators then  -- Including Not_An_Operator
               return;
            end if;

            Params := Actual_Expressions (Cond);
            if Is_Static_Object (Params (1)) then
               Var  := Params (1);
               Expr := Params (2);
            elsif Is_Static_Object (Params (2)) then
               Var  := Params (2);
               Expr := Params (1);
            else
               -- No simple variable in sight
               return;
            end if;
            if not Is_Trackable (Var) then
               return;
            end if;

            loop
               case Expression_Kind (Var) is
                  when A_Type_Conversion | A_Qualified_Expression =>
                     Var := Converted_Or_Qualified_Expression (Var);
                  when A_Selected_Component =>
                     if Declaration_Kind (Corresponding_Name_Declaration (Selector (Var)))
                                          = A_Discriminant_Specification
                     then
                        Discr := Selector (Var);
                        Var   := Prefix (Var);
                     else
                        Var := Selector (Var);
                     end if;
                  when An_Identifier =>
                     if Declaration_Kind (Corresponding_Name_Declaration (Var))
                       not in A_Variable_Declaration | A_Constant_Declaration | A_Parameter_Specification
                            | A_Loop_Parameter_Specification
                     then
                        return;
                     end if;
                     exit;
                  when others =>
                     -- Including An_Indexed_Component (statically indexed variable), not tracked
                     return;
               end case;
            end loop;

            case Relational_Operators'(Operator_Kind (Op)) is
               -- Remember that only "=" and "/=" are available for access types
               when An_Equal_Operator =>
                  Update_Variable (Enclosing_Element (Cond), Var, Discr,
                                   Min    => Discrete_Static_Expression_Value (Expr, Minimum),
                                   Max    => Discrete_Static_Expression_Value (Expr, Maximum),
                                   Target => Constraint);
               when A_Not_Equal_Operator =>
                  null; -- TBSL
               when A_Less_Than_Operator =>
                  Update_Variable (Enclosing_Element (Cond), Var, Discr,
                                   Min    => Not_Static,
                                   Max    => Discrete_Static_Expression_Value (Expr, Maximum),
                                   Offset => -1,
                                   Target => Constraint);
               when A_Less_Than_Or_Equal_Operator =>
                  Update_Variable (Enclosing_Element (Cond), Var, Discr,
                                   Min    => Not_Static,
                                   Max    => Discrete_Static_Expression_Value (Expr, Maximum),
                                   Target => Constraint);
               when A_Greater_Than_Operator =>
                  Update_Variable (Enclosing_Element (Cond), Var, Discr,
                                   Min    => Discrete_Static_Expression_Value (Expr, Minimum),
                                   Max    => Not_Static,
                                   Offset => +1,
                                   Target => Constraint);
               when A_Greater_Than_Or_Equal_Operator =>
                  Update_Variable (Enclosing_Element (Cond), Var, Discr,
                                   Min    => Discrete_Static_Expression_Value (Expr, Minimum),
                                   Max    => Not_Static,
                                   Target => Constraint);
            end case;
         when An_In_Membership_Test =>
            Var := Membership_Test_Expression (Cond);
            if not Is_Trackable (Var) then
               return;
            end if;

            loop
               case Expression_Kind (Var) is
                  when A_Type_Conversion | A_Qualified_Expression =>
                     Var := Converted_Or_Qualified_Expression (Var);
                  when A_Selected_Component =>
                     if Declaration_Kind (Corresponding_Name_Declaration (Selector (Var)))
                       = A_Discriminant_Specification
                     then
                        Discr := Selector (Var);
                        Var   := Prefix (Var);
                     else
                        Var := Selector (Var);
                     end if;
                  when An_Identifier =>
                     if Declaration_Kind (Corresponding_Name_Declaration (Var))
                        not in A_Variable_Declaration | A_Constant_Declaration | A_Parameter_Specification
                             | A_Loop_Parameter_Specification
                     then
                        return;
                     end if;
                     exit;
                  when others =>
                     -- Including An_Indexed_Component (statically indexed variable), not tracked
                     return;
               end case;
            end loop;

            declare
               Choices     : constant Asis.Element_List := Membership_Test_Choices (Cond);
               Bounds_List :          Extended_Biggest_Int_List (1..2);
            begin
               -- We handle only simple ranges
               if Choices'Length /= 1 or else Definition_Kind (Choices (1)) /= A_Constraint then
                  return;
               end if;
               Bounds_List := Discrete_Constraining_Values (Choices (1));
               Update_Variable (Enclosing_Element (Cond), Var, Discr, Min    => Bounds_List (1),
                                                                      Max    => Bounds_List (2),
                                                                      Target => Constraint);
            end;
         when A_Not_In_Membership_Test =>
            null; --TBSL
         when others =>
            null;
      end case;
   end Update_Condition;

   ---------------------
   -- Invalidate_Path --
   ---------------------

   procedure Invalidate_Path (Path : Asis.Element) is
   -- Pre: Path is the current path
   -- Add the path to Intrinsically_Unknown_Paths, unless it is already there
   --   (this can happen, f.e., if a while loop has a label)
      use Asis.Elements;
      use Element_Queues;
   begin
      if Is_Empty (Intrinsically_Unknown_Paths)
        or else not Is_Equal (Fetch (First (Intrinsically_Unknown_Paths)), Path)
      then
         Prepend (Intrinsically_Unknown_Paths, Path);
      end if;
   end Invalidate_Path;

   ----------------------------
   -- Clear_Invalidated_Path --
   ----------------------------

   procedure Clear_Invalidated_Path (Path : Asis.Element) is
      use Asis.Elements;
      use Element_Queues;
   begin
      if not Is_Empty (Intrinsically_Unknown_Paths)
        and then Is_Equal (Fetch (First (Intrinsically_Unknown_Paths)), Path)
      then
         Clear (Intrinsically_Unknown_Paths, 1);
      end if;
   end Clear_Invalidated_Path;

   ----------------
   -- Clear_Path --
   ----------------

   procedure Clear_Path (Path : Asis.Element) is
   -- Exiting a path.
   -- Delete this path from all variables that mention it.
      use Symbol_Table;

      procedure Clean_One_Simple_Variable (Entity      : in     Asis.Defining_Name;
                                           Value_Queue : in out Simple_Descr_List.Queue)
      is
         pragma Unreferenced (Entity);
         use Asis.Elements;
         use Simple_Descr_List;

         Curs : constant Cursor := First (Value_Queue);
      begin
         -- Curs may be empty if the variable has already been cleaned due to a labeled statement
         if Has_Element (Curs) and then Is_Equal (Path, Fetch (Curs).Attached_Path) then
            -- The variable state has been modified in the current path, remove the path
            Clear (Value_Queue, 1);
         end if;
      end Clean_One_Simple_Variable;
      procedure Clean_Simple_Variables is
        new Simple_Object_Table.On_Every_Entity_From_Scope (Clean_One_Simple_Variable);

      procedure Clean_One_Discriminated_Variable (Entity      : in     Asis.Defining_Name;
                                                  Value_Queue : in out Discriminated_Descr_List.Queue)
      is
         pragma Unreferenced (Entity);
         use Asis.Elements;
         use Discriminated_Descr_List;

         Curs : constant Cursor := First (Value_Queue);
      begin
         -- Curs may be empty if the variable has already been cleaned due to a labeled statement
         if Has_Element (Curs) and then Is_Equal (Path, Fetch (Curs).Attached_Path) then
            -- The variable has been modified in the current path, remove the path
            Clear (Value_Queue, 1);
         end if;
      end Clean_One_Discriminated_Variable;
      procedure Clean_Discriminated_Variables is
        new Discriminated_Object_Table.On_Every_Entity_From_Scope (Clean_One_Discriminated_Variable);
   begin  -- Clear_Path
      Clean_Simple_Variables        (All_Scopes);
      Clean_Discriminated_Variables (All_Scopes);
   end Clear_Path;


   ----------------------
   -- Untrack_Variable --
   ----------------------

   procedure Untrack_Variable (Var : Asis.Expression) is
   -- Delete this variable with its associated assignment list from the variable table
      use Simple_Descr_List, Discriminated_Descr_List;
   begin
      if Simple_Object_Table.Is_Present (Var) then
         declare
            Queue : Simple_Descr_List.Queue := Simple_Object_Table.Fetch (Var);
         begin
            Clear (Queue);
            Simple_Object_Table.Delete (Var);
         end;

      elsif Discriminated_Object_Table.Is_Present (Var) then
         declare
            Queue : Discriminated_Descr_List.Queue := Discriminated_Object_Table.Fetch (Var);
         begin
            Clear (Queue);
            Discriminated_Object_Table.Delete (Var);
         end;
      end if;
   end Untrack_Variable;

   ------------------------
   -- Process_Assignment --
   ------------------------

   procedure Process_Assignment (Path : Asis.Element; Var : Asis.Element; Expr : Asis.Expression) is
   -- Appropriate Element_Kinds of Var:
   --   - An_Expression
   --   - A_Defining_Name
   -- Note: Var cannot be a reference to a discriminant (even through renamings), since discriminants
   --       are not allowed as LHS of assignments.
      use Asis.Declarations, Asis.Elements, Asis.Expressions;
      Good_Var             : Asis.Expression;
      Current_Unit         : Asis.Declaration;
      Good_Path            : Asis.Element := Path;
      Is_Discriminated_Var : Boolean;

      procedure Make_Discriminants_Unknown (Of_Var : Asis.Expression) is
      -- make the discriminants unknown
      -- TBSL: keep the discriminants if the variable is constrained
         use Discriminated_Descr_List;
         LHS_Queue : constant Queue := Discriminated_Object_Table.Fetch (Of_Var);
         LHS       : constant Discriminated_Variable_Descr := Fetch (First (LHS_Queue));
      begin
         for D : Discriminant_Descr of LHS.Discriminants loop
            Update_Variable (Good_Path, Good_Var, D.Discrim_Name,
                             Min    => Not_Static,
                             Max    => Not_Static,
                             Target => Assigned);
         end loop;
      end Make_Discriminants_Unknown;

   begin   -- Process_Assignment
      if Expression_Kind (Var) in An_Identifier | A_Selected_Component then
         Good_Var := Ultimate_Name (Var);
         if Is_Nil (Good_Var)  -- Name includes a dereference
           or else Declaration_Kind (Corresponding_Name_Declaration (Good_Var))
                   in A_Component_Declaration | A_Parameter_Specification | A_Constant_Declaration
         then
            -- We don't track regular components, and we know nothing about the value of a formal parameter
            -- Normally, Process_Assignment should not be called on constants, but this can happen for
            -- constants given as actual parameters to a dispatching or attribute procedure call.
            return;
         end if;

         Current_Unit := Path;
         if Element_Kind (Current_Unit) in A_Statement | A_Path | An_Exception_Handler then
            Current_Unit := Enclosing_Program_Unit (Path);
         else
            -- It is already a program unit
            Current_Unit := Names (Current_Unit) (1);
         end if;
         if not Is_Equal (Enclosing_Program_Unit (Corresponding_Name_Declaration (Good_Var)), Current_Unit) then
            -- Modified from nested unit => Give up
            Untrack_Variable (Good_Var);
            return;
         end if;
      elsif Element_Kind (Var) = A_Defining_Name then
         Good_Var := Ultimate_Name (Var);
      else
         return;   -- indexed variable, dereference, function call...
      end if;

      if Simple_Object_Table.Is_Present (Good_Var) then
         Is_Discriminated_Var := False;
      elsif Discriminated_Object_Table.Is_Present (Good_Var) then
         Is_Discriminated_Var := True;
      else  -- not tracked
         return;
      end if;

      -- Blocks have no effect on tracking, attach the update to the innermost existing path
      while Statement_Kind (Good_Path) = A_Block_Statement loop
         Good_Path := Enclosing_Element (Good_Path);
      end loop;

      if Is_Discriminated_Var then
         -- Simulate an assignment to every discriminant
         case Expression_Kind (Expr) is
            when An_Identifier | A_Selected_Component =>
               if Discriminated_Object_Table.Is_Present (Ultimate_Name (Expr)) then
                  -- Variable_1 := Variable_2;
                  declare
                     use Discriminated_Descr_List;
                     LHS_Queue :          Queue := Discriminated_Object_Table.Fetch (Good_Var);
                     RHS_Queue : constant Queue := Discriminated_Object_Table.Fetch (Ultimate_Name (Expr));
                     LHS       : constant Discriminated_Variable_Descr := Fetch (First (LHS_Queue));
                     RHS       : constant Discriminated_Variable_Descr := Fetch (First (RHS_Queue));
                  begin
                     if Is_Equal (LHS.Attached_Path, RHS.Attached_Path) then
                        Replace (First (LHS_Queue), RHS);
                     else
                        Prepend (LHS_Queue, RHS);
                     end if;
                     Discriminated_Object_Table.Store (Good_Var, LHS_Queue);
                  end;
               else
                  Make_Discriminants_Unknown (Good_Var);
               end if;
            when A_Record_Aggregate =>
               declare
                  use Discriminated_Descr_List;
                  LHS_Queue  : constant Queue                        := Discriminated_Object_Table.Fetch (Good_Var);
                  LHS        : constant Discriminated_Variable_Descr := Fetch (First (LHS_Queue));
                  Rec_Assocs : constant Asis.Association_List := Record_Component_Associations (Expr,
                                                                                                Normalized => True);
               begin
                  for D in LHS.Discriminants'Range loop
                     -- With a normalized association, discriminants are the first components Rec_Assocs,
                     -- and in the same order as LHS
                     Update_Variable (Good_Path, Good_Var, LHS.Discriminants (D).Discrim_Name,
                                      Min    => Discrete_Static_Expression_Value (Component_Expression (Rec_Assocs (D)),
                                                                                  Minimum),
                                      Max    => Discrete_Static_Expression_Value (Component_Expression (Rec_Assocs (D)),
                                                                                  Maximum),
                                      Target => Assigned);
                  end loop;
               end;
            when others =>
               Make_Discriminants_Unknown (Good_Var);
         end case;

      else   -- Not a discriminated variable
         Update_Variable (Good_Path, Good_Var,
                          Min    => Discrete_Static_Expression_Value (Expr, Minimum),
                          Max    => Discrete_Static_Expression_Value (Expr, Maximum),
                          Target => Assigned);
      end if;
   end Process_Assignment;


   --------------------------------------------------------------------------------
   -- Exported elements
   --------------------------------------------------------------------------------

   ----------------------
   -- Expression_Value --
   ----------------------

   function Expression_Value (Expr : Asis.Expression; RM_Static : Boolean := False) return Object_Value_Set is
      Def : constant Asis.Expression      := Corresponding_Expression_Type_Definition (Expr);
      Val : constant Extended_Biggest_Int := Discrete_Static_Expression_Value(Expr, RM_Static => RM_Static);
   begin
      case Type_Category (Def, Follow_Derived => True) is
         when An_Enumeration_Type =>
            return (Kind => Enumerated, Imin => Val, Imax => Val);
         when A_Signed_Integer_Type =>
            return (Kind => Integer, Imin => Val, Imax => Val);
         when A_Modular_Type =>
            return (Kind => Modular, Imin => Val, Imax => Val);
         when An_Access_Type =>
            return (Kind => Pointer, Imin => Val, Imax => Val);
         when others =>
            return Unknown_Value (Untracked);
      end case;
   end Expression_Value;

   ------------------
   -- Object_Value --
   ------------------

   function Object_Value (Var   : Asis.Element;
                          Discr : Asis.Name := Nil_Element -- Nil_Element if no discriminant
                         ) return Object_Value_Set
   is
   -- Var is a variable name, or a selected name whose prefix is a variable and selector a discriminant
   -- Otherwise, returns Unknown_Value (Untracked)
      use Asis.Elements, Asis.Expressions;
      use Element_Queues, Simple_Descr_List, Discriminated_Descr_List;

      Good_Decl       : Asis.Declaration;
      Good_Var        : Asis.Expression;
      Good_Discr      : Asis.Name;
      Descriptor      : Value_Descr;
      Path_Assigned   : Boolean;
      Var_Path        : Asis.Element;
      Forced_Unknown  : Boolean;
      Is_Discriminant : Boolean;
      Result          : Object_Value_Set;

      function Var_Controls_Path (Path : Asis.Element) return Boolean is
      begin
         if Is_Discriminant then
            -- If it has a discriminant, the variable is not discrete and cannot control the path
            return False;
         end if;

         declare
            Var_Queue : constant Simple_Descr_List.Queue := Simple_Object_Table.Fetch (Good_Var);
            Curs      : Simple_Descr_List.Cursor         := First (Var_Queue);
            D         : Simple_Variable_Descr;
         begin
            while Has_Element (Curs) loop
               D := Fetch (Curs);
               if Is_Equal (D.Attached_Path, Path) then
                  return not D.Assigned_In_Path;
               end if;
               Curs := Next (Curs);
            end loop;
         end;

         return False;
      end Var_Controls_Path;

      function Min (Left, Right : Extended_Biggest_Int) return Extended_Biggest_Int is
        (if    Left  = Not_Static then Right
         elsif Right = Not_Static then Left
         else  Extended_Biggest_Int'Min (Left, Right));

      function Max (Left, Right : Extended_Biggest_Int) return Extended_Biggest_Int is
        (if    Left  = Not_Static then Right
         elsif Right = Not_Static then Left
         else  Extended_Biggest_Int'Max (Left, Right));

   begin  -- Object_Value
      if not Is_Nil (Discr) then
         Is_Discriminant := True;
         Good_Var        := Ultimate_Name (Var);
         Good_Discr      := Discr;
      elsif Expression_Kind (Var) = A_Selected_Component then
         if Declaration_Kind (Corresponding_Name_Declaration (Selector (Var))) /= A_Discriminant_Specification then
            -- Selected_Component, not discriminant => not tracked
            return Unknown_Value (Untracked);
         end if;

         Is_Discriminant := True;
         Good_Var        := Ultimate_Name (Prefix (Var));
         Good_Discr      := Selector (Var);
      else
         Is_Discriminant := False;
      end if;

      -- TBSL: if the variable/discriminant is not tracked, return at least the constraint from the subtype
      if Is_Discriminant then
         declare
            Var_Queue : constant Discriminated_Descr_List.Queue := Discriminated_Object_Table.Fetch
                                                                    (Good_Var,
                                                                     Default => Discriminated_Descr_List.Empty_Queue);
         begin
            if Is_Empty (Var_Queue) then
               -- Untracked variable
               return Unknown_Value (Untracked);
            end if;
            declare
               Descr_Table : constant Discriminated_Variable_Descr := Fetch (First (Var_Queue));
            begin
               Descriptor    := Descr_Table.Discriminants (Discriminant_Index
                                                           (Descr_Table.Discriminants, Good_Discr)).Value;
               Path_Assigned := Descr_Table.Assigned_In_Path;
               Var_Path      := Descr_Table.Attached_Path;
            end;
         end;
      else  -- No discriminant => Variable identifier
         Is_Discriminant := False;
         Good_Var        := Ultimate_Name (Var);
         declare
            Var_Queue : constant Simple_Descr_List.Queue := Simple_Object_Table.Fetch
                                                             (Good_Var, Default => Simple_Descr_List.Empty_Queue);
         begin
            if Is_Empty (Var_Queue) then
               -- Untracked variable
               return Unknown_Value (Untracked);
            end if;
            declare
               Var_Descr : constant Simple_Variable_Descr := Fetch (First (Var_Queue));
            begin
               Descriptor    := Var_Descr.Value;
               Path_Assigned := Var_Descr.Assigned_In_Path;
               Var_Path      := Var_Descr.Attached_Path;
            end;
         end;
      end if;

      Good_Decl := Corresponding_Name_Declaration (Good_Var);
      if Declaration_Kind (Good_Decl) not in A_Constant_Declaration | A_Loop_Parameter_Specification
        and then Mode_Kind (Good_Decl) not in An_In_Mode | A_Default_In_Mode
        and then not Is_Equal (Enclosing_Program_Unit (Good_Decl), Enclosing_Program_Unit (Var))
      then
         -- A variable accessed from a different unit than where the variable is declared
         -- We don't know where this subprogram is called from, therefore it is an
         -- unknown value, unless the enclosing program unit is an expression function (macro model)
         if Declaration_Kind (Enclosing_Element (Enclosing_Program_Unit (Var))) /= An_Expression_Function_Declaration
         then
            return Unknown_Value (Descriptor.Kind);
         end if;
      end if;

      -- If the variable is nested in an "intrinsically unknown" path, such as a loop, exception handler, etc.
      -- then the value is forced to unknown (modulo the constraints), unless the variable has been assigned in
      -- the current construct, or is a constant (including for loop control variables)
      if Is_Empty (Intrinsically_Unknown_Paths) then
         -- Not nested in an intrinsically unknown path
         Forced_Unknown := False;
      elsif Declaration_Kind (Good_Decl) in A_Constant_Declaration | A_Loop_Parameter_Specification then
         -- Not a variable anyway
         Forced_Unknown := False;
      elsif Mode_Kind (Good_Decl) in An_In_Mode | A_Default_In_Mode then
         -- An "in" parameter
         Forced_Unknown := False;
      elsif Path_Assigned
        and then Is_Equal (Var_Path, Fetch (First (Intrinsically_Unknown_Paths)))
      then
         -- Variable has been assigned in the current path
         Forced_Unknown := False;
      elsif Var_Controls_Path (Fetch (First (Intrinsically_Unknown_Paths))) then
         -- The constraint on the variable comes from the condition of the unknown path
         Forced_Unknown := False;
      else
         -- Not assigned in the innermost intrinsically unknown path
         Forced_Unknown := True;
      end if;

      -- If Forced_Unknown, the only thing we can trust is the constraint in the declaration of the variable
      case Descriptor.Kind is
         when Enumerated =>
            if Forced_Unknown then
               Result := (Kind => Enumerated,
                          Imin => Descriptor.Declaration_Min,
                          Imax => Descriptor.Declaration_Max);
            else
               Result := (Kind => Enumerated,
                          Imin => Max (Descriptor.Constraint_Min, Descriptor.Assigned_Min),
                          Imax => Min (Descriptor.Constraint_Max, Descriptor.Assigned_Max));
            end if;
         when Integer =>
            if Forced_Unknown then
               Result := (Kind => Integer,
                          Imin => Descriptor.Declaration_Min,
                          Imax => Descriptor.Declaration_Max);
            else
               Result := (Kind => Integer,
                          Imin => Max (Descriptor.Constraint_Min, Descriptor.Assigned_Min),
                          Imax => Min (Descriptor.Constraint_Max, Descriptor.Assigned_Max));
            end if;
         when Modular =>
            if Forced_Unknown then
               Result := (Kind => Modular,
                          Imin => Descriptor.Declaration_Min,
                          Imax => Descriptor.Declaration_Max);
            else
               Result := (Kind => Modular,
                          Imin => Max (Descriptor.Constraint_Min, Descriptor.Assigned_Min),
                          Imax => Min (Descriptor.Constraint_Max, Descriptor.Assigned_Max));
            end if;
         when Pointer =>
            if Forced_Unknown then
               Result := (Kind => Pointer,
                          Imin => Descriptor.Declaration_Min,
                          Imax => Descriptor.Declaration_Max);
            else
               Result := (Kind => Pointer,
                          Imin => Max (Descriptor.Constraint_Min, Descriptor.Assigned_Min),
                          Imax => Min (Descriptor.Constraint_Max, Descriptor.Assigned_Max));
            end if;
         when Untracked =>
            return (Kind => Untracked);
      end case;

      return Result;
   end Object_Value;


   ------------------------
   -- Object_Value_Image --
   ------------------------

   function Object_Value_Image (Var : Asis.Element; Wanted : Thick_Queries.Expression_Info) return Wide_String is
      Val : constant Object_Value_Set := Object_Value (Var);
   begin
      if Val.Kind = Untracked then
         return "";
      end if;

      case Wanted is
         when Exact =>
            return (if Val.Imin = Not_Static or else Val.Imin /= Val.Imax then "" else Biggest_Int_Img (Val.Imin));
         when Minimum =>
            return (if Val.Imin = Not_Static then "" else Biggest_Int_Img (Val.Imin));
         when Maximum =>
            return (if Val.Imax = Not_Static then "" else Biggest_Int_Img (Val.Imax));
      end case;
   end Object_Value_Image;


   --------------------------------------------------------------------------------
   -- Ruler plugs
   --------------------------------------------------------------------------------

   ---------------------
   -- Process_Handler --
   ---------------------

   procedure Process_Handler (Handler : Asis.Exception_Handler) is
   begin
      Invalidate_Path (Handler);
   end Process_Handler;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Inst : Asis.Declaration) is
      use Asis.Declarations, Asis.Elements, Asis.Expressions;

      Formals  : constant Asis.Declaration_List := Generic_Formal_Part (Corresponding_Name_Declaration
                                                                        (Ultimate_Name
                                                                         (Simple_Name (Generic_Unit_Name (Inst)))));
      Good_Var : Asis.Expression;
   begin
      for Formal : Asis.Declaration of Formals loop
         if Mode_Kind (Formal) = An_In_Out_Mode then
            for Name : Asis.Name of Names (Formal) loop
               Good_Var := Actual_Expression (Inst, Name, Return_Default => True);
               case Expression_Kind (Good_Var) is
                  when An_Identifier =>
                     Good_Var := Ultimate_Name (Good_Var);
                  when A_Selected_Component =>
                     if not Is_Expanded_Name (Good_Var) then
                        -- record component...
                        return;
                     end if;
                     Good_Var := Ultimate_Name (Good_Var);
                  when others =>
                     -- including Not_An_Expression (Nil_Element from dereference), indexed variable
                     return;
               end case;

               Untrack_Variable (Good_Var);
            end loop;
         end if;
      end loop;
   end Process_Instantiation;

   --------------------
   -- Process_Labels --
   --------------------

   procedure Process_Outer_Statement (Stmt : in Asis.Statement) is
      use Asis.Elements, Asis.Statements;
   begin
      if Label_Names (Stmt) /= Nil_Element_List then
         -- Statement has label(s): all bets are off
         Invalidate_Path (Enclosing_Element (Stmt));
         Clear_Path      (Enclosing_Element (Stmt));  -- Forget all we know about this path
         return;
      end if;

      if Statement_Kind (Stmt) in A_Loop_Statement | A_While_Loop_Statement | A_For_Loop_Statement then
         Invalidate_Path (Stmt);
      end if;
   end Process_Outer_Statement;

   --------------------------------
   -- Process_Object_Declaration --
   --------------------------------

   procedure Process_Object_Declaration (Decl : in Asis.Declaration) is
      use Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;

      function Build_Descriptor (Obj_Def : Asis.Element; Initial_Expr : Asis.Expression) return Value_Descr is
         -- Obj_Def: a type definition or a subtype mark
         use Utilities;

         Descriptor       : Value_Descr;
         Range_Descriptor : Object_Value_Set;
         Initial_Value    : Extended_Biggest_Int;
         Bounds           : Extended_Biggest_Int_List (1 .. 2);
      begin
         case Type_Category (Obj_Def) is
            when An_Enumeration_Type =>
               Descriptor.Kind := Enumerated;
            when A_Signed_Integer_Type =>
               Descriptor.Kind := Integer;
            when A_Modular_Type =>
               Descriptor.Kind := Modular;
            when An_Access_Type =>
               Descriptor.Kind := Pointer;
            when others =>
               -- Not an elementary type, not tracked
               return Untracked_Descriptor;
         end case;

         case Descriptor.Kind is
            when Discrete_Content_Kinds =>
               if Element_Kind (Obj_Def) = An_Expression then -- a subtype name
                  Range_Descriptor := Type_Table.Fetch (Simple_Name (Obj_Def), Default => (Kind => Untracked));
                  if Range_Descriptor.Kind = Untracked then
                     -- Some predefined type, where the declaration has not been analyzed
                     -- TBSL force analysis of type here
                     Descriptor.Declaration_Min := Biggest_Int'First;
                     Descriptor.Declaration_Max := Biggest_Int'Last;
                  else
                     Descriptor.Declaration_Min := Range_Descriptor.Imin;
                     Descriptor.Declaration_Max := Range_Descriptor.Imax;
                  end if;
               elsif Definition_Kind (Obj_Def) in A_Subtype_Indication
                                                | A_Discrete_Subtype_Definition
                                                | A_Discrete_Range
                 and then Is_Nil (Subtype_Constraint (Obj_Def))
               then
                  -- Get the bounds from the type
                  Range_Descriptor := Type_Table.Fetch (Subtype_Simple_Name (Obj_Def), Default => (Kind => Untracked));
                  if Range_Descriptor.Kind = Untracked then
                     -- Some predefined type, where the declaration has not been analyzed
                     -- TBSL force analysis of type here
                     Descriptor.Declaration_Min := Biggest_Int'First;
                     Descriptor.Declaration_Max := Biggest_Int'Last;
                  else
                     Descriptor.Declaration_Min := Range_Descriptor.Imin;
                     Descriptor.Declaration_Max := Range_Descriptor.Imax;
                  end if;
               else
                  Bounds                     := Discrete_Constraining_Values (Obj_Def);
                  Descriptor.Declaration_Min := Bounds (1);
                  Descriptor.Declaration_Max := Bounds (2);
               end if;
               Descriptor.Constraint_Min := Descriptor.Declaration_Min;
               Descriptor.Constraint_Max := Descriptor.Declaration_Max;

               Initial_Value := Discrete_Static_Expression_Value (Initial_Expr);
               if Initial_Value = Not_Static then
                  Descriptor.Assigned_Min := Descriptor.Constraint_Min;
                  Descriptor.Assigned_Max := Descriptor.Constraint_Max;
               else
                  Descriptor.Assigned_Min := Initial_Value;
                  Descriptor.Assigned_Max := Initial_Value;
               end if;

            when Pointer =>
               if Element_Kind (Obj_Def) = An_Expression then -- a subtype name
                  Range_Descriptor := Type_Table.Fetch (Simple_Name (Obj_Def), Default => (Kind => Untracked));
                  if Range_Descriptor.Kind = Untracked then
                     -- Some predefined type, where the declaration has not been analyzed
                     -- TBSL force analysis of type here
                     Descriptor.Declaration_Min := 0;  -- No predefined type has null exclusion
                     Descriptor.Declaration_Max := Biggest_Int'Last;
                  else
                     Descriptor.Declaration_Min := Range_Descriptor.Imin;
                     Descriptor.Declaration_Max := Range_Descriptor.Imax;
                  end if;
               elsif Trait_Kind (Obj_Def) = A_Null_Exclusion_Trait then
                  Descriptor.Declaration_Min := 1;
               elsif Type_Kind (Obj_Def) = An_Access_Type_Definition then
                  -- Named access type, no null exclusion
                  Descriptor.Declaration_Min := 0;
               elsif Definition_Kind (Obj_Def) = An_Access_Definition then
                  -- Anonymous access type, no null exclusion
                  Descriptor.Declaration_Min := 0;
               else
                  -- The type may not be in table, if from some package that has not been analyzed
                  -- (standard library f.e.)
                  Range_Descriptor := Type_Table.Fetch (Subtype_Simple_Name (Obj_Def),
                                                        Default => (Kind => Pointer,
                                                                    Imin => 0,
                                                                    Imax => Biggest_Int'Last));
                  Descriptor.Declaration_Min := Range_Descriptor.Imin;
               end if;
               Descriptor.Declaration_Max := Biggest_Int'Last;           -- Always for pointers
               Descriptor.Constraint_Min  := Descriptor.Declaration_Min;
               Descriptor.Constraint_Max  := Descriptor.Declaration_Max;

               if Is_Nil (Initial_Expr) then
                  Initial_Value := 0;  -- null is the default for access types
               else
                  Initial_Value := Discrete_Static_Expression_Value (Initial_Expr);
               end if;

               if Initial_Value = Not_Static then
                  Descriptor.Assigned_Min := Descriptor.Constraint_Min;
                  Descriptor.Assigned_Max := Descriptor.Constraint_Max;
               else
                  Descriptor.Assigned_Min := Initial_Value;
                  Descriptor.Assigned_Max := Initial_Value; -- Since Constraint_Max is always Biggest_Int'Last
               end if;

            when Untracked =>
               Failure ("Build descriptor: untracked");
         end case;
         return Descriptor;
      end Build_Descriptor;

      Def          : Asis.Definition;   -- TBSL Clean-up usage of Def
      Subtype_Name : Asis.Expression;
      Is_Parameter : Boolean;
   begin  -- Process_Object_Declaration
      case Declaration_Kind (Enclosing_Element (Enclosing_Program_Unit (Decl))) is
         when A_Package_Declaration | A_Generic_Package_Declaration | A_Package_Body_Declaration =>
            return;
         when others =>
            null;
      end case;

      if Corresponding_Aspects (Decl, "VOLATILE") /= Nil_Element_List
        or else Corresponding_Pragma_Set (Decl) (A_Volatile_Pragma)
      then -- Variable is volatile
         return;
      end if;

      Def          := Object_Declaration_View (Decl);
      Is_Parameter := Declaration_Kind (Decl) = A_Parameter_Specification;
      if Is_Parameter then
         if Declaration_Kind (Enclosing_Element (Decl))
            not in A_Procedure_Body_Declaration | A_Function_Body_Declaration
         then
            -- Nothing to track in specifications.
            return;
         end if;
         if Element_Kind (Def) = An_Expression then
         -- Case of formal parameters: Object_Declaration_View returns a name, not a definition
         -- (except for the case of access parameters, sigh...)
            Def := Type_Declaration_View (Corresponding_Name_Declaration (Simple_Name (Strip_Attributes (Def))));
         end if;
      end if;

      if Definition_Kind (Def) not in A_Type_Definition | A_Subtype_Indication | An_Access_Definition then
         -- Anonymous type => not discrete, no discriminant, but we still accept anonymous access types
         return;
      end if;

      case Type_Category (Decl, Follow_Derived => True) is
         when Discrete_Types | An_Access_Type =>
            for Name : Asis.Name of Names (Decl) loop
               Force_New_Evaluation;
               declare
                  use Simple_Descr_List;
                  Val_Queue : Simple_Descr_List.Queue;
                  Descr     : constant Value_Descr := Build_Descriptor (Def, Initialization_Expression (Decl));
               begin
                  if Descr.Kind /= Untracked then
                     Prepend (Val_Queue, (Enclosing_Element (Decl), False, Descr));
                     Simple_Object_Table.Store (Name, Val_Queue);
                  end if;
               end;
            end loop;
            return;
         when A_Task_Type =>
            -- Due to an ASIS bug, AdaControl crashes on discriminants of a task type
            -- TBSL: ignore until this is fixed
            return;
         when A_Fixed_Point_Type
            | A_Floating_Point_Type
            | An_Array_Type
            =>
            -- We don't track these
            return;
         when others =>
            null;
      end case;

      -- Only discriminated types after this point

      if Is_Parameter then
         -- Case of formal parameters: Object_Declaration_View returns a name, not a definition
         Subtype_Name := Object_Declaration_View (Decl);
      else
         Subtype_Name := Subtype_Simple_Name (Def);
      end if;

      -- Not an elementary type, may be tracked if type has discriminants
      declare
         -- We can ignore 'Class and 'Base below, since it doesn't change the discriminants
         Type_Discr_Part   : constant Asis.Definition := Discriminant_Part (Corresponding_First_Subtype
                                                                            (Corresponding_Name_Declaration
                                                                             (Simple_Name
                                                                              (Strip_Attributes
                                                                               (Subtype_Name)))));
         Object_Constraint   : constant Asis.Constraint := (if Is_Parameter
                                                            then (if Definition_Kind (Def) = A_Type_Definition
                                                                  then Nil_Element
                                                                  else Subtype_Constraint (Def))
                                                            else Subtype_Constraint (Object_Declaration_View (Decl)));
         Discr_Count         : Asis.ASIS_Natural := 0;
         Is_Formal_Parameter : constant Boolean := Declaration_Kind (Decl) = A_Parameter_Specification ;
      begin
         if Is_Nil (Type_Discr_Part) or else Definition_Kind (Type_Discr_Part) = An_Unknown_Discriminant_Part then
            return;
         end if;

         -- Count discriminants
         for Discr_Decl : Asis.Declaration of Discriminants (Type_Discr_Part) loop
            Discr_Count := Discr_Count + Names (Discr_Decl)'Length;
         end loop;

         for Var_Name : Asis.Defining_Name of Names (Decl) loop
            declare
               use Discriminated_Descr_List;
               Var_Descr   : Discriminated_Variable_Descr (Discr_Count);
               Discr_Inx   : Asis_Natural := 0;
               Discr_Decls : constant Discriminant_Specification_List := Discriminants (Type_Discr_Part);
               Descr_Queue : Discriminated_Descr_List.Queue;
               Init_Expr   : constant Asis.Expression := Initialization_Expression (Decl);
            begin
               Force_New_Evaluation;
               for Discr_Decl : Asis.Declaration of Discr_Decls  loop
                  for Discr_Name : Asis.Defining_Name of Names (Discr_Decl) loop
                     Discr_Inx := Discr_Inx + 1;
                     if Is_Nil (Object_Constraint) then
                        -- Constraint from the type declaration
                        Var_Descr.Discriminants (Discr_Inx) := (First_Defining_Name (Discr_Name),
                                                                Build_Descriptor
                                                                  (Object_Declaration_View (Discr_Decl),
                                                                   (if Is_Formal_Parameter
                                                                    then Nil_Element
                                                                    else Initialization_Expression (Discr_Decl)
                                                                   )));
                     else
                        -- Constraint from the object declaration
                        declare
                           Discr_Associations : constant Asis.Association_List := Discriminant_Associations
                                                                                   (Object_Constraint,
                                                                                    Normalized => True);
                        begin
                           Var_Descr.Discriminants (Discr_Inx) := (First_Defining_Name (Discr_Name),
                                                                   Build_Descriptor
                                                                    (Object_Declaration_View (Discr_Decl),
                                                                     Discriminant_Expression
                                                                      (Discr_Associations (Discr_Inx))));
                        end;
                     end if;
                  end loop;
               end loop;

               Prepend (Descr_Queue, Var_Descr);
               Discriminated_Object_Table.Store (Var_Name, Descr_Queue);
               if not Is_Formal_Parameter and not Is_Nil (Init_Expr) then
                  -- Don't consider initialization expression of formal parameters
                  Process_Assignment (Enclosing_Element (Decl), Var_Name, Init_Expr);
               end if;
            end;
         end loop;
      end;
   exception
      when Asis.Exceptions.ASIS_Failed =>   -- Safeguard for ASIS bugs
         Utilities.Trace ("ASIS FAILED on", Decl);  --## Rule line off No_Trace
   end Process_Object_Declaration;


   ------------------
   -- Process_Path --
   ------------------

   procedure Process_Path (Path : in Asis.Path) is
      use Asis.Elements, Asis.Expressions, Asis.Statements;

      Var : Asis.Expression;
   begin
      case Path_Kind (Path) is
         when An_If_Path | An_Elsif_Path =>
            Update_Condition (Condition_Expression (Path));
         when A_Case_Path =>
            Var := Case_Expression (Enclosing_Element (Path));
            while Expression_Kind (Var) in A_Type_Conversion | A_Qualified_Expression loop
               -- Don't underestimate people who qualify a type conversion of a type conversion...
               Var := Converted_Or_Qualified_Expression (Var);
            end loop;

            if not Is_Trackable (Var) then
               return;
            end if;

            declare
               Choices     : constant Asis.Element_List := Case_Path_Alternative_Choices (Path);
               Bounds_List :          Extended_Biggest_Int_List (1 .. 2);
               Val         :          Extended_Biggest_Int;
            begin
               -- We handle only simple ranges or value
               if Choices'Length = 1 then
                  if Element_Kind (Choices (1)) = An_Expression then
                     Val := Discrete_Static_Expression_Value (Choices (1));
                     Update_Variable (Path, Var, Min => Val, Max => Val, Target => Constraint);
                  elsif Definition_Kind (Choices (1)) = A_Constraint then
                     Bounds_List := Discrete_Constraining_Values (Choices (1));
                     Update_Variable (Path, Var,
                                      Min    => Bounds_List (1),
                                      Max    => Bounds_List (2),
                                      Target => Constraint);
                  end if;
               end if;
            end;
         when others =>
            null;
      end case;
   end Process_Path;

   -----------------------
   -- Post_Process_Path --
   -----------------------

   procedure Post_Process_Path (Path : in Asis.Element) is
      use Asis.Elements;
   begin
      if Path_Kind (Path) in An_Expression_Path then
         return;
      end if;

      Clear_Invalidated_Path (Path);
      Clear_Path (Path);
   end Post_Process_Path;

   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Stmt : in Asis.Statement) is
      use Asis.Elements, Asis.Expressions, Asis.Statements;

      procedure Process_For_Loop is
         use Asis.Declarations;
         use Utilities, Simple_Descr_List;
         Val_Queue    : Simple_Descr_List.Queue;
         Loop_Spec    : constant Asis.Declaration := For_Loop_Parameter_Specification (Stmt);
         Bounds       : Expression_List (1 .. 2);
         Bound_Values : Extended_Biggest_Int_List (1 .. 2);
         Kind         : Content_Kinds;
      begin
         if Declaration_Kind (Loop_Spec) /= A_Loop_Parameter_Specification then
            -- Element iterator, generalized iterator...
            -- No idea of values of loop parameter in these cases... don't even track
            return;
         end if;

         case Type_Category (Loop_Spec, Follow_Derived => True) is
            when An_Enumeration_Type =>
               Kind := Enumerated;
            when A_Signed_Integer_Type =>
               Kind := Integer;
            when A_Modular_Type =>
               Kind := Modular;
            when Not_A_Type =>
               -- case when the loop type is a generic formal (discrete) type
               -- give up
               return;
            when others =>
               -- Not a discrete type in a for loop!!!!
               Failure ("not a discrete type: " & Type_Categories'Wide_Image (Type_Category
                                                                              (Loop_Spec, Follow_Derived => True)),
                        Loop_Spec);
         end case;
         Bounds       := Discrete_Constraining_Bounds (Loop_Spec);
         Bound_Values := (Discrete_Static_Expression_Value (Bounds (1), Wanted => Minimum),
                          Discrete_Static_Expression_Value (Bounds (2), Wanted => Maximum));

         -- Create an uninitialized variable
         Prepend (Val_Queue, (Attached_Path    => Stmt,
                              Assigned_In_Path => False,   -- Won't be assigned to anyway...,
                              Value            => (Kind             => Kind,
                                                   Declaration_Min  => Bound_Values (1),
                                                   Declaration_Max  => Bound_Values (2),
                                                   Constraint_Min   => Bound_Values (1),
                                                   Constraint_Max   => Bound_Values (2),
                                                   Assigned_Min     => Bound_Values (1),
                                                   Assigned_Max     => Bound_Values (2))
                             ));
         Simple_Object_Table.Store (Names (Loop_Spec) (1), Val_Queue);
      end Process_For_Loop;

   begin  -- Process_Statement

      -- Note: all loop statements are already invalidated from Process_Outer_Statement
      case Statement_Kind (Stmt) is
         when An_Assignment_Statement =>
            Process_Assignment (Path => Enclosing_Element        (Stmt),
                                Var  => Assignment_Variable_Name (Stmt),
                                Expr => Assignment_Expression    (Stmt));
         when A_While_Loop_Statement =>
            Update_Condition (While_Condition (Stmt));
         when A_For_Loop_Statement =>
            Process_For_Loop;
         when A_Procedure_Call_Statement | An_Entry_Call_Statement =>
            -- All variables corresponding to [in] out parameters become Unknown
            -- Special cases where we cannot determine the mode of parameters:
            -- - Attributes.
            --   There is only one attribute with an out parameter, S'Read
            --   The other parameter is an access to stream, it doesn't harm much (possible false negative)
            --   to consider that it also becomes unknown => Process all parameters
            -- - Dipatching calls
            --   A bit more annoying, but it is safer to assume that all (variable) parameters become unknown

            declare
               No_Mode_Check : constant Boolean := Expression_Kind (Called_Name (Stmt)) = An_Attribute_Reference
                                                   or else Is_Dispatching_Call (Stmt);
               Actuals       : constant Asis.Parameter_Specification_List := Call_Statement_Parameters (Stmt);
               -- No need to use a normalized associatin for actuals, since [in] out parameters are always
               -- provided explicitely
            begin
               for Inx_Param in Actuals'Range loop
                  if No_Mode_Check
                    or else Mode_Kind (Enclosing_Element (Formal_Name (Stmt, Inx_Param))) in An_Out_Mode
                                                                                           | An_In_Out_Mode
                  then
                     Process_Assignment (Path => Enclosing_Element (Stmt),
                                         Var  => Actual_Parameter (Actuals (Inx_Param)),
                                         Expr => Nil_Element);
                  end if;
               end loop;
            end;
         when others =>
            null;
      end case;
   end Process_Statement;

   ----------------------------
   -- Post_Process_Statement --
   ----------------------------

   procedure Post_Process_Statement (Stmt : in Asis.Statement) is
      use Asis.Elements;
      -- NB: we handle here only statements whose sequence of statements is not included in a path
      --     (for the latters, see Post_Process_Path)
   begin
      case Statement_Kind (Stmt) is
         when A_Loop_Statement | A_While_Loop_Statement | A_For_Loop_Statement =>
            Clear_Invalidated_Path (Stmt);
            Clear_Path (Stmt);
         when others =>
            null;
      end case;
   end Post_Process_Statement;

   ---------------------------
   -- Process_Function_Call --
   ---------------------------

   procedure Process_Function_Call (Call : in Asis.Expression) is
   -- Alas! we must consider functions with in out parameters...
      use Asis.Elements, Asis.Expressions, Asis.Statements;
      Called : constant Asis.Declaration := Corresponding_Called_Function (Call);
   begin
      if        Is_Nil (Called)
        or else Expression_Kind (Simple_Name (Prefix (Call))) = An_Attribute_Reference
        or else Is_Dispatching_Call (Call)
      then
         -- No attribute function or predefined operator has in out parameters.
         -- TBSL: we cannot get the formal name of a dispatching operation
         return;
      end if;

      declare
         Actuals : constant Asis.Parameter_Specification_List := Function_Call_Parameters (Call);
         Path    : Asis.Element := Enclosing_Element (Call);
         -- We compute Path only when an in out parameter is encountered (hopefully, very rarely!)
         -- Note that going up the expression will happen only once.
      begin
         for Inx_Param in Actuals'Range loop
            if Mode_Kind (Enclosing_Element (Formal_Name (Call, Inx_Param))) in An_Out_Mode | An_In_Out_Mode then
               while Element_Kind (Path) in An_Expression | An_Association loop
                  Path := Enclosing_Element (Path);
               end loop;
               Process_Assignment (Path => Path,
                                   Var  => Actual_Parameter (Actuals (Inx_Param)),
                                   Expr => Nil_Element);
            end if;
         end loop;
      end;
   end Process_Function_Call;

   ------------------------------
   -- Process_Type_Declaration --
   ------------------------------

   procedure Process_Type_Declaration (Decl : in Asis.Declaration) is
      use Asis.Declarations, Asis.Elements;

      subtype Discrete_Type_Kinds is Asis.Type_Kinds range An_Enumeration_Type_Definition .. A_Modular_Type_Definition;
      Descr         : Object_Value_Set;
      Ultimate_Type : constant Asis.Declaration := Ultimate_Type_Declaration (Decl);
      Kind          : Asis.Type_Kinds;
   begin
      if Is_Nil (Ultimate_Type) then
         -- Some crazy cases, like a subtype of a type declared in a limited-withed package...
         -- Give up, we know nothing about the type
         return;
      end if;

      Kind := Type_Kind (Type_Declaration_View (Ultimate_Type));
      case Kind is
         when Discrete_Type_Kinds =>
            declare
               Bounds : constant Extended_Biggest_Int_List := Discrete_Constraining_Values (Decl, RM_Static => False);
               -- Bounds'Length = 0 in some crazy cases, like T'base...
               Imin   : constant Extended_Biggest_Int := (if Bounds'Length = 0 then Not_Static else Bounds (1));
               Imax   : constant Extended_Biggest_Int := (if Bounds'Length = 0 then Not_Static else Bounds (2));
            begin
               case Discrete_Type_Kinds'(Kind) is
                  when An_Enumeration_Type_Definition =>
                     Descr := (Enumerated, Imin, Imax);
                  when A_Signed_Integer_Type_Definition =>
                     Descr := (Integer, Imin, Imax);
                  when A_Modular_Type_Definition =>
                     Descr := (Modular, Imin, Imax);
               end case;
            end;
         when An_Access_Type_Definition =>
            if Trait_Kind (Type_Declaration_View (Decl)) = A_Null_Exclusion_Trait then
               Descr := (Pointer, 1, Biggest_Int'Last);
            else
               Descr := (Pointer, 0, Biggest_Int'Last);
            end if;
         when others =>
            return;
      end case;

      Type_Table.Store (Names (Decl) (1), Descr);
   end Process_Type_Declaration;

begin  -- Framework.Object_Tracker
   Thick_Queries.Object_Value_Image := Object_Value_Image'Access;
end Framework.Object_Tracker;