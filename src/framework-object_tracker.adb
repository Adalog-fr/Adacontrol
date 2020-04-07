----------------------------------------------------------------------
--  Framework.Variable_Tracker - Package body                       --
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
   -- When a variable is assigned (including [in] out parameters), the assigned value is intersected with the
   -- declaration condition. If this is the first assignment in this path, other assigned values in enclosing
   -- paths become "unknown", since a path has other parallel paths that may (or not) change the variable.
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

   type Value_Descr is
      record
         Kind             : Content_Kinds;
         Attached_Path    : Asis.Element;  -- The path or statement or body that contains the assignment or constraint
         Declaration_Min  : Extended_Biggest_Int;
         Declaration_Max  : Extended_Biggest_Int;
         Constraint_Min   : Extended_Biggest_Int;
         Constraint_Max   : Extended_Biggest_Int;
         Assigned_Min     : Extended_Biggest_Int;
         Assigned_Max     : Extended_Biggest_Int;
         Assigned_In_Path : Boolean;
      end record;
   package Value_Descr_List is new Linear_Queue (Value_Descr);
   package Object_Table     is new Framework.Symbol_Table.Data_Access (Value_Descr_List.Queue);
   package Type_Table       is new Framework.Symbol_Table.Data_Access (Object_Value_Set);

   Intrinsically_Unknown_Paths : Element_Queues.Queue;

   ---------------------
   -- Update_Variable --
   ---------------------

   type Value_Target is (Constraint, Assigned);
   -- The Declaration_Min/Max are never updated
   procedure Update_Variable (For_Path : Asis.Element;
                              Var      : Asis.Element;
                              Target   : Value_Target;
                              Min      : Asis.Expression;
                              Max      : Asis.Expression;
                              Offset   : Thick_Queries.Biggest_Int := 0)
     -- Registers that the value of Var is known to be in range Min..Max at the current place in path For_Path
     -- If any of Min and Max is Nil_Element, it is taken to be Not_Static
   is
      use Value_Descr_List;
      use Asis.Elements;

      Good_Path : Asis.Element := For_Path;
      Var_Queue : Queue        := Object_Table.Fetch (Var, Default => Empty_Queue);
      Var_Descr : Value_Descr;
      Imin      : Extended_Biggest_Int;
      Imax      : Extended_Biggest_Int;
   begin
      if Is_Empty (Var_Queue) then
         -- Untracked variable
         return;
      end if;
      Var_Descr := Fetch (First (Var_Queue));

      -- There are never two updates of constraints for the same path.
      -- => if the target is "Constraint", Var_Descr comes from some upper path
      -- => if the target is "Assigned", Var_Descr can come from the same path (in which case it is replaced)
      --    or some upper path
      if Is_Nil (Min) then
         Imin := Not_Static;
      else
         Imin := Discrete_Static_Expression_Value (Min, Wanted => Minimum);
      end if;
      case Target is
         when Constraint =>
            if Imin = Not_Static then
               Imin := Var_Descr.Constraint_Min;
            else
               Imin := Biggest_Int'Max (Imin + Offset, Var_Descr.Constraint_Min);
            end if;
         when Assigned =>
            if Imin = Not_Static then
               Imin := Var_Descr.Declaration_Min;
            else
               Imin := Biggest_Int'Max (Imin + Offset, Var_Descr.Declaration_Min);
            end if;
      end case;

      if Is_Nil (Max) then
         Imax := Not_Static;
      else
         Imax := Discrete_Static_Expression_Value (Max, Wanted => Maximum);
      end if;
      case Target is
         when Constraint =>
            if Imax = Not_Static then
               Imax := Var_Descr.Constraint_Max;
            else
               Imax := Biggest_Int'Min (Imax + Offset, Var_Descr.Constraint_Max);
            end if;
         when Assigned =>
            if Imax = Not_Static then
               Imax := Var_Descr.Declaration_Max;
            else
               Imax := Biggest_Int'Min (Imax + Offset, Var_Descr.Declaration_Max);
            end if;
      end case;

      case Target is
         when Assigned =>
            Var_Descr.Assigned_Min   := Imin;
            Var_Descr.Assigned_Max   := Imax;
            Var_Descr.Constraint_Min := Var_Descr.Declaration_Min;
            Var_Descr.Constraint_Max := Var_Descr.Declaration_Max;
         when Constraint =>
            if not Var_Descr.Assigned_In_Path or else Var_Descr.Assigned_Min = Not_Static then
               Var_Descr.Assigned_Min := Imin;
            else
               Var_Descr.Assigned_Min := Biggest_Int'Max (Var_Descr.Assigned_Min, Imin);
            end if;
            if not Var_Descr.Assigned_In_Path or else Var_Descr.Assigned_Max = Not_Static then
               Var_Descr.Assigned_Max := Imax;
            else
               Var_Descr.Assigned_Max := Biggest_Int'Min (Var_Descr.Assigned_Max, Imax);
            end if;
            Var_Descr.Constraint_Min := Imin;
            Var_Descr.Constraint_Max := Imax;
      end case;

      -- Blocks have no effect on tracking, attach the update to the innermost existing path
      while Statement_Kind (Good_Path) = A_Block_Statement loop
         Good_Path := Enclosing_Element (Good_Path);
      end loop;

      if Is_Equal (Good_Path, Var_Descr.Attached_Path) then
         Replace (First (Var_Queue), Var_Descr);
      else
         Var_Descr.Attached_Path    := Good_Path;
         Var_Descr.Assigned_In_Path := False;
         Prepend (Var_Queue, Var_Descr);
      end if;

      if Target = Assigned and not Var_Descr.Assigned_In_Path then
         -- First modification of the variable in this path => assignments (not constraints) in all enclosing
         -- paths become unknown
         declare
            Curs : Cursor := Next (First (Var_Queue)); -- First (Var_Queue) is the current path
            Elem : Value_Descr;
         begin
            while Has_Element (Curs) loop
               Elem := Fetch (Curs);

               Elem.Assigned_Min     := Not_Static;
               Elem.Assigned_Max     := Not_Static;
               Elem.Assigned_In_Path := True;

               Replace (Curs, Elem);
               Curs := Next (Curs);
            end loop;
         end;
         Var_Descr.Assigned_In_Path := True;
         Replace (First (Var_Queue), Var_Descr);
      end if;
      Object_Table.Store (Var, Var_Queue);
   end Update_Variable;

   ----------------------
   -- Update_Condition --
   ----------------------

   procedure Update_Condition (Cond : Asis.Expression) is
      use Asis.Elements, Asis.Expressions;

      Op     : Asis.Name;
      Var    : Asis.Expression;
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
               return;
            end if;

            loop
               case Expression_Kind (Var) is
                  when A_Type_Conversion | A_Qualified_Expression =>
                     Var := Converted_Or_Qualified_Expression (Var);
                  when A_Selected_Component =>
                     Var := Selector (Var);
                  when An_Identifier =>
                     if Declaration_Kind (Corresponding_Name_Declaration (Var))
                        not in A_Variable_Declaration | A_Constant_Declaration | A_Loop_Parameter_Specification
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
                  Update_Variable (Enclosing_Element (Cond), Var, Min    => Expr,
                                                                  Max    => Expr,
                                                                  Target => Constraint);
               when A_Not_Equal_Operator =>
                  null; -- TBSL
               when A_Less_Than_Operator =>
                  Update_Variable (Enclosing_Element (Cond), Var, Min    => Nil_Element,
                                                                  Max    => Expr,
                                                                  Offset => -1,
                                                                  Target => Constraint);
               when A_Less_Than_Or_Equal_Operator =>
                  Update_Variable (Enclosing_Element (Cond), Var, Min    => Nil_Element,
                                                                  Max    => Expr,
                                                                  Target => Constraint);
               when A_Greater_Than_Operator =>
                  Update_Variable (Enclosing_Element (Cond), Var, Min    => Expr,
                                                                  Max    => Nil_Element,
                                                                  Offset => +1,
                                                                  Target => Constraint);
               when A_Greater_Than_Or_Equal_Operator =>
                  Update_Variable (Enclosing_Element (Cond), Var, Min    => Expr,
                                                                  Max    => Nil_Element,
                                                                  Target => Constraint);
            end case;
         when An_In_Membership_Test =>
            Var := Simple_Name (Membership_Test_Expression (Cond));
            if not Is_Static_Object (Var) then
               return;
            end if;

            loop
               case Expression_Kind (Var) is
                  when A_Type_Conversion | A_Qualified_Expression =>
                     Var := Converted_Or_Qualified_Expression (Var);
                  when A_Selected_Component =>
                     Var := Selector (Var);
                  when An_Identifier =>
                     if Declaration_Kind (Corresponding_Name_Declaration (Var))
                        not in A_Variable_Declaration | A_Constant_Declaration | A_Loop_Parameter_Specification
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
               Bounds_List :          Asis.Element_List (1..2);
            begin
               -- We handle only simple ranges
               if Choices'Length /= 1 or else Definition_Kind (Choices (1)) /= A_Constraint then
                  return;
               end if;
               Bounds_List := Discrete_Constraining_Bounds (Choices (1));
               Update_Variable (Enclosing_Element (Cond), Var, Min    => Bounds_List (1),
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
      use Symbol_Table, Object_Table;

      procedure Clean_One_Variable (Entity : Asis.Defining_Name; Value_Queue : in out Value_Descr_List.Queue) is
         pragma Unreferenced (Entity);
         use Asis.Elements;
         use Value_Descr_List;

         Curs : constant Cursor := First (Value_Queue);
      begin
         -- Curs may be empty if the variable has already been cleaned due to a labeled statement
         if Has_Element (Curs) and then Is_Equal (Path, Fetch (Curs).Attached_Path) then
            -- The variable has been modified in the current path, remove the path
            Clear (Value_Queue, 1);
         end if;
      end Clean_One_Variable;
      procedure Clean_All_Variables is new On_Every_Entity_From_Scope (Clean_One_Variable);
   begin  -- Clear_Path
      Clean_All_Variables (All_Scopes);
   end Clear_Path;


   ----------------------
   -- Untrack_Variable --
   ----------------------

   procedure Untrack_Variable (Var : Asis.Expression) is
   -- Delete this variable with its associated assignment list from the variable table
      use Value_Descr_List;
      Queue : Value_Descr_List.Queue;
   begin
      if not Object_Table.Is_Present (Var) then
         return;
      end if;

      Queue := Object_Table.Fetch (Var);
      Clear (Queue);
      Object_Table.Delete (Var);
   end Untrack_Variable;

   ------------------------
   -- Process_Assignment --
   ------------------------

   procedure Process_Assignment (Path : Asis.Element; Var : Asis.Expression; Expr : Asis.Expression) is
      use Asis.Declarations, Asis.Elements, Asis.Expressions;
      Good_Var     : Asis.Expression;
      Current_Unit : Asis.Declaration;
   begin
      case Expression_Kind (Var) is
         when An_Identifier =>
            Good_Var := Ultimate_Name (Var);
         when A_Selected_Component =>
            if not Is_Expanded_Name (Var) then
               -- record component...
               return;
            end if;
            Good_Var := Ultimate_Name (Var);
         when others =>
            -- indexed variable, dereferences...
            return;
      end case;
      if Is_Nil (Good_Var)  -- Name includes a dereference
        or else Declaration_Kind (Corresponding_Name_Declaration (Good_Var)) = A_Component_Declaration
      then
         return;
      end if;

      if not Object_Table.Is_Present (Good_Var) then  -- not tracked
         return;
      end if;

      Current_Unit := Path;
      if Element_Kind (Current_Unit) in A_Statement | A_Path | An_Exception_Handler then
         Current_Unit := Enclosing_Program_Unit (Path);
      else
         -- It is already a program unit
         Current_Unit := Names (Current_Unit)(1);
      end if;
      if not Is_Equal (Enclosing_Program_Unit (Corresponding_Name_Declaration (Good_Var)), Current_Unit) then
         -- Modified from nested unit => Give up
         Untrack_Variable (Good_Var);
         return;
      end if;

      Update_Variable (Path, Good_Var, Min => Expr, Max => Expr, Target => Assigned);
   end Process_Assignment;


   --------------------------------------------------------------------------------
   -- Exported elements
   --------------------------------------------------------------------------------

   ------------------
   -- Object_Value --
   ------------------

   function Object_Value (Var : Asis.Element) return Object_Value_Set is
      use Asis.Elements, Asis.Expressions;
      use Element_Queues, Value_Descr_List;

      Good_Decl : Asis.Declaration;
      Good_Var  : constant Asis.Expression        := Ultimate_Name (Var);
      Var_Queue : constant Value_Descr_List.Queue := Object_Table.Fetch (Good_Var,
                                                                         Default => Value_Descr_List.Empty_Queue);
      Descr          : Value_Descr;
      Forced_Unknown : Boolean;

      function Var_Controls_Path (Path : Asis.Element) return Boolean is
         Curs : Value_Descr_List.Cursor := First (Var_Queue);
         D    : Value_Descr;
      begin
         while Has_Element (Curs) loop
            D := Fetch (Curs);
            if Is_Equal (D.Attached_Path, Path) then
               return not D.Assigned_In_Path;
            end if;
            Curs := Next (Curs);
         end loop;
         return False;
      end Var_Controls_Path;
   begin  -- Object_Value
      if Is_Empty (Var_Queue) then
         -- Untracked variable
         return Unknown_Value (Untracked);
      end if;

      Descr     := Fetch (First (Var_Queue));
      Good_Decl := Corresponding_Name_Declaration (Good_Var);
      if Declaration_Kind (Good_Decl) not in A_Constant_Declaration | A_Loop_Parameter_Specification
        and then not Is_Equal (Enclosing_Program_Unit (Good_Decl), Enclosing_Program_Unit (Var))
      then
         -- A variable accessed from a different unit than where the variable is declared
         -- We don't know where this subprogram is called from, therefore it is an
         -- unknown value, unless the enclosing program unit is an expression function (macro model)
         if Declaration_Kind (Enclosing_Element (Enclosing_Program_Unit (Var))) /= An_Expression_Function_Declaration
         then
            return Unknown_Value (Descr.Kind);
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
      elsif Descr.Assigned_In_Path and Is_Equal (Descr.Attached_Path,Fetch (First (Intrinsically_Unknown_Paths))) then
         Forced_Unknown := False;
      elsif Var_Controls_Path (Fetch (First (Intrinsically_Unknown_Paths))) then
         -- The constraint on the variable comes from the condition of the unknown path
         Forced_Unknown := False;
      else
         -- Not assigned in the innermost intrinsically unknown path
         Forced_Unknown := True;
      end if;

      -- If Forced_Unknown, the only thing we can trust is the constraint in the declaration of the variable
      case Descr.Kind is
         when Enumerated =>
            if Forced_Unknown then
               return (Kind => Enumerated, Imin => Descr.Declaration_Min, Imax => Descr.Declaration_Max);
            else
               return (Kind => Enumerated, Imin => Descr.Assigned_Min,   Imax => Descr.Assigned_Max);
            end if;
         when Integer =>
            if Forced_Unknown then
               return (Kind => Integer, Imin => Descr.Declaration_Min, Imax => Descr.Declaration_Max);
            else
               return (Kind => Integer, Imin => Descr.Assigned_Min,   Imax => Descr.Assigned_Max);
            end if;
         when Modular =>
            if Forced_Unknown then
               return (Kind => Modular, Imin => Descr.Declaration_Min, Imax => Descr.Declaration_Max);
            else
               return (Kind => Modular, Imin => Descr.Assigned_Min,   Imax => Descr.Assigned_Max);
            end if;
         when Pointer =>
            if Forced_Unknown then
               return (Kind => Pointer, Imin => Descr.Declaration_Min, Imax => Descr.Declaration_Max);
            else
               return (Kind => Pointer, Imin => Descr.Assigned_Min,   Imax => Descr.Assigned_Max);
            end if;
         when Untracked =>
            return (Kind => Untracked);
      end case;
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
                     -- indexed variable, dereferences...
                     return;
               end case;
               if Is_Nil (Good_Var) then -- Name includes a dereference
                  return;
               end if;

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
         Invalidate_Path (Enclosing_Element (Stmt));
         Clear_Path      (Enclosing_Element (Stmt));
      end if;

      if Statement_Kind (Stmt) in A_Loop_Statement | A_While_Loop_Statement | A_For_Loop_Statement
      then
         Invalidate_Path (Stmt);
      end if;
   end Process_Outer_Statement;

   --------------------------------
   -- Process_Object_Declaration --
   --------------------------------

   -- TBSL: formal parameters (?)
   procedure Process_Object_Declaration (Decl : in Asis.Declaration) is
      use Asis.Declarations, Asis.Definitions, Asis.Elements;

      Descriptor       : Value_Descr;
      Range_Descriptor : Object_Value_Set;
      Initial_Value    : Extended_Biggest_Int;
      Def              : Asis.Definition;
      Bounds           : Extended_Biggest_Int_List (1 .. 2);
   begin
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

      Def := Object_Declaration_View (Decl);
      case Type_Category (Decl) is
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
            return;
      end case;
      case Descriptor.Kind is
         when Discrete_Content_Kinds =>
            if Is_Nil (Subtype_Constraint (Def)) then
               -- Get the bounds from the type
               Range_Descriptor := Type_Table.Fetch (Subtype_Simple_Name (Def), Default => (Kind => Untracked));
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
               Bounds                     := Discrete_Constraining_Values (Def);
               Descriptor.Declaration_Min := Bounds (1);
               Descriptor.Declaration_Max := Bounds (2);
            end if;
            Descriptor.Constraint_Min := Descriptor.Declaration_Min;
            Descriptor.Constraint_Max := Descriptor.Declaration_Max;

            Initial_Value := Discrete_Static_Expression_Value (Initialization_Expression (Decl));
            if Initial_Value = Not_Static then
               Descriptor.Assigned_Min := Descriptor.Constraint_Min;
               Descriptor.Assigned_Max := Descriptor.Constraint_Max;
            else
               Descriptor.Assigned_Min := Initial_Value;
               Descriptor.Assigned_Max := Initial_Value;
            end if;

            for Name : Asis.Name of Names (Decl) loop
               declare
                  use Value_Descr_List;
                  Val_Queue : Value_Descr_List.Queue;
               begin
                  Prepend (Val_Queue, Descriptor);
                  Object_Table.Store (Name, Val_Queue);
               end;
            end loop;

         when Pointer =>
            if Trait_Kind (Def) = A_Null_Exclusion_Trait then
               Descriptor.Declaration_Min := 1;
            elsif Definition_Kind (Def) = An_Access_Definition then
               -- Anonymous access type, no null exclusion
               Descriptor.Declaration_Min := 0;
            else
               -- The type may not be in table, if from some package that has not been analyzed (standard library f.e.)
               Range_Descriptor := Type_Table.Fetch (Subtype_Simple_Name (Def),
                                                     Default => (Kind => Pointer,
                                                                 Imin => 0,
                                                                 Imax => Biggest_Int'Last));
               Descriptor.Declaration_Min := Range_Descriptor.Imin;
            end if;
            Descriptor.Declaration_Max := Biggest_Int'Last;           -- Always for pointers
            Descriptor.Constraint_Min  := Descriptor.Declaration_Min;
            Descriptor.Constraint_Max  := Descriptor.Declaration_Max;

            -- The case of an access object initialized by an allocator is the only case of a "static"
            -- (well, evaluatable) expression with side effect. We must therefore recompute it for every
            -- name
            for Name : Asis.Name of Names (Decl) loop
               if Is_Nil (Initialization_Expression (Decl)) then
                  Initial_Value := 0;  -- null is the default for access types
               else
                  Initial_Value := Discrete_Static_Expression_Value (Initialization_Expression (Decl));
               end if;

               if Initial_Value = Not_Static then
                  Descriptor.Assigned_Min := Descriptor.Constraint_Min;
                  Descriptor.Assigned_Max := Descriptor.Constraint_Max;
               else
                  Descriptor.Assigned_Min := Initial_Value;
                  Descriptor.Assigned_Max := Initial_Value; -- Since Constraint_Max is always Biggest_Int'Last
               end if;

               declare
                  use Value_Descr_List;
                  Val_Queue : Value_Descr_List.Queue;
               begin
                  Prepend (Val_Queue, Descriptor);
                  Object_Table.Store (Name, Val_Queue);
               end;
            end loop;

         when others =>
            -- Not an elementary type, not tracked
            return;
      end case;

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
            Var := Simple_Name (Case_Expression (Enclosing_Element (Path)));
            while Expression_Kind (Var) in A_Type_Conversion | A_Qualified_Expression loop
               -- Don't underestimate people who qualify a type conversion of a type conversion...
               Var := Converted_Or_Qualified_Expression (Var);
            end loop;

            -- An indexed component can be static, but we don't track it
            if Expression_Kind (Var) /= An_Indexed_Component and then Is_Static_Object (Var) then
               declare
                  Choices     : constant Asis.Element_List := Case_Path_Alternative_Choices (Path);
                  Bounds_List :          Asis.Element_List (1 .. 2);
               begin
                  -- We handle only simple ranges or value
                  if Choices'Length = 1 then
                     if Element_Kind (Choices (1)) = An_Expression then
                        Update_Variable (Path, Var,
                                         Min    => Choices (1),
                                         Max    => Choices (1),
                                         Target => Constraint);
                     elsif Definition_Kind (Choices (1)) = A_Constraint then
                        Bounds_List := Discrete_Constraining_Bounds (Choices (1));
                        Update_Variable (Path, Var,
                                         Min    => Bounds_List (1),
                                         Max    => Bounds_List (2),
                                         Target => Constraint);
                     end if;
                  end if;
               end;
            end if;
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
         use Utilities, Value_Descr_List;
         Val_Queue    : Value_Descr_List.Queue;
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
         Prepend (Val_Queue, (Kind             => Kind,
                              Attached_Path    => Stmt,
                              Declaration_Min  => Bound_Values (1),
                              Declaration_Max  => Bound_Values (2),
                              Constraint_Min   => Bound_Values (1),
                              Constraint_Max   => Bound_Values (2),
                              Assigned_Min     => Bound_Values (1),
                              Assigned_Max     => Bound_Values (2),
                              Assigned_In_Path => False   -- Won't be assigned to anyway...
                             ));
         Object_Table.Store (Names (Loop_Spec) (1), Val_Queue);
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
            if Is_Dispatching_Call (Stmt) or Expression_Kind (Called_Name (Stmt)) = An_Attribute_Reference then
               -- TBSL: we cannot get the formal name of a dispatching operation or attribute. False positive?
               return;
            end if;

            declare
               Actuals : constant Asis.Parameter_Specification_List := Call_Statement_Parameters (Stmt);
            begin
               for Inx_Param in Actuals'Range loop
                  if Mode_Kind (Enclosing_Element (Formal_Name (Stmt, Inx_Param))) in An_Out_Mode | An_In_Out_Mode
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
         when others =>
            null;
      end case;
      Clear_Path (Stmt);
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
               Bounds : constant Extended_Biggest_Int_List := Discrete_Constraining_Values (Decl, Static_Only => False);
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
