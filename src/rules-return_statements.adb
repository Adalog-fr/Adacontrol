----------------------------------------------------------------------
--  Rules.Statements - Package body                                 --
--                                                                  --
--  This software is (c) Adalog 2004-2018.                          --
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
  Asis.Elements;

-- Adalog
with
  Scope_Manager,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Return_Statements is
   use Framework, Framework.Language.Shared_Keys;

   -- Algorithm
   --
   -- A scoped store keeps counts of encountered return statements in the scope of the returned subprogram
   --  - The count is associated to the procedure itself for returns from the sequence of statements (including
   --    returns nested in block statements that constitute a different scope)
   --  - The count is associated to the innermost handler for returns located in an exception handler
   -- For depth, each return statement is checked when it appears.
   -- For *count Max, each return statement in excess of what is allowed is reported at the place where it appears.
   -- For *count Min, the count is checked when exiting the scope of the corresponding callable or block.
   --
   -- Tricks:
   -- A handler can contain a block, having returns in the regular sequence and in the handlers...
   -- A return is considered as belonging to a handler if it is included in at least one handler, the count is
   --   associated to the innermost handler.

   type Subrules is (Sr_Depth, Sr_Regular_Count, Sr_Handler_Count);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags  => Subrules,
                                                                             Prefix => "Sr_" );

   type Callables is (C_Procedure, C_Function, C_Entry);
   package Callables_Flag_Utilities  is new Framework.Language.Flag_Utilities (Callables, Prefix => "C_");

   type Callable_Usage is array (Callables) of Control_Kinds_Set;
   type Usage is array (Subrules) of Callable_Usage;
   No_Rule_Used : constant Usage := (others => (others => Empty_Control_Kinds_Set));
   Rule_Used : Usage := No_Rule_Used;
   Save_Used : Usage;

   Ctl_Labels : array (Subrules, Callables, Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Ctl_Values : array (Subrules, Callables, Control_Kinds) of Bounds_Values
     := (others => (others => (others => Unlimited_Bounds)));

   ---------- Declarations for subrules Count and Handler_Counts
   type Sequence_Counter is
      record
         Sequence : Asis.Element;
         Count    : Thick_Queries.Biggest_Natural := 0;
      end record;
   function Equivalent_Counters (Left, Right : Sequence_Counter) return Boolean;
   package Count_Store is new Scope_Manager.Scoped_Store (Data            => Sequence_Counter,
                                                          Equivalent_Keys => Equivalent_Counters);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control number and depth of return statements");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags ("Parameter (1): ");
      User_Message ("Parameter (2..3): [<bound>] <value>");
      Help_On_Bounds ("<bound>: ");
      User_Message   ("         (at least one parameter required)");
      Callables_Flag_Utilities.Help_On_Flags ("Other parameters: ", "(optional)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Framework.Language, Callables_Flag_Utilities, Subrules_Flag_Utilities, Utilities;
      use Ada.Strings.Wide_Unbounded;
      Subrule  : Subrules;
      Callable : Callables;
      Value    : Bounds_Values;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "subrule not specified");
      end if;

      Subrule  := Get_Flag_Parameter (Allow_Any => False);

      if Parameter_Exists then
         Value := Get_Bounds_Parameters (Rule_Id, Allow_Single => True);
      else
         Parameter_Error (Rule_Id, "missing bounds of allowed value");
      end if;

      if Parameter_Exists then
         while Parameter_Exists loop
            Callable := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Subrule) (Callable) (Ctl_Kind) then
               Parameter_Error (Rule_Id, Image (Callable, Lower_Case)
                                & " already specified for "
                                & Control_Kinds'Wide_Image (Ctl_Kind));
            end if;
            Ctl_Labels (Subrule,  Callable,  Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Ctl_Values (Subrule,  Callable,  Ctl_Kind) := Value;
            Rule_Used  (Subrule) (Callable) (Ctl_Kind) := True;
         end loop;
      else
         -- no callable kind specified => applies to all
         for C in Callables loop
            Ctl_Labels (Subrule, C, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Ctl_Values (Subrule, C, Ctl_Kind) := Value;
            if Rule_Used (Subrule) (C) (Ctl_Kind) then
               Parameter_Error (Rule_Id, Image (Callable, Lower_Case)
                                & " already specified for "
                                & Control_Kinds'Wide_Image (Ctl_Kind));
            end if;
            Rule_Used  (Subrule) (C) (Ctl_Kind) := True;
         end loop;
      end if;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := No_Rule_Used;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Count_Store.Activate;
   end Prepare;

   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Subrule  : Subrules;
                        Ctl_Kind : Control_Kinds;
                        Loc      : Locations.Location;
                        Entity   : Callables;
                        Value    : Thick_Queries.Biggest_Int)
   is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Reports, Thick_Queries;

   begin
      case Subrule is
         when Sr_Depth =>
            Report (Rule_Id,
                    To_Wide_String (Ctl_Labels (Sr_Depth, Entity, Ctl_Kind)),
                    Ctl_Kind,
                    Loc,
                    "Depth of return statement is "
                    & Bound_Image (Ctl_Values (Sr_Depth, Entity, Ctl_Kind))
                    & " ("   & Biggest_Int_Img (Value) & ')');
         when Sr_Regular_Count =>
            Report (Rule_Id,
                    To_Wide_String (Ctl_Labels (Sr_Regular_Count, Entity, Ctl_Kind)),
                    Ctl_Kind,
                    Loc,
                    "number of return statements is "
                    & Bound_Image (Ctl_Values (Sr_Regular_Count, Entity, Ctl_Kind))
                    & " ("   & Biggest_Int_Img (Value) & ')');
         when Sr_Handler_Count =>
            Report (Rule_Id,
                    To_Wide_String (Ctl_Labels (Sr_Handler_Count, Entity, Ctl_Kind)),
                    Ctl_Kind,
                    Loc,
                    "number of return statements in handler is "
                    & Bound_Image (Ctl_Values (Sr_Handler_Count, Entity, Ctl_Kind))
                    & " ("   & Biggest_Int_Img (Value) & ')');
      end case;
   end Do_Report;

   -------------------------
   -- Equivalent_Counters --
   -------------------------

   function Equivalent_Counters (Left, Right : Sequence_Counter) return Boolean is
      (Asis.Elements.Is_Equal (Left.Sequence, Right.Sequence));

   ------------------------------
   -- Process_Return_Statement --
   ------------------------------

   procedure Process_Return_Statement (Stmt : in Asis.Statement) is
      use Asis, Asis.Elements;
      use Thick_Queries, Utilities;

      Returned_From : Asis.Declaration;
      Handler       : Asis.Exception_Handler := Nil_Element;
      This_Callable : Callables;

      procedure Check_Depth is
         use Framework.Locations;

         Depth : Thick_Queries.Biggest_Natural := 0;
         Elem  : Asis.Element;
      begin
         Elem := Enclosing_Element (Stmt);
         loop
            case Element_Kind (Elem) is
               when A_Statement =>
                  if Statement_Kind (Elem) = An_Extended_Return_Statement then
                     -- This is a return from an extended return statement => Ignore
                     return;
                  end if;
                  Depth := Depth + 1;
               when A_Path =>
                  null;
               when others =>
                  exit;
            end case;
            Elem  := Enclosing_Element (Elem);
         end loop;

         if not Is_In (Depth, Ctl_Values (Sr_Depth, This_Callable, Check)) then
            Do_Report (Sr_Depth, Check, Get_Location (Stmt), This_Callable, Depth);
         elsif not Is_In (Depth, Ctl_Values (Sr_Depth, This_Callable, Search)) then
            Do_Report (Sr_Depth, Search, Get_Location (Stmt), This_Callable, Depth);
         end if;

         if not Is_In (Depth, Ctl_Values (Sr_Depth, This_Callable, Count)) then
            Do_Report (Sr_Depth, Count, Get_Location (Stmt), This_Callable, Depth);
         end if;
      end Check_Depth;

      procedure Check_Count (Subrule : Subrules; Key : Asis.Element) is
         use Framework.Locations, Scope_Manager;
         Data : Sequence_Counter := (Key, 0);
         Loc : constant Location := Get_Location (Stmt);
      begin
         Count_Store.Reset (Data, Current_Scope_Only);
         -- Must be found, since initialized at beginning of scope. If not, this will raise an exception in
         -- the following code, so we'll know there is a bug.

         Data       := Count_Store.Current_Data;
         Data.Count := Data.Count + 1;
         Count_Store.Update_Current (Data);

         if not Is_In (Data.Count, Ctl_Values (Subrule, This_Callable, Check)) then
            Do_Report (Subrule, Check, Loc, This_Callable, Data.Count);
         elsif not Is_In (Data.Count, Ctl_Values (Subrule, This_Callable, Search)) then
            Do_Report (Subrule, Search, Loc, This_Callable, Data.Count);
         end if;

         if not Is_In (Data.Count, Ctl_Values (Subrule, This_Callable, Count)) then
            Do_Report (Subrule, Count, Loc, This_Callable, Data.Count);
         end if;
      end Check_Count;

   begin  -- Process_Return_Statement
      if Rule_Used = (Subrules => (Callables => Empty_Control_Kinds_Set)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Identify the callable entity we are returning from, and the exception handler if any.
      -- Handler is Nil_Element if the return is in the normal sequence of statements.
      Returned_From := Enclosing_Element (Stmt);
      loop
         case Element_Kind (Returned_From) is
            when An_Exception_Handler =>  -- if nested handlers, consider only the first one
               if Is_Nil (Handler) then
                  Handler := Returned_From;
               end if;
            when A_Statement =>
               case Statement_Kind (Returned_From) is
                  when An_Accept_Statement =>
                     This_Callable := C_Entry;
                     exit;
                  when An_Extended_Return_Statement =>
                     -- Return nested in extended_return : ignore
                     return;
                  when others =>
                     null;
               end case;
            when A_Path =>
               null;
            when A_Declaration =>
               case Declaration_Kind (Returned_From) is
                  when A_Procedure_Body_Declaration =>
                     This_Callable := C_Procedure;
                     exit;
                  when A_Function_Body_Declaration =>
                     This_Callable := C_Function;
                     exit;
                  when An_Entry_Body_Declaration =>
                     This_Callable := C_Entry;
                     exit;
                  when others =>
                     Failure ("Return_Statements: unexpected enclosing declaration", Returned_From);
               end case;
            when others =>
               Failure ("Return_Statements: unexpected enclosing element", Returned_From);
         end case;
         Returned_From := Enclosing_Element (Returned_From);
      end loop;

      if Rule_Used (Sr_Depth) (This_Callable) /= Empty_Control_Kinds_Set then
         Check_Depth;
      end if;

      if Is_Nil (Handler) then
         if Rule_Used (Sr_Regular_Count) (This_Callable) /= Empty_Control_Kinds_Set then
            Check_Count (Sr_Regular_Count, Returned_From);
         end if;
      else
         if Rule_Used (Sr_Handler_Count) (This_Callable) /= Empty_Control_Kinds_Set then
            Check_Count (Sr_Handler_Count, Handler);
         end if;
      end if;

   end Process_Return_Statement;

   ------------------------
   -- Initialize_Counter --
   ------------------------

   procedure Initialize_Counter (Elem : in Asis.Element) is
      use Asis, Asis.Elements;
   begin
      if   Rule_Used (Sr_Regular_Count) = (Callables => Empty_Control_Kinds_Set)
        or Rule_Used (Sr_Handler_Count) = (Callables => Empty_Control_Kinds_Set)
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Return within block regular statements are counted as part of the enclosing callable,
      -- but their exception handlers have their own counts
      if Statement_Kind (Elem) /= A_Block_Statement then
         Count_Store.Push ((Elem, 0));
      end if;
      for H : Exception_Handler of Thick_Queries.Exception_Handlers (Elem) loop
         Count_Store.Push ((H, 0));
      end loop;
   end Initialize_Counter;

   ----------------------
   -- Finalize_Counter --
   ----------------------

   procedure Finalize_Counter (Elem : in Asis.Element) is
      use Asis, Asis.Elements;
      use Framework.Locations, Thick_Queries, Utilities;


      Callable_Elem : Asis.Element;
      This_Callable : Callables;

      procedure Check_Count (Subrule : Subrules; Loc : Location; Key : Asis.Element) is
         use Scope_Manager;
         Data : Sequence_Counter := (Key, 0);
      begin
         Count_Store.Reset (Data, Current_Scope_Only);
         -- Must be found, since initialized at beginning of scope. If not, this will raise an exception in
         -- the following code, so we'll know there is a bug.

         Data := Count_Store.Current_Data;

         -- Check only minimal value, maximum has been check by Process_Return_Statement
         if Data.Count < Ctl_Values (Subrule, This_Callable, Check).Min then
            Do_Report (Subrule, Check, Loc, This_Callable, Data.Count);
         elsif Data.Count < Ctl_Values (Subrule, This_Callable, Search).Min then
            Do_Report (Subrule, Search, Loc, This_Callable, Data.Count);
         end if;

         if Data.Count < Ctl_Values (Subrule, This_Callable, Count).Min then
            Do_Report (Subrule, Count, Loc, This_Callable, Data.Count);
         end if;
      end Check_Count;

      Loc : Location;
   begin  -- Finalize_Counter
      if   Rule_Used (Sr_Regular_Count) = (Callables => Empty_Control_Kinds_Set)
        or Rule_Used (Sr_Handler_Count) = (Callables => Empty_Control_Kinds_Set)
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Statement_Kind (Elem) = A_Block_Statement then
         Callable_Elem := Enclosing_Element (Enclosing_Program_Unit (Elem));
         if not Is_Callable_Construct (Callable_Elem) then
            -- Block in the statements of a package, task... There can't be any return here
            return;
         end if;
      else
         Callable_Elem := Elem;
      end if;

      case Declaration_Kind (Callable_Elem) is
         when A_Procedure_Body_Declaration =>
            This_Callable := C_Procedure;
         when A_Function_Body_Declaration =>
            This_Callable := C_Function;
         when An_Entry_Body_Declaration =>
            This_Callable := C_Entry;
         when others =>
            if Statement_Kind (Callable_Elem) = An_Accept_Statement then
               This_Callable := C_Entry;
            else
               Failure ("Return_Statements: bad elem in finalization", Callable_Elem);
            end if;
      end case;

      if Statement_Kind (Elem) /= A_Block_Statement then
         declare
            Stmts : constant Asis.Statement_List := Thick_Queries.Statements (Elem);
         begin
            if Stmts = Nil_Element_List then
               -- case of accept without a body
               Loc := Get_Location (Elem);
            else
               Loc := Get_Location (Stmts (1));
            end if;
            Check_Count (Sr_Regular_Count, Loc, Elem);
         end;
      end if;
      for H : Exception_Handler of Thick_Queries.Exception_Handlers (Elem) loop
         Check_Count (Sr_Handler_Count, Get_Location (H), H);
      end loop;
   end Finalize_Counter;

begin  -- Rules.Return_Statements
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Return_Statements;
