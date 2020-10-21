----------------------------------------------------------------------
--  Rules.Unsafe_Paired_Calls - Package body                        --
--                                                                  --
--  This software is (c) Adalog 2004-2016.                          --
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
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  Rules.Unsafe_Paired_Calls.Services,
  Scope_Manager,
  Thick_Queries,
  Utilities;

package body Rules.Unsafe_Paired_Calls is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- Analysis starts from (any) procedure or entry call.
   -- First identify if the call is an opening call, a closing call, or anything else
   -- (exit immediately if the latter).
   --
   -- For opening calls, check:
   --    - That the call is the first statement in a sequence
   --    - That there is no call for the same lock in an enclosing scope (opening calls are kept in a scoped store)
   --    - That the current sequence of statements is terminated by a matching closing call
   --    - That there is an exception part with a "when others" handler
   --    - That every handler includes exactly one closing call
   -- For closing calls check:
   --    - That the call is the last statemement in a sequence, except for possible return, exit and null,
   --    - Or that the call is followed by a return, exit, or goto statement (whether the statement transfers
   --      control out of the scope of an opening call will be checked when traversing this statement)
   --    - That the current (or exited) sequence of statements starts with a matching opening call
   --
   -- In addition, for every return, exit, or goto statement that transfers control outside of the scope of an
   -- opening call:
   --    - Check that the previous statement is a closing call (the match with the opening call has already been
   --      tested)
   --    - Check that only one opening call scope is left
   --
   -- Note that the correspondance must be checked for both opening and closing calls, for the case where an opening
   -- call has no closing call, or a closing call has no opening call

   Rules_Used : Control_Index := 0;
   Save_Used  : Control_Index;

   type SP_Role is (Opening, Closing);
   type SP_Lock_Parameter_Kind is (None, Entity_Spec, In_Def, In_Out_Def);
   type Lock_Parameter (Kind: SP_Lock_Parameter_Kind := None) is
      record
         case Kind is
            when None =>
               null;
            when Entity_Spec =>
               Position : Locations.Location;
               Entity   : Entity_Specification;
            when In_Def | In_Out_Def =>
               Formal : Asis.Defining_Name;
         end case;
      end record;
   type SP_Context is new Basic_Rule_Context with
      record
         Role         : SP_Role;
         Rule_Numbers : Control_Index_Set;
         Lock         : Lock_Parameter;
      end record;

   Checked_Subprograms  : Context_Store;
   package Active_Procs is new Scope_Manager.Scoped_Store (Asis.Element,
                                                           Equivalent_Keys => Asis.Elements.Is_Equal);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Framework.Variables, Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls calls like P/V operations that are not safely paired");
      User_Message;
      User_Message ("Parameter(1): First subprogram");
      User_Message ("Parameter(2): Second subprogram");
      User_Message ("Parameter(3): (optional) type of lock parameter");
      User_Message;
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Conditionals_Allowed");
      Help_On_Variable (Rule_Id & ".Name_As_Given");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Framework.Locations, Utilities, Ada.Strings.Wide_Fixed;
      First_SP  : Entity_Specification;
      Second_SP : Entity_Specification;
      Lock_Type : Entity_Specification;
      Lock_Pos  : Location;

      procedure Associate_With_Set (Specification : in Entity_Specification;
                                    Role          : in SP_Role;
                                    LP            : in Lock_Parameter)
      is
         Existing  : Root_Context'Class := Association (Checked_Subprograms, Specification);
         Rules_Set : Control_Index_Set := (others => False);
      begin
         if Existing = No_Matching_Context then
            Rules_Set (Rules_Used) := True;
            Associate (Checked_Subprograms,
                       Specification,
                       SP_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Role, Rules_Set, LP));
         else
            SP_Context (Existing).Rule_Numbers (Rules_Used) := True;
            Update (Checked_Subprograms, Existing);
         end if;
      end Associate_With_Set;

   begin  -- Add_Control
      if Rules_Used = Control_Index_Set'Last then
         Parameter_Error (Rule_Id,
                          "this rule can be given at most"
                          & Control_Index'Wide_Image(Control_Index_Set'Last)
                          & " times");
      end if;
      Rules_Used := Rules_Used + 1;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "first subprogram missing");
      end if;
      First_SP := Get_Entity_Parameter;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "Second subprogram missing");
      end if;
      Second_SP := Get_Entity_Parameter;

      if Parameter_Exists then
         Lock_Pos  := Source_Location;
         Lock_Type := Get_Entity_Parameter;
         if Parameter_Exists then
            Parameter_Error (Rule_Id, "spurious parameter after type name");
         end if;
         if Index (To_Upper (Image (Lock_Type)), "'CLASS") /= 0 then
            Parameter_Error (Rule_Id, "class wide type not allowed for lock parameter");
         end if;

         Associate_With_Set (First_SP,  Opening, (Entity_Spec, Lock_Pos, Lock_Type));
         Associate_With_Set (Second_SP, Closing, (Entity_Spec, Lock_Pos, Lock_Type));
      else
         Associate_With_Set (First_SP,  Opening, (Kind => None));
         Associate_With_Set (Second_SP, Closing, (Kind => None));
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
            Rules_Used := 0;
            Clear (Checked_Subprograms);
            Conditionals_Allowed := (Value => On);
            Name_As_Given        := (Value => Off);
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
      if Rules_Used = 0 then
         return;
      end if;

      Balance (Checked_Subprograms);
      Active_Procs.Activate;
   end Prepare;

   --------------
   -- Key_Name --
   --------------

   function Key_Name (Call : Asis.Statement) return Asis.Name is
      use Thick_Queries;
   begin
      if Name_As_Given.Value = On then
         return Called_Simple_Name (Call);
      else
         return Ultimate_Name (Called_Simple_Name (Call));
      end if;
   end Key_Name;

   ------------------
   -- Call_Context --
   ------------------

   function Call_Context (Call : Asis.Statement) return Root_Context'Class is
      use Asis.Elements;
   begin
      if Is_Nil (Call) then
         return No_Matching_Context;
      end if;
      return Matching_Context (Checked_Subprograms, Key_Name (Call));
   end Call_Context;

   ---------------------------
   -- Update_Lock_Parameter --
   ---------------------------

   procedure Update_Lock_Parameter (Lock_Call : in Asis.Element; Lock_Context : in out SP_Context) is
   -- Initially, a SP_Context has Entity_Specification for its Lock.Kind field.
   -- We must delay analyzing the lock until we have a way of getting to the corresponding
   -- element, i.e. the first time we have a call to the procedure.
   -- This procedure updates the Lock field of Lock_Context according to the provided Lock_Call
      use Framework.Language, Thick_Queries, Utilities;
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
   begin
      if Lock_Context.Lock.Kind /= Entity_Spec then
         -- Already transformed (or None)
         return;
      end if;

      declare
         Mark : Asis.Expression;
      begin
         -- Note that we iterate through all parameters, and that we transform Lock_Context
         -- as soon as we find a parameter of the appropriate type.
         -- This is intended to diagnose the case where more than one parameter is of the
         -- provided type.
         for Spec : Asis.Parameter_Specification of Called_Profile (Lock_Call) loop
            Mark := Simple_Name (Declaration_Subtype_Mark (Spec));
            if Matches (Lock_Context.Lock.Entity, Mark) then
               if Lock_Context.Lock.Kind /= Entity_Spec or Names (Spec)'Length /= 1 then
                  Parameter_Error (Rule_Id,
                                   "more than one parameter of the provided type",
                                   Lock_Context.Lock.Position);
               else
                  case Mode_Kind (Spec) is
                     when An_In_Mode | A_Default_In_Mode =>
                        -- Only discrete and access types allowed
                        case Type_Kind (Type_Declaration_View (Corresponding_Name_Declaration (Mark))) is
                           when An_Enumeration_Type_Definition
                              | A_Signed_Integer_Type_Definition
                              | A_Modular_Type_Definition
                              | An_Access_Type_Definition
                              =>
                              null;
                           when others =>
                              Parameter_Error (Rule_Id,
                                               "only discrete and access types allowed for lock parameter",
                                               Lock_Context.Lock.Position);
                        end case;

                        Lock_Context.Lock := (In_Def,
                                              Formal => Names (Spec) (1));
                     when An_In_Out_Mode =>
                        Lock_Context.Lock := (In_Out_Def,
                                              Formal => Names (Spec) (1));
                     when An_Out_Mode =>
                        Parameter_Error (Rule_Id,
                                         "parameter of the provided type is of mode ""out"" in "
                                         & Full_Name_Image (Called_Name (Lock_Call)),
                                         Lock_Context.Lock.Position);
                     when Not_A_Mode =>
                        Failure ("not a mode for parameter");
                  end case;
               end if;
            end if;
         end loop;
         if Lock_Context.Lock.Kind = Entity_Spec then
            Parameter_Error (Rule_Id,
                             "No parameter of the provided type in " & Full_Name_Image (Called_Name (Lock_Call)),
                             Lock_Context.Lock.Position);
         end if;
      end;
      Update (Checked_Subprograms, Lock_Context);
   end Update_Lock_Parameter;

   -------------------------
   -- Has_Same_Lock_Param --
   -------------------------

   function Has_Same_Lock_Param (Call           : Asis.Statement;
                                 Called_Context : SP_Context;
                                 Other_Call     : Asis.Statement;
                                 Other_Context  : SP_Context) return Boolean
   is
   -- Returns True if Lock.Kind is none, or if the Lock parameters are the same
      use Thick_Queries, Utilities;
   begin
      if Other_Context.Lock.Kind /= Called_Context.Lock.Kind then
         return False;
      end if;

      case Called_Context.Lock.Kind is
         when None =>
            return True;
         when Entity_Spec =>
            Failure ("Lock not updated");
         when In_Def =>
            return Same_Value (Actual_Expression (Call,       Called_Context.Lock.Formal),
                               Actual_Expression (Other_Call, Other_Context.Lock.Formal));
         when In_Out_Def =>
            declare
               Lock_Object       : constant Asis.Expression := Actual_Expression (Call, Called_Context.Lock.Formal);
               Other_Lock_Object : constant Asis.Expression := Actual_Expression (Other_Call,
                                                                                  Other_Context.Lock.Formal);
            begin
               return Variables_Proximity (Lock_Object, Other_Lock_Object) = Same_Variable;
            end;
      end case;
   end Has_Same_Lock_Param;

   -------------------------
   -- Is_Matching_Locking --
   -------------------------

   function Is_Matching_Locking (Call           : Asis.Statement;
                                 Called_Context : SP_Context;
                                 Other_Call     : Asis.Statement) return Boolean
   is
   -- Returns True if Call and Other_Call are matching opening/closing calls (in any order).
      Other_Context : Root_Context'Class := Call_Context (Other_Call);
   begin
      if Other_Context = No_Matching_Context then
         return False;
      end if;

      -- One opening call and one closing call
      if SP_Context (Other_Context).Role = Called_Context.Role then
         return False;
      end if;

      Update_Lock_Parameter (Other_Call, SP_Context (Other_Context));

      -- Must belong to the same control
      if (SP_Context (Other_Context).Rule_Numbers and Called_Context.Rule_Numbers) = Empty_Control_Index_Set then
         return False;
      end if;

      -- Must have matching lock parameters
      return Has_Same_Lock_Param (Call, Called_Context, Other_Call, SP_Context (Other_Context));
   end Is_Matching_Locking;

   ----------------
   -- Call_Image --
   ----------------

   function Call_Image (The_Call : Asis.Statement) return Wide_String is
   -- Precondition: the matching context exists
      use Asis, Asis.Statements;
      use Thick_Queries, Utilities;

      Called_Context : constant SP_Context  := SP_Context (Call_Context (The_Call));
      Sp_Image       : constant Wide_String := Full_Name_Image (Called_Name (The_Call));

      function Selected_Variable_Image (Var : Asis.Expression) return Wide_String is
         use Asis.Elements, Asis.Expressions;
         Sel : Asis.Expression;
      begin
         if Expression_Kind (Var) /= A_Selected_Component then
            return Full_Name_Image (Var);
         end if;
         Sel := Selector (Var);
         if Declaration_Kind (Corresponding_Name_Declaration (Sel)) = A_Component_Declaration then
            return Selected_Variable_Image (Prefix (Var)) & '.' & Name_Image (Sel);
         else
            return Full_Name_Image (Var);
         end if;
      end Selected_Variable_Image;
   begin  -- Call_Image
      case Called_Context.Lock.Kind is
         when None =>
            return Sp_Image;
         when In_Def =>
            -- The lock value can be a static expression of a discrete type, or
            -- a constant of any type, or anything else (since it is an error).
            declare
               Expr      : constant Asis.Expression := Actual_Expression (The_Call, Called_Context.Lock.Formal);
               Val_Image : constant Wide_String     := Static_Expression_Value_Image (Expr);
            begin
               -- The image of an access value is not a real value
               if Is_Access_Expression (Expr) or else Val_Image = "" then
                  return Sp_Image & " (different or non static lock value)";
               else
                  return Sp_Image & " with lock value " & Val_Image ;
               end if;
            end;
         when In_Out_Def =>
            return Sp_Image
              & " with lock variable "
              & Selected_Variable_Image (Actual_Expression (The_Call, Called_Context.Lock.Formal));
         when Entity_Spec =>
            Failure ("lock field not initialized");
      end case;
   end Call_Image;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Element) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports, Thick_Queries, Unsafe_Paired_Calls.Services;

      function Is_Same_Opening_Locking (Called_Context : SP_Context;
                                        Other_Call     : Asis.Statement) return Boolean
      is
      -- Returns True if Call and Other_Call refer to the same subprograms, with matching lock parameters
         use Asis.Expressions;

         Other_Context : Root_Context'Class := Call_Context (Other_Call);
      begin
         if Other_Context = No_Matching_Context then
            return False;
         end if;

         if not Is_Equal (Corresponding_Name_Definition (Key_Name (Call)),
                          Corresponding_Name_Definition (Key_Name (Other_Call)))
         then
            return False;
         end if;

         Update_Lock_Parameter (Other_Call, SP_Context (Other_Context));

         return Has_Same_Lock_Param (Call, Called_Context, Other_Call, SP_Context (Other_Context));
      end Is_Same_Opening_Locking;

      procedure Check_Opening_Call (Opening_Call_Context : SP_Context) is
         Opening_Sig   : constant Nesting_Signature := Signature (Call, With_Check => True);
         Opening_Block : Asis.Statement renames Opening_Sig (Opening_Sig'First);
         Enclosing     : constant Asis.Element := Enclosing_Element (Opening_Block);
      begin
         -- Check that the opening block is directly in a handled sequence of statements
         if not Is_Handled_Sequence_Container (Enclosing) then
            Report (Rule_Id,
                    Opening_Call_Context,
                    Get_Location (Call),
                    "opening call to " & Full_Name_Image (Called_Name (Call))
                    & " is not directly in a handled sequence of statements");
            return;  -- Cannot proceed, since we don't know where we are (to get statements and handlers)
         end if;

         declare
            Stats      : constant Statement_List         := Thick_Queries.Statements         (Enclosing);
            Handlers   : constant Exception_Handler_List := Thick_Queries.Exception_Handlers (Enclosing);
            Other_Call : Asis.Statement;
         begin
            -- Check that there is no call to same lock in enclosing scopes
            -- Note that this check is done (and this SP later added) when we encounter
            -- a *call*, i.e. after the declarative part of the enclosing unit.
            -- Therefore, this will *not* prevent having P/V pairs in nested subprograms,
            -- even if the outer one also has P/V pairs, as it should be.
            Active_Procs.Reset (Scope_Manager.All_Scopes);
            while Active_Procs.Data_Available loop
               if Is_Same_Opening_Locking (Opening_Call_Context, Active_Procs.Current_Data) then
                  Report (Rule_Id,
                          Opening_Call_Context,
                          Get_Location (Call),
                          "nested call to " & Call_Image (Call));
               end if;
               Active_Procs.Next;
            end loop;

            -- This block must be the first statement
            if not Is_Equal (Opening_Block, Stats (Stats'First)) then
               Report (Rule_Id,
                       Opening_Call_Context,
                       Get_Location (Call),
                       "opening call to " & Full_Name_Image (Called_Name (Call)) & " is not the first statement");
               return;
            end if;

            -- OK, add ourself now (i.e. don't add if the opening call is incorrect)
            Active_Procs.Push (Call);

            -- Last statement must be a matching closing call
            Other_Call := Matching_Call (Effective_Last_Statement (Stats), Opening_Sig);
            if not Is_Matching_Locking (Call, Opening_Call_Context, Other_Call) then
               Report (Rule_Id,
                       Opening_Call_Context,
                       Get_Next_Word_Location (Effective_Last_Statement (Stats), Starting => From_Tail),
                       "sequence must end with closing call matching " & Call_Image (Call));
            end if;

            -- Construct must have exception handlers
            if Is_Nil (Handlers) then
               Report (Rule_Id,
                       Opening_Call_Context,
                       Get_Next_Word_Location (Stats, "END"),
                       "construct must have exception handlers");
               return;
            end if;

            -- Here, we have at least one exception handler
            -- Construct must have a "when others" exception handler
            if Definition_Kind (Exception_Choices (Handlers (Handlers'Last)) (1)) /= An_Others_Choice then
               Report (Rule_Id,
                       Opening_Call_Context,
                       Get_Previous_Word_Location (Handlers, "EXCEPTION" ),
                       "construct must have a ""when others"" exception handler");
            end if;

            -- Every handler must include directly one and only one call to an SP matching the opening call
            for H : Asis.Exception_Handler of Handlers loop
               declare
                  Call_Count : Asis.ASIS_Natural := 0;
               begin
                  for Stmt : Asis.Statement of Handler_Statements (H) loop
                     if Is_Matching_Locking (Call, Opening_Call_Context, Matching_Call (Stmt, Opening_Sig)) then
                        Call_Count := Call_Count + 1;
                     end if;
                  end loop;
                  case Call_Count is
                     when 0 =>
                        Report (Rule_Id,
                                Opening_Call_Context,
                                Get_Location (H),
                                "handler must have a closing call matching " & Call_Image (Call));
                     when 1 => --OK
                        null;
                     when others =>
                        Report (Rule_Id,
                                Opening_Call_Context,
                                Get_Location (H),
                                "handler must have only one closing call matching " & Call_Image (Call));
                  end case;
               end;
            end loop;
         end;
      end Check_Opening_Call;

      procedure Check_Closing_Call (Closing_Call_Context : SP_Context) is
         use Scope_Manager;
         Enclosing          : Asis.Element := Call;
         Enclosing_Sequence : Asis.Element;
         Opening_Call       : Asis.Statement := Nil_Element;
         Closing_Block      : Asis.Statement;

         function Next_Nonnull_Statement (Stmt : Asis.Statement) return Asis.Statement is
         -- Returns the statement following Stmt, skipping any null statements.
         -- Returns Nil_Element if Stmt is last in its sequence, or followed only by null statements
            Sequence : constant Asis.Statement_List := Thick_Queries.Statements (Enclosing_Element (Stmt));
            Inx : Asis.List_Index := Sequence'First;
         begin
            while not Is_Equal (Sequence (Inx), Stmt) loop
               Inx := Inx + 1;
            end loop;

            Inx := Inx + 1;
            while Inx <= Sequence'Last and then Statement_Kind (Sequence (Inx)) = A_Null_Statement loop
               Inx := Inx + 1;
            end loop;

            if Inx > Sequence'Last then
               return Nil_Element;
            else
               return Sequence (Inx);
            end if;
         end Next_Nonnull_Statement;
      begin  -- Check_Closing_Call
         -- Find enclosing handled sequence of statements or exception handler
         loop
            Enclosing := Enclosing_Element (Enclosing);
            if Is_Handled_Sequence_Container (Enclosing) then
               Enclosing_Sequence := Enclosing;
               exit;
            elsif Element_Kind (Enclosing) = An_Exception_Handler then
               Enclosing_Sequence := Enclosing_Element (Enclosing);
               exit;
            end if;
         end loop;

         -- Find matching opening call
         Active_Procs.Reset (All_Scopes);
         while Active_Procs.Data_Available loop
            Opening_Call := Active_Procs.Current_Data;
            exit when Is_Matching_Locking (Call, Closing_Call_Context, Opening_Call);
            Active_Procs.Next;
         end loop;
         if not Active_Procs.Data_Available then
            Report (Rule_Id,
                    Closing_Call_Context,
                    Get_Location (Call),
                    "closing call to " & Full_Name_Image (Called_Name (Call))
                                       & " has no matching opening call at start of sequence");
            return;
         end if;

         declare
            Opening_Sig          : constant Nesting_Signature := Signature (Opening_Call, With_Check => False);
            Opening_Call_Context : SP_Context;
            Stats                : constant Statement_List    := Thick_Queries.Statements (Enclosing_Sequence);
            Other_Call           : Asis.Statement;
         begin
            Opening_Call_Context := SP_Context (Call_Context (Opening_Call));

            -- First statement must be matching call according to signature
            Other_Call := Matching_Call (Stats (Stats'First), Opening_Sig);
            if Is_Dispatching_Call (Other_Call) then
               Uncheckable (Rule_Id, False_Negative, Get_Location (Other_Call), "Dispatching call");
               return;
            end if;

            -- No effective statements after this call
            if Element_Kind (Enclosing) /= An_Exception_Handler
              and then not Is_Equal (Call,
                                     Effective_Last_Statement (Thick_Queries.Statements (Enclosing_Element (Call))))
            then
               Report (Rule_Id,
                       Closing_Call_Context,
                       Get_Location (Call),
                       "Disallowed statements after closing call to " & Full_Name_Image (Called_Name (Call)));
               return;
            end if;

            Closing_Block := Matching_Block (Call, Opening_Sig);
            if not Is_Nil (Closing_Block)
              and then Statement_Kind (Next_Nonnull_Statement (Closing_Block))
                       in A_Goto_Statement | A_Return_Statement | An_Exit_Statement
            then
               -- If followed by breaking statement, correct usage will be checked by the breaking statement
               -- since it can happen at any level of nesting
               return;
            end if;

            if Is_Nil (Other_Call)
              or else Call_Context (Other_Call) = No_Matching_Context
              or else not Is_Matching_Locking (Opening_Call, Opening_Call_Context, Call)
            then
               Report (Rule_Id,
                       Closing_Call_Context,
                       Get_Location (Call),
                       "closing call to " & Call_Image (Call) & " has no matching block at start of sequence");
               return;
            end if;

            -- This block must have the same structure as the opening block, and at the same level
            if Is_Nil (Closing_Block) then
               Report (Rule_Id,
                       Closing_Call_Context,
                       Get_Location (Call),
                       "No matching opening block for closing call to " & Full_Name_Image (Called_Name (Call)));
               return;
            end if;

            -- This block must not be nested in a composite statement below the level of the opening block, unless
            -- followed by a breaking statement
            if not Is_Equal ((if Element_Kind (Enclosing) = An_Exception_Handler
                                   then Enclosing_Element (Enclosing_Element (Closing_Block))
                                   else Enclosing_Element (Closing_Block)),
                             Enclosing_Element (Opening_Sig (1)))
            then
               Report (Rule_Id,
                       Closing_Call_Context,
                       Get_Location (Call),
                       "Closing call to " & Full_Name_Image (Called_Name (Call))
                                          & " is nested within another statement");
               return;
            end if;

            -- This block must be the last statement, not counting final null, return and exit statements
            -- except in exception handlers
            if Element_Kind (Enclosing) /= An_Exception_Handler
              and then not Is_Equal (Closing_Block,
                                     Effective_Last_Statement (Thick_Queries.Statements
                                                               (Enclosing_Element (Closing_Block))))
            then
               Report (Rule_Id,
                       Closing_Call_Context,
                       Get_Location (Call),
                       "closing call to " & Full_Name_Image (Called_Name (Call)) & " is not the last statement");
               return;
            end if;

            if not Is_Matching_Locking (Call, Closing_Call_Context, Other_Call) then
               Report (Rule_Id,
                       Closing_Call_Context,
                       Get_Location (Call),
                       "call does not match opening call " & Call_Image (Other_Call));
            end if;

         end;
      end Check_Closing_Call;

      use Ada.Characters.Handling, Ada.Exceptions;
   begin   -- Process_Call
      if Rules_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Dispatching_Call (Call) then
         Uncheckable (Rule_Id, False_Negative, Get_Location (Call), "Dispatching call");
         return;
      end if;

      declare
         Called_Proc_Context : Root_Context'Class := Call_Context (Call);
      begin
         if Called_Proc_Context = No_Matching_Context then
            return;
         end if;
         Update_Lock_Parameter (Call, SP_Context (Called_Proc_Context));

         case SP_Context (Called_Proc_Context).Role is
            when Opening =>
               begin
                  Check_Opening_Call (SP_Context (Called_Proc_Context));
               exception
                  when Occur: Invalid_Nesting =>
                     Report (Rule_Id,
                             Called_Proc_Context,
                             Get_Location (Call),
                             "Invalid placement of opening call to " & Full_Name_Image (Called_Name (Call))
                             & ": " & To_Wide_String (Exception_Message (Occur)));
               end;
            when Closing =>
               begin
                  Check_Closing_Call (SP_Context (Called_Proc_Context));
               exception
                  when Occur: Invalid_Nesting =>
                     Report (Rule_Id,
                             Called_Proc_Context,
                             Get_Location (Call),
                             "Invalid placement of closing call to " & Full_Name_Image (Called_Name (Call))
                             & ": " & To_Wide_String (Exception_Message (Occur)));
               end;
         end case;
      end;
   end Process_Call;


   --------------------------------
   -- Process_Breaking_Statement --
   --------------------------------

   procedure Process_Breaking_Statement (Stmt : Asis.Statement) is
   -- Handle return, exit, and goto statements
   -- There must not be any active opening block inside an exited scope, unless the preceding statement is a call to
   -- a closing block (the matching with the opening block has been checked at the point of the call).
   -- Exited scopes are for
   --  - return: the returned subprogram and all scopes within it
   --  - exit: all scopes within the exited loop
   --  - goto: all scopes within the scope of the target label
      use Asis, Asis.Elements;
      use Framework.Locations, Framework.Reports, Rules.Unsafe_Paired_Calls.Services, Scope_Manager, Thick_Queries;
   begin
      if Rules_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Stats        : constant Asis.Statement_List := Thick_Queries.Statements (Enclosing_Element (Stmt));
         Stat_Inx     : Asis.ASIS_Natural;
         Target_Depth : Scope_Range;
      begin
         Target_Depth := Target_Statement_Depth (Stmt);

         Active_Procs.Reset (Scope_Manager.All_Scopes);
         if not Active_Procs.Data_Available then
            -- no active opening block
            return;
         end if;

         Stat_Inx := Statement_Index (Stmt, Within => Stats)-1;
         -- Ignore null statements between breaking statement and (supposed) closing block
         while Stat_Inx >= Stats'First and then Statement_Kind (Stats (Stat_Inx)) = A_Null_Statement loop
            Stat_Inx := Stat_Inx -1;
         end loop;

         -- Check open blocks that are included into target's scope (and left)
         while Active_Procs.Data_Available and then Target_Depth < Active_Procs.Current_Data_Level loop
            if Stat_Inx < Stats'First
              or else not Is_Matching_Locking (Active_Procs.Current_Data,
                                               SP_Context (Call_Context (Active_Procs.Current_Data)),
                                               Matching_Call (Stats (Stat_Inx),
                                                              Signature (Active_Procs.Current_Data,
                                                                         With_Check => False)))
            then
               Report (Rule_Id,
                       Call_Context (Active_Procs.Current_Data),
                       Get_Location (Stmt),
                       "Statement not preceded by closing block matching " & Call_Image (Active_Procs.Current_Data));
            end if;
            Active_Procs.Next;
         end loop;

         -- Check open blocks that include target
         while Active_Procs.Data_Available and then Target_Depth >= Active_Procs.Current_Data_Level loop
            if Stat_Inx >= Stats'First
              and then Is_Matching_Locking (Active_Procs.Current_Data,
                                            SP_Context (Call_Context (Active_Procs.Current_Data)),
                                            Matching_Call (Stats (Stat_Inx),
                                              Signature (Active_Procs.Current_Data, With_Check => False)))
            then
               -- target is inside scope of opening block, but there is a corresponding closing block
               Report (Rule_Id,
                       Call_Context (Active_Procs.Current_Data),
                       Get_Location (Matching_Call (Stats (Stat_Inx),
                                     Signature (Active_Procs.Current_Data, With_Check => False))),
                       "Closing block followed by breaking statement that does not leave scope of "
                           & Call_Image (Active_Procs.Current_Data));
            end if;
            Active_Procs.Next;
         end loop;
      end;
   end Process_Breaking_Statement;

begin  -- Rules.Unsafe_Paired_Calls
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);

   Framework.Variables.Register (Conditionals_Allowed'Access, Rule_Id & ".CONDITIONALS_ALLOWED");
   Framework.Variables.Register (Name_As_Given'Access,        Rule_Id & ".NAME_AS_GIVEN");
end Rules.Unsafe_Paired_Calls;
