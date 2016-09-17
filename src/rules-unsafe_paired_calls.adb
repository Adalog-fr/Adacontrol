----------------------------------------------------------------------
--  Rules.Unsafe_Paired_Calls - Package body                        --
--                                                                  --
--  This module is  (c) BelgoControl and Adalog  2004-2005. The Ada --
--  Controller  is  free software;  you can redistribute  it and/or --
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
   -- Analysis starts from (any) procedure call.
   -- First identify if the call is an opening call, a closing call, or anything else (exit immediately if the latter).
   -- For opening calls, check:
   --    - That the call is the first statement in a sequence
   --    - That there is no call for the same lock in an enclosing scope (opening calls are kept in a scoped store)
   --    - That the current sequence of statements is terminated by a matching closing call
   --    - That there is an exception parts with a "when others" handler
   --    - That every handler includes exactly one closing call
   -- For closing calls check:
   --    - That the call is the last statemement in a sequence, except for possible return, exit and null
   --    - That the current sequence of statements starts with a matching opening call
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
               Position : Location;
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
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Conditionals_Allowed");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Utilities, Ada.Strings.Wide_Fixed;
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
            Rules_Used  := 0;
            Clear (Checked_Subprograms);
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

   ------------------
   -- Call_Context --
   ------------------

   function Call_Context (Call : Asis.Statement) return Root_Context'Class is
      use Asis.Elements;
      use Thick_Queries;
   begin
      if Is_Nil (Call) then
         return No_Matching_Context;
      end if;
      return Matching_Context (Checked_Subprograms, Ultimate_Name (Called_Simple_Name (Call)));
   end Call_Context;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Element) is
      use Asis, Asis.Elements, Asis.Statements;
      use Framework.Reports, Thick_Queries, Utilities, Unsafe_Paired_Calls.Services;

      function Call_Image (The_Call : Asis.Statement) return Wide_String is
         -- Precondition: the matching context exists
         Called_Context : constant SP_Context := SP_Context (Call_Context (The_Call));
         Sp_Image       : constant Wide_String := Full_Name_Image (Called_Name (The_Call));

         function Selected_Variable_Image (Var : Asis.Expression) return Wide_String is
            use Asis.Expressions;
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
                  Val_Image : constant Wide_String
                    := Static_Expression_Value_Image (Actual_Expression (The_Call, Called_Context.Lock.Formal));
               begin
                  if Val_Image = "" then
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

      procedure Update_Lock_Parameter (Lock_Call : in Asis.Element; Lock_Context : in out SP_Context) is
      -- Initially, a SP_Context has Entity_Specification for its Lock field.
      -- We must delay analyzing the lock until we have a way of getting to the corresponding
      -- element, i.e. the first time we have a call to the procedure.
      -- This procedure update the Lock field of Lock_Context according to the provided Lock_Call

         use Framework.Language;
         use Asis.Declarations, Asis.Expressions;
      begin
         if Lock_Context.Lock.Kind /= Entity_Spec then
            -- Already transformed (or None)
            return;
         end if;

         declare
            Profile : constant Asis.Parameter_Specification_List := Called_Profile (Lock_Call);
            Mark    : Asis.Expression;
         begin
            -- Note that we iterate through all parameters, and that we transform Lock_Context
            -- as soon as we find a parameter of the appropriate type.
            -- This is intended to diagnose the case where more than one parameter is of the
            -- provided type.
            for I in Profile'Range loop
               Mark := Simple_Name (Declaration_Subtype_Mark (Profile (I)));
               if Matches (Lock_Context.Lock.Entity, Mark) then
                  if Lock_Context.Lock.Kind /= Entity_Spec or Names (Profile (I))'Length /= 1 then
                     Parameter_Error (Rule_Id,
                                      "more than one parameter of the provided type",
                                      Lock_Context.Lock.Position);
                  else
                     case Mode_Kind (Profile (I)) is
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
                                                 Formal => Names (Profile (I))(1));
                        when An_In_Out_Mode =>
                           Lock_Context.Lock := (In_Out_Def,
                                                 Formal => Names (Profile (I))(1));
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

      function Has_Same_Lock_Param (Called_Context : SP_Context;
                                    Other_Call     : Asis.Statement;
                                    Other_Context  : SP_Context) return Boolean
      is
      -- Returns True if Lock.Kind is none, or if the Lock parameters are the same
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

         if not Is_Equal (Corresponding_Name_Definition (Ultimate_Name (Called_Name (Call))),
                          Corresponding_Name_Definition (Ultimate_Name (Called_Name (Other_Call))))
         then
            return False;
         end if;

         Update_Lock_Parameter (Other_Call, SP_Context (Other_Context));

         return Has_Same_Lock_Param (Called_Context, Other_Call, SP_Context (Other_Context));
      end Is_Same_Opening_Locking;

      function Is_Matching_Locking (Called_Context : SP_Context;
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

         -- Must matching lock parameter
         return Has_Same_Lock_Param (Called_Context, Other_Call, SP_Context (Other_Context));
      end Is_Matching_Locking;

      ------------------------
      -- Check_Opening_Call --
      ------------------------

      procedure Check_Opening_Call (Called_Context : SP_Context; Called_Sig : Nesting_Signature) is
         Enclosing  : Asis.Element;
      begin
         Enclosing := Enclosing_Element (Called_Sig (Called_Sig'First));

         -- Check that the call (or top if) is in an handled sequence of statements
         if not Is_Handled_Sequence_Container (Enclosing) then
            Report (Rule_Id,
                    Called_Context,
                    Get_Location (Call),
                    "opening call to " & Full_Name_Image (Called_Name (Call))
                    & " is not directly in a handled sequence of statements");
            return;  -- Cannot proceed, since we don't know where we are (to get stats and handlers)
         end if;

         declare
            Stats      : constant Statement_List         := Thick_Queries.Statements         (Enclosing);
            Handlers   : constant Exception_Handler_List := Thick_Queries.Exception_Handlers (Enclosing);
            Other_Call : Asis.Statement;
         begin
            -- No call to same SP in enclosing scopes
            -- Note that this check is done (and this SP later added) when we encounter
            -- a *call*, i.e. after the declarative part of the enclosing unit.
            -- Therefore, this will *not* prevent having P/V pairs in enclosed subprograms,
            -- even if the outer one also has P/V pairs, as it should be.
            Active_Procs.Reset (Scope_Manager.All_Scopes);
            while Active_Procs.Data_Available loop
               if Is_Same_Opening_Locking (Called_Context, Active_Procs.Current_Data) then
                  Report (Rule_Id,
                          Called_Context,
                          Get_Location (Call),
                          "nested call to " & Call_Image (Call));
               end if;
               Active_Procs.Next;
            end loop;

            -- OK, add ourself
            Active_Procs.Push (Call);

            -- This call (or top if) must be the first statement
            Other_Call := Matching_Call (Effective_Last_Statement (Stats), Called_Sig);
            if not Is_Equal (Called_Sig (Called_Sig'First), Stats (Stats'First)) then
               Report (Rule_Id,
                       Called_Context,
                       Get_Location (Call),
                       "opening call to " & Full_Name_Image (Called_Name (Call)) & " is not the first statement");

            -- Last statement must be corresponding call
            elsif not Is_Matching_Locking (Called_Context, Other_Call) then
               Report (Rule_Id,
                       Called_Context,
                       Get_Next_Word_Location (Effective_Last_Statement (Stats), Starting => From_Tail),
                       "sequence must end with closing call matching " & Call_Image (Call));
            end if;

            -- Construct must have exception handlers
            if Is_Nil (Handlers) then
               Report (Rule_Id,
                       Called_Context,
                       Get_Next_Word_Location (Stats, "END"),
                       "construct must have exception handlers");
               return;
            end if;

            -- Here, we have at least one exception handler
            -- Construct must have a "when others" exception handler
            if Definition_Kind (Exception_Choices (Handlers (Handlers'Last)) (1)) /= An_Others_Choice then
               Report (Rule_Id,
                       Called_Context,
                       Get_Previous_Word_Location (Handlers, "EXCEPTION" ),
                       "construct must have a ""when others"" exception handler");
            end if;

            -- Every handler must include directly one and only one call to an SP matching the opening call
            for I in Handlers'Range loop
               declare
                  Handler_Stats : constant Asis.Statement_List := Handler_Statements (Handlers (I));
                  Call_Count    : Asis.ASIS_Natural := 0;
               begin
                  for J in Handler_Stats'Range loop
                     if Is_Matching_Locking (Called_Context, Matching_Call (Handler_Stats (J), Called_Sig)) then
                        Call_Count := Call_Count + 1;
                     end if;
                  end loop;
                  case Call_Count is
                     when 0 =>
                        Report (Rule_Id,
                                Called_Context,
                                Get_Location (Handlers (I)),
                                "handler must have a closing call matching " & Call_Image (Call));
                     when 1 => --OK
                        null;
                     when others =>
                        Report (Rule_Id,
                                Called_Context,
                                Get_Location (Handlers (I)),
                                "handler must have only one closing call matching " & Call_Image (Call));
                  end case;
               end;
            end loop;
         end;
      end Check_Opening_Call;

      procedure Check_Closing_Call (Called_Context : SP_Context; Called_Sig : Nesting_Signature) is
         Enclosing  : Asis.Element;
      begin
         Enclosing := Enclosing_Element (Called_Sig (Called_Sig'First));

         declare
            Stats      : constant Statement_List := Thick_Queries.Statements (Enclosing);
            Other_Call : Asis.Statement;
         begin

            -- This call (or top if) must be the last statement, not counting final null, return and exit statements
            -- except in exception handlers
            if Element_Kind (Enclosing) /= An_Exception_Handler
              and then not Is_Equal (Called_Sig (Called_Sig'First), Effective_Last_Statement (Stats))
            then
               Report (Rule_Id,
                       Called_Context,
                       Get_Location (Call),
                       "closing call to " & Full_Name_Image (Called_Name (Call)) & " is not the last statement");
               return;
            end if;

            -- First statement must be matching call according to signature
            if Element_Kind (Enclosing) = An_Exception_Handler then
               declare
                  Good_Stats : constant Statement_List := Thick_Queries.Statements (Enclosing_Element (Enclosing));
               begin
                  Other_Call := Matching_Call (Good_Stats (Good_Stats'First), Called_Sig);
               end;
            else
               Other_Call := Matching_Call (Stats (Stats'First), Called_Sig);
            end if;
            if Is_Nil (Other_Call) or else Call_Context (Other_Call) = No_Matching_Context then
               Report (Rule_Id,
                       Called_Context,
                       Get_Location (Call),
                       "closing call to " & Call_Image (Call) & " has no matching opening call at start of sequence");
               return;
            end if;

            if Is_Dispatching_Call (Other_Call) then
               Uncheckable (Rule_Id, False_Negative, Get_Location (Other_Call), "Dispatching call");
               return;
            end if;

            if not Is_Matching_Locking (Called_Context, Other_Call) then
               Report (Rule_Id,
                       Called_Context,
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
         Called_Context : Root_Context'Class := Call_Context (Call);
      begin
         if Called_Context = No_Matching_Context then
            return;
         end if;
         Update_Lock_Parameter (Call, SP_Context (Called_Context));

         case SP_Context (Called_Context).Role is
            when Opening =>
               begin
                  Check_Opening_Call (SP_Context (Called_Context), Signature (Call));
               exception
                  when Occur: Invalid_Nesting =>
                     Report (Rule_Id,
                             Called_Context,
                             Get_Location (Call),
                             "Invalid placement of opening call to " & Full_Name_Image (Called_Name (Call))
                             & ": " & To_Wide_String (Exception_Message (Occur)));
               end;
            when Closing =>
               begin
                  Check_Closing_Call (SP_Context (Called_Context), Signature (Call));
               exception
                  when Occur: Invalid_Nesting =>
                     Report (Rule_Id,
                             Called_Context,
                             Get_Location (Call),
                             "Invalid placement of closing call to " & Full_Name_Image (Called_Name (Call))
                             & ": " & To_Wide_String (Exception_Message (Occur)));
               end;
         end case;
      end;
   end Process_Call;

begin  -- Rules.Unsafe_Paired_Calls
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);

   Framework.Variables.Register (Conditionals_Allowed'Access, Rule_Id & ".CONDITIONALS_ALLOWED");
end Rules.Unsafe_Paired_Calls;
