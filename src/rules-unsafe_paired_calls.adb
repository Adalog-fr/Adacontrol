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
  Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;

package body Rules.Unsafe_Paired_Calls is
   use Framework;

   Rules_Used : Rule_Index := 0;
   Save_Used  : Rule_Index;

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
         Role        : SP_Role;
         Rule_Number : Rule_Index;
         Lock        : Lock_Parameter;
      end record;

   Checked_Subprograms  : Context_Store;

   package Active_Procs is new Framework.Scope_Manager.Scoped_Store (Asis.Element,
                                                                     Equivalent_Keys => Asis.Elements.Is_Equal);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(1): First subprogram");
      User_Message ("Parameter(2): Second subprogram");
      User_Message ("Parameter(3): (optional) type of lock parameter");
      User_Message ("Controls calls like P/V operations that are not safely paired");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language, Utilities, Ada.Strings.Wide_Fixed;
      First_SP  : Entity_Specification;
      Second_SP : Entity_Specification;
      Lock_Type : Entity_Specification;
      Lock_Pos  : Location;
   begin
      if Rules_Used = Rule_Index'Last then
         Parameter_Error ("Rule cannot be specified more than"
                          & Rule_Index'Wide_Image (Rule_Index'Last)
                          & " times: " & Rule_Id);
      end if;
      Rules_Used := Rules_Used + 1;

      if not Parameter_Exists then
         Parameter_Error ("First subprogram missing for rule " & Rule_Id);
      end if;
      First_SP := Get_Entity_Parameter;

      if not Parameter_Exists then
         Parameter_Error ("Second subprogram missing for rule " & Rule_Id);
      end if;
      Second_SP := Get_Entity_Parameter;

      if Parameter_Exists then
         Lock_Pos  := Source_Location;
         Lock_Type := Get_Entity_Parameter;
         if Parameter_Exists then
            Parameter_Error ("Spurious parameter after type name");
         end if;
         if Index (To_Upper (Image (Lock_Type)), "'CLASS") /= 0 then
            Parameter_Error ("Class wide type not allowed for lock parameter");
         end if;

         Associate (Checked_Subprograms,
                    First_SP,
                    SP_Context'(Basic.New_Context (Rule_Type,Label)
                                with Opening, Rules_Used, (Entity_Spec, Lock_Pos, Lock_Type)));
         Associate (Checked_Subprograms,
                    Second_SP,
                    SP_Context'(Basic.New_Context (Rule_Type,Label)
                                with Closing, Rules_Used, (Entity_Spec, Lock_Pos, Lock_Type)));
      else
         Associate (Checked_Subprograms,
                    First_SP,
                    SP_Context'(Basic.New_Context (Rule_Type,Label)
                                with Opening, Rules_Used, (Kind => None)));
         Associate (Checked_Subprograms,
                    Second_SP,
                    SP_Context'(Basic.New_Context (Rule_Type,Label)
                                with Closing, Rules_Used, (Kind => None)));
      end if;

   exception
      when Already_In_Store =>
         Parameter_Error ("Entity already given for rule " & Rule_Id);
   end Add_Use;

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
      Balance (Checked_Subprograms);
   end Prepare;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Element) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Statements;
      use Framework.Reports, Thick_Queries, Utilities;

      function Call_Image (The_Call : Asis.Statement) return Wide_String is
         -- Precondition: the matching context exists
         Call_Context : constant SP_Context := SP_Context (Matching_Context (Checked_Subprograms,
                                                                             Called_Simple_Name (The_Call)));
         Sp_Image     : constant Wide_String := Full_Name_Image (Called_Name (The_Call));
      begin
         case Call_Context.Lock.Kind is
            when None =>
               return Sp_Image;
            when In_Def =>
               -- The lock value can be a static expression of a discrete type, or
               -- a constant of any type, or anything else (since it is an error).
               declare
                  Val_Image : constant Wide_String := Static_Expression_Value_Image (Actual_Expression
                                                                                     (Call, Call_Context.Lock.Formal));
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
                 & Full_Name_Image (Actual_Expression (Call, Call_Context.Lock.Formal));
            when Entity_Spec =>
               Failure ("lock field not initialized");
         end case;
      end Call_Image;

      procedure Check_Lock_Parameter (Lock_Call : in Asis.Element; Lock_Context : in out SP_Context) is
         -- Initially, we just have an Entity_Specification for the Lock field of a context.
         -- We must delay analyzing the lock until we have a way of getting to the corresponding
         -- element, i.e. the first time we have a call to the procedure.
         use Framework.Language;
         use Asis.Expressions;
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
               Mark := Declaration_Subtype_Mark (Profile (I));
               if Expression_Kind (Mark) = A_Selected_Component then
                  Mark := Selector (Mark);
               end if;
               if Matches (Mark, Lock_Context.Lock.Entity) then
                  if Lock_Context.Lock.Kind /= Entity_Spec or Names (Profile (I))'Length /= 1 then
                     Parameter_Error ("More than one parameter of the provided type", Lock_Context.Lock.Position);
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
                                 Parameter_Error ("Only discrete and access types allowed for lock parameter",
                                                  Lock_Context.Lock.Position);
                           end case;

                           Lock_Context.Lock := (In_Def,
                                                 Formal => Names (Profile (I))(1));
                        when An_In_Out_Mode =>
                           Lock_Context.Lock := (In_Out_Def,
                                                 Formal => Names (Profile (I))(1));
                        when An_Out_Mode =>
                           Parameter_Error ("Parameter of the provided type is of mode ""out"" in "
                                            & Full_Name_Image (Called_Name (Lock_Call)),
                                            Lock_Context.Lock.Position);
                        when Not_A_Mode =>
                           Failure ("not a mode for parameter");
                     end case;
                  end if;
               end if;
            end loop;
            if Lock_Context.Lock.Kind = Entity_Spec then
               Parameter_Error ("No parameter of the provided type in " & Full_Name_Image (Called_Name (Lock_Call)),
                                Lock_Context.Lock.Position);
            end if;
         end;
         Update (Checked_Subprograms, Lock_Context);
      end Check_Lock_Parameter;

   begin   -- Process_Call
      if Rules_Used = 0 then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Dispatching_Call (Call) then
         return;
      end if;

      declare
         Called_Context : Root_Context'Class    := Matching_Context (Checked_Subprograms, Called_Simple_Name (Call));
         Enclosing      : constant Asis.Element := Enclosing_Element (Call);

         procedure Check_Other_SP_Call (Other_Call : Asis.Statement; Where : Wide_String)  is
            -- Checks that Other_Call is a procedure or entry call and the called entity is the other one
            -- from the pair.
            Good_Context : SP_Context renames SP_Context (Called_Context);
         begin
            if Statement_Kind (Other_Call) /= A_Procedure_Call_Statement
              and Statement_Kind (Other_Call) /= An_Entry_Call_Statement
            then
               Report (Rule_Id,
                       Called_Context,
                       Get_Location (Call),
                       "construct must " & Where & " with operation matching " & Call_Image (Call));
               return;
            end if;

            declare
               Other_Context : Root_Context'Class := Matching_Context (Checked_Subprograms,
                                                                       Ultimate_Name (Called_Name (Other_Call)));
            begin
               Check_Lock_Parameter (Other_Call, SP_Context (Other_Context));

               if Other_Context = No_Matching_Context then
                  Report (Rule_Id,
                          Called_Context,
                          Get_Location (Call),
                          "construct must " & Where & " with operation matching " & Call_Image (Call));
               elsif SP_Context (Other_Context).Rule_Number /= SP_Context (Called_Context).Rule_Number then
                  Report (Rule_Id,
                          Called_Context,
                          Get_Location (Call),
                          "construct must " & Where & " with operation matching " & Call_Image (Call));
               else
                  case Good_Context.Lock.Kind is
                     when None =>
                        return;
                     when Entity_Spec =>
                        Failure ("Lock not updated");
                     when In_Def =>
                        if not Same_Value (Actual_Expression (Call, Good_Context.Lock.Formal),
                                           Actual_Expression (Other_Call, SP_Context (Other_Context).Lock.Formal))
                        then
                           Report (Rule_Id,
                                   Called_Context,
                                   Get_Location (Call),
                                   "construct must " & Where & " with call matching " & Call_Image (Call));
                        end if;
                     when In_Out_Def =>
                        declare
                           Lock_Object : constant Asis.Expression
                             := Actual_Expression (Call, Good_Context.Lock.Formal);
                           Other_Lock_Object :constant Asis.Expression
                             := Actual_Expression (Other_Call, SP_Context (Other_Context).Lock.Formal);
                        begin
                           if Variables_Proximity (Lock_Object, Other_Lock_Object) /= Same_Variable then
                              Report (Rule_Id,
                                      Called_Context,
                                      Get_Location (Call),
                                      "construct must " & Where & " with call matching " & Call_Image (Call));
                           end if;
                        end;
                  end case;
               end if;
            end;
         end Check_Other_SP_Call;

         function Is_Same_Lock (Other_Call : Asis.Statement) return Boolean is
            -- Returns True if Other_Call is a call to the same procedure or entry as Call, and either
            -- Lock.Kind is none, or the Lock parameters are the same
            use Asis.Expressions;

            Good_Context : SP_Context renames SP_Context (Called_Context);
         begin
            if Is_Equal (Corresponding_Name_Definition (Ultimate_Name (Called_Name (Call))),
                         Corresponding_Name_Definition (Ultimate_Name (Called_Name (Other_Call))))
            then
               case Good_Context.Lock.Kind is
                  when None =>
                     return True;
                  when Entity_Spec =>
                     Failure ("Lock not updated");
                  when In_Def =>
                     return Same_Value (Actual_Expression (Call, Good_Context.Lock.Formal),
                                        Actual_Expression (Other_Call, Good_Context.Lock.Formal));
                  when In_Out_Def =>
                     declare
                        Lock_Object : constant Asis.Expression      := Actual_Expression (Call,
                                                                                          Good_Context.Lock.Formal);
                        Other_Lock_Object :constant Asis.Expression := Actual_Expression (Other_Call,
                                                                                          Good_Context.Lock.Formal);
                     begin
                        return Variables_Proximity (Lock_Object, Other_Lock_Object) = Same_Variable;
                     end;
               end case;
            else
               return False;
            end if;
         end Is_Same_Lock;

         procedure Check_First (Stats : Asis.Statement_List; Handlers : Asis.Exception_Handler_List) is
         begin
            Check_Lock_Parameter (Call, SP_Context (Called_Context));

            -- This call must be the first statement
            if not Is_Equal (Call, Stats (Stats'First)) then
               Report (Rule_Id,
                       Called_Context,
                       Get_Location (Call),
                       "call to " & Full_Name_Image (Called_Name (Call))
                       & " is not the first of a sequence of statements");
               return;
            end if;

            -- No call to same SP in enclosing scopes
            -- Note that this check is done (and this SP later added) when we encounter
            -- a *call*, i.e. after the declarative part of the enclosing unit.
            -- Therefore, this will *not* prevent having P/V pairs in enclosed subprograms,
            -- even if the outer one also has P/V pairs, as it should be.
            Active_Procs.Reset (Scope_Manager.All_Scopes);
            while Active_Procs.Data_Available loop
               if Is_Same_Lock (Active_Procs.Current_Data) then
                  Report (Rule_Id,
                          Called_Context,
                          Get_Location (Call),
                          "nested call to " & Call_Image (Call));
               end if;
               Active_Procs.Next;
            end loop;

            -- OK, add ourself
            Active_Procs.Push (Call);

            -- Last statement must be corresponding call
            Check_Other_SP_Call (Stats (Stats'Last), "end");

            -- Construct must have exception handlers
            if Is_Nil (Handlers) then
               Report (Rule_Id,
                       Called_Context,
                       Get_Location (Enclosing),
                       "construct must have exception handlers");
               return;
            end if;

            -- Construct must have a "when others" exception handler
            if Definition_Kind (Exception_Choices (Handlers (Handlers'Last))(1)) /= An_Others_Choice then
               Report (Rule_Id,
                       Called_Context,
                       Get_Location (Enclosing),
                       "construct must have a ""when others"" exception handler");
               return;
            end if;
         end Check_First;

         procedure Check_Last (Stats : Asis.Statement_List; Handlers : Asis.Exception_Handler_List) is
         begin
            Check_Lock_Parameter (Call, SP_Context (Called_Context));

            -- This call must be the last statement
            if not Is_Equal (Call, Stats (Stats'Last)) then
               Report (Rule_Id,
                       Called_Context,
                       Get_Location (Call),
                       "call to " & Full_Name_Image (Called_Name (Call))
                       & " is not the last of a sequence of statements");
               return;
            end if;

            -- First statement must be corresponding call
            Check_Other_SP_Call (Stats (Stats'First), "start");

            -- Every handler must include directly one and only one call to this SP
            for I in Handlers'Range loop
               declare
                  Handler_Stats : constant Asis.Statement_List := Handler_Statements (Handlers (I));
                  Call_Count    : Asis.ASIS_Natural := 0;
               begin
                  for J in Handler_Stats'Range loop
                     if (Statement_Kind (Handler_Stats (J)) = A_Procedure_Call_Statement
                         or Statement_Kind (Handler_Stats (J)) = An_Entry_Call_Statement)
                       and then Is_Same_Lock (Handler_Stats (J))
                     then
                        Call_Count := Call_Count + 1;
                     end if;
                  end loop;
                  case Call_Count is
                     when 0 =>
                        Report (Rule_Id,
                                Called_Context,
                                Get_Location (Handlers (I)),
                                "handler must have a call to " & Call_Image (Call));
                     when 1 =>
                        --OK
                        null;
                     when others =>
                        Report (Rule_Id,
                                Called_Context,
                                Get_Location (Handlers (I)),
                                "handler must have only one call to " & Call_Image (Call));
                  end case;
               end;
            end loop;
         end Check_Last;
     begin
         if Called_Context = No_Matching_Context then
            return;
         end if;

         case SP_Context (Called_Context).Role is
            when Opening =>
               case Element_Kind (Enclosing) is
                  when A_Declaration =>
                     case Declaration_Kind (Enclosing) is
                        when A_Function_Body_Declaration
                          | A_Procedure_Body_Declaration
                          | A_Package_Body_Declaration
                          | A_Task_Body_Declaration
                          | An_Entry_Body_Declaration
                          =>
                           Check_First (Body_Statements (Enclosing), Body_Exception_Handlers (Enclosing));
                        when others =>
                           Report (Rule_Id,
                                   Called_Context,
                                   Get_Location (Call),
                                   "call to " & Full_Name_Image (Called_Name (Call))
                                   & " is not the first of a sequence of statements");
                     end case;

                  when A_Statement =>
                     case Statement_Kind (Enclosing) is
                        when A_Block_Statement =>
                           Check_First (Block_Statements (Enclosing), Block_Exception_Handlers (Enclosing));
                        when An_Accept_Statement =>
                           Check_First (Accept_Body_Statements (Enclosing), Accept_Body_Exception_Handlers (Enclosing));
                        when others =>
                           Report (Rule_Id,
                                   Called_Context,
                                   Get_Location (Call),
                                   "call to " & Full_Name_Image (Called_Name (Call))
                                   & " is not the first of a sequence of statements");
                     end case;

                  when others =>
                     Report (Rule_Id,
                             Called_Context,
                             Get_Location (Call),
                             "call to " & Full_Name_Image (Called_Name (Call))
                             & " is not the first of a sequence of statements");
               end case;

            when Closing =>
               case Element_Kind (Enclosing) is
                  when A_Declaration =>
                     case Declaration_Kind (Enclosing) is
                        when A_Function_Body_Declaration
                          | A_Procedure_Body_Declaration
                          | A_Package_Body_Declaration
                          | A_Task_Body_Declaration
                          | An_Entry_Body_Declaration
                          =>
                           Check_Last (Body_Statements (Enclosing), Body_Exception_Handlers (Enclosing));
                        when others =>
                           Report (Rule_Id,
                                   Called_Context,
                                   Get_Location (Call),
                                   "call to " & Full_Name_Image (Called_Name (Call))
                                   & " is not the last of a sequence of statements");
                     end case;

                  when A_Statement =>
                     case Statement_Kind (Enclosing_Element (Call)) is
                        when A_Block_Statement =>
                           Check_Last (Block_Statements (Enclosing), Block_Exception_Handlers (Enclosing));
                        when An_Accept_Statement =>
                           Check_Last (Accept_Body_Statements (Enclosing), Accept_Body_Exception_Handlers (Enclosing));
                        when others =>
                           Report (Rule_Id,
                                   Called_Context,
                                   Get_Location (Call),
                                   "call to " & Full_Name_Image (Called_Name (Call))
                                   & " is not the last of a sequence of statements");
                     end case;

                  when An_Exception_Handler =>
                     null;

                  when others =>
                     Report (Rule_Id,
                             Called_Context,
                             Get_Location (Call),
                             "call to " & Full_Name_Image (Called_Name (Call))
                             & " is not the last of a sequence of statements");
               end case;

         end case;
      end;

   end Process_Call;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access,
                                              Prepare => Prepare'Access);
end Rules.Unsafe_Paired_Calls;
