----------------------------------------------------------------------
--  Rules.Duplicate_Initialization_Calls - Package body             --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2007.           --
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

-- Asis
with
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Element_Queues;

package body Rules.Duplicate_Initialization_Calls is
   use Framework, Framework.Control_Manager;

   -- Algorithm
   --
   -- We simply keep in the context of each indicated procedure a list of all encountered calls.
   -- A new call is checked against all existing calls, then added to the list.

   type Procedure_Context is new Basic_Rule_Context with
      record
         Profile_Checked : Boolean;
         Has_Out         : Boolean;
         Other_Calls     : Framework.Element_Queues.Queue;
      end record;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Applicable_Calls  : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control that indicated procedures are not called twice with identical in parameters,");
      User_Message ("or twice on the same out actual parameter");
      User_Message;
      User_Message ("Parameter(s): <Procedure name>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label     : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Framework.Element_Queues;

   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Applicable_Calls,
                       Entity,
                       Procedure_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                          Profile_Checked => False,
                                          Has_Out         => False,
                                          Other_Calls     => Empty_Queue));
         exception
            when Already_In_Store =>
               Parameter_Error (Rule_Id, "entity already given: " & Image (Entity));
         end;
      end loop;

      Rule_Used := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Clear (Applicable_Calls);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Applicable_Calls);
   end Prepare;

   ----------------------------
   -- Process_Procedure_Call --
   ----------------------------

   procedure Process_Procedure_Call (Call : in Asis.Statement) is
      use Framework.Element_Queues, Framework.Locations, Framework.Reports, Thick_Queries, Utilities;
      use Asis, Asis.Elements, Asis.Expressions;

      procedure Check_Profile (Formals             : in  Asis.Parameter_Specification_List;
                               Out_Parameter_Found : out Boolean)
      is
         -- Check that the profile includes only in parameters, except possibly one out parameter
         use Framework.Language;
      begin
         Out_Parameter_Found := False;
         for Param : Asis.Parameter_Specification of Formals loop
            case Mode_Kind (Param) is
               when Not_A_Mode =>
                  Failure ("not a mode in parameter specification");
               when A_Default_In_Mode | An_In_Mode =>
                  null;
               when An_Out_Mode =>
                  if Out_Parameter_Found then
                     Parameter_Error (Rule_Id,
                                      "not a proper initialization procedure, more than one out parameter ("
                                      & Adjust_Image (Full_Name_Image (Formal_Parameter (Param)))
                                      & ')'
                                     );
                  end if;
                  Out_Parameter_Found := True;
               when An_In_Out_Mode =>
                  Parameter_Error (Rule_Id,
                                   "not a proper initialization procedure, parameter "
                                   & Adjust_Image (Full_Name_Image (Formal_Parameter (Param)))
                                   & " has in out mode"
                                  );
            end case;
         end loop;
      end Check_Profile;

      procedure Check_Actuals (Actuals : Asis.Association_List; Context : Procedure_Context) is
         -- Check that all parameters are static (case where there is no out parameter)
      begin
         for Assoc : Asis.Association of Actuals loop
            if not Is_Static_Expression (Actual_Parameter (Assoc)) then
               Report (Rule_Id,
                       Context,
                       Get_Location (Actual_Parameter (Assoc)),
                       "non static value in call to initialization procedure");
            end if;
         end loop;
      end Check_Actuals;

      function Are_Equivalent_Calls (Formals   : Asis.Parameter_Specification_List;
                                     L_Actuals : Asis.Association_List;
                                     R_Call    : Asis.Statement)
                                     return Boolean
      is
         -- check that calls do not have the same parameters
         R_Actuals : constant Asis.Association_List := Actual_Parameters (R_Call, Normalized => True);

         All_In_Parameters_Equal : Boolean := True;
         Var_Proximity           : Proximity;
      begin
         for I in Formals'Range loop
            case Mode_Kind (Formals (I)) is
               when Not_A_Mode | An_In_Out_Mode =>
                  Failure ("bad mode in Are_Equivalent_Calls");
               when A_Default_In_Mode | An_In_Mode =>
                  if not Same_Value (Actual_Parameter (L_Actuals (I)), Actual_Parameter (R_Actuals (I))) then
                     All_In_Parameters_Equal := False;
                  end if;
               when An_Out_Mode =>
                  -- At this point, there can be only one out parameter
                  -- (checked by Check_Profile)
                  Var_Proximity := Variables_Proximity (Actual_Parameter (L_Actuals (I)),
                                                        Actual_Parameter (R_Actuals (I)));
                  if Var_Proximity = Same_Variable then
                     return True;
                  elsif Var_Proximity = Different_Variables then
                     return False;
                  else
                     Uncheckable (Rule_Id,
                                  False_Negative,
                                  Get_Location (Actual_Parameter (L_Actuals (I))),
                                  "non statically determinable out parameter");
                     return False;
                  end if;
            end case;
         end loop;

         return All_In_Parameters_Equal;
      end Are_Equivalent_Calls;

   begin  -- Process_Procedure_Call
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Current_Context : Root_Context'Class
           := Matching_Context (Applicable_Calls, Ultimate_Name (Called_Simple_Name (Call)));
      begin
         if Current_Context = No_Matching_Context then
            return;
         end if;

         declare
            Good_Context : Procedure_Context renames Procedure_Context (Current_Context);
            Current      : Cursor := First (Good_Context.Other_Calls);
            Formals      : constant Asis.Parameter_Specification_List := Called_Profile (Call);
            L_Actuals    : constant Asis.Association_List             := Actual_Parameters (Call, Normalized => True);
         begin
            if not Good_Context.Profile_Checked then
               Check_Profile (Formals, Good_Context.Has_Out);
               Good_Context.Profile_Checked := True;
            end if;

            if not Good_Context.Has_Out then
               Check_Actuals (L_Actuals, Good_Context);
            end if;

            while Has_Element (Current) loop
               if Are_Equivalent_Calls (Formals, L_Actuals, Fetch (Current)) then
                  Report (Rule_Id,
                          Current_Context,
                          Get_Location (Call),
                          "initialization call duplicates call at " & Image (Get_Location (Fetch (Current))));
                  return;
                  -- No need to add this call to the queue
               end if;
               Current := Next (Current);
            end loop;
            Append (Good_Context.Other_Calls, Call);
         end;
         Update (Applicable_Calls, Current_Context);
      end;
   end Process_Procedure_Call;

begin  -- Rules.Duplicate_Initialization_Calls
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Duplicate_Initialization_Calls;
