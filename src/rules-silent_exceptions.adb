----------------------------------------------------------------------
--  Rules.Silent_Exceptions - Package body                          --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
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

-- ASIS
with
  Asis.Elements,
  Asis.Statements;

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Silent_Exceptions is
   use Framework;

   type Usage is array (Rule_Types) of Boolean;
   Rule_Used : Usage := (others => False);
   Save_Used : Usage;
   Labels    : array (Rule_Types) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type Search_Result_Kind is (No_Path, Some_Paths, All_Paths);
   type Search_Result is array (Rule_Types) of Search_Result_Kind;

   type Proc_Context is new Rule_Context with
      record
         Usage : Search_Result;
      end record;

   Rule_Uses : Context_Store;


   ----------
   -- "or" --
   ----------

   -- Combine two parallel paths
   -- Truth table:
   --             No_Path     Some_Paths   All_Paths
   -- No_Path     No_Path     Some_Paths   Some_Paths
   -- Some_Paths  Some_Paths  Some_Paths   Some_Paths
   -- All_Paths   Some_Paths  Some_Paths   All_Paths
   function "or" (L, R : Search_Result) return Search_Result is
      Result : Search_Result := (others => Some_Paths);
   begin
      for I in Result'Range loop
         if L (I) = R (I) then
            Result (I) := L (I);
         end if;
      end loop;
      return Result;
   end "or";

   -----------
   -- "and" --
   -----------

   -- Combine two serial paths
   -- Result is the strongest of both paths
   -- Truth table:
   --             No_Path     Some_Paths   All_Paths
   -- No_Path     No_Path     Some_Paths   All_Paths
   -- Some_Paths  Some_Paths  Some_Paths   All_Paths
   -- All_Paths   All_Paths   All_Paths    All_Paths
   function "and" (L, R : Search_Result) return Search_Result is
      Result : Search_Result;
   begin
      for I in Result'Range loop
         Result (I) := Search_Result_Kind'Max (L (I), R (I));
      end loop;
      return Result;
   end "and";

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): <report procedure name>");
      User_Message ("Control exception handlers that do not re-raise an exception ");
      User_Message ("nor call a report procedure");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Use_Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if Rule_Used (Use_Rule_Type) then
         Parameter_Error (Rule_Id &
                            ": this rule can be specified only once for check " &
                            "and once for search");
      end if;
      Labels    (Use_Rule_Type) := To_Unbounded_Wide_String (Label);
      Rule_Used (Use_Rule_Type) := True;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
            Value  : Proc_Context := (Usage => (others => No_Path));
         begin
            Value.Usage (Use_Rule_Type) := All_Paths;
            Associate (Rule_Uses, Entity, Value);
         exception
            when Already_In_Store =>
               Value := Proc_Context (Association (Rule_Uses, Entity));
               Value.Usage (Use_Rule_Type) := All_Paths;
               Update (Rule_Uses, Value);
         end;
      end loop;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => False);
            Labels    := (others => Null_Unbounded_WIde_String);
            Clear (Rule_Uses);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      begin
         Associate (Rule_Uses,
                    Value ("ADA.EXCEPTIONS.RAISE_EXCEPTION"),
                    Proc_Context'(Usage => (others => All_Paths)));
      exception
         when Already_In_Store =>
            null;
      end;
      begin
         Associate (Rule_Uses,
                    Value ("ADA.EXCEPTIONS.RERAISE_OCCURRENCE"),
                    Proc_Context'(Usage => (others => All_Paths)));
      exception
         when Already_In_Store =>
            null;
      end;

      Balance (Rule_Uses);
   end Prepare;

   --------------------------
   -- Statement_List_Usage --
   --------------------------

   function Statement_List_Usage (Statements : Asis.Statement_List) return Search_Result is
      use Asis;
      use Asis.Elements;
      use Asis.Statements;
      Result : Search_Result := (others => No_Path);
   begin
      for I in Statements'Range loop
         if Label_Names (Statements (I)) /= Nil_Element_List then
            -- We have a <<label>>
            -- all bets are off
            Result := (others => Some_Paths);
         end if;

         case Statement_Kind (Statements (I)) is
            when A_Null_Statement
              | An_Assignment_Statement
              | A_Delay_Relative_Statement
              | A_Delay_Until_Statement
              | A_Terminate_Alternative_Statement
              | An_Abort_Statement
              | A_Code_statement
              =>
               null;

            when An_Exit_Statement
              | A_Goto_Statement
              | A_Return_Statement
              | A_Requeue_Statement
              | A_Requeue_Statement_With_Abort
              =>
               -- Next statements (if any) will not be executed
               -- This is not completely true for an exit with a condition,
               -- but this must happen in a loop, hence the result (for the loop)
               -- will be Some_Paths at best
               return Result;

            when An_Entry_Call_Statement
              | A_Procedure_Call_Statement =>
               declare
                  Context : constant Rule_Context'Class
                    := Matching_Context (Rule_Uses, Called_Name (Statements (I)));
               begin
                  if Context /= No_Matching_Context then
                     Result := Result and Proc_Context(Context).Usage;
                  end if;
               end;

            when A_Raise_Statement =>
               Result := (others => All_Paths);

            when An_If_Statement =>
               declare
                  Paths : constant Asis.Path_List := Statement_Paths (Statements (I));
               begin
                  if Paths'Length = 1 then
                     -- No else part, do as if we had else null;
                     Result := Result and
                              ((Statement_List_Usage (Sequence_Of_Statements (Paths (1)))) or
                               (others => No_Path)
                              );
                  else
                     Result := Result and
                               (Statement_List_Usage (Sequence_Of_Statements (Paths (1))) or
                                Statement_List_Usage (Sequence_Of_Statements (Paths (2)))
                               );
                  end if;
               end;

            when  A_Case_Statement
              | A_Selective_Accept_Statement
              | A_Timed_Entry_Call_Statement
              | A_Conditional_Entry_Call_Statement
              | An_Asynchronous_Select_Statement
              =>
               declare
                  Paths : constant Asis.Path_List := Statement_Paths (Statements (I));
                  Temp  : Search_Result := Statement_List_Usage (Sequence_Of_Statements (Paths (1)));
               begin
                  for I in 2 .. Paths'Last loop
                     Temp := Temp or Statement_List_Usage (Sequence_Of_Statements (Paths (I)));
                  end loop;
                  Result := Result and Temp;
               end;

            when A_Loop_Statement
              | A_While_Loop_Statement
              | A_For_Loop_Statement
              =>
               -- Consider we have a parallel branch which is No_Path for the case where
               -- the loop is not executed
               Result := Result and
                         ((Statement_List_Usage (Loop_Statements (Statements (I)))) or
                          (others => No_Path)
                         );

            when A_Block_Statement =>
               Result := Result and Statement_List_Usage (Block_Statements (Statements (I)));

            when An_Accept_Statement =>
               Result := Result and Statement_List_Usage (Accept_Body_Statements (Statements (I)));

            when Not_A_Statement =>
               Utilities.Failure ("Not a statement in statements list");
         end case;

         if Result = (Result'Range => All_Paths) then
            -- No need to consider further
            return Result;
         end if;
      end loop;

      return Result;
   end Statement_List_Usage;

   -------------------------------
   -- Process_Exception_Handler --
   -------------------------------

   procedure Process_Exception_Handler (Handler : in Asis.Exception_Handler) is
      use Asis.Statements;
      use Framework.Reports;
      use Ada.Strings.Wide_Unbounded;

      Usage : Search_Result;
   begin
      if Rule_Used = (Rule_Used'Range => False) Then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Usage := Statement_List_Usage (Handler_Statements (Handler));

      -- Note: since Check < Search, if both messages apply, only Check
      --       will be output
      for I in Rule_Types loop
         if Rule_Used (I) then
            case Usage (I) is
               when No_Path =>
                  Report (Rule_Id,
                          To_Wide_String (Labels (I)),
                          I,
                          Get_Location (Handler),
                          "no re-raise or report procedure call in exception handler");
                  exit;
               when Some_Paths =>
                  Report (Rule_Id,
                          To_Wide_String (Labels (I)),
                          I,
                          Get_Location (Handler),
                          "some paths have no re-raise or report procedure call " &
                            "in exception handler, check manually");
                  exit;
               when All_Paths =>
                  null;
            end case;
        end if;
      end loop;
   end Process_Exception_Handler;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access,
                                     Prepare => Prepare'Access);
end Rules.Silent_Exceptions;
