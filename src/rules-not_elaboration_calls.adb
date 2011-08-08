----------------------------------------------------------------------
--  Rules.Not_Elaboration_Calls - Package body                      --
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

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Asis
with
  Asis.Elements;

-- Adactl
with
  Framework.Language,
  Framework.Reports,
  Framework.Rules_Manager,
  Framework.Scope_Manager;

package body Rules.Not_Elaboration_Calls is
   use Framework, Utilities;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Subprograms : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): <subprogram name>");
      User_Message ("Control subprogram calls that happen elsewhere than as part");
      User_Message ("of the elaboration of a library package.");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Subprograms, Entity, Basic.New_Context (Ctl_Kind, Ctl_Label));
         exception
            when Already_In_Store =>
               Parameter_Error (Rule_Id, "subprogram already given: " & Image (Entity));
         end;
      end loop;

      Rule_Used  := True;

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
            Clear (Subprograms);
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
      Balance (Subprograms);
   end Prepare;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Element) is
      use Thick_Queries;
      use Asis, Asis.Elements;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         use Framework.Reports, Framework.Scope_Manager;
         Current_Context : constant Root_Context'Class := Matching_Context (Subprograms,
                                                                            Called_Simple_Name (Call));
         Scopes          : constant Scope_List := Active_Scopes;
      begin
         if Current_Context = No_Matching_Context then
            return;
         end if;

         -- The enclosing scopes must not contain any procedure, function or entry
         for I in Scopes'Range loop
            case Declaration_Kind (Scopes (I)) is
               when A_Procedure_Body_Declaration
                 | A_Function_Body_Declaration
                 | A_Task_Body_Declaration
                 | A_Protected_Body_Declaration
                 | An_Entry_Body_Declaration
                 =>
                  Report (Rule_Id,
                          Current_Context,
                          Get_Location (Call),
                          "call of """ & To_Title (Last_Matching_Name (Subprograms)) & '"');
               when others =>
                  -- This covers :
                  --   all specifications (no call can happen there)
                  --   things that are not declarations, i.e. statements and exception
                  --   handlers
                  null;
            end case;
         end loop;
      end;
   end Process_Call;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Not_Elaboration_Calls;
