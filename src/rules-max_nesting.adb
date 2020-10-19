----------------------------------------------------------------------
--  Rules.Max_Nesting - Package body                                --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
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

-- Asis
with
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements;

-- Adalog
with
  Scope_Manager,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Reports,
  Framework.Rules_Manager;
pragma Elaborate (Framework.Language);

package body Rules.Max_Nesting is
   use Framework, Scope_Manager;

   -- Algorithm:
   --
   -- Only thing worth noting is that the nesting level is one less than the depth
   -- (i.e.: a level 2 unit is nested once). We actually count depths, not nesting,
   -- therefore the offset is adjusted in Add_Control

   type Subrules is (Sr_Default, Sr_All, Sr_Generic, Sr_Separate, Sr_Task);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Subrules,
                                                                             Prefix => "Sr_" );
   type Used_Set is array (Subrules) of Boolean;
   Not_Used : constant Used_Set := (others => False);

   Rule_Used    : Used_Set := Not_Used;
   Save_Used    : Used_Set;

   Max_Depth  : array (Subrules, Control_Kinds) of Scope_Range := (others => (others => Scope_Range'Last));
   Labels     : array (Subrules, Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   Not_Counted : Scope_Range := 0;
   -- Number of active scopes not counted for the Sr_All subrule depth
   -- i.e.: for loops and accept statements

   Generic_Count : Scope_Range := 0;
   -- Depth for the Sr_Generic subrule

   Separate_Count : Scope_Range := 0;
   -- Depth for the Sr_Separate subrule

   Task_Count  : Scope_Range := 0;
   -- Depth for the Sr_Task subrule

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control scopes nested deeper than a given limit.");
      User_Message;
      Help_On_Flags (Header => "Parameter(1):", Footer => "(optional, default=all)", Extra_Value => "");
      User_Message ("Parameter(2): <maximum allowed nesting level>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language, Subrules_Flag_Utilities;

      Max : Asis.ASIS_Integer;
      Sr  : Subrules;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "max nesting value expected");
      end if;

      Sr  := Get_Flag_Parameter (Allow_Any => True);
      if Sr = Sr_Default then
         Sr := Sr_All;
      end if;
      Max := Get_Integer_Parameter (Min => 0);

      if Max_Depth (Sr, Ctl_Kind) /= Scope_Range'Last then
         Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
      end if;
      Max_Depth (Sr, Ctl_Kind) := Scope_Range (Max) + 1;
      Labels    (Sr, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);

     Rule_Used (Sr) := True;
   exception
      when Constraint_Error =>
         Parameter_Error (Rule_Id,
                          "specified nesting greater than allowed maximum of"
                          & Scope_Range'Wide_Image (Scope_Range'Last - 1));
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := Not_Used;
            Max_Depth := (others => (others => Scope_Range'Last));
            Labels    := (others => (others => Null_Unbounded_Wide_String));
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Not_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -----------
   -- Reset --
   -----------

   procedure Reset is
   -- Must reset global counters to 0, otherwise they will be left for next unit
   -- at the nesting level we were on when something bad happened
   begin
      Not_Counted    := 0;
      Generic_Count  := 0;
      Separate_Count := 0;
      Task_Count     := 0;
   end Reset;

   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Sr : Subrules; Depth : Scope_Range; Scope : Asis.Element) is
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Declarations, Asis.Elements;
      use Utilities, Subrules_Flag_Utilities, Framework.Locations, Framework.Reports;
      Scope_Body : Asis.Declaration;
   begin
      -- Don't report on body if there is an explicit spec
      case Declaration_Kind (Scope) is
         when A_Procedure_Body_Declaration
            | A_Function_Body_Declaration
            | A_Package_Body_Declaration
            =>
            Scope_Body := Scope;
            if Is_Subunit (Scope_Body) then
               Scope_Body := Corresponding_Body_Stub (Scope_Body);
            end if;
            if not Is_Nil (Corresponding_Declaration (Scope_Body)) then
               return;
            end if;
         when A_Task_Body_Declaration
            | A_Protected_Body_Declaration
            =>
            -- Those always have an explicit spec
            return;
         when others =>
            null;
      end case;

      -- We check only if it is equal to the first forbidden level.
      -- It is not useful to issue a message if there are even deeper levels.
      if Depth > Max_Depth (Sr, Check) then
         Report (Rule_Id,
                 To_Wide_String (Labels (Sr, Check)),
                 Check,
                 Get_Location (Scope),
                 Choose (Sr = Sr_All, "", Image (Sr, Lower_Case) & ' ')
                    & "nesting deeper than" & Scope_Range'Wide_Image (Max_Depth (Sr, Check)-1)
                    & " (" & Trim_All(Scope_Range'Wide_Image (Depth-1)) & ')');  -- Nesting is Depth-1
      elsif Depth > Max_Depth (Sr, Search) then
         Report (Rule_Id,
                 To_Wide_String (Labels (Sr, Search)),
                 Search,
                 Get_Location (Scope),
                 Choose (Sr = Sr_All, "", Image (Sr, Lower_Case) & ' ')
                    & "nesting deeper than" & Scope_Range'Wide_Image (Max_Depth (Sr, Search)-1)
                    & " (" & Trim_All(Scope_Range'Wide_Image (Depth-1)) & ')');  -- Nesting is Depth-1
      end if;

      -- But counting is independent
      if Depth > Max_Depth (Sr, Count) then
         Report (Rule_Id,
                 To_Wide_String (Labels (Sr, Count)),
                 Count,
                 Get_Location (Scope),
                 "");
      end if;
   end Do_Report;


   -------------------------
   -- Process_Scope_Enter --
   -------------------------

   procedure Process_Scope_Enter (Scope : in Asis.Element) is
      use Asis, Asis.Compilation_Units, Asis.Elements;
      use Thick_Queries;
   begin
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Only Sr_Separate is interested in stubs (to report on stub rather than on separate body)
      if Declaration_Kind (Scope) in A_Body_Stub then
         if Rule_Used (Sr_Separate) then
            Do_Report (Sr_Separate, Separate_Count+1, Scope); -- Separate_Count+1 since the stub is not entered
         end if;
         return;
      end if;

      if Rule_Used (Sr_All) then
         -- Do not count exception handlers and statements other than blocks
         case Element_Kind (Scope) is
            when A_Statement =>
               if Statement_Kind (Scope) /= A_Block_Statement then
                  Not_Counted := Not_Counted + 1;
                  return;
               end if;
            when An_Exception_Handler =>
               Not_Counted := Not_Counted + 1;
               return;
            when others =>
               null;
         end case;
         Do_Report (Sr_All, Current_Depth - Not_Counted, Scope);
      end if;

      -- Count generic nesting
      if Rule_Used (Sr_Generic)
        and then Is_Generic_Unit (Scope)
      then
         Generic_Count := Generic_Count + 1;
         Do_Report (Sr_Generic, Generic_Count, Scope);
      end if;

      -- Count task nesting
      if Rule_Used (Sr_Task) then
         case Declaration_Kind (Scope) is
            when A_Single_Task_Declaration | A_Task_Type_Declaration =>
               -- No need to increment task_count, a task cannot be declared in a task spec
               Do_Report (Sr_Task, Task_Count+1, Scope);
            when A_Task_Body_Declaration =>
               Task_Count := Task_Count + 1;
            when others =>
               null;
         end case;
      end if;

      if Rule_Used (Sr_Separate)
        and then Is_Compilation_Unit (Scope)
        and then Unit_Class (Enclosing_Compilation_Unit (Scope)) = A_Separate_Body
      then
         Separate_Count := Separate_Count + 1;
      end if;
  end Process_Scope_Enter;

   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Process_Scope_Exit (Scope : in Asis.Element) is
      use Asis, Asis.Compilation_Units, Asis.Elements;
      use Thick_Queries;
   begin
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Declaration_Kind (Scope) in A_Body_Stub then
         return;
      end if;

      if Rule_Used (Sr_All) then
         case Element_Kind (Scope) is
            when A_Statement =>
               if Statement_Kind (Scope) /= A_Block_Statement then
                  Not_Counted := Not_Counted - 1;
               end if;
            when An_Exception_Handler =>
               Not_Counted := Not_Counted - 1;
            when others =>
               null;
         end case;
      end if;

      if Rule_Used (Sr_Generic)
        and then Is_Generic_Unit (Scope)
      then
            Generic_Count := Generic_Count - 1;
      end if;

      if Rule_Used (Sr_Task) then
         case Declaration_Kind (Scope) is
            when A_Task_Body_Declaration =>
               Task_Count := Task_Count - 1;
            when others =>
               null;
         end case;
      end if;

      if Rule_Used (Sr_Separate)
        and then Is_Compilation_Unit (Scope)
        and then Unit_Class (Enclosing_Compilation_Unit (Scope)) = A_Separate_Body
      then
         Separate_Count := Separate_Count - 1;
      end if;
   end Process_Scope_Exit;

begin  -- Rules.Max_Nesting
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Reset_CB       => Reset'Access);
end Rules.Max_Nesting;
