----------------------------------------------------------------------
--  Rules.Use_Clauses - Package body                                --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2018.           --
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

-- ASIS
with
  Asis.Clauses,
  Asis.Elements;

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Scope_Manager,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Reports.Fixes;
pragma Elaborate (Framework.Language);

package body Rules.Use_Clauses is
   use Framework, Framework.Control_Manager;

   type Subrules is (Sr_Default,
                     Sr_Package,  Sr_Local,          Sr_Global,
                     Sr_Type,     Sr_Type_Local,     Sr_Type_Global,
                     Sr_All_Type, Sr_All_Type_Local, Sr_All_Type_Global);
   subtype True_Subrules is Subrules range Subrules'Succ (Subrules'First) .. Subrules'Last;
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "SR_");

   type Usage is array (Subrules, Control_Kinds) of Boolean;

   Rule_Used  : Usage := (others => (others => False));
   Save_Used  : Usage;
   Ctl_Labels : array (Subrules, Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type Usage_Type is array (Subrules, Control_Kinds) of Boolean;
   type Package_Context is new Root_Context with
      record
         Allowed : Usage_Type;
      end record;

   Allowed_Packages : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of use clauses or use [all] type clauses that mention");
      User_Message ("any package/type other than the ones passed as parameters (if any)");
      User_Message;
      Help_On_Flags ("Parameter(1): ", Footer => "(optional)", Extra_Value => "");
      User_Message ("Parameter(2..): <Allowed package/type name>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language, Subrules_Flag_Utilities;

      Subrule : Subrules;
   begin
      if Parameter_Exists then
         Subrule := Get_Flag_Parameter (Allow_Any => True);
         if Subrule = Sr_Default then
            Subrule := Sr_Package;
         end if;
      else
         Subrule := Sr_Package;
      end if;

      case True_Subrules'(Subrule) is
         when Sr_Package =>
            if Rule_Used (Sr_Local, Ctl_Kind) or Rule_Used (Sr_Global, Ctl_Kind) then
               Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
            end if;
            Ctl_Labels (Sr_Local,  Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Ctl_Labels (Sr_Global, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Rule_Used  (Sr_Local,  Ctl_Kind) := True;
            Rule_Used  (Sr_Global, Ctl_Kind) := True;
         when Sr_Type =>
            if Rule_Used (Sr_Type_Local, Ctl_Kind) or Rule_Used (Sr_Type_Global, Ctl_Kind) then
               Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
            end if;
            Ctl_Labels (Sr_Type_Local,  Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Ctl_Labels (Sr_Type_Global, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Rule_Used  (Sr_Type_Local,  Ctl_Kind) := True;
            Rule_Used  (Sr_Type_Global, Ctl_Kind) := True;
         when Sr_All_Type =>
            if Rule_Used (Sr_All_Type_Local, Ctl_Kind) or Rule_Used (Sr_All_Type_Global, Ctl_Kind) then
               Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
            end if;
            Ctl_Labels (Sr_All_Type_Local,  Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Ctl_Labels (Sr_All_Type_Global, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Rule_Used  (Sr_All_Type_Local,  Ctl_Kind) := True;
            Rule_Used  (Sr_All_Type_Global, Ctl_Kind) := True;
         when others =>  -- Subrules that mention explicitely local or global
            if Rule_Used (Subrule, Ctl_Kind) then
               Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
            end if;
            Ctl_Labels (Subrule, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Rule_Used  (Subrule, Ctl_Kind) := True;
      end case;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
            Value  : Package_Context := (Allowed => (others => (others => False)));
         begin
            case Subrule is
               when Sr_Package =>
                  Value.Allowed (Sr_Local,  Ctl_Kind) := True;
                  Value.Allowed (Sr_Global, Ctl_Kind) := True;
               when Sr_Type =>
                  Value.Allowed (Sr_Type_Local,  Ctl_Kind) := True;
                  Value.Allowed (Sr_Type_Global, Ctl_Kind) := True;
               when Sr_All_Type =>
                  Value.Allowed (Sr_All_Type_Local,  Ctl_Kind) := True;
                  Value.Allowed (Sr_All_Type_Global, Ctl_Kind) := True;
               when others =>
                  Value.Allowed (Subrule, Ctl_Kind) := True;
            end case;
            Associate (Allowed_Packages, Entity, Value);
         exception
            when Already_In_Store =>
               Value := Package_Context (Association (Allowed_Packages, Entity));
               case Subrule is
                  when Sr_Package =>
                     Value.Allowed (Sr_Local,  Ctl_Kind) := True;
                     Value.Allowed (Sr_Global, Ctl_Kind) := True;
                  when Sr_Type =>
                     Value.Allowed (Sr_Type_Local,  Ctl_Kind) := True;
                     Value.Allowed (Sr_Type_Global, Ctl_Kind) := True;
                  when Sr_All_Type =>
                     Value.Allowed (Sr_All_Type_Local,  Ctl_Kind) := True;
                     Value.Allowed (Sr_All_Type_Global, Ctl_Kind) := True;
                  when others =>
                     Value.Allowed (Subrule, Ctl_Kind) := True;
               end case;
               Update (Allowed_Packages, Value);
         end;
      end loop;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := (others => (others => False));
            Ctl_Labels := (others => (others => Null_Unbounded_Wide_String));
            Clear (Allowed_Packages);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => (others => False));
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Allowed_Packages);
   end Prepare;

   ------------------------
   -- Process_Use_Clause --
   ------------------------

   procedure Process_Use_Clause (Clause : in Asis.Clause) is
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Clauses, Asis.Elements;
      use Framework.Locations, Framework.Reports, Scope_Manager, Thick_Queries, Utilities;
      Loc : Subrules;
   begin
      if Rule_Used = (Subrules => (Control_Kinds => False)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if In_Context_Clauses then
         case Clause_Kind (Clause) is
            when A_Use_Package_Clause =>
               Loc := Sr_Global;
            when A_Use_Type_Clause =>
               Loc := Sr_Type_Global;
            when A_Use_All_Type_Clause =>
               Loc := Sr_All_Type_Global;
            when others =>
               Failure ("Use_Clauses: bad clause", Clause);
         end case;
      else
         case Clause_Kind (Clause) is
            when A_Use_Package_Clause =>
               Loc := Sr_Local;
            when A_Use_Type_Clause =>
               Loc := Sr_Type_Local;
            when A_Use_All_Type_Clause =>
               Loc := Sr_All_Type_Local;
            when others =>
               Failure ("Use_Clauses: bad clause", Clause);
         end case;
      end if;

      declare
         Names : constant Asis.Name_List := Clause_Names (Clause);
         Fix   : Fixes.Incremental_Fix;

         function Use_Qualifier return Wide_String is
         begin
            case Loc is
               when Sr_Local | Sr_Global =>
                  return "";
               when Sr_Type_Local | Sr_Type_Global =>
                  return "type ";
               when Sr_All_Type_Local | Sr_All_Type_Global =>
                  return "all type ";
               when others =>
                  Failure ("Use clauses: bad loc " & Subrules'Wide_Image (Loc));
            end case;
         end Use_Qualifier;
      begin
         for N in Names'Range loop
            declare
               Context : constant Root_Context'Class := Matching_Context (Allowed_Packages, Names (N));
            begin
               if Rule_Used (Loc, Check) and then
                 (Context = No_Matching_Context or else
                    not Package_Context (Context).Allowed (Loc, Check))
               then
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Labels (Loc, Check)),
                          Check,
                          Get_Location (Clause),
                          "use " & Use_Qualifier & "clause for """ & Extended_Name_Image (Names (N)) & '"');
                  Fixes.List_Remove (Fix, N, From => Clause);
               elsif Rule_Used (Loc, Search) and then
                 (Context = No_Matching_Context or else
                    not Package_Context (Context).Allowed (Loc, Search))
               then
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Labels (Loc, Search)),
                          Search,
                          Get_Location (Clause),
                          "use " & Use_Qualifier & "clause for """ & Extended_Name_Image (Names (N)) & '"');
                  Fixes.List_Remove (Fix, N, From => Clause);
               end if;

               if Rule_Used (Loc, Count) and then
                 (Context = No_Matching_Context or else
                    not Package_Context (Context).Allowed (Loc, Count))
               then
                  Report (Rule_Id,
                          To_Wide_String (Ctl_Labels (Loc, Count)),
                          Count,
                          Get_Location (Clause),
                          "");
               end if;
            end;
         end loop;
         Fixes.Flush (Fix);
      end;
   end Process_Use_Clause;

begin  -- Rules.Use_Clauses
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Use_Clauses;
