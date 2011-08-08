----------------------------------------------------------------------
--  Rules.Use_Clauses - Package body                                --
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
  Asis.Clauses;

-- Ada
with
  Ada.Strings.Wide_Unbounded;

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
pragma Elaborate (Framework.Language);

package body Rules.Use_Clauses is
   use Framework;

   type Subrules is (Both, Local, Global);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

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
      Help_On_Flags ("Parameter(1): ", Extra_Value => "<Allowed package name>", Footer => "(optional)");
      User_Message ("Parameter(2..): <Allowed package name>");
      User_Message ("Control occurrences of use clauses that mention");
      User_Message ("any package but the ones passed as parameters (if any)");
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
      else
         Subrule := Both;
      end if;

      if Subrule = Both then
         if Rule_Used (Local, Ctl_Kind) or Rule_Used (Global, Ctl_Kind) then
            Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
         end if;
         Ctl_Labels (Local,  Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
         Ctl_Labels (Global, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
         Rule_Used  (Local,  Ctl_Kind) := True;
         Rule_Used  (Global, Ctl_Kind) := True;
      else
         if Rule_Used (Subrule, Ctl_Kind) then
            Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
         end if;
         Ctl_Labels (Subrule, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
         Rule_Used  (Subrule, Ctl_Kind) := True;
      end if;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
            Value  : Package_Context := (Allowed => (others => (others => False)));
         begin
            if Subrule = Both then
               Value.Allowed (Local,  Ctl_Kind) := True;
               Value.Allowed (Global, Ctl_Kind) := True;
            else
               Value.Allowed (Subrule, Ctl_Kind) := True;
            end if;
            Associate (Allowed_Packages, Entity, Value);
         exception
            when Already_In_Store =>
               Value := Package_Context (Association (Allowed_Packages, Entity));
               if Subrule = Both then
                  Value.Allowed (Local,  Ctl_Kind) := True;
                  Value.Allowed (Global, Ctl_Kind) := True;
               else
                  Value.Allowed (Subrule, Ctl_Kind) := True;
               end if;
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

   -------------------------------
   -- Process_Use_Clause --
   -------------------------------

   procedure Process_Use_Clause (Clause : in Asis.Clause) is
      use Ada.Strings.Wide_Unbounded, Asis.Clauses, Thick_Queries;
      use Framework.Reports, Framework.Scope_Manager;
      Loc : Subrules;
   begin
      if Rule_Used = (Subrules => (Control_Kinds => False)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if In_Context_Clauses then
         Loc := Global;
      else
         Loc := Local;
      end if;

      declare
         Names : constant Asis.Name_List := Clause_Names (Clause);
      begin
         for N in Names'Range loop
            declare
               Context : constant Root_Context'Class := Matching_Context (Allowed_Packages, Names (N));
            begin
               for R in Control_Kinds loop
                  if Rule_Used (Loc, R) and then
                    (Context = No_Matching_Context or else
                     not Package_Context (Context).Allowed (Loc, R))
                  then
                     Report (Rule_Id,
                             To_Wide_String (Ctl_Labels (Loc, R)),
                             R,
                             Get_Location (Clause),
                             "use clause for """ & Extended_Name_Image (Names (N)) & '"');
                     exit;
                  end if;
               end loop;
            end;
         end loop;
      end;
   end Process_Use_Clause;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Use_Clauses;
