----------------------------------------------------------------------
--  Rules.Unreferenced_Units - Package body                         --
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

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Asis
with
  Asis.Clauses,
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Binary_Map,
  Utilities;

-- AdaControl
with
  Framework.Queries,
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Units is
   use Framework, Framework.Control_Manager, Ada.Strings.Wide_Unbounded;

   type Subrules is (Unreferenced, Unchecked);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Usages is array (Subrules) of Boolean;
   Rule_Used : Usages := (others => False);
   Save_Used : Usages;

   Ctl_Contexts : array (Subrules) of Basic_Rule_Context;

   type Unit_Info is
      record
         Checked : Boolean;
         Withed  : Boolean;
         Loc     : Locations.Location;
      end record;
   -- If Processed is True, Loc is the location of the unit
   -- otherwise, it is the location of the first encountered with that references the unit.

   package Units_Map is new Binary_Map (Unbounded_Wide_String, Unit_Info);
   Units_Table : Units_Map.Map;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control units not analyzed, or not referenced from the rest of the project");
      User_Message;
      Help_On_Flags ("Parameter(s):");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities;

      Subrule : Subrules;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Subrule := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Subrule) then
               if not Basic.Merge_Context (Ctl_Contexts (Subrule), Ctl_Kind, Ctl_Label) then
                  Parameter_Error (Rule_Id, "rule already specified for " & Subrules'Wide_Image (Subrule));
               end if;
            else
               Ctl_Contexts (Subrule) := Basic.New_Context (Ctl_Kind, Ctl_Label);
               Rule_Used    (Subrule) := True;
            end if;
         end loop;
      else
         Ctl_Contexts := (others => Basic.New_Context (Ctl_Kind, Ctl_Label));
         Rule_Used    := (others => True);
      end if;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager, Units_Map;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => False);
            Clear (Units_Table);
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
      use Units_Map;
   begin
      Balance (Units_Table);
   end Prepare;

   --------------
   -- Finalize --
   --------------

   procedure Report_One (Key : in Unbounded_Wide_String; Value : in out Unit_Info) is
      use Framework.Reports, Utilities;
   begin
      if Value.Withed then
         if not Value.Checked and Rule_Used (Unchecked) then
            Report (Rule_Id,
                    Ctl_Contexts (Unchecked),
                    Value.Loc,
                    "unit " & To_Title (Strip_Profile (To_Wide_String (Key))) & " not processed by AdaControl");
         end if;
      else
         if Rule_Used (Unreferenced) then
            Report (Rule_Id,
                    Ctl_Contexts (Unreferenced),
                    Value.Loc,
                    "unit " & To_Title (Strip_Profile (To_Wide_String (Key))) & " not withed by any unit");
         end if;
      end if;
   end Report_One;

   procedure Report_All is new Units_Map.Iterate (Report_One);

   procedure Finalize is
   begin
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Report_All (Units_Table);
   end Finalize;

   ------------
   -- Update --
   ------------

   type Usage_Kind is (Definition, Reference);

   procedure Update (Name : Asis.Expression; Usage : Usage_Kind) is
      use Framework.Locations, Framework.Queries, Units_Map;

      Key  : constant Unbounded_Wide_String := To_Key (Name);
      Info : Unit_Info := Fetch (Units_Table,
                                 Key,
                                 Default_Value => (Checked => False, Withed => False, Loc => Null_Location));
   begin
      case Usage is
         when Definition =>
            Info.Loc     := Get_Location (Name);
            Info.Checked := True;
            Add (Units_Table, Key, Info);
         when Reference =>
            if not Info.Withed then
               if not Info.Checked then
                  Info.Loc := Get_Location (Name);
               end if;
               Info.Withed := True;
               Add (Units_Table, Key, Info);
            end if;
      end case;
   end Update;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Unit : in Asis.Compilation_Unit) is
      use Asis, Asis.Compilation_Units, Asis.Declarations, Asis.Elements;
   begin
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Unit_Kind (Unit) in A_Subunit then
         -- those are never withed by definition...
         return;
      end if;

      Update (Names (Unit_Declaration (Unit))(1), Definition);
   end Process_Unit;

   -------------------------
   -- Process_With_Clause --
   -------------------------

   procedure Process_With_Clause (Clause : in Asis.Clause) is
      use Utilities;
      use Asis, Asis.Clauses, Asis.Compilation_Units, Asis.Elements, Asis.Expressions;

      function Element_Unit (E : Asis.Expression) return Asis.Compilation_Unit is
      begin
         case Expression_Kind (E) is
            when An_Identifier =>
               return Enclosing_Compilation_Unit (Corresponding_Name_Declaration (E));
            when A_Selected_Component =>
               return Enclosing_Compilation_Unit (Corresponding_Name_Declaration (Selector (E)));
            when others =>
               Failure ("Element_Unit");
         end case;
      end Element_Unit;
   begin  -- Process_With_Clause
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Id    : Asis.Expression;
      begin
         for N : Asis.Name of Clause_Names (Clause) loop
            Id := N;
            --
            if Unit_Origin (Element_Unit (Id)) = An_Application_Unit then  --##Rule line off Use_Ultimate_Origin
                                                                           --  Don't follow renamings here
               loop
                  case Expression_Kind (Id) is
                     when An_Identifier =>
                        Update (Id, Reference);
                        exit;
                     when A_Selected_Component =>
                        Update (Selector (Id), Reference);
                        Id := Prefix (Id);
                     when others =>
                        Failure ("Incorrect name in with clause", Clause);
                  end case;
               end loop;
            end if;
         end loop;
      end;
   end Process_With_Clause;

begin  -- Rules.Units
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB         => Help'Access,
                                     Add_Control_CB  => Add_Control'Access,
                                     Command_CB      => Command'Access,
                                     Prepare_CB      => Prepare'Access,
                                     Finalize_CB     => Finalize'Access);
end Rules.Units;
