----------------------------------------------------------------------
--  Rules.Entities - Package body                                   --
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

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Entities is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- This rule was intended to be the simplest one: each time an identifier is encountered,
   -- just check if it is associated to a context.
   --
   -- Alas! Nothing is simple when renamings come into play.
   -- If the identifier is declared by renaming, every record field and variable name which
   -- is part of the renamed expression is "used", (but not parts that are evaluated by the
   -- elaboration of the declaration). This is taken care of by Thick_Queries.Used_Identifiers.

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   type Entity_Context is new Basic_Rule_Context with
      record
         Places        : Language.Shared_Keys.Places_Set;
         Instance_Only : Boolean;
      end record;
   Searched_Entities  : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Framework.Language.Shared_Keys, Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of any Ada entity");
      User_Message;
      User_Message ("Parameter(s): {<location>} [instance] <Entity name>");
      Help_On_Scope_Places (Header => "<location>:", Expected => (S_All => False, others => True));
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Framework.Language.Shared_Keys;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      while Parameter_Exists loop
         declare
            Places        : constant Places_Set           := Get_Places_Set_Modifiers (Rule_Id, Allow_All => False);
            Instance_Only : constant Boolean              := Get_Modifier ("INSTANCE");
            Entity        : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Searched_Entities, Entity, Entity_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label)
                                                                  with Places, Instance_Only));
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
            Clear (Searched_Entities);
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
      Balance (Searched_Entities);
   end Prepare;


   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Entity : Asis.Expression; Loc : Locations.Location) is
      use Framework.Reports, Framework.Language.Shared_Keys;
      use Utilities;

      Current_Context : constant Root_Context'Class := Matching_Context (Searched_Entities,
                                                                         Entity,
                                                                         Extend_To => (Instance => True,
                                                                                       Renaming => False));
      -- Renaming => False because renamings have been dealt with by Used_Indentifiers
   begin
      if Current_Context /= No_Matching_Context then
         declare
            Good_Context : Entity_Context renames Entity_Context (Current_Context);
         begin
            if Is_Applicable (Good_Context.Places)
              and then (not Good_Context.Instance_Only
                        or Last_Matching_Kind (Searched_Entities) in Instance .. From_Instance)
            then
               if Good_Context.Places = Everywhere then
                  Report (Rule_Id,
                          Current_Context,
                          Loc,
                          "use of element """
                          & Adjust_Image (To_Title (Last_Matching_Name (Searched_Entities)))
                          & '"');
               else
                  Report (Rule_Id,
                          Current_Context,
                          Loc,
                          Image (Good_Context.Places)
                          & "use of element """
                          & Adjust_Image (To_Title (Last_Matching_Name (Searched_Entities)))
                          & '"');
               end if;
            end if;
         end;
      end if;
   end Do_Report;


   -----------------------
   -- Process_Attribute --
   -----------------------

   procedure Process_Attribute (Attr : in Asis.Expression) is
      use Framework.Locations;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Do_Report (Attr, Get_Location (Attr));
   end Process_Attribute;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Identifier : in Asis.Expression) is
      use Framework.Locations, Thick_Queries;

   begin   -- Process_Identifier
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      for Id : Asis.Expression of Used_Identifiers (Identifier) loop
         Do_Report (Id, Get_Location (Identifier));
      end loop;
   end Process_Identifier;

begin  -- Rules.Entities
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Entities;
