----------------------------------------------------------------------
--  Rules.Aspects - Package body                                    --
--                                                                  --
--  This software is (c) Adalog 2004-2012.                          --
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
   Asis.Definitions,
   Asis.Elements,
   Asis.Expressions;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Aspects is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- This rule is very simple: just check if the aspect mark is associated to a context.
   -- One little nasty detail though: Corresponding_Name_Definition does not work on an aspect mark,
   -- it is therefore necessary to pass an image of the identifier rather than the identifier itself
   -- to the context store.

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Searched_Entities  : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of aspect specifications");
      User_Message;
      User_Message ("Parameter(s): <Aspect_Mark> | implicit_true | all");
      User_Message ("             (optional, default = all)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            declare
               Entity : constant Wide_String := Get_Name_Parameter;
            begin
               Associate (Searched_Entities, Value (Entity), Basic.New_Context (Ctl_Kind, Ctl_Label) );
            exception
               when Already_In_Store =>
                  Parameter_Error (Rule_Id, "Aspect mark already given: " & Entity);
            end;
         end loop;
      else
         Associate (Searched_Entities, Value ("all"), Basic.New_Context (Ctl_Kind, Ctl_Label) );
      end if;

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

   procedure Do_Report (Entity : Asis.Expression) is
      use Framework.Locations, Framework.Reports;

      function Mark_Name (E : Asis.Expression) return Wide_String is
         use Asis, Asis.Elements, Asis.Expressions;
      begin
         if Expression_Kind (E) = An_Attribute_Reference then -- Can only be 'Class, per Ada syntax
            return Name_Image (Prefix (E)) & "'Class";
         else
            return Name_Image (E);
         end if;
      end Mark_Name;

      Current_Context : constant Root_Context'Class := Control_Manager.Association (Searched_Entities,
                                                                                    Value (Mark_Name (Entity)));
   begin   -- Do_Report
      if Current_Context = No_Matching_Context then
         -- Reminder: Report does nothing for No_Matching_Context
         Report (Rule_Id,
                 Control_Manager.Association (Searched_Entities, Value ("all")),
                 Get_Location (Entity),
                 "use of aspect """
                 & Mark_Name(Entity)
                 & '"');
      else
         Report (Rule_Id,
                 Current_Context,
                 Get_Location (Entity),
                 "use of aspect """
                 & Mark_Name(Entity)
                 & '"');
      end if;
   end Do_Report;


   --------------------
   -- Process_Aspect --
   --------------------

   procedure Process_Aspect (The_Aspect : in Asis.Definition) is
      use Asis.Elements, Asis.Definitions;
      use Framework.Locations, Framework.Reports;
   begin   -- Process_Aspect
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Do_Report (Aspect_Mark (The_Aspect));

      if Is_Nil (Aspect_Definition (The_Aspect)) then
         declare
            Current_Context : constant Root_Context'Class := Control_Manager.Association (Searched_Entities,
                                                                                          Value ("implicit_true"));
         begin
            if Current_Context /= No_Matching_Context then
               Report (Rule_Id,
                       Current_Context,
                       Get_Location (The_Aspect),
                       "use of aspect with implicit ""True"" association");
            end if;
         end;
      end if;
   end Process_Aspect;

begin  -- Rules.Aspects
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Aspects;
