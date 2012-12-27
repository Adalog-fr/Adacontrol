----------------------------------------------------------------------
--  Rules.Dependencies - Package body                               --
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

-- Ada
with
  Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis.Clauses,
  Asis.Compilation_Units,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.String_Set;
pragma Elaborate (Framework.Language);

package body Rules.Dependencies is
   use Framework, Framework.Control_Manager;

   -- Counting subrules must stay together:
   type Subrules is (Sr_Others, Sr_Raw, Sr_Direct, Sr_Parent);
   subtype Counting_Subrules is Subrules range Sr_Raw .. Sr_Parent;
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "SR_");

   Allowed_Entities  : Context_Store;

   Counting_Subrules_Count : Control_Index := 0;
   Others_Subrule_Used     : Boolean;
   Rule_Used               : Boolean;
   Save_Used               : Boolean;

   type Counting_Subrule_Contexts is new Basic_Rule_Context with
      record
         Count_Kind : Counting_Subrules;
         Bounds     : Framework.Language.Shared_Keys.Bounds_Values;
      end record;
   Counting_Contexts : array (Control_Index range 1 .. Control_Index'Last) of Counting_Subrule_Contexts;
   Others_Context    : Basic_Rule_Context;

   Raw_Count       : Thick_Queries.Biggest_Natural;
   Direct_Name_Set : Framework.String_Set.Set;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Framework.Language.Shared_Keys, Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control units that depend on units others than those indicated,");
      User_Message ("or whose number of dependencies is not in the specified range");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags (Header => "Parameter(1)   :");
      User_Message ("For subrule ""others"":");
      User_Message ("Parameter(2..3): allowed units");
      User_Message ("For other subrules:");
      User_Message ("Parameter(2..3): <bound> <value>");
      User_Message ("                (at least one parameter required)");
      Help_On_Bounds (Header => "<bound>: ");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Framework.Language.Shared_Keys, Subrules_Flag_Utilities;

      Sr : Subrules;
   begin
      Sr := Get_Flag_Parameter (Allow_Any => False);

      case Sr is
         when Counting_Subrules =>
            if Counting_Subrules_Count = Control_Index'Last then
               Parameter_Error (Rule_Id, "rule specified too many times");
            end if;
            Counting_Subrules_Count := Counting_Subrules_Count + 1;

            begin
               Counting_Contexts (Counting_Subrules_Count) := (Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                                               Count_Kind => Sr,
                                                               Bounds     => Get_Bounds_Parameters (Rule_Id));
            exception
               when Constraint_Error =>
                  Parameter_Error (Rule_Id, "maximum value negative or too big");
            end;

         when Sr_Others =>
            if Others_Subrule_Used then
               Parameter_Error (Rule_Id, """others"" subrule already specified");
            end if;

            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "at least one parameter required");
            end if;

            while Parameter_Exists loop
               declare
                  Entity : constant Entity_Specification := Get_Entity_Parameter;
               begin
                  Associate (Allowed_Entities, Entity, Null_Context);
               exception
                  when Already_In_Store =>
                     Parameter_Error (Rule_Id, "entity already given: " & Image (Entity));
               end;
            end loop;
            Others_Context      := Basic.New_Context (Ctl_Kind, Ctl_Label);
            Others_Subrule_Used := True;
      end case;

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
            Counting_Subrules_Count := 0;
            Others_Subrule_Used     := False;
            Rule_Used               := False;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   ----------------
   -- Enter_Unit --
   ----------------

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      use Framework.String_Set;
      pragma Unreferenced (Unit);
   begin
      if Counting_Subrules_Count = 0 then
         return;
      end if;

      Raw_Count := 0;
      Clear (Direct_Name_Set);
   end Enter_Unit;


   --------------------------
   -- Exit_Context_Clauses --
   --------------------------

   procedure Exit_Context_Clauses (Unit : in Asis.Compilation_Unit) is
      use Ada.Strings.Wide_Fixed;
      use Asis, Asis.Compilation_Units;
      use Framework.String_Set, Thick_Queries;

      procedure Do_Report (Title : Wide_String;
                           Count : Biggest_Natural;
                           Info  : Counting_Subrule_Contexts)
      is
         use Asis.Elements;
         use Framework.Language.Shared_Keys, Framework.Reports;
      begin
         if not Is_In (Count, Info.Bounds) then
            if Info.Bounds.Min = 0 then
               Report (Rule_Id,
                       Info,
                       Get_Location (Unit_Declaration (Unit)),
                       Title & " > " & Biggest_Int_Img (Info.Bounds.Max)
                       & " (" & Biggest_Int_Img (Count) & ')');
            elsif Info.Bounds.Max = Biggest_Natural'Last then
               Report (Rule_Id,
                       Info,
                       Get_Location (Unit_Declaration (Unit)),
                       Title & " < " & Biggest_Int_Img (Info.Bounds.Min)
                       & " (" & Biggest_Int_Img (Count) & ')');
            else
               Report (Rule_Id,
                       Info,
                       Get_Location (Unit_Declaration (Unit)),
                       Title & " not in "
                       & Biggest_Int_Img (Info.Bounds.Min) & ".." & Biggest_Int_Img (Info.Bounds.Max)
                       & " (" & Biggest_Int_Img (Count) & ')');
            end if;
         end if;
      end Do_Report;

   begin  -- Exit_Context_Clauses
      if Counting_Subrules_Count = 0 then
         return;
      end if;

      for I in Control_Index range 1 .. Counting_Subrules_Count loop
         case Counting_Contexts (I).Count_Kind is
            when Sr_Raw =>
               Do_Report ("number of withed units", Raw_Count, Counting_Contexts (I));
            when Sr_Direct =>
               Do_Report ("direct dependencies", Biggest_Int (Cardinal (Direct_Name_Set)), Counting_Contexts (I));
            when Sr_Parent =>
               case Unit_Kind (Unit) is
                  when A_Subunit | A_Package_Body =>
                     -- Do not report subunits, package bodies always have specs
                     null;
                  when A_Subprogram_Body =>
                     -- Report only if there is no spec
                     if Is_Nil (Corresponding_Declaration (Unit)) then
                        Do_Report ("number of parents", Biggest_Int (Count (Unit_Full_Name (Unit), ".")),
                                   Counting_Contexts (I));
                     end if;
                  when others =>
                     Do_Report ("number of parents", Biggest_Int (Count (Unit_Full_Name (Unit), ".")),
                                Counting_Contexts (I));
               end case;
         end case;
      end loop;
   end Exit_Context_Clauses;


   -------------------------
   -- Process_With_Clause --
   -------------------------

   procedure Process_With_Clause (Clause : in Asis.Clause) is
      use Asis, Asis.Clauses, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Framework.String_Set, Thick_Queries, Utilities;
   begin
      if not Rule_Used then
         return;
      end if;

      declare
         Names : constant Asis.Name_List := Clause_Names (Clause);
         Elem  : Asis.Expression;
      begin
         if Counting_Subrules_Count /= 0 then
            Raw_Count := Raw_Count + Names'Length;
         end if;
         for N in Names'Range loop
            if Counting_Subrules_Count /= 0 then
               Elem := Names (N);
               Add (Direct_Name_Set, To_Upper (Full_Name_Image (Ultimate_Name (Elem))));
               while Expression_Kind (Elem) = A_Selected_Component loop
                  Elem := Prefix (Elem);
                  Add (Direct_Name_Set, To_Upper (Full_Name_Image (Elem)));
               end loop;
            end if;

            if Others_Subrule_Used then
               Elem := Names (N);
               if Matching_Context (Allowed_Entities, Elem, Extend_To => All_Extensions) = No_Matching_Context then
                  Report (Rule_Id,
                          Others_Context,
                          Get_Location (Elem),
                          "unit depends on " & Full_Name_Image (Ultimate_Name (Elem)));
               end if;
            end if;
         end loop;
      end;
   end Process_With_Clause;

begin  -- Rules.Dependencies
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Dependencies;
