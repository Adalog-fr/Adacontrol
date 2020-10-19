----------------------------------------------------------------------
--  Rules.Not_Selected_Name - Package body                          --
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

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Reports.Fixes;
pragma Elaborate (Framework.Language);

package body Rules.Not_Selected_Name is
   use Framework, Framework.Control_Manager;

   type Exceptions is (None, Unit, Compilation, Family);
   package Exceptions_Flag_Utilities is new Framework.Language.Flag_Utilities (Exceptions);

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   type Selected_Context is new Basic_Rule_Context with
      record
         Allowed : Exceptions;
      end record;
   Searched_Entities  : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Exceptions_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control usages of an Ada entities that do not use selected notation");
      User_Message;
      Help_On_Flags ("Parameter(1):");
      User_Message ("Parameter(2..): <Entity name>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Exceptions_Flag_Utilities;
      Exc : Exceptions;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "exceptions keyword expected");
      end if;
      Exc := Get_Flag_Parameter (Allow_Any => False);

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one entity required");
      end if;
      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Searched_Entities,
                       Entity,
                       Selected_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Exc));
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

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Element : in Asis.Expression) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Utilities, Thick_Queries;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Current_Context : constant Root_Context'Class := Matching_Context (Searched_Entities,
                                                                            Element,
                                                                            Extend_To => (Instance => True,
                                                                                          Renaming => False));
         Encl : Asis.Element;
      begin
         if Current_Context = No_Matching_Context then
            return;
         end if;

         Encl := Enclosing_Element (Element);
         if Expression_Kind (Encl) = A_Selected_Component
           and then Is_Equal (Selector (Encl), Element)
         then
            return;
         end if;

         case Selected_Context (Current_Context).Allowed is
            when None =>
               null;
            when Unit =>
               declare
                  Elem_Name : constant Wide_String := To_Upper (Full_Name_Image (Enclosing_Program_Unit (Element)));
                  Decl_Name : constant Wide_String := To_Upper (Full_Name_Image
                                                                (Enclosing_Program_Unit
                                                                 (Corresponding_Name_Declaration
                                                                  (Element))));
               begin
                  if Elem_Name = Decl_Name or else Starts_With (Elem_Name, Decl_Name & '.') then
                     return;
                  end if;
               end;
            when Compilation =>
               -- Using Full_Name_Image below avoids having to check for specs/bodies
               if To_Upper (Full_Name_Image (Names
                            (Unit_Declaration
                             (Enclosing_Compilation_Unit
                              (Element))) (1))) =
                  To_Upper (Full_Name_Image (Names
                            (Unit_Declaration
                             (Enclosing_Compilation_Unit
                              (Corresponding_Name_Declaration (Element)))) (1)))
               then
                  return;
               end if;
            when Family =>
               declare
                  Elem_Unit_Name : constant Wide_String := To_Upper (Full_Name_Image
                                                                     (Names
                                                                      (Unit_Declaration
                                                                       (Enclosing_Compilation_Unit
                                                                        (Element))) (1)));
                  Decl_Unit_Name : constant Wide_String := To_Upper (Full_Name_Image
                                                                     (Names
                                                                      (Unit_Declaration
                                                                       (Enclosing_Compilation_Unit
                                                                        (Corresponding_Name_Declaration
                                                                         (Element)))) (1)));
               begin
                  if Elem_Unit_Name = Decl_Unit_Name or else Starts_With (Elem_Unit_Name, Decl_Unit_Name & '.') then
                     return;
                  end if;
               end;
         end case;

         Report (Rule_Id,
                 Current_Context,
                 Get_Location (Element),
                 "non-selected use of element """ & To_Title (Last_Matching_Name (Searched_Entities)) & '"');
         Fixes.Replace (Element, By => Full_Name_Image (Element));
      end;
   end Process_Identifier;

begin  -- Rules.Not_Selected_Name
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Not_Selected_Name;
