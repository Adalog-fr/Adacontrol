----------------------------------------------------------------------
--  Rules.Allocators - Package body                                 --
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

-- Asis
with
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

package body Rules.Allocators is
   use Framework, Framework.Control_Manager;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Entities  : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): task | protected | <allocated type> (optional)");
      User_Message ("Control occurrences of allocators, either all of them,");
      User_Message ("or just those for tasks, protected types, or specific type(s)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds) is
      use Framework.Language;
      Entity : Entity_Specification;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Entity := Get_Entity_Parameter;
            Associate (Entities, Entity, Basic.New_Context (Ctl_Kind, Ctl_Label));
         end loop;
      else
         Entity := Value ("ALL");
         Associate (Entities, Entity, Basic.New_Context (Ctl_Kind, Ctl_Label));
      end if;

      Rule_Used  := True;
   exception
      when Already_In_Store =>
         Parameter_Error (Rule_Id, "type or keyword already given: " & Image (Entity));
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
            Clear (Entities);
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
      Balance (Entities);
   end Prepare;

   -----------------------
   -- Process_Allocator --
   -----------------------

   procedure Process_Allocator (Element : in Asis.Element) is
      use Utilities, Thick_Queries;
      use Asis, Asis.Expressions, Asis.Elements;

      Found : Boolean;

      procedure Check (Current_Context : Root_Context'Class) is
         use Framework.Reports;
      begin
         if Current_Context = No_Matching_Context then
            Found := False;
            return;
         end if;

         if Last_Matching_Name (Entities) = "" then
            Report (Rule_Id,
                    Current_Context,
                    Get_Location (Element),
                    "allocator");
         else
            Report (Rule_Id,
                    Current_Context,
                    Get_Location (Element),
                    "allocator for " & To_Title (Last_Matching_Name (Entities)));
         end if;
         Found := True;
      end Check;

      E : Asis.Element;
   begin  -- Process_Allocator
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Retrieve in E the good subtype mark
      case Expression_Kind (Element) is
         when An_Allocation_From_Subtype =>
            E := Subtype_Simple_Name (Allocator_Subtype_Indication (Element));
         when An_Allocation_From_Qualified_Expression =>
            E := Simple_Name (Converted_Or_Qualified_Subtype_Mark (Allocator_Qualified_Expression (Element)));
         when others =>
            Failure (Rule_Id & ": Unexpected element", Element);
      end case;

      -- E can be an attribute, T'Base or T'Class
      -- T'Base has the same first named subtype as T
      -- T'Base is only allowed for scalar types, therefore we cannot have T'Base'Class
      -- nor T'Class'Base
      if Expression_Kind (E) = An_Attribute_Reference then
         case A4G_Bugs.Attribute_Kind (E) is
            when A_Base_Attribute =>
               E := Simple_Name (First_Subtype_Name (Simple_Name (Prefix (E))));
            when A_Class_Attribute =>
               null;
            when others =>
               Failure ("Unexpected attribute", E);
         end case;
      else
         E := First_Subtype_Name (E);
      end if;

      Check (Matching_Context (Entities, E, Extend_To => All_Extensions));
      if not Found and Expression_Kind (E) /= An_Attribute_Reference then
         E := A4G_Bugs.Corresponding_Name_Declaration (E);
         if Is_Type_Declaration_Kind (E, A_Task_Type_Declaration) then
            Check (Control_Manager.Association (Entities, "TASK"));
         elsif Is_Type_Declaration_Kind (E, A_Protected_Type_Declaration) then
            Check (Control_Manager.Association (Entities, "PROTECTED"));
         end if;
      end if;

      if not Found then
         Check (Control_Manager.Association (Entities, "ALL"));
      end if;
   end Process_Allocator;

begin  -- Rules.Allocators
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Allocators;
