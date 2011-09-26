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
  Asis.Declarations,
  Asis.Definitions,
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

   type Rule_Context is new Basic_Rule_Context with
      record
         Inconsistent_Only : Boolean;
      end record;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of allocators, either all of them,");
      User_Message ("or just those for tasks, protected types, or specific type(s)");
      User_Message;
      User_Message ("Parameter(s): [inconsisent] task | protected | <allocated type> (optional)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds) is
      use Framework.Language;
      Entity       : Entity_Specification;
      Inconsistent : Boolean;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Inconsistent := Get_Modifier ("INCONSISTENT");
            Entity := Get_Entity_Parameter (Ghost => "ALL");
            Associate (Entities,
                       Entity,
                       Rule_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Inconsistent_Only => Inconsistent));
         end loop;
      else
         Entity := Value ("ALL");
         Associate (Entities,
                    Entity,
                    Rule_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Inconsistent_Only => False));
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
      Allocator_Subtype : Asis.Expression;

      procedure Check (Current_Context : Root_Context'Class) is
         use Framework.Reports;
         use Asis.Declarations, Asis.Definitions;

         Designated_Subtype : Asis.Element;
         Def                : Asis.Definition;

         function Subtype_Match (Left, Right : Asis.Expression) return Boolean is
            LL : Asis.Expression := Left;
            RR : Asis.Expression := Right;
         begin
            while Expression_Kind (LL) = An_Attribute_Reference loop
               if A4G_Bugs.Attribute_Kind (LL) /= A4G_Bugs.Attribute_Kind (RR) then
                  return False;
               end if;
               LL := Prefix (LL);
               RR := Prefix (RR);
            end loop;
            if Expression_Kind (RR) = An_Attribute_Reference then
               -- more attributes right than left...
               return False;
            end if;

            return Is_Equal (Corresponding_Name_Definition (Simple_Name (LL)),
                             Corresponding_Name_Definition (Simple_Name (RR)));
         end Subtype_Match;

      begin   -- Check
         if Current_Context = No_Matching_Context then
            Found := False;
            return;
         end if;

         if Rule_Context (Current_Context).Inconsistent_Only then
            Designated_Subtype := Thick_Queries.Corresponding_Expression_Type_Definition (Element);

            if Is_Nil (Designated_Subtype) then
               -- This can happen if the allocator is of an anonymous access type, like in:
               --   procedure P (X : access Integer := new Integer'(1))
               -- More cases (presumably) in Ada 2005
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Element),
                            "Unable to determine the designated type of the allocator");
               Found := True; -- Well, the context was found...
               return;
            end if;

            loop
               if Type_Kind (Designated_Subtype) = A_Derived_Type_Definition then
                  -- This one cannot be anonymous, get to the real definition
                  Designated_Subtype := Type_Declaration_View (Ultimate_Type_Declaration
                                                               (Enclosing_Element (Designated_Subtype)));
               elsif Formal_Type_Kind (Designated_Subtype) = A_Formal_Derived_Type_Definition then
                  -- There can be no constraint at that level, go to the parent type
                  Designated_Subtype := Type_Declaration_View (Corresponding_Name_Declaration
                                                               (Simple_Name
                                                                (Strip_Attributes
                                                                 (Subtype_Simple_Name (Designated_Subtype)))));
               else
                  exit;
               end if;
            end loop;

            Designated_Subtype := Asis.Definitions.Access_To_Object_Definition (Designated_Subtype);
            if Is_Class_Wide_Subtype (Designated_Subtype) then
               -- Call wide target => the allocator is generally of a specific type
               -- Don't consider this inconsistent
               Found := False;
               return;
            end if;

            if Is_Nil (Subtype_Constraint (Designated_Subtype)) then
               Def := Subtype_Simple_Name (Designated_Subtype);
               if A4G_Bugs.Attribute_Kind (Def) = A_Base_Attribute then
                  Found := False;
                  return;
               end if;
               Def := Corresponding_Name_Declaration (Simple_Name (Strip_Attributes (Def)));
               case Declaration_Kind (Def) is
                  when A_Private_Type_Declaration | An_Incomplete_Type_Declaration =>
                     Def := Corresponding_Type_Declaration (Def);
                  when others =>
                     null;
               end case;
               Def := Type_Declaration_View (Def);
               if         Type_Kind (Def)        /= An_Unconstrained_Array_Definition
                 and then Formal_Type_Kind (Def) /= A_Formal_Unconstrained_Array_Definition
                 and then not Subtype_Match (Subtype_Simple_Name (Designated_Subtype),
                                             Simple_Name         (Allocator_Subtype))
               then
                  Report (Rule_Id,
                          Current_Context,
                          Get_Location (Element),
                          "allocator for "
                          & To_Title (Last_Matching_Name (Entities))
                          & " not consistent with designated subtype "
                          & Extended_Name_Image (Subtype_Simple_Name (Designated_Subtype))
                         );
               end if;
            else
               -- There is a constraint in the designated subtype indication
               Report (Rule_Id,
                       Current_Context,
                       Get_Location (Element),
                       "allocator for "
                       & To_Title (Last_Matching_Name (Entities))
                       & " not consistent with designated subtype "
                       & Extended_Name_Image (Subtype_Simple_Name (Designated_Subtype))
                       & " with constraint"
                      );

            end if;
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
            Allocator_Subtype := Subtype_Simple_Name (Allocator_Subtype_Indication (Element));
         when An_Allocation_From_Qualified_Expression =>
            Allocator_Subtype := Simple_Name (Converted_Or_Qualified_Subtype_Mark
                                              (Allocator_Qualified_Expression (Element)));
         when others =>
            Failure (Rule_Id & ": Unexpected element", Element);
      end case;

      -- Allocator_Subtype can be an attribute, T'Base or T'Class
      -- T'Base has the same first named subtype as T
      -- T'Base is only allowed for scalar types, therefore we cannot have T'Base'Class
      -- nor T'Class'Base
      if Expression_Kind (Allocator_Subtype) = An_Attribute_Reference then
         case A4G_Bugs.Attribute_Kind (Allocator_Subtype) is
            when A_Base_Attribute =>
               E := Simple_Name (First_Subtype_Name (Simple_Name (Prefix (Allocator_Subtype))));
            when A_Class_Attribute =>
               E := Allocator_Subtype;
            when others =>
               Failure ("Unexpected attribute", Allocator_Subtype);
         end case;
      else
         E := First_Subtype_Name (Allocator_Subtype);
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
