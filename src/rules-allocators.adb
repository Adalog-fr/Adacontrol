----------------------------------------------------------------------
--  Rules.Allocators - Package body                                 --
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

-- Asis
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- AdaControl
with
   Framework.Language.Shared_Keys;
pragma Elaborate (Framework.Language.Shared_Keys);
package body Rules.Allocators is
   use Framework, Framework.Control_Manager, Framework.Language.Shared_Keys;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Entities  : Context_Store;

   type Filter_Kinds is (Always, Anonymous, Inconsistent, Never);
   type Filter_Set   is array (Filter_Kinds) of Boolean;
   type Context_Set  is array (Filter_Kinds) of Basic_Rule_Context;
   type Rule_Context is new Root_Context with
      record
         Active_Filter : Filter_Set := (others => False);
         Contexts      : Context_Set;
      end record;

   Expected_Categories : constant Categories_Set := Basic_Set + Cat_Private;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of allocators, either all of them,");
      User_Message ("or just those for specific type(s) or types belonging");
      User_Message ("to the indicated categories, or just those whose indicated");
      User_Message ("subtype is inconsistent with the access type declaration");
      User_Message;
      User_Message ("Parameter(s): [anonymous | inconsisent | not] [<category>|<entity>]");
      Help_On_Categories (Expected => Expected_Categories);
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Framework.Language;
      Entity      : Entity_Specification;
      Filter      : Filter_Kinds;
      No_Positive : Boolean := True;
   begin
      while Parameter_Exists loop
         if Get_Modifier ("INCONSISTENT") then
            Filter      := Inconsistent;
            Entity      := Get_Entity_Parameter (Allow_Extended => Parens_OK, Ghost => "ALL");
            No_Positive := False;
         elsif Get_Modifier ("ANONYMOUS") then
            Filter      := Anonymous;
            Entity      := Get_Entity_Parameter (Allow_Extended => Parens_OK, Ghost => "ALL");
            No_Positive := False;
         elsif Get_Modifier ("NOT") then
            Filter := Never;
            Entity := Get_Entity_Parameter (Allow_Extended => Parens_OK);
         else
            Filter      := Always;
            Entity      := Get_Entity_Parameter (Allow_Extended => Parens_OK, Ghost => "ALL");
            No_Positive := False;
         end if;
         Check_Category (Rule_Id, Entity, Expected_Categories);

         declare
            Context       : Root_Context'Class := Association (Entities, Entity);
            Active_Filter : Filter_Set := (others => False);
            Contexts      : Context_Set;
         begin
            if Context = No_Matching_Context then
               Active_Filter (Filter) := True;
               Contexts      (Filter) := Basic.New_Context (Ctl_Kind, Ctl_Label);
               Associate (Entities,
                          Entity,
                          Rule_Context'(Active_Filter => Active_Filter, Contexts => Contexts));
            else
               if Rule_Context (Context).Active_Filter (Filter) then
                  if Filter = Always then
                     Parameter_Error (Rule_Id, "type or keyword already given: " & Image (Entity));
                  else
                     Parameter_Error (Rule_Id, "type or keyword already given with "
                                      & Filter_Kinds'Wide_Image (Filter)
                                      & ": "
                                      & Image (Entity));
                  end if;
               end if;

               Rule_Context (Context).Active_Filter (Filter) := True;
               Rule_Context (Context).Contexts      (Filter) := Basic.New_Context (Ctl_Kind, Ctl_Label);
               Update (Entities, Context);
            end if;
         end;
      end loop;

      if No_Positive then -- Including no parameter at all
         Entity := Value ("ALL");
         Associate (Entities,
                    Entity,
                    Rule_Context'(Active_Filter => (Always => True, others => False),
                                  Contexts      => (others => Basic.New_Context (Ctl_Kind, Ctl_Label))));
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

   procedure Process_Allocator (Allocator : in Asis.Expression) is
      use Utilities, Thick_Queries;
      use Asis, Asis.Expressions, Asis.Elements;

      Found             : Boolean := False;
      Allocated_Subtype : Asis.Expression;

      procedure Check (Current_Context : Root_Context'Class) is
         use Framework.Locations, Framework.Reports;

         Allocator_Subtype  : Asis.Element;

         function Subtype_Match (Left, Right : Asis.Expression) return Boolean is
            LL : Asis.Expression := Left;
            RR : Asis.Expression := Right;
         begin
            while Expression_Kind (LL) = An_Attribute_Reference loop
               if Attribute_Kind (LL) /= Attribute_Kind (RR) then
                  return False;
               end if;
               LL := Prefix (LL);
               RR := Prefix (RR);
            end loop;
            if Expression_Kind (RR) = An_Attribute_Reference then
               -- more attributes right than left...
               return False;
            end if;

            return Is_Equal (First_Defining_Name (LL), First_Defining_Name (RR));
         end Subtype_Match;

         procedure Check_Inconsistent is
            use Asis.Declarations, Asis.Definitions;

            Designated_Subtype : Asis.Element;
            Designated_Name    : Asis.Expression;
            Def                : Asis.Definition;
         begin
            -- Designated_Subtype is (temporarily) the allocators's type
            Allocator_Subtype := Thick_Queries.Corresponding_Expression_Type_Definition (Allocator);

            if Is_Nil (Allocator_Subtype) then
               -- This can happen if the allocator is of an anonymous access type,
               -- appearing as a component of an aggregate. See Corresponding_Expression_Type_Definition
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Allocator),
                            "Unable to determine the designated type of the allocator");
               Found := True; -- Well, the context was found...
               return;
            end if;

            -- Find designated subtype
            loop
               if Type_Kind (Allocator_Subtype) = A_Derived_Type_Definition then
                  -- This one cannot be anonymous, get to the real definition
                  Allocator_Subtype := Type_Declaration_View (Ultimate_Type_Declaration
                                                              (Enclosing_Element (Allocator_Subtype)));
               elsif Formal_Type_Kind (Allocator_Subtype) = A_Formal_Derived_Type_Definition then
                  -- There can be no constraint at that level, go to the parent type
                  Allocator_Subtype := Type_Declaration_View (Corresponding_Name_Declaration
                                                              (Simple_Name
                                                               (Strip_Attributes
                                                                (Subtype_Simple_Name (Allocator_Subtype)))));
               elsif Definition_Kind (Allocator_Subtype) = An_Access_Definition then
                  Designated_Subtype := Simple_Name (Anonymous_Access_To_Object_Subtype_Mark
                                                     (Allocator_Subtype));
                  Designated_Name    := Designated_Subtype;
                  exit;
               else
                  Designated_Subtype := Asis.Definitions.Access_To_Object_Definition (Allocator_Subtype);
                  Designated_Name    := Subtype_Simple_Name (Designated_Subtype);
                  exit;
               end if;
            end loop;
            -- Now, Designated_Subtype is really the designated subtype

            if Is_Class_Wide_Subtype (Designated_Name) then
               -- Class wide target => the allocator is generally of a specific type
               -- Don't consider this inconsistent
               return;
            end if;

            if Element_Kind (Designated_Subtype) = An_Expression      -- a subtype mark, no constraint
              or else Is_Nil (Subtype_Constraint (Designated_Subtype))
            then
               if Attribute_Kind (Designated_Name) = A_Base_Attribute then
                  return;
               end if;
               Def := Type_Declaration_View (Corresponding_Full_Type_Declaration
                                             (Corresponding_Name_Declaration
                                              (Simple_Name (Strip_Attributes (Designated_Name)))));
               if         Type_Kind (Def)        /= An_Unconstrained_Array_Definition
                 and then Formal_Type_Kind (Def) /= A_Formal_Unconstrained_Array_Definition
                 and then not Subtype_Match (Designated_Name, Allocated_Subtype)
               then
                  Found := True;
                  Report (Rule_Id,
                          Rule_Context (Current_Context).Contexts (Inconsistent),
                          Get_Location (Allocator),
                          "allocator for "
                          & Full_Name_Image (Allocated_Subtype)
                          & " not consistent with designated subtype "
                          & Full_Name_Image (Designated_Name)
                         );
               end if;
            else
               -- There is a constraint in the designated subtype indication
               Found := True;
               Report (Rule_Id,
                       Rule_Context (Current_Context).Contexts (Inconsistent),
                       Get_Location (Allocator),
                       "allocator for "
                       & Full_Name_Image (Allocated_Subtype)
                       & " not consistent with designated subtype "
                          & Full_Name_Image (Designated_Name)
                       & " with constraint"
                      );

            end if;
         end Check_Inconsistent;

      begin   -- Check
         if Current_Context = No_Matching_Context then
            return;
         end if;

         for F in Rule_Context (Current_Context).Active_Filter'Range loop
            if  Rule_Context (Current_Context).Active_Filter (F) then
               case F is
                  when Always =>
                     Found := True;
                     Report (Rule_Id,
                             Rule_Context (Current_Context).Contexts (Always),
                             Get_Location (Allocator),
                             "allocator for " & To_Title (Last_Matching_Name (Entities)));

                  when Anonymous =>
                     Allocator_Subtype := Thick_Queries.Corresponding_Expression_Type_Definition (Allocator);

                     if Is_Nil (Allocator_Subtype)  -- Since it happens only for anonymous access types...
                       or else Definition_Kind (Allocator_Subtype) = An_Access_Definition
                     then
                        Found := True;
                        Report (Rule_Id,
                                Rule_Context (Current_Context).Contexts (Anonymous),
                                Get_Location (Allocator),
                                "allocator of an anonymous access type");
                     end if;

                  when Inconsistent =>
                     Check_Inconsistent;

                  when Never =>
                     Found := True;
               end case;
            end if;
         end loop;
      end Check;

      E : Asis.Element;
      use Categories_Utilities;
   begin  -- Process_Allocator
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Retrieve in E the good subtype mark
      case Expression_Kind (Allocator) is
         when An_Allocation_From_Subtype =>
            Allocated_Subtype := Subtype_Simple_Name (Allocator_Subtype_Indication (Allocator));
         when An_Allocation_From_Qualified_Expression =>
            Allocated_Subtype := Simple_Name (Converted_Or_Qualified_Subtype_Mark
                                              (Allocator_Qualified_Expression (Allocator)));
         when others =>
            Failure (Rule_Id & ": Unexpected element", Allocator);
      end case;

      -- Allocated_Subtype can be an attribute, T'Base or T'Class
      -- T'Base has the same first named subtype as T
      -- T'Base is only allowed for scalar types, therefore we cannot have T'Base'Class
      -- nor T'Class'Base
      if Expression_Kind (Allocated_Subtype) = An_Attribute_Reference then
         case Attribute_Kind (Allocated_Subtype) is
            when A_Base_Attribute =>
               E := Simple_Name (First_Subtype_Name (Simple_Name (Prefix (Allocated_Subtype))));
            when A_Class_Attribute =>
               E := Allocated_Subtype;
            when others =>
               Failure ("Unexpected attribute", Allocated_Subtype);
         end case;
      else
         E := First_Subtype_Name (Allocated_Subtype);
      end if;

      -- Check entity
      Check (Matching_Context (Entities, E, Extend_To => All_Extensions));

      -- Entity not found, check category
      if not Found and Expression_Kind (E) /= An_Attribute_Reference then
         Check (Control_Manager.Association (Entities, Image (Matching_Category (E,
                                                                                 From_Cats          => Full_Set,
                                                                                 Follow_Derived     => True,
                                                                                 Privacy            => Follow_Private,
                                                                                 Separate_Extension => False))));
      end if;

      -- Category not found, check all
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
