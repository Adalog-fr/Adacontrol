----------------------------------------------------------------------
--  Rules.Generic_Aliasing - Package body                           --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2013.           --
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
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Control_Manager,
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Generic_Aliasing is
   use Framework, Thick_Queries;

   -- Algorithm:
   --
   -- Another one that looked simple...
   -- Principle is easy: compare all pairs of generic parameters (N**2 loop, but we don't expect many parameters).
   -- If they are not of the same declaration_kind, no need to check further (but beware that we have two
   -- declaration_kinds for generic formal packages).
   --
   -- Difficulties:
   -- - except for formal types: renaming must be considered
   -- - for formal types: base types must be compared
   -- - for subprograms: attributes are the same if they are the same, and their prefixes are the same.
   --                    predefined operators have no definition. How can we recognize identical ones?
   --                    We can eliminate those that are obviously different (different operator_kinds),
   --                    but we have no way to differentiate a "+" on Integer from a "+" on Float.

   type    Extended_Subrules is (Sr_All, Sr_Variable, Sr_Type, Sr_Subprogram, Sr_Package);
   subtype Subrules is Extended_Subrules range Extended_Subrules'Succ (Sr_All) .. Extended_Subrules'Last;
   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Extended_Subrules, Prefix => "SR_");

   subtype Rule_Detail is Thick_Queries.Result_Confidence;
   package Detail_Modifier_Utilities is new Framework.Language.Modifier_Utilities (Rule_Detail);

   type Detail_Activity is array (Rule_Detail) of Boolean;
   No_Detail_Active : constant Detail_Activity := (others => False);

   type Subrules_Set is array (Subrules) of Detail_Activity;
   Empty_Set       : constant Subrules_Set := (others => No_Detail_Active);
   All_Set : constant array (Rule_Detail) of Subrules_Set
     := (Unlikely => (Sr_Variable | Sr_Subprogram => (Unlikely => True, others => False),
                      others                      => (Certain  => True, others => False)),
         Possible => (Sr_Variable | Sr_Subprogram => (Possible => True, others => False),
                      others                      => (Certain  => True, others => False)),
         Certain  => (others                      => (Certain  => True, others => False)));


   Rule_Used : Subrules_Set := Empty_Set;
   Save_Used : Subrules_Set;
   Contexts  : array (Subrules, Rule_Detail) of Framework.Control_Manager.Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrules_Flags_Utilities, Detail_Modifier_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control instantiations where the same actual is given for");
      User_Message ("more than one formal");
      Help_On_Flags     (Header => "Parameter(s): [<condition>] ", Footer => "(optional, default = all)");
      Help_On_Modifiers (Header => "<condition>:", Footer => "(default = certain)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Framework.Control_Manager, Framework.Language, Subrules_Flags_Utilities, Detail_Modifier_Utilities;
      SR : Extended_Subrules;
      Detail : Rule_Detail;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Detail := Get_Modifier (Required => False, Default => Certain);
            SR     := Get_Flag_Parameter (Allow_Any => False);
            if SR = Sr_All then
               if Rule_Used = Empty_Set then
                  Rule_Used := All_Set (Detail);
                  for R in Subrules loop
                     for D in Rule_Detail loop
                        if Rule_Used (R) (D) then
                           Contexts  (R, D) := Basic.New_Context (Ctl_Kind, Ctl_Label);
                        end if;
                     end loop;
                  end loop;
               else
                  Parameter_Error (Rule_Id, "subrule already specified");
               end if;
            else
               if        (SR = Sr_Type    and Detail /= Certain)
                 or else (SR = Sr_Package and Detail /= Certain)
               then
                  Parameter_Error (Rule_Id, Image (Detail) & " not allowed for " & Subrules'Wide_Image (SR));
               end if;
               if Rule_Used (SR) (Detail) then
                  if not Basic.Merge_Context (Contexts (SR, Detail), Ctl_Kind, Ctl_Label) then
                     Parameter_Error (Rule_Id, "Parameter already given : " & Subrules'Wide_Image (SR));
                  end if;
               else
                  Rule_Used (SR) (Detail) := True;
                  Contexts  (SR, Detail)  := Basic.New_Context (Ctl_Kind, Ctl_Label);
               end if;
            end if;
         end loop;
      else
         -- Default to "certain all"
         if Rule_Used = Empty_Set then
            Rule_Used := All_Set (Certain);
            for R in Subrules loop
               if Rule_Used (R) (Certain) then
                  Contexts  (R, Certain) := Basic.New_Context (Ctl_Kind, Ctl_Label);
               end if;
            end loop;
         else
            Parameter_Error (Rule_Id, "subrule already specified");
         end if;
      end if;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := Empty_Set;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Empty_Set;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Instantiation : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Utilities;

      function Name_Proximity (Left, Right : Asis.Expression) return Proximity is
         -- Left and Right should refer to a (formal) type or object
         Good_Left  : Asis.Expression := Left;
         Good_Right : Asis.Expression := Right;
      begin
         -- Get rid of case when the prefix is itself an attribute reference
         case Attribute_Kind (Good_Left) is
            when A_Base_Attribute =>
               -- 'Base does not change the type
               Good_Left := Prefix (Good_Left);
            when A_Class_Attribute =>
               if Attribute_Kind (Good_Right) /= A_Class_Attribute then
                  return (Certain, None);
               end if;
               -- Both are 'Class: check the prefixes
               Good_Left  := Prefix (Good_Left);
               Good_Right := Prefix (Good_Right);
            when Not_An_Attribute =>
               null;
            when others =>
               Failure ("Prefix_Proximity: Unexpected attribute for left type", Good_Left);
         end case;

         case Attribute_Kind (Good_Right) is
            when A_Base_Attribute =>
               -- 'Base does not change the type
               Good_Right := Prefix (Good_Right);
            when A_Class_Attribute =>
               -- Since Good_Left is not 'Class
               return (Certain, None);
            when Not_An_Attribute =>
               null;
            when others =>
               Failure ("Prefix_Proximity: Unexpected attribute for right type", Good_Right);
         end case;

         if Expression_Kind (Good_Left) = An_Identifier
           and then Expression_Kind (Good_Right) = An_Identifier
           and then Declaration_Kind (Corresponding_Name_Declaration (Good_Left))
                       in An_Ordinary_Type_Declaration .. A_Subtype_Declaration
           and then Declaration_Kind (Corresponding_Name_Declaration (Good_Right))
                       in An_Ordinary_Type_Declaration .. A_Subtype_Declaration
         then
            -- Both are types
            if Is_Equal (A4G_Bugs.Corresponding_First_Subtype (Corresponding_Name_Declaration (Good_Left)),
                         A4G_Bugs.Corresponding_First_Subtype (Corresponding_Name_Declaration (Good_Right)))
            then
               return (Certain, Complete);
            else
               return (Certain, None);
            end if;
         end if;

         -- Normal case: variables
         return Variables_Proximity (Good_Left, Good_Right);
      end Name_Proximity;

      function Is_Same_Attribute (Left, Right : Asis.Expression) return Boolean is
         -- Expected Expression_Kind: An_Attribute_Reference
      begin
         if Attribute_Kind (Left) /= Attribute_Kind (Right) then
            return False;
         end if;

         if Attribute_Kind (Left) /= An_Implementation_Defined_Attribute then
            return True;
         end if;

         -- Must compare images for implementation defined attributes
         return To_Upper (Name_Image (Attribute_Designator_Identifier (Left))) =
                To_Upper (Name_Image (Attribute_Designator_Identifier (Right)));
      end Is_Same_Attribute;

      function Subprogram_Proximity (Left, Right : Asis.Expression) return Proximity is
         -- Confidence = Possible and Overlap = Partial do not apply to subprograms
         Good_Left  : Asis.Expression;
         Good_Right : Asis.Expression;
         Left_Def  : Asis.Defining_Name;
         Right_Def : Asis.Defining_Name;
      begin
         -- Get rid quickly of accesses to subprograms...
         if Expression_Kind (Left) = An_Explicit_Dereference then
            if Expression_Kind (Right) /= An_Explicit_Dereference then
               return (Unlikely, Complete);
            end if;
            if Variables_Proximity (Prefix (Left), Prefix (Right)) = Same_Variable then
               return (Certain, Complete);
            else
               return (Unlikely, Complete);
            end if;
         elsif Expression_Kind (Right) = An_Explicit_Dereference then
            return (Unlikely, Complete);
         end if;

         Good_Left  := Ultimate_Name (Left);
         Good_Right := Ultimate_Name (Right);
         if Expression_Kind (Good_Left) = An_Attribute_Reference then
            if Expression_Kind (Good_Right) /= An_Attribute_Reference then
               return (Certain, None);
            end if;
            -- Both are attributes. The SP are the same if they are the same attribute and the prefixes
            -- are the same types or objects (only types have SP attributes for predefine attributes, but
            -- objects can have implementation defined attributes, e.g. Gnat's X'Img)
            if Is_Same_Attribute (Good_Left, Good_Right) then
               return Name_Proximity (Prefix (Good_Left), Prefix (Good_Right));
            else
               return (Certain, None);
            end if;
         elsif Expression_Kind (Good_Right) = An_Attribute_Reference then
           return (Certain, None);
         end if;

         -- Here, neither is an attribute
         -- Therefore, only predefined operators may be functions without a declaration.
         Left_Def  := Corresponding_Name_Definition (Good_Left);
         Right_Def := Corresponding_Name_Definition (Good_Right);
         if not Is_Nil (Left_Def) and not Is_Nil (Right_Def) then
            -- Nothing weird
            if Is_Equal (First_Defining_Name (Left_Def), First_Defining_Name (Right_Def)) then
               return (Certain, Complete);
            else
               return (Certain, None);
            end if;
         elsif not Is_Nil (Left_Def) or not Is_Nil (Right_Def) then
            -- Only one is a predefined operator => not the same
            return (Certain, None);
         elsif Operator_Kind (Good_Left) /= Operator_Kind (Good_Right) then
            -- Both are predefined operators, but they are different
            return (Certain, None);
         else
            -- Both are predefined operators, with the same name.
            -- Not completely sure they are the same, since one could be a unary operator, and
            -- the other one the corresponding binary operator. Consider they are the same,
            -- possibly making a false positive (better than a false negative).
            Uncheckable (Rule_Id,
                         False_Positive,
                         Get_Location (Right),
                         "several undistinguishable " & Name_Image (Good_Left) & " operators");
            return (Certain, Complete);
         end if;
      end Subprogram_Proximity;

      function Is_Defaulted (Actual : Asis.Expression) return Boolean is
      -- True iff Actual is not an explicitely given actual of the instantiation
      -- The enclosing element of the Actual is the association
      begin
         return not Is_Equal (Enclosing_Element (Enclosing_Element (Actual)), Instantiation);
      end Is_Defaulted;

      function Matching_Context (Subrule : Subrules; Detail : Rule_Detail)
                                 return Framework.Control_Manager.Root_Context'Class
      is
         use Framework.Control_Manager;
      begin
         for D in reverse Rule_Detail range Rule_Detail'First .. Detail loop
            if Rule_Used (Subrule) (D) then
               return Contexts (Subrule, D);
            end if;
         end loop;
         return No_Matching_Context;
      end Matching_Context;

   begin  -- Process_Instantiation
      if Rule_Used = Empty_Set then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Actuals      : constant Asis.Association_List := Generic_Actual_Part (Instantiation, Normalized => True);
         Left_Actual  : Asis.Element;
         Right_Actual : Asis.Element;
         Left_Formal  : Asis.Declaration;
         Right_Formal : Asis.Declaration;
         Left_Kind    : Asis.Declaration_Kinds;
         Right_Kind   : Asis.Declaration_Kinds;
         Prox         : Proximity;

         use Detail_Modifier_Utilities;
      begin
         for Left_Inx in List_Index range Actuals'First .. Actuals'Last - 1 loop
            Left_Actual := Simple_Name (Actual_Parameter (Actuals (Left_Inx)));
            Left_Formal := Formal_Parameter (Actuals (Left_Inx));
            Left_Kind   := Declaration_Kind (Enclosing_Element (Left_Formal));

            --##Rule Off Simplifiable_statements ## Keep symetry with Left_Inx
            for Right_Inx in List_Index range Left_Inx + 1 .. Actuals'Last loop
               --##Rule On Simplifiable_statements
               Right_Formal := Formal_Parameter (Actuals (Right_Inx));
               Right_Kind   := Declaration_Kind  (Enclosing_Element (Right_Formal));

               if Left_Kind = Right_Kind
                 or else (    Left_Kind  = A_Formal_Package_Declaration
                          and Right_Kind = A_Formal_Package_Declaration_With_Box)
                 or else (    Left_Kind  = A_Formal_Package_Declaration_With_Box
                          and Right_Kind = A_Formal_Package_Declaration)
               then
                  Right_Actual := Simple_Name (Actual_Parameter (Actuals (Right_Inx)));
                  case Declaration_Kind (Enclosing_Element (Right_Formal)) is
                     when A_Formal_Object_Declaration =>
                        if Rule_Used (Sr_Variable) /= No_Detail_Active
                          and then Mode_Kind (Enclosing_Element(Left_Formal))  = An_In_Out_Mode
                          and then Mode_Kind (Enclosing_Element(Right_Formal)) = An_In_Out_Mode
                        then
                           Prox := Variables_Proximity (Left_Actual, Right_Actual);
                           if Prox.Overlap /= None then
                              Report (Rule_Id,
                                      Matching_Context (Sr_Variable, Prox.Confidence),
                                      Get_Location (Right_Actual),
                                      "Parameter is same as parameter #" & ASIS_Integer_Img (Left_Inx)
                                      & " at " & Image (Get_Location (Left_Actual))
                                      & " (" & Image (Prox.Confidence, Title_Case) & ')');
                           end if;
                        end if;
                     when A_Formal_Type_Declaration | A_Formal_Incomplete_Type_Declaration =>
                        if Rule_Used (Sr_Type) (Certain)
                          and then Name_Proximity (Left_Actual, Right_Actual) = Same_Variable
                        then
                           Report (Rule_Id,
                                   Contexts (Sr_Type, Certain),
                                   Get_Location (Right_Actual),
                                   "Parameter is same as parameter #" & ASIS_Integer_Img (Left_Inx)
                                   & " at " & Image (Get_Location (Left_Actual)));
                        end if;
                     when A_Formal_Procedure_Declaration | A_Formal_Function_Declaration =>
                        if Rule_Used (Sr_Subprogram) /= No_Detail_Active then
                           Prox := Subprogram_Proximity (Left_Actual, Right_Actual);
                           if Prox.Overlap /= None then
                              -- Be careful that we may have defaulted parameters as one of the aliased SP
                              if Is_Defaulted (Right_Actual) then
                                 if Is_Defaulted (Left_Actual) then
                                    Report (Rule_Id,
                                            Matching_Context (Sr_Subprogram, Prox.Confidence),
                                            Get_Location (Generic_Unit_Name(Instantiation)),
                                            "Defaulted parameter " & Defining_Name_Image (Right_Formal)
                                            & " is same as defaulted parameter " & Defining_Name_Image (Left_Formal)
                                            & " (" & Image (Prox.Confidence, Title_Case) & ')');
                                 else
                                    Report (Rule_Id,
                                            Matching_Context (Sr_Subprogram, Prox.Confidence),
                                            Get_Location (Generic_Unit_Name(Instantiation)),
                                            "Defaulted parameter " & Defining_Name_Image (Right_Formal)
                                            & " is same as parameter #" & ASIS_Integer_Img (Left_Inx)
                                            & " at " & Image (Get_Location (Left_Actual))
                                            & " (" & Image (Prox.Confidence, Title_Case) & ')');
                                 end if;
                              elsif Is_Defaulted (Left_Actual) then
                                 Report (Rule_Id,
                                         Matching_Context (Sr_Subprogram, Prox.Confidence),
                                         Get_Location (Right_Actual),
                                         "Parameter is same as defaulted parameter " & Defining_Name_Image (Left_Formal)
                                         & " (" & Image (Prox.Confidence, Title_Case) & ')');
                              else
                                 Report (Rule_Id,
                                         Matching_Context (Sr_Subprogram, Prox.Confidence),
                                         Get_Location (Right_Actual),
                                         "Parameter is same as parameter #" & ASIS_Integer_Img (Left_Inx)
                                         & " at " & Image (Get_Location (Left_Actual))
                                         & " (" & Image (Prox.Confidence, Title_Case) & ')');
                              end if;
                           end if;
                        end if;
                     when A_Formal_Package_Declaration | A_Formal_Package_Declaration_With_Box =>
                        if Rule_Used (Sr_Package)(Certain)
                          and then Is_Equal (Corresponding_Name_Definition (Ultimate_Name (Left_Actual)),
                                             Corresponding_Name_Definition (Ultimate_Name (Right_Actual)))
                        then
                           Report (Rule_Id,
                                   Contexts (Sr_Package, Certain),
                                   Get_Location (Right_Actual),
                                   "Parameter is same as parameter #" & ASIS_Integer_Img (Left_Inx)
                                   & " at " & Image (Get_Location (Left_Actual)));
                        end if;
                     when others =>
                        Failure ("Generic_Aliasing: Unexpected formal parameter", Enclosing_Element (Right_Formal));
                  end case;

               end if;
            end loop;
         end loop;
      end;
   end Process_Instantiation;

begin  -- Rules.Generic_Aliasing
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Generic_Aliasing;
