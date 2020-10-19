----------------------------------------------------------------------
--  Rules.Type_Usage - Package body                                 --
--                                                                  --
--  This software is (c) Alstom and Adalog 2004-2013.               --
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

-- ASIS
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Limited_Views;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Control_Manager,
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);
pragma Elaborate (Framework.Language.Shared_Keys);

package body Rules.Type_Usage is
   use Framework, Framework.Language.Shared_Keys;

   type Subrules is (Sr_Attribute, Sr_Index);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "SR_");

   type Usage_Flags is array (Subrules) of Boolean;
   No_Rule_Used : constant Usage_Flags := (others => False);
   Rule_Used : Usage_Flags := No_Rule_Used;
   Save_Used : Usage_Flags;

   type Usage_Context is new Framework.Control_Manager.Basic_Rule_Context with
      record
         Aspects : Framework.Language.Shared_Keys.Aspects_Set;
      end record;
   Attribute_Contexts : Framework.Control_Manager.Context_Store;
   Index_Contexts     : Framework.Control_Manager.Context_Store;
   package Attribute_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Attribute_Contexts);
   package Index_Iterator     is new Framework.Control_Manager.Generic_Context_Iterator (Index_Contexts);

   Attr_Expected_Categories  : constant Categories_Set := Basic_Set + Cat_Private;
   Index_Expected_Categories : constant Categories_Set := Discrete_Set;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls usage of types at various places");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags (Header => "Parameter(1):", Extra_Value => "<attribute>");
      User_Message;
      User_Message ("For <attribute>:");
      User_Message ("Parameter(2): <category>");
      Aspects_Utilities.Help_On_Flags (Header => "Parameter(3..): [not] ", Footer => "(optional)");
      Help_On_Categories (Expected => Attr_Expected_Categories);
      User_Message;
      User_Message ("For index:");
      User_Message ("Parameter(2): <entity>|<category>");
      Aspects_Utilities.Help_On_Flags (Header => "Parameter(3..): [not] ", Footer => "(optional)");
      Help_On_Categories (Expected => Index_Expected_Categories);
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Framework.Control_Manager, Framework.Language, Subrules_Flag_Utilities;
      Subrule : Subrules;
      Param   : Entity_Specification;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "Parameters required for rule " & Rule_Id);
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => True);
      case Subrule is
         when Sr_Attribute =>
            declare
               Attr : constant Wide_String := Get_Name_Parameter;
            begin
               if Attr (1) /= ''' then
                  Parameter_Error (Rule_Id, "parameter must be subrule or attribute");
               end if;

               if not Parameter_Exists then
                  Parameter_Error (Rule_Id, "type category expected");
               end if;

               Param := Get_Entity_Parameter (Allow_Extended => Parens_OK or Box_OK);
               Check_Category (Rule_Id, Param, Attr_Expected_Categories);

               Associate (Attribute_Contexts,
                          Value (Attr & '_' & Image (Param)),
                          Usage_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Get_Aspects_Parameter (Rule_Id)),
                          Additive => True);
            exception
               when Already_In_Store =>
                  Parameter_Error (Rule_Id, "category already given for this attribute: " & Image (Param));
            end;

            Rule_Used (Sr_Attribute) := True;

         when Sr_Index =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "type category expected");
            end if;

            Param := Get_Entity_Parameter (Allow_Extended => Parens_OK);
            if Value (Param) /= Cat_Any then
               Check_Category (Rule_Id, Param, Index_Expected_Categories);
            end if;

            begin
               Associate (Index_Contexts,
                          Param,
                          Usage_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Get_Aspects_Parameter (Rule_Id)),
                          Additive => True);
            exception
               when Already_In_Store =>
                  Parameter_Error (Rule_Id, "category already given for Index: " & Image (Param));
            end;

            Rule_Used (Sr_Index) := True;
      end case;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := No_Rule_Used;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Iter         : in out Control_Manager.Context_Iterator;
                        Type_Aspects : in Language.Shared_Keys.Aspects_Set;
                        Message      : in Wide_String;
                        Loc          : in Locations.Location)
   is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Control_Manager, Framework.Reports;

      Extra           : Unbounded_Wide_String;
      Current_Context : Usage_Context;
      Applicable      : Boolean;

      procedure Check_Aspect (A : Aspects) is
         use Utilities;
      begin
         case Current_Context.Aspects (A) is
            when Unspecified =>
               null;
            when Present =>
               if Type_Aspects (A) = Present then
                  Append (Extra, To_Lower (Aspects'Wide_Image (A)) & ' ');
               else
                  Applicable := False;
               end if;
            when Absent =>
               if Type_Aspects (A) = Present then
                  Applicable := False;
               else
                  Append (Extra, "not " & To_Lower (Aspects'Wide_Image (A)) & ' ');
               end if;
         end case;
      end Check_Aspect;

   begin  -- Do_Report
      while not Is_Exhausted (Iter) loop
         Applicable := True;
         Extra      := Null_Unbounded_Wide_String;

         Current_Context := Usage_Context (Value (Iter));
         for A in Aspects loop
            Check_Aspect (A);
            exit when not Applicable;
         end loop;

         if Applicable then
            if Extra = Null_Unbounded_Wide_String then
               Report (Rule_Id,
                       Current_Context,
                       Loc,
                       Message);
            else
               Report (Rule_Id,
                       Current_Context,
                       Loc,
                       Message & " with " & To_Wide_String (Extra));
            end if;
         end if;
         Next (Iter);
      end loop;
   end Do_Report;


   ------------------------------
   -- Process_Array_Definition --
   ------------------------------

   procedure Process_Array_Definition (Definition : Asis.Definition) is
      use Framework.Control_Manager, Framework.Locations, Thick_Queries;
      use Asis.Declarations, Asis.Elements;

      function Get_Index_Location (Inx : Asis.List_Index) return Location is
         -- Returns the location of the Inx'th index in Definition
         use Asis, Asis.Definitions;
      begin
         case Type_Kind (Definition) is
            when A_Constrained_Array_Definition =>
               return Get_Location (Discrete_Subtype_Definitions (Definition) (Inx));
            when An_Unconstrained_Array_Definition =>
               return Get_Location (Index_Subtype_Definitions (Definition) (Inx));
            when others =>
               Utilities.Failure ("Get_Index_Location: not an array definition", Definition);
         end case;
      end Get_Index_Location;

   begin   -- Process_Array_Definition
      if not Rule_Used (Sr_Index) then
         return;
      end if;

      declare
         Subtypes : constant Asis.Defining_Name_List := Index_Subtypes_Names (Definition);
         Iterator : Context_Iterator                 := Index_Iterator.Create;
         Decl     : Asis.Declaration;
         First_St : Asis.Declaration;
         Type_Aspects : Aspects_Set;
      begin
         for S in Subtypes'Range loop
            if Is_Nil (Subtypes (S)) then
               -- Special case for array (1..10) (Integer by default)

               -- Exact subtype
               Reset (Iterator, Framework.Value ("STANDARD.INTEGER"));
               Do_Report (Iterator,
                          Type_Aspects => No_Aspect,
                          Message      => "Type used as index: " & Last_Matching_Name (Iterator),
                          Loc          => Get_Index_Location (S));

               -- We know it is already a first named subtype...

               -- Category
               declare
                  Cat_Name : constant Wide_String := "RANGE";
               begin
                  Reset (Iterator, Framework.Value (Cat_Name));
                  Do_Report (Iterator,
                             Type_Aspects => No_Aspect,
                             Message      => "Type category used as index: " & Cat_Name,
                             Loc          => Get_Index_Location (S));
               end;

            else
               Type_Aspects := Corresponding_Aspects_Set (Enclosing_Element (Definition));

               -- Exact subtype
               Reset (Iterator, Subtypes (S), Extend_To => All_Extensions);
               Do_Report (Iterator,
                          Type_Aspects => Type_Aspects,
                          Message      => "Type used as index: " & Last_Matching_Name (Iterator),
                          Loc          => Get_Index_Location (S));

               -- First subtype (aka type), if different
               Decl     := Enclosing_Element (Subtypes (S));
               First_St := A4G_Bugs.Corresponding_First_Subtype (Decl);
               if not Is_Equal (First_St, Decl) then
                  Reset (Iterator, Names (First_St) (1), Extend_To => All_Extensions);
                  Do_Report (Iterator,
                             Type_Aspects => Type_Aspects,
                             Message      => "Type used as index: " & Last_Matching_Name (Iterator),
                             Loc          => Get_Index_Location (S));
               end if;

               -- Category
               declare
                  Cat_Name : constant Wide_String := Image (Type_Category (First_St, Follow_Derived => True));
               begin
                  Reset (Iterator, Framework.Value (Cat_Name));
                  Do_Report (Iterator,
                             Type_Aspects => Type_Aspects,
                             Message      => "Type category used as index: " & Cat_Name,
                             Loc          => Get_Index_Location (S));
               end;
            end if;
         end loop;
      end;
   end Process_Array_Definition;


   -----------------------
   -- Process_Attribute --
   -----------------------

   procedure Process_Attribute (Attribute : Asis.Expression) is
      use Framework.Control_Manager, Framework.Locations, Thick_Queries, Utilities;
      use Asis, Asis.Elements, Asis.Expressions, Asis.Limited_Views;
   begin
      if not Rule_Used (Sr_Attribute) then
         return;
      end if;

      declare
         Iterator  : Context_Iterator := Attribute_Iterator.Create;
         Attr_Name : constant Wide_String := ''' & To_Upper (Attribute_Name_Image (Attribute));
         Cat_Name  : constant Wide_String := Image (Type_Category (Prefix (Attribute), Follow_Derived => True));
         Pfx_Name  : Asis.Expression      := Simple_Name (Strip_Attributes (Attribute));
      begin
         if Is_From_Limited_View (Pfx_Name) then
            Pfx_Name := A4G_Bugs.Get_Nonlimited_View (Pfx_Name);
         end if;
         if Expression_Kind (Pfx_Name) /= An_Identifier
           or else Declaration_Kind (Corresponding_Name_Declaration (Pfx_Name))
                   not in An_Ordinary_Type_Declaration .. A_Subtype_Declaration
         then
            -- We deal only with (sub)types here!
            return;
         end if;

         -- Category
         Reset (Iterator, Framework.Value (Attr_Name & '_' & Cat_Name));
         Do_Report (Iterator,
                    Type_Aspects => Corresponding_Aspects_Set (Pfx_Name),
                    Message      => "Type category used with " & Attr_Name & ": " & Cat_Name,
                    Loc          => Get_Location (Attribute));
      end;
   end Process_Attribute;

begin  -- Rules.Type_Usage
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Type_Usage;
