----------------------------------------------------------------------
--  Rules.Array_Declarations - Package body                         --
--                                                                  --
--  This software is (c) SAGEM DS and Adalog 2004-2006.             --
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
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Queries;
pragma Elaborate (Framework.Language);
pragma Elaborate (Framework.Language.Shared_Keys);

package body Rules.Array_Declarations is
   use Framework, Framework.Control_Manager, Framework.Language.Shared_Keys;

   type Subrules is (First, Last, Length, Dimensions, Index, Component);
   subtype Dim_Subrules is Subrules range First .. Dimensions;
   package Subrules_Flag_Utilities  is new Framework.Language.Flag_Utilities (Subrules);

   type Usage is array (Subrules) of Control_Kinds_Set;
   Not_Used  : constant Usage := (others => Empty_Control_Kinds_Set);
   Rule_Used : Usage := Not_Used;
   Save_Used : Usage;

   Labels : array (Dim_Subrules, Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Values : array (Dim_Subrules, Control_Kinds) of Language.Shared_Keys.Bounds_Values
     := (others => (others => (Min => 0, Max => Thick_Queries.Biggest_Natural'Last)));
   Length_Is_All : Control_Kinds_Set := Empty_Control_Kinds_Set;

   type Index_Context (Nb_Dims : Asis.ASIS_Positive) is new Basic_Rule_Context with
      record
         Index_Types : Entity_Specification_List (1 .. Nb_Dims);
      end record;
   Index_Contexts : Context_Store;
   package Index_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Index_Contexts);

   type Compo_Context is new Basic_Rule_Context with
      record
         Aspects : Framework.Language.Shared_Keys.Aspects_Set;
      end record;
   Compo_Contexts : Context_Store;
   package Compo_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Compo_Contexts);

   Component_Expected_Categories : constant Categories_Set := Basic_Set + Cat_Private;
   Index_Expected_Categories     : constant Categories_Set := Discrete_Set;

   ----------------
   -- List_Image --
   ----------------

   function List_Image (L : Entity_Specification_List) return Wide_String is
      use type Asis.ASIS_Integer;   -- Gela-ASIS compatibility
   begin
      if L'Length = 1 then
         return Image (L (L'First));
      else
         return Image (L (L'First)) & ", " & List_Image (L (L'First + 1 .. L'Last));
      end if;
   end List_Image;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: "& Rule_Id);
      User_Message ("Controls various parameters related to array types or objects declarations");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags (Header => "Parameter(1):",
                                             Footer => "(optional modifier for length: all)");
      User_Message;
      User_Message ("For First, Last, Length, Dimensions:");
      User_Message ("Parameter(2..3): <bound> <value>");
      User_Message ("                (at least one parameter required)");
      Help_On_Bounds (Header => "<bound>:");
      User_Message ("For first, last, and dimensions, alternatively:");
      User_Message ("Parameter(2): <value>");
      User_Message;
      User_Message ("For index:");
      User_Message ("Parameter(2..) : <entity>|<category>");
      Help_On_Categories (Expected => Index_Expected_Categories);
      User_Message;
      User_Message ("For component:");
      User_Message ("Parameter(2)  : <entity>|<category>");
      User_Message ("Parameter(3..): [not] pack | size | component_size (optional)");
      Help_On_Categories (Expected => Component_Expected_Categories);
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities, Thick_Queries, Utilities;
      use Ada.Strings.Wide_Unbounded;
      Subrule : Subrules;

      function Build_Index_List return Entity_Specification_List is
         Entity  : constant Entity_Specification := Get_Entity_Parameter (Allow_Extended => Box_OK or Parens_OK);
      begin
         case Categories'(Value (Entity)) is
            when Cat_Any =>
               null;
            when Discrete_Categories =>
               null;
            when others =>
               Parameter_Error (Rule_Id, "Not a possible category for index (" & Image (Entity) &')');
         end case;

         if Parameter_Exists then
            return Entity & Build_Index_List;
         else
            return (1 => Entity);
         end if;
      end Build_Index_List;

   begin   -- Add_Control
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "parameters required");
      end if;

      Length_Is_All (Ctl_Kind) := Get_Modifier ("ALL");
      Subrule := Get_Flag_Parameter (Allow_Any => False);
      if Length_Is_All (Ctl_Kind) and Subrule /= Length then
         Parameter_Error (Rule_Id, """all"" allowed only for subrule ""length""");
      end if;

      if Subrule in Dim_Subrules and Rule_Used (Subrule) (Ctl_Kind) then
         Parameter_Error (Rule_Id, "rule already specified for " & Control_Kinds'Wide_Image (Ctl_Kind));
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "two or three parameters required");
      end if;

      case Subrule is
         when First | Last | Dimensions =>
            Values (Subrule, Ctl_Kind) := Get_Bounds_Parameters (Rule_Id,
                                                                 Bound_Min    => Biggest_Int'First,
                                                                 Bound_Max    => Biggest_Int'Last,
                                                                 Allow_Single => True);
            Labels    (Subrule, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Rule_Used (Subrule) (Ctl_Kind) := True;

         when Length =>
            Values    (Subrule, Ctl_Kind)  := Get_Bounds_Parameters (Rule_Id);
            Labels    (Subrule, Ctl_Kind)  := To_Unbounded_Wide_String (Ctl_Label);
            Rule_Used (Subrule) (Ctl_Kind) := True;

         when Index =>
            declare
               Index_List : constant Entity_Specification_List := Build_Index_List;
            begin
               for E : Entity_Specification of Index_List loop
                  Check_Category (Rule_Id, E, Index_Expected_Categories);
               end loop;
               Associate (Index_Contexts,
                          Value (Integer_Img (Index_List'Length)),
                          Index_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                         Nb_Dims     => Index_List'Length,
                                         Index_Types => Index_List),
                          Additive => True);
            exception
               when Already_In_Store =>
                  Parameter_Error (Rule_Id, "Index combination already given: " & List_Image (Index_List));
            end;
            Rule_Used (Subrule) := (others => True);

         when Component =>
            declare
               Entity      : constant Entity_Specification := Get_Entity_Parameter (Allow_Extended => Parens_OK);
               The_Aspects : constant Aspects_Set := Get_Aspects_Parameter (Rule_Id,
                                                                            Expected => (Representation => Absent,
                                                                                         others         => Present));
            begin
               Check_Category (Rule_Id, Entity, Component_Expected_Categories);
               Associate (Compo_Contexts,
                          Entity,
                          Compo_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with The_Aspects),
                          Additive => True);
            exception
               when Already_In_Store =>
                  Parameter_Error (Rule_Id, "entity already given: " & Image (Entity));
            end;
            Rule_Used (Subrule) := (others => True);
      end case;

    end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : in Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used     := Not_Used;
            Length_Is_All := Empty_Control_Kinds_Set;
            Clear (Compo_Contexts);
            Clear (Index_Contexts);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Not_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ------------------------
   -- Get_Bound_Location --
   ------------------------

   function Get_Bound_Location (Definition : Asis.Definition; Dim : Asis.List_Index)
                                return Framework.Locations.Location
   is
      use Asis, Asis.Definitions, Asis.Elements;
      use Framework.Locations, Utilities;
   begin
      if Definition_Kind (Definition) = A_Constraint then
         case Discrete_Range_Kind (Discrete_Ranges (Definition) (Dim)) is
            when A_Discrete_Subtype_Indication =>
               return Get_Location
                 (Asis.Definitions.Subtype_Mark            --## rule line off Avoid_Query
                    (Discrete_Ranges (Definition) (Dim)));  --   we don't want the selector
            when A_Discrete_Range_Attribute_Reference =>
               return Get_Location (Range_Attribute (Discrete_Ranges (Definition) (Dim)));
            when A_Discrete_Simple_Expression_Range =>
               return Get_Location (Lower_Bound (Discrete_Ranges (Definition) (Dim)));
            when Not_A_Discrete_Range =>
               Failure ("Get_Bound_Location: not a discrete range");
         end case;
      else
         -- Array
         if Definition_Kind (Definition) = A_Subtype_Indication then
            -- Better let message appear with the subtype
            return Get_Location (Definition);
         end if;

         case Type_Kind (Definition) is
            when A_Constrained_Array_Definition =>
               return Get_Location (Discrete_Subtype_Definitions (Definition) (Dim));
            when An_Unconstrained_Array_Definition =>
               return Get_Location (Index_Subtype_Definitions (Definition) (Dim));
            when others =>
               Failure ("Get_Bound_Location: not an array definition");
         end case;
      end if;
   end Get_Bound_Location;


   --------------------
   -- Process_Length --
   --------------------

   procedure Process_Length (Definition : Asis.Definition; Active_Kinds : Control_Kinds_Set) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Reports, Thick_Queries;
      Lengths : constant Extended_Biggest_Natural_List := Discrete_Constraining_Lengths (Definition);

      function Declaration_Name return Wide_String is
         use Asis, Asis.Elements;
         use Utilities;
         Decl : Asis.Declaration := Enclosing_Element (Definition);
      begin
         while Element_Kind (Decl) /= A_Declaration and Element_Kind (Decl) /= A_Statement loop
            Decl := Enclosing_Element (Decl);
         end loop;
         case Declaration_Kind (Decl) is
            when A_Type_Declaration =>
               return "type";
            when A_Subtype_Declaration =>
               return "subtype";
            when A_Constant_Declaration | A_Return_Constant_Specification =>
               return "constant";
            when A_Variable_Declaration | A_Return_Variable_Specification =>
               return "variable";
            when A_Component_Declaration =>
               return "component";
            when Not_A_Declaration =>
               -- A statement: allocator...
               return "variable";
            when others =>
               Failure ("Process_Length: unexpected declaration", Decl);
         end case;
      end Declaration_Name;

   begin  -- Process_Length
      for L in Lengths'Range loop
         if Lengths (L) /= Not_Static then
            -- Max
            if Active_Kinds (Check) and then Lengths (L) > Values (Length, Check).Max then
               Report (Rule_Id,
                       To_Wide_String (Labels (Length, Check)),
                       Check,
                       Get_Bound_Location (Definition, L),
                       "array " & Declaration_Name & " dimension is bigger than "
                       & Biggest_Int_Img (Values (Length, Check).Max)
                       & " (" & Biggest_Int_Img (Lengths (L)) & ')');

            elsif Active_Kinds (Search) and then Lengths (L) > Values (Length, Search).Max then
               Report (Rule_Id,
                       To_Wide_String (Labels (Length, Search)),
                       Search,
                       Get_Bound_Location (Definition, L),
                       "array " & Declaration_Name & " dimension is bigger than "
                       & Biggest_Int_Img (Values (Length, Search).Max)
                       & " (" & Biggest_Int_Img (Lengths (L)) & ')');
            end if;

            if Active_Kinds (Count) and then Lengths (L) > Values (Length, Count).Max then
               Report (Rule_Id,
                       To_Wide_String (Labels (Length, Count)),
                       Count,
                       Get_Bound_Location (Definition, L),
                       "");
            end if;

            -- Min
            if Active_Kinds (Check) and then Lengths (L) < Values (Length, Check).Min then
               Report (Rule_Id,
                       To_Wide_String (Labels (Length, Check)),
                       Check,
                       Get_Bound_Location (Definition, L),
                       "array " & Declaration_Name & " dimension is smaller than "
                       & Biggest_Int_Img (Values (Length, Check).Min)
                       & " (" & Biggest_Int_Img (Lengths (L)) & ')');

            elsif Active_Kinds (Search) and then Lengths (L) < Values (Length, Search).Min then
               Report (Rule_Id,
                       To_Wide_String (Labels (Length, Search)),
                       Search,
                       Get_Bound_Location (Definition, L),
                       "array " & Declaration_Name & " dimension is smaller than "
                       & Biggest_Int_Img (Values (Length, Search).Min)
                       & " (" & Biggest_Int_Img (Lengths (L)) & ')');
            end if;

            if Active_Kinds (Count) and then Lengths (L) < Values (Length, Count).Min then
               Report (Rule_Id,
                       To_Wide_String (Labels (Length, Count)),
                       Count,
                       Get_Bound_Location (Definition, L),
                       "");
            end if;
         end if;
      end loop;
   end Process_Length;

   ------------------------------
   -- Process_Index_Constraint --
   ------------------------------

   procedure Process_Index_Constraint (Definition : Asis.Definition) is
      use Asis, Asis.Elements;

      procedure Process_First_Last is
         use Ada.Strings.Wide_Unbounded;
         use Framework.Reports, Thick_Queries;
         Bounds : constant Asis.Element_List := Discrete_Constraining_Bounds (Definition);
         Val    : Extended_Biggest_Int;

         Sr : Subrules range First .. Last := First;
         Sr_Used : constant array (Subrules range First .. Last) of Boolean
           := (First => Rule_Used (First) /= (Control_Kinds => False),
               Last  => Rule_Used (Last)  /= (Control_Kinds => False));
         Bound_Msg : constant array (Subrules range First .. Last) of Wide_String (1 .. 5)
           := ("lower", "upper");

      begin  -- Process_First_Last
         for B in Bounds'Range loop
            if Sr_Used (Sr) then
               -- Lower bound
               Val := Discrete_Static_Expression_Value (Bounds (B));
               if Val /= Not_Static then
                  if  Rule_Used (Sr) (Check) and Rule_Used (Sr) (Search) then
                     if    not Is_In (Val, Values (Sr, Check))
                       and not Is_In (Val, Values (Sr, Search))
                     then
                        Report (Rule_Id,
                                To_Wide_String (Labels (Sr, Check)),
                                Check,
                                Get_Bound_Location (Definition, (B + 1) / 2),
                                Bound_Msg (Sr) & " bound of array is "
                                & Bound_Image (Values (Sr, Check))
                                & " and "
                                & Bound_Image (Values (Sr, Search))
                                & " (" & Biggest_Int_Img (Val) & ')');
                     elsif not Is_In (Val, Values (Sr, Search)) then
                        Report (Rule_Id,
                                To_Wide_String (Labels (Sr, Search)),
                                Search,
                                Get_Bound_Location (Definition, (B + 1) / 2),
                                Bound_Msg (Sr) & " bound of array is "
                                & Bound_Image (Values (Sr, Search))
                                & " (" & Biggest_Int_Img (Val) & ')');
                     end if;

                  elsif Rule_Used (Sr) (Check)
                    and then not Is_In (Val, Values (Sr, Check))
                  then
                     Report (Rule_Id,
                             To_Wide_String (Labels (Sr, Check)),
                             Check,
                             Get_Bound_Location (Definition, (B + 1) / 2),
                             Bound_Msg (Sr) & " bound of array is "
                             & Bound_Image (Values (Sr, Check))
                             & " (" & Biggest_Int_Img (Val) & ')');

                  elsif Rule_Used (Sr) (Search)
                    and then not Is_In (Val, Values (Sr, Search))
                  then
                     Report (Rule_Id,
                             To_Wide_String (Labels (Sr, Search)),
                             Search,
                             Get_Bound_Location (Definition, (B + 1) / 2),
                             Bound_Msg (Sr) & " bound of array is "
                             & Bound_Image (Values (Sr, Search))
                             & " (" & Biggest_Int_Img (Val) & ')');
                  end if;

                  if Rule_Used (Sr) (Count)
                    and then not Is_In (Val, Values (Sr, Count))
                  then
                     Report (Rule_Id,
                             To_Wide_String (Labels (Sr, Count)),
                             Count,
                             Get_Bound_Location (Definition, (B + 1) / 2),
                             "");
                  end if;
               end if;
            end if;
            case Sr is
               when First =>
                  Sr := Last;
               when Last =>
                  Sr := First;
            end case;
         end loop;
      end Process_First_Last;

   begin  -- Process_Index_Constraint
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

       if (Rule_Used (First) or Rule_Used (Last)) /= Empty_Control_Kinds_Set then
          Process_First_Last;
       end if;

      if Rule_Used (Length) /= Empty_Control_Kinds_Set
        and then Type_Kind (Definition) /= An_Unconstrained_Array_Definition
      then
         Process_Length (Definition, Rule_Used (Length));
      end if;
   end Process_Index_Constraint;

   ------------------------------
   -- Process_Array_Definition --
   ------------------------------

   procedure Process_Array_Definition (Definition : Asis.Definition) is

      procedure Process_Index is
         use Asis.Declarations, Asis.Elements;
         use Framework.Locations, Framework.Queries, Framework.Reports;
         use Thick_Queries, Utilities;

         Index_Subtypes : Asis.Element_List := Index_Subtypes_Names (Definition);
         Iterator       : Context_Iterator := Index_Iterator.Create;
         All_Dims_Match : Boolean;

      begin  -- Process_Index
         Reset (Iterator, Value (Integer_Img (Index_Subtypes'Length)));
         while not Is_Exhausted (Iterator) loop
            declare
               Entities_List : constant Entity_Specification_List := Index_Context (Value (Iterator)).Index_Types;
               Cat           : Categories;
            begin
               All_Dims_Match := True;
               for E in Entities_List'Range loop
                  if Is_Nil (Index_Subtypes (E)) then
                     -- Case of X : array (1..10) of ...
                     -- This defaults to Integer
                     Index_Subtypes (E) := Names (Standard_Value ("INTEGER")) (1);
                  end if;
                  if not Matches (Entities_List (E), Index_Subtypes (E), All_Extensions) then
                     -- No subtype match
                     if not Matches (Entities_List (E), First_Subtype_Name (Index_Subtypes (E)), All_Extensions) then
                        -- No type match
                        Cat := Value (Entities_List (E));
                        if Cat = Cat_Any
                          or else not Matches (Enclosing_Element (Index_Subtypes (E)),
                                               Cat,
                                               Follow_Derived     => True,
                                               Privacy            => Stop_At_Private,
                                               Separate_Extension => False)
                        then
                           -- No category match
                           All_Dims_Match := False;
                           exit;
                        end if;
                     end if;
                  end if;
               end loop;
               if All_Dims_Match then
                  if Entities_List'Length = 1 then
                     Report (Rule_Id,
                             Value (Iterator),
                             Get_Location (Definition),
                             "array index is " & Image (Entities_List (1)));
                  else
                     Report (Rule_Id,
                             Value (Iterator),
                             Get_Location (Definition),
                             "array indices are " & List_Image (Entities_List));
                  end if;
               end if;
            end;

            Next (Iterator);
         end loop;
      end Process_Index;

      procedure Process_Component is
         use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
         use Thick_Queries, Utilities;
         Array_Comp : constant Asis.Expression := Component_Definition_View
                                                   (Array_Component_Definition (Definition));
         First_St   : Asis.Declaration;
         Iterator   : Context_Iterator := Compo_Iterator.Create;

         Compo_Aspects : constant Aspects_Set := Corresponding_Aspects_Set (Definition);

         procedure Compo_Report (Iter : in out Context_Iterator; In_Case : Casing) is
            use Ada.Strings.Wide_Unbounded;
            use Framework.Locations, Framework.Reports;
            Extra           : Unbounded_Wide_String;
            Current_Context : Compo_Context;
            Applicable      : Boolean;

            procedure Check_Aspect (A : Aspects) is
            begin
               case Current_Context.Aspects (A) is
                  when Unspecified =>
                     null;
                  when Present =>
                     if Compo_Aspects (A) = Present then
                        Append (Extra, To_Lower (Aspects'Wide_Image (A)) & ' ');
                     else
                        Applicable := False;
                     end if;
                  when Absent =>
                     if Compo_Aspects (A) = Present then
                        Applicable := False;
                     else
                        Append (Extra, "not " & To_Lower (Aspects'Wide_Image (A)) & ' ');
                     end if;
               end case;
            end Check_Aspect;
         begin  -- Compo_Report
            while not Is_Exhausted (Iter) loop
               Applicable := True;
               Extra      := Null_Unbounded_Wide_String;

               Current_Context := Compo_Context (Value (Iter));
               for A in Aspects loop
                  Check_Aspect (A);
               end loop;

               if Applicable then
                  if Extra = Null_Unbounded_Wide_String then
                     Report (Rule_Id,
                             Current_Context,
                             Get_Location (Array_Comp),
                             "array component is """
                             & Set_Casing (Last_Matching_Name (Compo_Contexts), In_Case)
                             & '"');
                  else
                     Report (Rule_Id,
                             Current_Context,
                             Get_Location (Array_Comp),
                             "component is """
                             & Set_Casing (Last_Matching_Name (Compo_Contexts), In_Case)
                             & """ of array with "
                             & To_Wide_String (Extra));
                  end if;
               end if;
               Next (Iter);
            end loop;
         end Compo_Report;
      begin  -- Process_Component
         if Definition_Kind (Array_Comp) = An_Access_Definition then
            -- Anonymous access type
            -- Give up on matching (sub)types, only match access category
            -- Category
            Reset (Iterator, Framework.Value (Image (An_Access_Type)));
            Compo_Report (Iterator, Lower_Case);
         else
            -- Exact subtype
            Reset (Iterator, Subtype_Simple_Name (Array_Comp), Extend_To => All_Extensions);
            Compo_Report (Iterator, Title_Case);

            declare
               -- We can now get rid of 'Base if any (cannot be 'Class)
               Comp_Decl  : constant Asis.Declaration := Corresponding_Name_Declaration
                                                          (Strip_Attributes
                                                           (Subtype_Simple_Name (Array_Comp)));
            begin
               -- First subtype (aka type), if different
               First_St := A4G_Bugs.Corresponding_First_Subtype (Comp_Decl);
               if not Is_Equal (First_St, Comp_Decl) then
                  Reset (Iterator, Names (First_St) (1), Extend_To => All_Extensions);
                  Compo_Report (Iterator, Title_Case);
               end if;

               -- Category
               Reset (Iterator, Framework.Value (Image (Type_Category (First_St, Follow_Derived => True))));
               Compo_Report (Iterator, Lower_Case);
            end;
         end if;
      end Process_Component;

      procedure Process_Dimensions is
         use Ada.Strings.Wide_Unbounded;
         use Asis, Asis.Definitions, Asis.Elements;
         use Framework.Locations, Framework.Reports, Thick_Queries, Utilities;

         Nb_Dim : Biggest_Int;
      begin
         case Type_Kind (Definition) is
            when A_Constrained_Array_Definition =>
               Nb_Dim := Discrete_Subtype_Definitions (Definition)'Length;
            when An_Unconstrained_Array_Definition =>
               Nb_Dim := Index_Subtype_Definitions (Definition)'Length;
            when others =>
               Failure ("Process_Dimension: not an array definition");
         end case;

         -- Max
         if Rule_Used (Dimensions) (Check) and then Nb_Dim > Values (Dimensions, Check).Max then
                  Report (Rule_Id,
                          To_Wide_String (Labels (Dimensions, Check)),
                          Check,
                          Get_Location (Definition),
                          "array has more than " & Biggest_Int_Img (Values (Dimensions, Check).Max)
                          & " dimensions (" & Biggest_Int_Img (Nb_Dim) & ')');

         elsif Rule_Used (Dimensions) (Search) and then Nb_Dim > Values (Dimensions, Search).Max then
            Report (Rule_Id,
                    To_Wide_String (Labels (Dimensions, Search)),
                    Search,
                    Get_Location (Definition),
                    "array has more than " & Biggest_Int_Img (Values (Dimensions, Search).Max)
                    & " dimensions (" & Biggest_Int_Img (Nb_Dim) & ')');
         end if;

         if Rule_Used (Dimensions) (Count) and then Nb_Dim > Values (Dimensions, Count).Max then
            Report (Rule_Id,
                    To_Wide_String (Labels (Dimensions, Count)),
                    Count,
                    Get_Location (Definition),
                    "");
         end if;

         -- Min
         if Rule_Used (Dimensions) (Check) and then Nb_Dim < Values (Dimensions, Check).Min then
            Report (Rule_Id,
                    To_Wide_String (Labels (Dimensions, Check)),
                    Check,
                    Get_Location (Definition),
                    "array has less than " & Biggest_Int_Img (Values (Dimensions, Check).Min)
                          & " dimensions (" & Biggest_Int_Img (Nb_Dim) & ')');

         elsif Rule_Used (Dimensions) (Search) and then Nb_Dim < Values (Dimensions, Search).Min then
            Report (Rule_Id,
                    To_Wide_String (Labels (Dimensions, Search)),
                    Search,
                    Get_Location (Definition),
                    "array has less than " & Biggest_Int_Img (Values (Dimensions, Search).Min)
                    & " dimensions (" & Biggest_Int_Img (Nb_Dim) & ')');
         end if;

         if Rule_Used (Dimensions) (Count) and then Nb_Dim < Values (Dimensions, Count).Min then
            Report (Rule_Id,
                    To_Wide_String (Labels (Dimensions, Count)),
                    Count,
                    Get_Location (Definition),
                    "");
         end if;
      end Process_Dimensions;
   begin  -- Process_Array_Definition
      Process_Index_Constraint (Definition);

      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (Dimensions) /= Empty_Control_Kinds_Set then
         Process_Dimensions;
      end if;

      if not Is_Empty (Index_Contexts) then
         Process_Index;
      end if;

      if not Is_Empty (Compo_Contexts) then
         Process_Component;
      end if;
   end Process_Array_Definition;


   --------------------------------
   -- Process_Object_Declaration --
   --------------------------------

   procedure Process_Object_Declaration (Declaration : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
   begin
      if Rule_Used (Length) = Empty_Control_Kinds_Set then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Length_Is_All = Empty_Control_Kinds_Set then
         return;
      end if;

      declare
         Obj_Def : constant Asis.Definition := Object_Declaration_View (Declaration);
      begin
         if not Is_Array_Subtype (Obj_Def) then
            return;
         end if;
         if Definition_Kind (Obj_Def) /= A_Subtype_Indication or else not Is_Nil (Subtype_Constraint (Obj_Def))  then
            -- will be handled by Process_Array_Definition or Process_Index_Constraint
            return;
         end if;

         -- No 'BASE to fear below, it's not applicable to array types
         if Type_Kind (Type_Declaration_View (Corresponding_Name_Declaration (Subtype_Simple_Name (Obj_Def)))) =
           An_Unconstrained_Array_Definition
         then
            -- Unconstrained type, size comes from initialization => dynamic
            return;
         end if;

         Process_Length (Obj_Def, Rule_Used (Length) and Length_Is_All);
      end;
   end Process_Object_Declaration;

begin  -- Rules.Array_Declarations
   Rules_Manager.Register (Rule_Id,
                           Rules_Manager.Semantic,
                           Help_CB        => Help'Access,
                           Add_Control_CB => Add_Control'Access,
                           Command_CB     => Command'Access);
end Rules.Array_Declarations;
