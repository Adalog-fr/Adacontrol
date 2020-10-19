----------------------------------------------------------------------
--  Framework.Language.Shared_Keys - Package body                   --
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
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Limited_Views;

-- Adalog
with
  A4G_Bugs;

-- Adacontrol
with
  Scope_Manager;

package body Framework.Language.Shared_Keys is

   type Min_Max is (Not_A_Bound, Min, Max);
   package Min_Max_Utilities is new Modifier_Utilities (Min_Max);


   -------------------
   -- Is_Applicable --
   -------------------

   function Is_Applicable (Expected_Places : Places_Set) return Boolean is
      use Scope_Manager, Scope_Places_Utilities, Thick_Queries;
      use Asis, Asis.Elements;
      Scope_Kind : constant Declaration_Kinds := Declaration_Kind (Current_Scope);

      Locations  : constant Modifier_Set := (S_All        => False,
                                             S_Block      => Statement_Kind (Current_Scope) = A_Block_Statement,
                                             S_Library    => Current_Depth = 0,
                                             S_Local      => not Is_Current_Scope_Global,
                                             S_Own        => Scope_Kind = A_Package_Body_Declaration,
                                             S_Private    => In_Private_Part,
                                             S_Public     => Scope_Kind in A_Package_Declaration
                                                                         | A_Generic_Package_Declaration
                                                             and not In_Private_Part,
                                             S_In_Generic => Is_Generic_Unit (Current_Scope)
                                                             or else Is_Part_Of_Generic (Current_Scope),
                                             S_Task_Body  => Scope_Kind = A_Task_Body_Declaration);
   begin
      if Expected_Places.Specified (S_All) then
         return True;
      end if;

      return (Expected_Places.Specified and Locations) = Expected_Places.Presence;
   end Is_Applicable;

   -----------
   -- Image --
   -----------

   function Image (Set     : Places_Set;
                   Default : Places_Set := No_Places) return Wide_String
   is
      use Scope_Places_Utilities;

      function Image (From : Scope_Places) return Wide_String is
         use  Utilities;
      begin
         if not Set.Specified (From) then
            if From = Scope_Places'Last then
               return "";
            else
               return Image (From => Scope_Places'Succ (From));
            end if;
         end if;

         if From = Scope_Places'Last then
            if Set.Presence (From) then
               return Image (From, Lower_Case) & ' ';
            else
               return "not " & Image (From, Lower_Case) & ' ';
            end if;
         else
            if Set.Presence (From) then
               return Image (From, Lower_Case) & ' ' & Image (From => Scope_Places'Succ (From));
            else
               return "not " & Image (From, Lower_Case) & ' ' & Image (From => Scope_Places'Succ (From));
            end if;
         end if;
      end Image;
   begin    -- Image
      if Set.Specified = (Set.Specified'Range => False) or else Set = Default then
         return "";
      end if;

      return Image (From => Scope_Places'First);
   end Image;

   --------------------------
   -- Help_On_Scope_Places --
   --------------------------

   procedure Help_On_Scope_Places (Header : Wide_String  := "";
                                   Expected : Scope_Places_Utilities.Modifier_Set  := Scope_Places_Utilities.Full_Set;
                                   With_Not : Boolean := True)
   is
   begin
      if With_Not then
         Scope_Places_Utilities.Help_On_Modifiers (Header => Header & " [not]", Expected => Expected);
      else
         Scope_Places_Utilities.Help_On_Modifiers (Header => Header,            Expected => Expected);
      end if;
   end Help_On_Scope_Places;


   ------------------------------
   -- Get_Places_Set_Modifiers --
   ------------------------------

   function Get_Places_Set_Modifiers (Rule_Id : Wide_String; Allow_All : Boolean := True) return  Places_Set is
      use Scope_Places_Utilities;
      Result   : Places_Set := No_Places;
      Loc      : Scope_Places;
      Found    : Boolean;
      Presence : Boolean;
   begin
      loop
         Presence := not Get_Modifier ("NOT");
         Get_Modifier (Loc, Found, Expected => (S_All => Allow_All, others => True));
         exit when not Found;
         if Loc = S_All and not Presence then
            Parameter_Error (Rule_Id, """all"" cannot be specified with ""not""");
         end if;
         Result.Specified (Loc) := True;
         Result.Presence  (Loc) := Presence;
      end loop;

      if Result = No_Places then
         return Everywhere;
      elsif Result.Specified (S_All) and Result.Specified /= Empty_Set then
         Parameter_Error (Rule_Id, """all"" cannot be specified with other locations");
      else
         return Result;
      end if;
   end Get_Places_Set_Modifiers;

   ---------------------------
   -- Get_Bounds_Parameters --
   ---------------------------

   function Get_Bounds_Parameters (Rule_Id      : Wide_String;
                                   Bound_Min    : Thick_Queries.Biggest_Int := 0;
                                   Bound_Max    : Thick_Queries.Biggest_Int := Thick_Queries.Biggest_Natural'Last;
                                   Allow_Single : Boolean                   := False)
                                   return Bounds_Values
   is
      use Thick_Queries, Min_Max_Utilities;

      Min_Given : Boolean := False;
      Max_Given : Boolean := False;
      Result    : Bounds_Values := (Bound_Min, Bound_Max);
   begin
      if Allow_Single and then Is_Integer_Parameter then
         Result.Min := Get_Integer_Parameter (Min => Bound_Min, Max => Bound_Max);
         Result.Max := Result.Min;                --## rule line off Assignments
         return Result;
      end if;

      while Parameter_Exists loop
         case Min_Max'(Get_Modifier (Required => False)) is
            when Not_A_Bound =>
               exit;
            when Min =>
               if Min_Given then
                  Parameter_Error (Rule_Id, "Min value given more than once");
               end if;
               Result.Min := Get_Integer_Parameter (Min => Bound_Min, Max => Bound_Max);
               Min_Given  := True;
            when Max =>
               if Max_Given then
                  Parameter_Error (Rule_Id, "Max value given more than once");
               end if;
               Result.Max := Get_Integer_Parameter (Min => Bound_Min, Max => Bound_Max);
               Max_Given  := True;
         end case;
      end loop;

      if Result.Min > Result.Max then
         Parameter_Error (Rule_Id, "Min value must be less than Max");
      end if;

      return Result;
   end Get_Bounds_Parameters;

   -----------
   -- Is_In --
   -----------

   function Is_In (Val : Thick_Queries.Biggest_Int; Bounds : Bounds_Values) return Boolean is
   begin
      return Val in Bounds.Min .. Bounds.Max;
   end Is_In;

   -----------------
   -- Bound_Image --
   -----------------

   function Bound_Image (Bounds : Language.Shared_Keys.Bounds_Values) return Wide_String is
      use Thick_Queries;
   begin
      if Bounds.Min = Bounds.Max then
         return "not " & Biggest_Int_Img (Bounds.Min);
      elsif Bounds.Min = Biggest_Int'First then
         return "more than " & Biggest_Int_Img (Bounds.Max);
      elsif Bounds.Max = Biggest_Int'Last then
         return "less than " & Biggest_Int_Img (Bounds.Min);
      else
         return "not in " & Biggest_Int_Img (Bounds.Min) & " .. " & Biggest_Int_Img (Bounds.Max);
      end if;
   end Bound_Image;

   --------------------
   -- Help_On_Bounds --
   --------------------

   procedure Help_On_Bounds (Header : Wide_String  := "") is
   begin
      Min_Max_Utilities.Help_On_Modifiers (Header => Header, Expected => (Not_A_Bound => False, others => True));
   end Help_On_Bounds;

   -----------
   -- Image --
   -----------

   function Image (Item : Thick_Queries.Type_Categories) return Wide_String is
      use Thick_Queries;
   begin
      case Item is
         when Not_A_Type =>
            return "";
         when An_Enumeration_Type =>
            return "()";
         when A_Signed_Integer_Type =>
            return "RANGE";
         when A_Modular_Type =>
            return "MOD";
         when A_Fixed_Point_Type =>
            return "DELTA";
         when A_Floating_Point_Type =>
            return "DIGITS";
         when An_Array_Type =>
            return "ARRAY";
         when A_Record_Type =>
            return "RECORD";
         when A_Tagged_Type =>
            return "TAGGED";
         when An_Extended_Tagged_Type =>
            return "EXTENSION";
         when An_Interface_Type =>
            return "INTERFACE";
         when An_Access_Type =>
            return "ACCESS";
         when A_Derived_Type =>
            return "NEW";
         when A_Private_Type =>
            return "PRIVATE";
         when A_Task_Type =>
            return "TASK";
         when A_Protected_Type =>
            return "PROTECTED";
      end case;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Spec : Entity_Specification) return Categories is
   begin
      if Spec.Kind /= Regular_Id then
         return Cat_Any;
      end if;
      return Value (To_Wide_String (Spec.Specification));
   end Value;


   -----------
   -- Value --
   -----------

   function Value (Spec : Wide_String) return Categories is
   begin
      if Spec = "()" then
         return Cat_Enum;
      else
         return Categories'Wide_Value ("CAT_" & Spec);
      end if;
   exception
      when Constraint_Error =>
         return Cat_Any;
   end Value;

   ---------
   -- "+" --
   ---------

   function "+" (Left : Categories_Set; Right : Categories) return Categories_Set is
      Result : Categories_Set := Left;
   begin
      Result (Right) := True;
      return Result;
   end "+";


   ------------------------
   -- Help_On_Categories --
   ------------------------

   procedure Help_On_Categories (Header   : Wide_String    := "<category>:";
                                 Expected : Categories_Set := Categories_Utilities.Full_Set)
   is
   begin
      Categories_Utilities.Help_On_Modifiers (Header, Expected => Expected);
   end Help_On_Categories;


   --------------------
   -- Check_Category --
   --------------------

   procedure Check_Category (Rule_Id : Wide_String; Spec : Entity_Specification; Expected : Categories_Set) is
      use Categories_Utilities;
      Cat : constant Categories := Value (Spec);
   begin
      if Cat = Cat_Any then
         -- either Spec is not a category, or it is really Cat_Any, but then it has to be expected since this one
         -- is caught by the parser otherwise.
         return;
      end if;

      if not Expected (Cat) then
         Parameter_Error (Rule_Id, "Category not allowed: " & Image (Cat));
      end if;
   end Check_Category;


   -------------
   -- Matches --
   -------------

   Match_Table : constant array (Thick_Queries.Type_Categories) of Categories
     := (Thick_Queries.Not_A_Type                   => Cat_Any,
         -- For Matches: Since Cat_Any is eliminated first, will return false
         Thick_Queries.An_Enumeration_Type          => Cat_Enum,
         Thick_Queries.A_Signed_Integer_Type        => Cat_Range,
         Thick_Queries.A_Modular_Type               => Cat_Mod,
         Thick_Queries.A_Fixed_Point_Type           => Cat_Delta,
         Thick_Queries.A_Floating_Point_Type        => Cat_Digits,
         Thick_Queries.An_Array_Type                => Cat_Array,
         Thick_Queries.A_Record_Type                => Cat_Record,
         Thick_Queries.A_Tagged_Type                => Cat_Tagged,
         Thick_Queries.An_Extended_Tagged_Type      => Cat_Extension,
         Thick_Queries.An_Interface_Type            => Cat_Interface,
         Thick_Queries.An_Access_Type               => Cat_Access,
         Thick_Queries.A_Derived_Type               => Cat_New,
         Thick_Queries.A_Private_Type               => Cat_Private,
         Thick_Queries.A_Task_Type                  => Cat_Task,
         Thick_Queries.A_Protected_Type             => Cat_Protected);

   function Matches (Elem               : in Asis.Element;
                     Cat                : in Categories;
                     Follow_Derived     : in Boolean;
                     Privacy            : in Thick_Queries.Privacy_Policy;
                     Separate_Extension : in Boolean)
                     return Boolean
   is
      use Thick_Queries;
   begin
      if Cat = Cat_Any then
         return True;
      end if;
      return Match_Table (Type_Category (Elem, Follow_Derived, Privacy, Separate_Extension)) = Cat;
   end Matches;


   -----------------------
   -- Matching_Category --
   -----------------------

   function Matching_Category (Elem               : in Asis.Element;
                               From_Cats          : in Categories_Utilities.Unconstrained_Modifier_Set;
                               Follow_Derived     : in Boolean;
                               Privacy            : in Thick_Queries.Privacy_Policy;
                               Separate_Extension : in Boolean)
                               return Categories
   is
      use Thick_Queries;
      Cat : constant Categories := Match_Table (Type_Category (Elem, Follow_Derived, Privacy, Separate_Extension));
   begin
      if From_Cats (Cat) then
         return Cat;
      else
         return Cat_Any;
      end if;
   end Matching_Category;


   ---------------------------
   -- Get_Aspects_Parameter --
   ---------------------------

   function Get_Aspects_Parameter (Rule_Id  : Wide_String;
                                   Expected : Aspects_Set := (others => Present))
                                   return Aspects_Set
   is
      use Aspects_Utilities, Utilities;

      Result : Aspects_Set := (others => Unspecified);
      Temp   : Aspect_Presence;
      A      : Aspects;
   begin
      while Parameter_Exists loop
         if Get_Modifier ("NOT") then
            Temp := Absent;
         else
            Temp := Present;
         end if;

         A := Get_Flag_Parameter (Allow_Any => False);
         if Expected (A) /= Present then
            Parameter_Error (Rule_Id, "aspect not allowed for this rule");
         end if;
         if Result (A) /= Unspecified then
            Parameter_Error (Rule_Id, "aspect already specified: " & Image (A, Title_Case));
         end if;

         Result (A) := Temp;
      end loop;
      return Result;
   end Get_Aspects_Parameter;


   -------------------------------
   -- Corresponding_Aspects_Set --
   -------------------------------

   function Corresponding_Aspects_Set (Elem : Asis.Element) return Aspects_Set is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions, Asis.Limited_Views;
      use Thick_Queries, Utilities;

      Decl   : Asis.Declaration;
      Result : Aspects_Set := (others => Absent);
   begin
      case Element_Kind (Elem) is
         when A_Declaration =>
            Decl := Elem;
         when A_Defining_Name | A_Definition =>
            Decl := Enclosing_Element (Elem);
         when An_Expression =>
            Decl := Corresponding_Name_Declaration (Simple_Name (Elem));
         when others =>
            Failure ("Corresponding_Aspects_Set: incorrect elem", Elem);
      end case;
      if Is_From_Limited_View (Decl) then
         Decl := A4G_Bugs.Get_Nonlimited_View (Decl);
      end if;

      for R : Asis.Representation_Clause of Corresponding_Representation_Clauses (Decl) loop
         case Representation_Clause_Kind (R) is
            when An_Enumeration_Representation_Clause | A_Record_Representation_Clause =>
               Result (Representation) := Present;
            when others =>
               null;
         end case;
      end loop;
      if Declaration_Kind (Decl) = An_Ordinary_Type_Declaration
      -- Pragma pack does not apply to objects
        and then Corresponding_Pragma_Set (Names (Decl) (1)) (A_Pack_Pragma)
      then
         Result (Pack) := Present;
      end if;

      if not Is_Nil (Attribute_Clause_Expression (A_Size_Attribute, Decl)) then
         Result (Size) := Present;
      end if;

      if not Is_Nil (Attribute_Clause_Expression (A_Component_Size_Attribute, Decl)) then
         Result (Component_Size) := Present;
      end if;

      for Def : Asis.Definition of Aspect_Specifications (Decl) loop
         declare
            Mark : constant Wide_String := To_Upper (Extended_Name_Image (Aspect_Mark (Def)));
         begin
            if Mark = "PACK" and then
              (Discrete_Static_Expression_Value (Aspect_Definition (Def)) = Not_Static or
              Discrete_Static_Expression_Value (Aspect_Definition (Def)) = 1)
            then
               Result (Pack) := Present;
            elsif Mark = "SIZE" then
               Result (Size) := Present;
            elsif Mark = "COMPONENT_SIZE" then
               Result (Component_Size) := Present;
            end if;
         end;
      end loop;

      return Result;
   end Corresponding_Aspects_Set;
end Framework.Language.Shared_Keys;
