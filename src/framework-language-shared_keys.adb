----------------------------------------------------------------------
--  Framework.Language.Shared_Keys - Package body                   --
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

-- ASIS
with
  Asis.Elements;

-- Adacontrol
with
  Framework.Scope_Manager,
  Framework.Language.Scanner;

package body Framework.Language.Shared_Keys is

   -------------------
   -- Is_Applicable --
   -------------------

   function Is_Applicable (Expected_Places : Places_Set) return Boolean is
      use Framework.Scope_Manager, Scope_Places_Utilities;
      use Asis, Asis.Elements;
      Scope_Kind : constant Declaration_Kinds := Declaration_Kind (Current_Scope);

      Locations  : constant Places_Set := (S_All       => False,
                                           S_Block     => Statement_Kind (Current_Scope) = A_Block_Statement,
                                           S_Library   => Current_Depth = 0,
                                           S_Local     => not Is_Current_Scope_Global,
                                           S_Nested    => Current_Depth /= 0,
                                           S_Own       => Scope_Kind = A_Package_Body_Declaration,
                                           S_Private   => In_Private_Part,
                                           S_Public    => (Scope_Kind = A_Package_Declaration
                                                           or Scope_Kind = A_Generic_Package_Declaration)
                                                          and not In_Private_Part,
                                           S_Task_Body => Scope_Kind = A_Task_Body_Declaration);
   begin
      if Expected_Places (S_All) then
         return True;
      end if;

      return (Expected_Places and Locations) = Expected_Places;
   end Is_Applicable;

   ------------------------------
   -- Get_Places_Set_Parameter --
   ------------------------------

   function Get_Places_Set_Modifiers return Places_Set is
      use Scope_Places_Utilities, Framework.Language.Scanner;
      Loc : constant Places_Set := Get_Modifier_Set;
   begin
      if Loc = Empty_Set then
         return Everywhere;
      elsif Loc (S_All) and Loc /= Everywhere then
         Syntax_Error ("""all"" cannot be specified with other locations", Current_Token.Position);
      else
         return Loc;
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
         Result.Max := Result.Min;                --## rule line off Multiple_Assignments
         return Result;
      end if;

      while Parameter_Exists loop
         case Get_Modifier (Required => True) is
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
         Thick_Queries.An_Access_Type               => Cat_Access,
         Thick_Queries.A_Derived_Type               => Cat_New,
         Thick_Queries.A_Private_Type               => Cat_Private,
         Thick_Queries.A_Task_Type                  => Cat_Task,
         Thick_Queries.A_Protected_Type             => Cat_Protected);

   function Matches (Elem               : in Asis.Element;
                     Cat                : in Categories;
                     Follow_Derived     : in Boolean := False;
                     Follow_Private     : in Boolean := False;
                     Separate_Extension : in Boolean := False)
                     return Boolean
   is
      use Thick_Queries;
   begin
      if Cat = Cat_Any then
         return True;
      end if;
      return Match_Table (Type_Category (Elem, Follow_Derived, Follow_Private, Separate_Extension)) = Cat;
   end Matches;


   -----------------------
   -- Matching_Category --
   -----------------------

   function Matching_Category (Elem               : in Asis.Element;
                               From_Cats          : in Categories_Utilities.Unconstrained_Modifier_Set;
                               Follow_Derived     : in Boolean := False;
                               Follow_Private     : in Boolean := False;
                               Separate_Extension : in Boolean := False)
                               return Categories
   is
      use Thick_Queries;
      Cat : constant Categories
        := Match_Table (Type_Category (Elem, Follow_Derived, Follow_Private, Separate_Extension));
   begin
      if From_Cats (Cat) then
         return Cat;
      else
         return Cat_Any;
      end if;
   end Matching_Category;


end Framework.Language.Shared_Keys;
