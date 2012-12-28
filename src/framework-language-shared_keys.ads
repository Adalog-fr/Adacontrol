----------------------------------------------------------------------
--  Framework.Language.Shared_Keys - Package specification          --
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

-- Adalog
with
   Thick_Queries;

-- Adacontrol
with
   Framework.Language;
pragma Elaborate (Framework.Language);
-- We must "with" our parent, because the pragma Elaborate is required
-- here, as for any unit that instantiates a generic from Framework.Language
package Framework.Language.Shared_Keys is

   -----------------------------------------------------------------------------------
   -- Scope_Places
   -----------------------------------------------------------------------------------

   type Scope_Places is (S_All, S_Block,   S_Library, S_Local,      S_Nested,
                         S_Own, S_Private, S_Public,  S_In_Generic, S_Task_Body);
   package Scope_Places_Utilities is new Modifier_Utilities (Scope_Places, "S_");

   subtype Places_Set is Scope_Places_Utilities.Modifier_Set;
   Everywhere : constant Places_Set := (S_All => True, others => False);
   function Get_Places_Set_Modifiers (Allow_All : Boolean := True) return Places_Set;
   function Is_Applicable (Expected_Places : Places_Set) return Boolean;
   -- Checks if Current_Scope matches all Scope_Places in Expected_Places


   -----------------------------------------------------------------------------------
   -- Min_Max
   -----------------------------------------------------------------------------------

   type Bounds_Values is
      record
         Min : Thick_Queries.Biggest_Int;
         Max : Thick_Queries.Biggest_Int;
      end record;
   Unlimited_Bounds : constant Bounds_Values := (Min => Thick_Queries.Biggest_Int'First,
                                                 Max => Thick_Queries.Biggest_Int'Last);
   function Get_Bounds_Parameters (Rule_Id      : Wide_String;
                                   Bound_Min    : Thick_Queries.Biggest_Int := 0;
                                   Bound_Max    : Thick_Queries.Biggest_Int := Thick_Queries.Biggest_Natural'Last;
                                   Allow_Single : Boolean                   := False)
                                   return Bounds_Values;
   -- Gets Min and Max parameters in the form min <val>, max <val> (in any order)
   -- or the Bound_Min (resp Bound_Max) value for unspecified bounds
   -- If Allow_Single, a single value (without specifying Min or Max) is allowed and
   -- returned in both Min and Max

   function Is_In (Val : Thick_Queries.Biggest_Int; Bounds : Bounds_Values) return Boolean;

   function Bound_Image (Bounds : Language.Shared_Keys.Bounds_Values) return Wide_String;
   -- Basically, prints an message like "not in Min .. Max", but adjusts the message
   -- if only one bound has been specified.

   procedure Help_On_Bounds (Header : Wide_String  := "");

   -----------------------------------------------------------------------------------
   -- Categories
   -----------------------------------------------------------------------------------

   type Categories is (Cat_Any,
                       Cat_Enum,  Cat_Range,   Cat_Mod,    Cat_Delta,     Cat_Digits,
                       Cat_Array, Cat_Record,  Cat_Tagged, Cat_Extension, Cat_Access,
                       Cat_New,   Cat_Private, Cat_Task,   Cat_Protected);
   subtype Discrete_Categories is Categories range Cat_Enum .. Cat_Mod;
   package Categories_Utilities is new Modifier_Utilities (Categories,
                                                           Prefix   => "CAT_",
                                                           Box_Pos  => 0,
                                                           Pars_Pos => 1);

   function Value (Spec : Entity_Specification) return Categories;
   -- If Spec kind is a Regular_Id that matches the image of a category (or "()"),
   -- return that category.
   -- Return Cat_Any otherwise.

   function Matches (Elem               : in Asis.Element;
                     Cat                : in Categories;
                     Follow_Derived     : in Boolean := False;
                     Privacy            : in Thick_Queries.Privacy_Policy := Thick_Queries.Stop_At_Private;
                     Separate_Extension : in Boolean := False)
                     return Boolean;
   -- Appropriate Element_Kinds for Elem:
   --       A_Declaration
   --       A_Definition
   --       An_Expression
   -- Appropriate Declaration_Kinds:
   --       An_Ordinary_Type_Declaration
   --       A_Task_Type_Declaration
   --       A_Protected_Type_Declaration
   --       A_Private_Type_Declaration
   --       A_Private_Extension_Declaration
   --       A_Subtype_Declaration
   --       A_Formal_Type_Declaration
   -- Appropriate Definition_Types:
   --       A_Type_Definition
   --       A_Task_Definition
   --       A_Protected_Definition

   function Matching_Category (Elem               : in Asis.Element;
                               From_Cats          : in Categories_Utilities.Unconstrained_Modifier_Set;
                               Follow_Derived     : in Boolean := False;
                               Privacy            : in Thick_Queries.Privacy_Policy := Thick_Queries.Stop_At_Private;
                               Separate_Extension : in Boolean := False)
                               return Categories;
   -- Appropriate Element_Kinds for Elem:
   -- Same as Matches above
   --
   -- Return the category of Elem it is in From_Cats, Cat_Any otherwise

   function Image (Item : Thick_Queries.Type_Categories) return Wide_String;
   -- Image of a type category, in upper case
end Framework.Language.Shared_Keys;
