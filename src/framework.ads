----------------------------------------------------------------------
--  Framework - Package specification                               --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2008.           --
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
  Ada.Characters.Wide_Latin_1,
  Ada.Strings.Wide_Maps;
private with
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
   Asis;

-- Adactl
with
  Adactl_Constants;
package Framework is
   use Adactl_Constants;

   -------------------------------------------------------------------
   -- Misc utility constants
   -------------------------------------------------------------------

   Wide_HT    : Wide_Character renames Ada.Characters.Wide_Latin_1.HT;
   Tab_Chars  : constant Ada.Strings.Wide_Maps.Wide_Character_Set := Ada.Strings.Wide_Maps.To_Set
                                                                      (Wide_String'(Ada.Characters.Wide_Latin_1.HT,
                                                                                    Ada.Characters.Wide_Latin_1.VT,
                                                                                    Ada.Characters.Wide_Latin_1.FF));
   Separators : constant Ada.Strings.Wide_Maps.Wide_Character_Set := Ada.Strings.Wide_Maps.To_Set
                                                                      (Wide_String'(' ',
                                                                                    Ada.Characters.Wide_Latin_1.HT,
                                                                                    Ada.Characters.Wide_Latin_1.VT,
                                                                                    Ada.Characters.Wide_Latin_1.FF));


   -------------------------------------------------------------------
   -- The ASIS context                                              --
   -------------------------------------------------------------------

   Adactl_Context : aliased Asis.Context;

   -------------------------------------------------------------------
   --  General types for rules                                      --
   -------------------------------------------------------------------

   type Control_Index is range 0 ..  Max_Controls_For_Rule;
   type Control_Index_Set is array (Control_Index range 1 .. Max_Controls_Set) of Boolean; -- Purposedly limited
   pragma Pack (Control_Index_Set);
   Empty_Control_Index_Set : constant Control_Index_Set := (others => False);

   type Extended_Control_Kinds is (None, Check, Search, Count);
   subtype Control_Kinds is Extended_Control_Kinds range Check .. Count;
   subtype Fixes_Kinds   is Extended_Control_Kinds range None .. Search;

   type Control_Kinds_Set is array (Control_Kinds) of Boolean;
   pragma Pack (Control_Kinds_Set);
   Empty_Control_Kinds_Set : constant Control_Kinds_Set := (others => False);

   type Uncheckable_Kinds is (False_Positive, False_Negative, Missing_Unit);
   subtype Uncheckable_Consequence is Uncheckable_Kinds range False_Positive .. False_Negative;

   type Matching_Extension is (Instance, Renaming);
   type Extension_Set is array (Matching_Extension) of Boolean;
   No_Extension   : constant Extension_Set := (others => False);
   All_Extensions : constant Extension_Set := (others => True);

   Short_Name : Boolean := False;

   -------------------------------------------------------------------
   --  Entity_Specification                                         --
   -------------------------------------------------------------------

   -- An Entity_Specification is the structure that corresponds to
   -- the specification of an Ada entity in the command language

   type Entity_Specification is private;
   type Entity_Specification_List is array (Asis.List_Index range <>) of Entity_Specification;

   type Entity_Specification_Kinds is (Box, Equal, Regular_Id, All_Id);
   type Entity_Specification_Kinds_Set is array (Entity_Specification_Kinds) of Boolean;
   Nothing_OK : constant Entity_Specification_Kinds_Set := (                    others => False);
   Box_OK     : constant Entity_Specification_Kinds_Set := (Box        => True, others => False);
   Equal_OK   : constant Entity_Specification_Kinds_Set := (Equal      => True, others => False);
   Parens_OK  : constant Entity_Specification_Kinds_Set := (Regular_Id => True, others => False);
   All_OK     : constant Entity_Specification_Kinds_Set := (others     => True                 );

   function Entity_Specification_Kind (Entity : in Entity_Specification) return Entity_Specification_Kinds;

   function Image   (Entity : in Entity_Specification) return Wide_String;
   function Value   (Name   : in Wide_String)          return Entity_Specification;
   -- (pseudo) entity specification corresponding to a string
   -- Name can be in any case

   function Matches (Entity    : in Entity_Specification;
                     Name      : in Asis.Element;
                     Extend_To : in Extension_Set := No_Extension) return Boolean;
   -- Appropriate element kinds for Matches:
   --   like Matching_Context, see Framework.Control_Manager

private
   use Ada.Strings.Wide_Unbounded;

   --
   -- Entity_Specification
   --

   type Entity_Specification (Kind : Entity_Specification_Kinds := Regular_Id) is
      record
         case Kind is
            when Box | Equal =>
               null;
            when Regular_Id | All_Id =>
               Specification : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         end case;
      end record;

end Framework;
