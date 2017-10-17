----------------------------------------------------------------------
--  Framework - Package specification                               --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2008.         --
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
private with
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis,
  Asis.Text;

-- Adactl
with
  Adactl_Constants;
package Framework is
   use Adactl_Constants;

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

   type Control_Kinds is (Check, Search, Count);
   type Control_Kinds_Set is array (Control_Kinds) of Boolean;
   pragma Pack (Control_Kinds_Set);
   Empty_Control_Kinds_Set : constant Control_Kinds_Set := (others => False);

   type Uncheckable_Kinds is (False_Positive, False_Negative, Missing_Unit);
   subtype Uncheckable_Consequence is Uncheckable_Kinds range False_Positive .. False_Negative;

   type Matching_Extension is (Instance, Renaming);
   type Extension_Set is array (Matching_Extension) of Boolean;
   No_Extension   : constant Extension_Set := (others => False);
   All_Extensions : constant Extension_Set := (others => True);

   -------------------------------------------------------------------
   --  Location                                                     --
   -------------------------------------------------------------------

   -- A location is the position of an element in a file

   type Location is private;
   Null_Location : constant Location;
   function "+" (Left : Location; Right : Asis.Text.Character_Position) return Location;
   -- Moves location by Right characters to the right
   function "-" (Left : Location; Right : Asis.Text.Character_Position) return Location;
   -- Moves location by Right characters to the left

   type Search_Start is (From_Head, From_Tail);

   function Create_Location (File         : in Wide_String;
                             First_Line   : in Asis.Text.Line_Number;
                             First_Column : in Asis.Text.Character_Position) return Location;
   function Get_Location (E : in Asis.Element) return Location;
   -- Returns location of an element

   function Get_End_Location (E : in Asis.Element) return Location;
   -- Returns location of end of an element

   function Get_Previous_Word_Location (E        : in Asis.Element;
                                        Matching : Wide_String := "";
                                        Starting : Search_Start := From_Head)
                                        return Location;
   -- Returns the location of the first "word" (identifier of keyword) that immediately
   -- precedes the beginning of E (if Starting = From_Head) or the end of E (if Starting = From_Tail).
   -- If Matching is specified, returns the location of the first word identical to Matching
   -- Matching must be given in upper-case

   function Get_Previous_Word_Location (L        : in Asis.Element_List;
                                        Matching : Wide_String := "";
                                        Starting : Search_Start := From_Head)
                                        return Location;
   -- Returns the location of the first "word" (identifier of keyword) that immediately
   -- precedes the beginning of the first element of E (if Starting = From_Head)
   -- or the end of the last element of E (if Starting = From_Tail).
   -- If Matching is specified, returns the location of the first word identical to Matching
   -- Matching must be given in upper-case

   function Get_Next_Word_Location (E        : in Asis.Element;
                                    Matching : in Wide_String  := "";
                                    Starting : in Search_Start := From_Tail;
                                    Skipping : in Natural      := 0)
                                    return Location;
   -- Returns the location of the first "word" (identifier of keyword) that immediately
   -- follows the beginning of E (if Starting = From_Head) or the end of E (if Starting = From_Tail),
   -- after skipping Skipping words.
   -- If Matching is specified, returns the location of the first word identical to Matching
   -- Matching must be given in upper-case

   function Get_Next_Word_Location (L        : in Asis.Element_List;
                                    Matching : Wide_String := "";
                                    Starting : Search_Start := From_Tail)
                                    return Location;
   -- Returns the location of the first "word" (identifier of keyword) that immediately
   -- follows the beginning of the first element of E (if Starting = From_Head)
   -- or the end of the last element of E (if Starting = From_Tail).
   -- If Matching is specified, returns the location of the first word identical to Matching
   -- Matching must be given in upper-case
   -- Precondition: L is not empty

   function Get_File_Name (L : in Location) return Wide_String;
   -- Returns location file name

   function Get_First_Line (L : in Location) return Asis.Text.Line_Number;
   -- Returns location first line

   function Get_First_Column (L : in Location) return Asis.Text.Character_Position;
   -- Returns location first column

   Short_Name : Boolean := False;
   function Image (L          : in Location;
                   Separator  : in Wide_Character := ':';
                   Quoted     : in Boolean        := False)
                   return Wide_String;
   -- Returns image of a location
   -- i.e. file:1:1
   -- If Short_Name = True, strip File name from any path
   -- Separator: character used to separate file name, lines and cols
   -- Fails if L = Null_Location

   function Safe_Image (L          : in Location;
                        Separator  : in Wide_Character := ':')
                        return Wide_String;
   -- Like Image, but returns the string "unknown location" if L = Null_Location


   function Value (S : in Wide_String) return Location;
   -- Returns location value of a string
   -- raises Constraint_Error for an incorrect input string


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
   Regular_OK : constant Entity_Specification_Kinds_Set := (Regular_Id => True, others => False);
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
   -- Location
   --

   type Location is record
      File_Name    : Unbounded_Wide_String;
      First_Line   : Asis.Text.Line_Number        := 0;
      First_Column : Asis.Text.Character_Position := 0;
   end record;
   Null_Location : constant Location := (Null_Unbounded_Wide_String, 0, 0);


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

   --
   -- State of Enabling/Disabling messages
   --
   Report_Enabled : Boolean;

end Framework;
