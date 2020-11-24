----------------------------------------------------------------------
--  Framework.Locations - Package specification                     --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2019.           --
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
  Asis,
  Asis.Text;

package Framework.Locations is

   -- A location is the position of an element in a file

   type Location is private;
   Null_Location : constant Location;
   function "+" (Left : Location; Right : Asis.Text.Character_Position) return Location;
   -- Moves location by Right characters to the right
   function "-" (Left : Location; Right : Asis.Text.Character_Position) return Location;
   -- Moves location by Right characters to the left
   function "<=" (Left, Right : Location) return Boolean;
   -- with Pre => Get_File_Name (Left) = Get_File_Name (Right)
   -- Check if Left is declared before Right in the same file
   function No_Indent (L : Location; Elem : Asis.Element) return Location;
   -- Returns the location of the beginning of the line that contains L if there are only spaces
   --   to the left of L.
   -- Returns L otherwise
   -- Elem is any element from the same compilation unit as L.

   type Search_Start is (From_Head, From_Tail);

   function Create_Location (File         : in Wide_String;
                             First_Line   : in Asis.Text.Line_Number;
                             First_Column : in Asis.Text.Character_Position) return Location;
   function Get_Location (E : in Asis.Element)      return Location;
   function Get_Location (E : in Asis.Element_List) return Location;
   -- Returns location of an element or first element of a list

   function Get_End_Location (E : in Asis.Element)      return Location;
   function Get_End_Location (E : in Asis.Element_List) return Location;
   -- Returns location of end of an element or last element of a list

   function Get_Next_Token_Location (E : in Asis.Element) return Location;
   -- Returns the location of the first token (not space or comment) that immediately follows E

   function Get_Previous_Word_Location (E        : in Asis.Element;
                                        Matching : in Wide_String  := "";
                                        Starting : in Search_Start := From_Head;
                                        Skipping : in Natural      := 0)
                                        return Location;
   -- Returns the location of the first "word" (identifier of keyword) that immediately
   -- precedes the beginning of E (if Starting = From_Head) or the end of E (if Starting = From_Tail).
   -- after skipping Skipping words.
   -- If Matching is specified, returns the location of the first word identical to Matching
   -- Matching must be given in upper-case

   function Get_Previous_Word_Location (L        : in Asis.Element_List;
                                        Matching : in Wide_String := "";
                                        Starting : in Search_Start := From_Head)
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
                                    Matching : in Wide_String  := "";
                                    Starting : in Search_Start := From_Tail)
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

   function Is_Long_File_Name return Boolean;
   -- Returns True if Long_File_Name variable is set to On, False otherwise

private

   type Location is record
      File_Name    : Unbounded_Wide_String;
      First_Line   : Asis.Text.Line_Number        := 0;
      First_Column : Asis.Text.Character_Position := 0;
   end record;
   Null_Location : constant Location := (Null_Unbounded_Wide_String, 0, 0);
end Framework.Locations;
