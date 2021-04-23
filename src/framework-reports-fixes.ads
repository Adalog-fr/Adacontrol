----------------------------------------------------------------------
--  Framework.Reports.Fixes - Package specification                 --
--                                                                  --
--  This software is (c) Adalog 2004-2016.                          --
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

-- Asis
with
   Asis,
   Asis.Text;

-- Adalog
private with
   Linear_Queue;

package Framework.Reports.Fixes is

   -- This package provides the facilities for generating fixes.
   -- Since a fix corresponds to a message, these services must be called after the corresponding call to Report
   -- for Check or Search, but not after a call to Report for Count

   Line_Delimiter : constant Wide_String; -- This string causes a line break in inserted text

   function Indentation_Of (Original : Asis.Element) return Wide_String;
   -- Leading part of the line that contains Original, up to the first non-blank character
   -- Useful to insert a line with the same indentation as something else
   -- Returning the part of the string allows to preserve possibe control characters.

   procedure Refactor (Original : Asis.Element);
   -- Activates the "refactor" menu of GPS. To be used, f.e., when the name of an element is changed

   procedure Replace (Original : Asis.Element;
                      By       : Wide_String);
   procedure Replace (Original : Asis.Element_List;
                      By       : Wide_String);

   procedure Replace (Original   : Asis.Element;
                      By         : Asis.Element;
                      Add_Before : Wide_String := "";
                      Add_After  : Wide_String := "");
   procedure Replace (Original   : Asis.Element;
                      By         : Asis.Element_List;
                      Add_Before : Wide_String := "";
                      Add_After  : Wide_String := "");
   procedure Replace (Original   : Asis.Element_List;
                      By         : Asis.Element_List;
                      Add_Before : Wide_String := "";
                      Add_After  : Wide_String := "");

   procedure Replace (From   : Location;
                      Length : Positive;
                      By     : Wide_String);

   type Insert_Place is (Before, After);
   procedure Insert (Text : Wide_String; Place : Insert_Place; Elem : Asis.Element; Full_Line : Boolean := False);
   procedure Insert (Text : Wide_String; From  : Location);
   -- Insert at From characters (can be outside original line, spaces added as needed)

   procedure Break (Place : Location; Indent_New : Asis.Text.Character_Position := 0);
   -- Inserts a line break

   procedure Delete (Elem  : Asis.Element);
   procedure Delete (Elems : Asis.Element_List);
   procedure Delete (From  : Location; To : Location);
   -- From included, To excluded
   -- Note: To is excluded so that a deletion up to an element does not have to search the preceding element,
   --       which could be on a previous line.

   procedure List_Remove (Inx : Asis.List_Index; From : Asis.Element);
   -- From is expected to be an element accepting a list of names (like a with clause
   -- or a use clause);
   -- Inx is the index of the element to be removed from From
   -- The element is removed from From; if it is the only element in From, then From
   -- is deleted in whole.
   -- See List_Remove with an incremental fix below for the case of successive List_Remove from
   -- a same clause
   procedure List_Remove (Name : Asis.Name);
   -- Like above, but Name is searched in the Enclosing_Element to find its index


   --------------------------------------------------------------------------------
   -- Incremental fixes
   -- An Incremental_Fix accumulates (in order) several fixes.
   -- The fix is emitted (and the Incremental_Fix is reset) by calling Flush
   --------------------------------------------------------------------------------

   type Incremental_Fix is private;
   procedure Insert (Fix       : in out Incremental_Fix;
                     Text      :        Wide_String;
                     Place     :        Insert_Place;
                     Elem      :        Asis.Element);
   -- Like Insert, but several fixes at the same place are merged (in the order they are given)
   procedure Break  (Fix       : in out Incremental_Fix;
                     Place     :        Insert_Place;
                     Elem      :        Asis.Element);
   procedure List_Remove (Fix  : in out Incremental_Fix;
                          Inx  : Asis.List_Index;
                          From : Asis.Element);
   -- If all elements are removed from the clause, Flush will remove the whole clause
   -- All List_Remove in the same incremental fix must be from the same clause.
   procedure Flush  (Fix : in out Incremental_Fix);

private
   Line_Delimiter : constant Wide_String := Asis.Text.Delimiter_Image;

   type Delayed_Fix_Kind is (Insert, List_Remove, Deleted);
   type Delayed_Fix (Kind : Delayed_Fix_Kind) is
      record
         case Kind is
            when Insert =>
               Place : Insert_Place;
               Elem  : Asis.Element;
               Text  : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
            when List_Remove =>
               From  : Asis.Element;
               Count : Asis.List_Index;
               Inx   : Asis.List_Index;
            when Deleted =>
               null;
         end case;
      end record;

   package Fix_List is new Linear_Queue (Delayed_Fix);
   type Incremental_Fix is new Fix_List.Queue;
end Framework.Reports.Fixes;
