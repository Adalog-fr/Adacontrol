----------------------------------------------------------------------
--  Framework.Reports.Fixes - Package body                          --
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
with
   Ada.Strings.Wide_Fixed,
   Ada.Wide_Text_IO;

-- ASIS
with
   Asis.Clauses;

-- AdaControl
with
   Utilities;

package body Framework.Reports.Fixes is
   use Asis.Text;

   Marker : constant Wide_String := "!";

   package Coord_IO is new Ada.Wide_Text_IO.Integer_IO (Asis.ASIS_Natural);

   -------------------------------------------------------------------------------------------
   --  Low level production
   -------------------------------------------------------------------------------------------

   --------------------
   -- Text_Generator --
   --------------------

   package Text_Generator is
   -- A generator for the text (possibly encompassing several lines) corresponding to a span
   -- Next_Line returns only text within Span (unlike functions is Asis.Text, no extra spaces is added before the start)
   -- If the ending column is beyond the end of the last line, it is extended with spaces.
      procedure Init (The_Span : Span; Base_Element : Asis.Element);

      function Has_More_Text return Boolean;
      function Next_Line     return Wide_String;
   end Text_Generator;

   package body Text_Generator is
      Active_Span    : Span;
      Active_Element : Asis.Element;

      procedure Init (The_Span : Span; Base_Element : Asis.Element) is
      begin
         Active_Span    := The_Span;
         Active_Element := Base_Element;
      end Init;

      function Has_More_Text return Boolean is
      begin
         return not Is_Nil (Active_Span) ;
      end Has_More_Text;

      function Next_Line return Wide_String is
      begin
         if not Has_More_Text  then
            return "";
         end if;

         declare
            Result : constant Asis.Program_Text := Line_Image (Lines (Active_Element,
                                                                      Active_Span.First_Line,
                                                               Active_Span.First_Line) (Active_Span.First_Line));
            First  : Character_Position;
            Last   : Character_Position;
         begin
            First := Active_Span.First_Column;
            if Active_Span.First_Line = Active_Span.Last_Line then
               Last := Active_Span.Last_Column;
            else
               Last := Result'Last;
            end if;

            Active_Span.First_Line   := Active_Span.First_Line + 1;
            Active_Span.First_Column := 1;
            if Active_Span.First_Line > Active_Span.Last_Line then
               Active_Span := Nil_Span;  -- safety for Is_Nil
            end if;

            if Last <= Result'Last then
               return Result (First .. Last);
            else
               return Result (First .. Result'Last) & (Result'Last+1 .. Last => ' ');
            end if;
         end;
      end Next_Line;
   end Text_Generator;

   -----------------
   -- Gen_Replace --
   -----------------

   -- Replace text covered by Extent (which can be empty for simple insertions, or cover
   -- several lines) by text fetched from Text_Generator
   procedure Gen_Replace (From, To : Location) is
      use Reports, Utilities;
   begin
      Raw_Report (Image (From)
                  & ':'
                  & " Replace:"
                  & ASIS_Integer_Img (To.First_Line) & ':' & ASIS_Integer_Img(To.First_Column));

      while Text_Generator.Has_More_Text loop
         Raw_Report (Marker & Text_Generator.Next_Line);
      end loop;
   end Gen_Replace;

   -----------------
   -- Gen_Replace --
   -----------------

   -- Like previous procedure, using provided text
   procedure Gen_Replace (From, To : Location; By : Wide_String) is
      use Ada.Strings.Wide_Fixed;
      use Reports, Utilities;
      Start : Natural := By'First;
      Stop  : Natural;
   begin
      Raw_Report (Image (From)
                  & ':'
                  & " Replace:"
                  & ASIS_Integer_Img (To.First_Line) & ':' & ASIS_Integer_Img (To.First_Column));

      if By = "" then
         Raw_Report (Marker);
         return;
      end if;

      while Start <= By'Last loop
         Stop := Index (By, Pattern => Delimiter_Image, From => Start);
         exit when Stop = 0;

         Raw_Report (Marker & By (Start .. Stop - 1));
         Start := Stop + Delimiter_Image'Length;
      end loop;
      if Start <= By'Last then   -- By was not terminated by Delimiter_Image
         Raw_Report (Marker & By (Start .. By'Last));
      end if;
   end Gen_Replace;

   ----------------
   -- Gen_Insert --
   ----------------

   -- Breaks the current line at Pos, inserting Text in front of the new line
   -- (before the text that follows Pos)
   -- If End_Break, also add line break after Text.
   type Break_Addition is (None, Before, After, Both);
   procedure Gen_Insert (Pos : Location; Text : Wide_String; Add_Break : Break_Addition := None) is
      -- Breaks line
      use Reports;
   begin
      Raw_Report (Image (Pos)
                  & ':'
                  & " Insert");

      case Add_Break is
         when Before | Both =>
            Raw_Report (Marker);
         when None | After =>
            null;
      end case;
      Raw_Report (Marker & Text);
      case Add_Break is
         when After | Both =>
            Raw_Report (Marker);
         when None | Before =>
            null;
      end case;
   end Gen_Insert;

   ----------------
   -- Gen_Delete --
   ----------------

   -- Delete the corresponding Span
   -- Formally equivalent to a Gen_Replace with "", but it is easier for optimizations of conflicts in the fixer
   -- to know that it is a deletion
   procedure Gen_Delete (From, To : Location) is
      use Reports, Utilities;
   begin
      Raw_Report (Image (From)
                  & ':'
                  & " Delete:"
                  & ASIS_Integer_Img (To.First_Line) & ':' & ASIS_Integer_Img (To.First_Column));
   end Gen_Delete;

   -------------------------------------------------------------------------------------------
   --  Exported services
   -------------------------------------------------------------------------------------------

   -------------
   -- Replace --
   -------------

   procedure Replace (Original : Asis.Element; By : Wide_String) is
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Replace (From => Get_Location (Original), To => Get_End_Location (Original), By => By);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original : Asis.Element; By : Asis.Element) is
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Text_Generator.Init (Element_Span (By), Original);
      Gen_Replace (From => Get_Location (Original), To => Get_End_Location (Original));
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original : Asis.Element; By : Asis.Element_List) is
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Text_Generator.Init ((First_Line     => Element_Span (By (By'First)).First_Line,
                            First_Column   => Element_Span (By (By'First)).First_Column,
                            Last_Line      => Element_Span (By (By'Last)).Last_Line,
                            Last_Column    => Element_Span (By (By'Last)).Last_Column),
                           Base_Element => Original);
      Gen_Replace (From => Get_Location (Original), To => Get_End_Location (Original));
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (From : Location; Length : Positive; By : Wide_String) is
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Replace (From, From + Length - 1, By);
   end Replace;

   ------------
   -- Insert --
   ------------

   procedure Insert (Text : Wide_String;  Place : Insert_Place;  Elem : Asis.Element; Full_Line : Boolean := False) is
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      declare
         Place_Span : Span := Element_Span (Elem);
      begin
         if Full_Line then
            Place_Span.First_Column := 1;
            case Place is
               when Before =>
                  Gen_Insert (Get_Location (Elem), Text, Add_Break => Both);
               when After =>
                  Gen_Insert (Get_Location (Elem) + 1, Text, Add_Break => Both);
            end case;
         else
            case Place is
               when Before =>
                  Gen_Insert (Get_Location (Elem), Text, Add_Break => None);
               when After =>
                  Place_Span.First_Line   := Place_Span.Last_Line;
                  Place_Span.First_Column := Place_Span.Last_Column+1;
                  Gen_Insert (Get_End_Location (Elem) + 1, Text, Add_Break => None);
            end case;
         end if;
      end;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (Text : Wide_String; From  : Location) is
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Insert (From, Text, Add_Break => None);
   end Insert;

   -----------
   -- Break --
   -----------

   procedure Break (Place : Location; Indent_New : Asis.Text.Character_Position) is
      use Ada.Strings.Wide_Fixed;
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Insert (Place, Indent_New * ' ', Add_Break => Before);
   end Break;

   ------------
   -- Delete --
   ------------

   procedure Delete (Elem  : Asis.Element) is
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Delete (From => Get_Location     (Elem),
                  To   => Get_End_Location (Elem));
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (Elems : Asis.Element_List) is
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Delete (From => Get_Location     (Elems (Elems'First)),
                  To   => Get_End_Location (Elems (Elems'Last)));
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (From  : Location; To : Location) is
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Delete (From, To - 1);
   end Delete;

   -----------------
   -- List_Remove --
   -----------------

   procedure List_Remove (Inx : Asis.List_Index; From : Asis.Element) is
      use Asis.Clauses;
      use Framework.Reports;
   begin
      if not Generate_Fixes then
         return;
      end if;

      --  TBSL more clever removal if all elems removed
      declare
         Elements : constant Asis.Element_List := Clause_Names (From);
      begin
         if Elements'Length = 1 then
            -- Remove the whole clause
            Delete (From);
         elsif Inx /= Elements'Last then
            Delete (Get_Location (Elements (Inx)), Get_Location (Elements (Inx + 1)));
         else
            -- Last character of span of From is ';'
            Delete (From => Get_End_Location (Elements (Inx - 1)) +1, To => Get_End_Location (From));
         end if;
      end;
   end List_Remove;

   ------------
   -- Insert --
   ------------

   procedure Insert (Fix : in out Incremental_Fix; Text : Wide_String; Place : Insert_Place; Elem : Asis.Element) is
      use Fix_List;
      use Framework.Reports;

      Curs         : Cursor := First (Fix);
      Current      : Delayed_Fix;
      Current_Span : Span;
      Elem_Span    : constant Span := Element_Span (Elem);
      Line         : Line_Number_Positive;
      Col          : Character_Position_Positive;
   begin
      if not Generate_Fixes then
         return;
      end if;

      case Place is
         when Before =>
            Line := Elem_Span.First_Line;
            Col  := Elem_Span.First_Column;
         when After =>
            Line := Elem_Span.Last_Line;
            Col  := Elem_Span.Last_Column;
      end case;

      -- Linear search, we expect only very short lists
      while Has_Element (Curs) loop
         Current := Fetch (Curs);
         Current_Span := Element_Span (Current.Elem);
         if Current.Place = Place
           and Current_Span.First_Line = Line
           and Current_Span.First_Column = Col
         then
            Append (Current.Text, Text);
            Replace (Curs, Current);
            return;
         end if;
         Curs := Next (Curs);
      end loop;

      -- Not found
      -- Use Prepend rather than Append: since it will be generally used in a stack fashion, this will
      -- (slightly) reduce search time.
      Prepend (Fix, (Place, Elem, To_Unbounded_Wide_String (Text)));
   end Insert;

   -----------
   -- Flush --
   -----------

   procedure Flush  (Fix : in out Incremental_Fix) is
      use Fix_List;
      use Framework.Reports;

      Curs    : Cursor := First (Fix);
      Current : Delayed_Fix;
   begin
      if not Generate_Fixes then
         return;
      end if;

      while Has_Element (Curs) loop
         Current := Fetch (Curs);
         Insert (To_Wide_String (Current.Text), Current.Place, Current.Elem);
         Curs := Next (Curs);
      end loop;
      Clear (Fix);
   end Flush;

begin  -- Framework.Fixes
   Coord_IO.Default_Width := 1;
end Framework.Reports.Fixes;
