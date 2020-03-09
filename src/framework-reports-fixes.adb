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
   Asis.Clauses,
   Asis.Elements;

-- Adalog
with
   A4G_Bugs,
   Thick_Queries;

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

   ------------------
   -- Gen_Refactor --
   ------------------

   procedure Gen_Refactor (From, To : Location) is
      use Utilities;
   begin
      Raw_Report (Image (From)
                  & ':'
                  & " Refactor:"
                  & ASIS_Integer_Img (Get_First_Line (To)) & ':' & ASIS_Integer_Img (Get_First_Column (To)));
   end Gen_Refactor;

   -----------------
   -- Gen_Replace --
   -----------------

   -- Like previous procedure, using provided text
   procedure Gen_Replace (From, To : Location; By : Wide_String) is
      use Ada.Strings.Wide_Fixed;
      use Utilities;
      Start : Natural := By'First;
      Stop  : Natural;
   begin
      Raw_Report (Image (From)
                  & ':'
                  & " Replace:"
                  & ASIS_Integer_Img (Get_First_Line (To)) & ':' & ASIS_Integer_Img (Get_First_Column (To)));

      if By = "" then
         Raw_Report (Marker);
         return;
      end if;

      while Start <= By'Last loop
         Stop := Index (By, Pattern => Line_Delimiter, From => Start);
         exit when Stop = 0;

         Raw_Report (Marker & By (Start .. Stop - 1));
         Start := Stop + Line_Delimiter'Length;
      end loop;
      if Start <= By'Last then   -- By was not terminated by Line_Delimiter
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
      use Ada.Strings.Wide_Fixed;
      -- Breaks line
      Inx : Natural := Text'First;
      Acc : Natural;
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

      while Inx <= Text'Last loop
         Acc := Index (Text (Inx .. Text'Last), Line_Delimiter);
         exit when Acc = 0;
         Raw_Report (Marker & Text (Inx .. Acc - 1));
         Inx := Acc + Line_Delimiter'Length;
      end loop;
      Raw_Report (Marker & Text (Inx .. Text'Last));

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
      use Utilities;
   begin
      Raw_Report (Image (From)
                  & ':'
                  & " Delete:"
                  & ASIS_Integer_Img (Get_First_Line (To)) & ':' & ASIS_Integer_Img (Get_First_Column (To)));
   end Gen_Delete;

   -------------------------------------------------------------------------------------------
   --  Exported services
   -------------------------------------------------------------------------------------------

   --------------------
   -- Indentation_Of --
   --------------------

   function Indentation_Of (Original : Asis.Element) return Wide_String is
      The_Lines : constant Line_List         := Lines (Original);
      Line_Text : constant Asis.Program_Text := Line_Image (The_Lines (The_Lines'First));
      Inx       : Positive := Line_Text'First;
   begin
      while Line_Text (Inx) <= ' ' loop
         Inx := Inx + 1;
      end loop;
      return Line_Text (Line_Text'First .. Inx - 1);
   end Indentation_Of;

   --------------
   -- Refactor --
   --------------

   procedure Refactor (Original : Asis.Element) is
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Refactor (From => Get_Location (Original), To => Get_End_Location (Original));
   end Refactor;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original : Asis.Element; By : Wide_String) is
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Replace (From => Get_Location (Original), To => Get_End_Location (Original), By => By);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original : Asis.Element_List; By : Wide_String) is
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Replace (From => Get_Location     (Original (Original'First)),
                   To   => Get_End_Location (Original (Original'Last)),
                   By   => By);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original   : Asis.Element;
                      By         : Asis.Element;
                      Add_Before : Wide_String := "";
                      Add_After  : Wide_String := "")
   is
   begin
      if not Generate_Fixes then
         return;
      end if;

      declare
         By_Image : constant Asis.Program_Text := Element_Image (By);
      begin
         Replace (Original,
                  By => Add_Before
                        & By_Image (A4G_Bugs.Element_Span (By).First_Column .. By_Image'Last)
                        & Add_After);
      end;
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original   : Asis.Element;
                      By         : Asis.Element_List;
                      Add_Before : Wide_String := "";
                      Add_After  : Wide_String := "")
   is
      use Thick_Queries;
   begin
      if not Generate_Fixes then
         return;
      end if;

      declare
         By_Image : constant Asis.Program_Text := Element_List_Image (By);
      begin
         Replace (Original,
                  By => Add_Before
                        & By_Image (A4G_Bugs.Element_Span (By (By'First)).First_Column .. By_Image'Last)
                        & Add_After);
      end;
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original   : Asis.Element_List;
                      By         : Asis.Element_List;
                      Add_Before : Wide_String := "";
                      Add_After  : Wide_String := "")
   is
      use Thick_Queries;
   begin
      if not Generate_Fixes then
         return;
      end if;

      declare
         By_Image : constant Asis.Program_Text := Element_List_Image (By);
      begin
         Replace (Original,
                  By => Add_Before
                        & By_Image (A4G_Bugs.Element_Span (By (By'First)).First_Column .. By_Image'Last)
                        & Add_After);
      end;
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (From : Location; Length : Positive; By : Wide_String) is
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
   begin
      if not Generate_Fixes then
         return;
      end if;

      declare
         Place_Span : Span := A4G_Bugs.Element_Span (Elem);
         Break      : Break_Addition;
      begin
         if Full_Line then
            Break := Both;
         else
            Break := None;
         end if;

         Place_Span.First_Column := 1;
         case Place is
            when Before =>
               Gen_Insert (Get_Location (Elem), Text, Add_Break => Break);
            when After =>
               Place_Span.First_Line   := Place_Span.Last_Line;
               Place_Span.First_Column := Place_Span.Last_Column + 1;
               Gen_Insert (Get_End_Location (Elem) + 1, Text, Add_Break => Break);
         end case;
      end;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (Text : Wide_String; From  : Location) is
   begin
      if not Generate_Fixes then
         return;
      end if;

      Gen_Insert (From, Text, Add_Break => None);
   end Insert;

   -----------
   -- Break --
   -----------

   procedure Break (Place : Location; Indent_New : Asis.Text.Character_Position := 0) is
      use Ada.Strings.Wide_Fixed;
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
      use Asis;
   begin
      if not Generate_Fixes then
         return;
      end if;

      if Elems = Nil_Element_List then
         return;
      end if;

      Gen_Delete (From => Get_Location     (Elems (Elems'First)),
                  To   => Get_End_Location (Elems (Elems'Last)));
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (From  : Location; To : Location) is
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
   begin
      if not Generate_Fixes then
         return;
      end if;

      declare
         Elements : constant Asis.Element_List := Clause_Names (From);
      begin
         if Elements'Length = 1 then
            -- Remove the whole clause
            Delete (From);
         elsif Inx = Elements'Last then
            -- Last character of span of From is ';'
            Delete (From => Get_End_Location (Elements (Inx - 1)) +1, To => Get_End_Location (From));
         else
            Delete (From => Get_Location (Elements (Inx)), To => Get_Location (Elements (Inx + 1)));
         end if;
      end;
   end List_Remove;

   -----------------
   -- List_Remove --
   -----------------

   procedure List_Remove (Name : Asis.Name) is
      use Asis.Clauses, Asis.Elements;
      use Utilities;
   begin
      if not Generate_Fixes then
         return;
      end if;

      declare
         From     : constant Asis.Element      := Enclosing_Element (Name);
         Elements : constant Asis.Element_List := Clause_Names (From);
      begin
         for E in Elements'Range loop
            if Is_Equal (Name, Elements (E)) then
               List_Remove (E, From);
               return;
            end if;
         end loop;

         -- not found
         Failure ("List_Remove: no found", Name);
      end;
   end List_Remove;

   ------------
   -- Insert --
   ------------

   procedure Insert (Fix       : in out Incremental_Fix;
                     Text      :        Wide_String;
                     Place     :        Insert_Place;
                     Elem      :        Asis.Element)

   is
      use Fix_List;

      Curs         : Cursor := First (Fix);
      Current_Span : Span;
      Elem_Span    : constant Span := A4G_Bugs.Element_Span (Elem);
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
         declare
            Current : Delayed_Fix := Fetch (Curs);
         begin
            if Current.Kind = Insert then
               Current_Span := A4G_Bugs.Element_Span (Current.Elem);
               if Current.Place = Place and then
                 (case Place is
                     when Before => Current_Span.First_Line   = Line
                                    and Current_Span.First_Column = Col,
                     when After  => Current_Span.Last_Line   = Line
                                    and Current_Span.Last_Column = Col)
               then
                  Append (Current.Text, Text);
                  Replace (Curs, Current);
                  return;
               end if;
            end if;
         end;
         Curs := Next (Curs);
      end loop;

      -- Not found
      -- Use Prepend rather than Append: since it will be generally used in a stack fashion, this will
      -- (slightly) reduce search time.
      Prepend (Fix, (Insert, Place, Elem, To_Unbounded_Wide_String (Text)));
   end Insert;

   -----------
   -- Break --
   -----------

   procedure Break  (Fix       : in out Incremental_Fix;
                     Place     :        Insert_Place;
                     Elem      :        Asis.Element)
   is
   begin
      if not Generate_Fixes then
         return;
      end if;

      Insert (Fix, Line_Delimiter, Place, Elem);
   end Break;

   -----------------
   -- List_Remove --
   -----------------

   procedure List_Remove (Fix  : in out Incremental_Fix;
                          Inx  : Asis.List_Index;
                          From : Asis.Element)
   is
      use Asis.Elements;
      use Fix_List;

      Curs : Cursor := First (Fix);
   begin
      if not Generate_Fixes then
         return;
      end if;

      -- Linear search, we expect only very short lists
      -- Keep the List_Remove fixes sorted by Inx, the first one holds the
      -- count of fixes.
      while Has_Element (Curs) loop
         declare
            Current : Delayed_Fix := Fetch (Curs);
            Temp    : Asis.List_Index;
            use Utilities;
         begin
            if Current.Kind = List_Remove then
               if not Is_Equal (From, Current.From) then
                  Failure ("List_Remove from different lists", From);
               end if;
               if Inx < Current.Inx then
                  Prepend (Fix, (List_Remove, From, Current.Count + 1, Inx));
               else
                  Temp        := Current.Inx;
                  Current.Inx := Inx;
                  Replace (Curs, Current);
                  Prepend (Fix, (List_Remove, From, Current.Count + 1, Temp));
               end if;
               return;
            end if;
         end;
         Curs := Next (Curs);
      end loop;

      -- Not found
      -- Use Prepend rather than Append: since we look-up the list, better find the fix early
      Prepend (Fix, (List_Remove, From, 1, Inx));
   end List_Remove;


   -----------
   -- Flush --
   -----------

   procedure Flush  (Fix : in out Incremental_Fix) is
      use Fix_List;
      use Asis.Clauses;

      Curs             : Cursor;
      Delete_Curs      : Cursor;
      Head_Deleted_Inx : Asis.ASIS_Natural := 0;
   begin
      if not Generate_Fixes then
         return;
      end if;

      Curs := First (Fix);
      while Has_Element (Curs) loop
         declare
            Current : constant Delayed_Fix  := Fetch (Curs);
         begin
            case Current.Kind is
               when Insert =>
                  Insert (To_Wide_String (Current.Text), Current.Place, Current.Elem);
               when List_Remove =>
                  declare
                     Names : constant Asis.Name_List := Clause_Names (Current.From);
                  begin
                     if Current.Count = Names'Length then
                        Delete (Current.From);

                        -- Remove other fixes for the same clause
                        Delete_Curs := Next (Curs);
                        while Has_Element (Delete_Curs) loop
                           if Fetch (Delete_Curs).Kind = List_Remove then
                              Replace (Delete_Curs, (Kind => Deleted));
                           end if;
                           Delete_Curs := Next (Delete_Curs);
                        end loop;
                     elsif Current.Inx = Head_Deleted_Inx + 1 then
                        -- Names has more than 1 element, otherwise it would have been caught above
                        -- Last character of span of From is ';'
                        -- There is at least one non deleted element, therefore it is safe to access the next element
                        Delete (From => Get_Location (Names (Current.Inx)),
                                To   => Get_Location (Names (Current.Inx + 1)));
                        Head_Deleted_Inx := Current.Inx;
                     else
                        Delete (From => Get_End_Location (Names (Current.Inx - 1)) +1,
                                To   => Get_End_Location (Names (Current.Inx))     +1);
                     end if;
                  end;
               when Deleted =>
                  null;
            end case;
         end;
         Curs := Next (Curs);
      end loop;
      Clear (Fix);
   end Flush;

begin  -- Framework.Reports.Fixes
   Coord_IO.Default_Width := 1;
end Framework.Reports.Fixes;
