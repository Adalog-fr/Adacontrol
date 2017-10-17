----------------------------------------------------------------------
--  Framework.Fixes - Package body                                  --
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
   Ada.Characters.Handling,
   Ada.Strings.Wide_Fixed,
   Ada.Wide_Text_IO;

-- ASIS
with
   Asis.Clauses,
   Asis.Compilation_Units,
   Asis.Elements;

-- Adalog
with
   Utilities;

-- AdaControl
with
   Framework.String_Set;

package body Framework.Fixes is
   use Asis.Text;

   Generate_Fixes : Boolean := False;
   Comments_Only  : Boolean;

   Patch_File : Ada.Wide_Text_IO.File_Type;
   Patch_Name : Unbounded_Wide_String;
   Seen_Files : Framework.String_Set.Set;

   Marker : constant Wide_Character := '!';

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
   procedure Gen_Replace (Extent : Span; From_File : Wide_String) is
      use Ada.Wide_Text_IO, Coord_IO;
      use Utilities;
   begin
      Comments_Only := False;

      Put (Patch_File, Quote (From_File));
      Put (Patch_File, ':');
      Put (Patch_File, Extent.First_Line);
      Put (Patch_File, ':');
      Put (Patch_File, Extent.First_Column);
      Put (Patch_File, ':');
      Put (Patch_File, "Replace:");
      Put (Patch_File, Extent.Last_Line);
      Put (Patch_File, ':');
      Put (Patch_File, Extent.Last_Column);
      New_Line (Patch_File);

      while Text_Generator.Has_More_Text loop
         Put (Patch_File, Marker);
         Put_Line (Patch_File, Text_Generator.Next_Line);
      end loop;
   end Gen_Replace;

   -----------------
   -- Gen_Replace --
   -----------------

   -- Like previous procedure, using provided text
   procedure Gen_Replace (Extent : Span ; From_File : Wide_String; By : Wide_String) is
      use Ada.Strings.Wide_Fixed, Ada.Wide_Text_IO, Coord_IO;
      use Utilities;
      Start : Natural := By'First;
      Stop  : Natural;
   begin
      Comments_Only := False;

      Put (Patch_File, Quote (From_File));
      Put (Patch_File, ':');
      Put (Patch_File, Extent.First_Line);
      Put (Patch_File, ':');
      Put (Patch_File, Extent.First_Column);
      Put (Patch_File, ':');
      Put (Patch_File, "Replace:");
      Put (Patch_File, Extent.Last_Line);
      Put (Patch_File, ':');
      Put (Patch_File, Extent.Last_Column);
      New_Line (Patch_File);

      if By = "" then
         Put (Patch_File, Marker);
         New_Line (Patch_File);
         return;
      end if;

      while Start <= By'Last loop
         Stop := Index (By, Pattern => Delimiter_Image, From => Start);
         exit when Stop = 0;

         Put (Patch_File, Marker);
         Put (Patch_File, By (Start .. Stop - 1));
         New_Line (Patch_File);
         Start := Stop + Delimiter_Image'Length;
      end loop;
      if Start <= By'Last then   -- By was not terminated by Delimiter_Image
         Put (Patch_File, Marker);
         Put_Line (Patch_File, By (Start .. By'Last));
      end if;
   end Gen_Replace;

   ----------------
   -- Gen_Insert --
   ----------------

   -- Breaks the current line at Pos, inserting Text in front of the new line
   -- (before the text that follows Pos)
   -- If End_Break, also add line break after Text.
   procedure Gen_Insert (Pos : Span; From_File : Wide_String; Text : Wide_String; End_Break : Boolean := False) is
      -- Breaks line
      use Ada.Wide_Text_IO, Coord_IO;
      use Utilities;
   begin
      Comments_Only := False;

      Put (Patch_File, Quote (From_File));
      Put (Patch_File, ':');
      Put (Patch_File, Pos.First_Line);
      Put (Patch_File, ':');
      Put (Patch_File, Pos.First_Column);
      Put (Patch_File, ':');
      if End_Break then
         Put (Patch_File, "Insert_Break");
      else
         Put (Patch_File, "Insert");
      end if;
      New_Line (Patch_File);

      Put (Patch_File, Marker);
      Put_Line (Patch_File, Text);
   end Gen_Insert;

   ----------------
   -- Gen_Delete --
   ----------------

   -- Delete the corresponding Span
   -- Formally equivalent to a Gen_Replace with "", but it is easier for optimizations of conflicts in the fixer
   -- to know that it is a deletion
   procedure Gen_Delete (Extent : Span ; From_File : Wide_String) is
      use Ada.Wide_Text_IO, Coord_IO;
      use Utilities;
   begin
      Comments_Only := False;

      Put (Patch_File, Quote (From_File));
      Put (Patch_File, ':');
      Put (Patch_File, Extent.First_Line);
      Put (Patch_File, ':');
      Put (Patch_File, Extent.First_Column);
      Put (Patch_File, ':');
      Put (Patch_File, "Delete:");
      Put (Patch_File, Extent.Last_Line);
      Put (Patch_File, ':');
      Put (Patch_File, Extent.Last_Column);
      New_Line (Patch_File);
   end Gen_Delete;

   ----------------------
   -- Source_File_Name --
   ----------------------

   function Source_File_Name (Elem : Asis.Element) return Wide_String is
      use Asis.Compilation_Units, Asis.Elements;
   begin
      return Text_Name (Enclosing_Compilation_Unit (Elem));
   end Source_File_Name;

   -------------------------------------------------------------------------------------------
   --  Exported services
   -------------------------------------------------------------------------------------------

   --------------------
   -- Set_Patch_File --
   --------------------

   procedure Set_Patch_File (File_Name : Wide_String) is
      use Ada.Characters.Handling, Ada.Wide_Text_IO;
      use Framework.String_Set;
   begin
      if Is_Open (Patch_File) then
         if Comments_Only then
            Delete (Patch_File);
            Delete (Seen_Files, To_Wide_String (Patch_Name));
         else
            Close (Patch_File);
         end if;
      end if;

      if File_Name = "" then
         Generate_Fixes := False;
         return;
      end if;

      begin
         Open (Patch_File, In_File, To_String (File_Name));

         -- File exists
         Close (Patch_File);
         if Is_Present (Seen_Files, File_Name) then
            Open (Patch_File, Append_File, To_String (File_Name));
            Comments_Only := False; -- File not previously deleted => we had non-comments fixes
         else
            Create (Patch_File, Out_File, To_String (File_Name));
            Comments_Only := True;
         end if;
      exception
         when Name_Error =>
            -- File does not exist
            Create (Patch_File, Out_File, To_String (File_Name));
            Comments_Only := True;
      end;

      Generate_Fixes := True;
      Add (Seen_Files, File_Name);
      Patch_Name := To_Unbounded_Wide_String (File_Name);
   end Set_Patch_File;


   -----------
   -- "and" --
   -----------

   function "and" (Left : Wide_String; Right : Wide_String) return Wide_String is
   begin
      return Left & Delimiter_Image & Right;
   end "and";


   -------------
   -- Message --
   -------------

   procedure Message (Text : Wide_String) is
      use Ada.Wide_Text_IO;
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      Put_Line (Patch_File, Text);
   end Message;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original : Asis.Element; By : Wide_String) is
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      Gen_Replace (Element_Span (Original), Source_File_Name (Original), By);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original : Asis.Element; By : Asis.Element) is
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      Text_Generator.Init (Element_Span (By), Original);
      Gen_Replace (Element_Span (Original), Source_File_Name (Original));
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (Original : Asis.Element; By : Asis.Element_List) is
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      Text_Generator.Init ((First_Line     => Element_Span (By (By'First)).First_Line,
                            First_Column   => Element_Span (By (By'First)).First_Column,
                            Last_Line      => Element_Span (By (By'Last)).Last_Line,
                            Last_Column    => Element_Span (By (By'Last)).Last_Column),
                           Base_Element => Original);
      Gen_Replace (Element_Span (Original), Source_File_Name (Original));
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (From : Location; Length : Positive; By : Wide_String) is
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      Gen_Replace ((First_Line   => From.First_Line,
                    First_Column => From.First_Column,
                    Last_Line    => From.First_Line,
                    Last_Column  => From.First_Column + Length - 1),
                   Get_File_Name (From),
                   By);
   end Replace;

   ------------
   -- Insert --
   ------------

   procedure Insert (Text : Wide_String;  Place : Insert_Place;  Elem : Asis.Element; Full_Line : Boolean := False) is
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      declare
         Place_Span : Span := Element_Span (Elem);
      begin
         if Full_Line then
            Place_Span.First_Column := 1;
            case Place is
               when Before =>
                  Gen_Insert (Place_Span, Source_File_Name (Elem), Text, End_Break => True);
               when After =>
                  Place_Span.First_Line := Place_Span.First_Line + 1;
                  Gen_Insert (Place_Span, Source_File_Name (Elem), Text, End_Break => True);
            end case;
         else
            case Place is
               when Before =>
                  Gen_Replace ((First_Line   => Place_Span.First_Line,
                                First_Column => Place_Span.First_Column,
                                Last_Line    => Place_Span.First_Line,
                                Last_Column  => Place_Span.First_Column - 1),
                               Source_File_Name (Elem),
                               Text);
               when After =>
                  Gen_Replace ((First_Line   => Place_Span.Last_Line,
                                First_Column => Place_Span.Last_Column + 1,
                                Last_Line    => Place_Span.Last_Line,
                                Last_Column  => Place_Span.Last_Column),
                               Source_File_Name (Elem),
                               Text);
            end case;
         end if;
      end;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (Text : Wide_String; From  : Location) is
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      Gen_Replace ((First_Line   => From.First_Line,
                    First_Column => From.First_Column,
                    Last_Line    => From.First_Line,
                    Last_Column  => From.First_Column - 1),
                   Get_File_Name (From),
                   Text);
   end Insert;

   -----------
   -- Break --
   -----------

   procedure Break (Place : Location; Indent_New : Asis.Text.Character_Position) is
      use Ada.Strings.Wide_Fixed;
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      Gen_Insert ((First_Line   | Last_Line   => Place.First_Line,
                   First_Column | Last_Column => Place.First_Column),
                  Get_File_Name (Place),
                  Indent_New * ' ');
   end Break;

   ------------
   -- Delete --
   ------------

   procedure Delete (Elem  : Asis.Element) is
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      Gen_Delete (Element_Span (Elem), Source_File_Name (Elem));
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (Elems : Asis.Element_List) is
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      declare
         Head_Span : constant Span := Element_Span (Elems (Elems'First));
         Tail_Span : constant Span := Element_Span (Elems (Elems'Last));
      begin
         Gen_Delete ((First_Line   => Head_Span.First_Line,
                      First_Column => Head_Span.First_Column,
                      Last_Line    => Tail_Span.Last_Line,
                      Last_Column  => Tail_Span.Last_Column),
                     Source_File_Name (Elems (Elems'First)));
      end;
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (From  : Location; To : Location) is
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      Gen_Delete ((First_Line   => From.First_Line,
                   First_Column => From.First_Column,
                   Last_Line    => To.First_Line,
                   Last_Column  => To.First_Column - 1),
                   Get_File_Name (From));
   end Delete;

   -----------------
   -- List_Remove --
   -----------------

   procedure List_Remove (Inx : Asis.List_Index; From : Asis.Element) is
      use Asis.Clauses, Asis.Compilation_Units, Asis.Elements;
   begin
      if not (Generate_Fixes and Report_Enabled) then
         return;
      end if;

      --  TBSL more clever removal if all elems removed
      declare
         Elements : constant Asis.Element_List := Clause_Names (From);
         S1, S2   : Span;
      begin
         if Elements'Length = 1 then
            -- Remove the whole clause
            Delete (From);
         elsif Inx /= Elements'Last then
            Delete (Get_Location (Elements (Inx)), Get_Location (Elements (Inx + 1)));
         else
            S1 := Element_Span (Elements (Inx - 1));
            S2 := Element_Span (From);  -- Last character of span of From is ';'
            Gen_Replace ((S1.Last_Line, S1.Last_Column + 1, S2.Last_Line, S2.Last_Column - 1),
                         Text_Name (Enclosing_Compilation_Unit (From)),
                         "");
         end if;
      end;
   end List_Remove;

   ------------
   -- Insert --
   ------------

   procedure Insert (Fix : in out Incremental_Fix; Text : Wide_String; Place : Insert_Place; Elem : Asis.Element) is
      use Fix_List;
      use Asis.Elements;

      Curs         : Cursor := First (Fix);
      Current      : Delayed_Fix;
      Current_Span : Span;
      Elem_Span    : constant Span := Element_Span (Elem);
      Line         : Line_Number_Positive;
      Col          : Character_Position_Positive;
   begin
      if not (Generate_Fixes and Report_Enabled) then
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

      Curs    : Cursor := First (Fix);
      Current : Delayed_Fix;
   begin
      if not (Generate_Fixes and Report_Enabled) then
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
end Framework.Fixes;
