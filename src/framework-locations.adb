----------------------------------------------------------------------
--  Framework.Locations - Package body                              --
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

-- Ada
with
  Ada.Directories,
  Ada.Characters.Handling,
  Ada.Strings,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis.Compilation_Units,
  Asis.Elements;

-- Adalog
with
  A4G_Bugs,
  Utilities;

-- AdaControl
with
  Framework.Variables.Shared_Types;

package body Framework.Locations is

   use Framework.Variables.Shared_Types;

   Long_File_Name : aliased Switch_Type.Object := (Value => Off);

   ---------
   -- "+" --
   ---------

   function "+" (Left : Location; Right : Asis.Text.Character_Position) return Location is
   begin
      return (Left.File_Name, Left.First_Line, Left.First_Column + Right);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Location; Right : Asis.Text.Character_Position) return Location is
   begin
      return (Left.File_Name, Left.First_Line, Left.First_Column - Right);
   end "-";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Location) return Boolean is
      use Utilities;
   begin
      if Left.File_Name /= Right.File_Name then
         Failure ("Compare locations from different files");
      end if;

      return Left.First_Line < Right.First_Line or else
        (Left.First_Line = Right.First_Line and Left.First_Column <= Right.First_Column);
   end "<=";

   ---------------
   -- No_Indent --
   ---------------

   function No_Indent (L : Location; Elem : Asis.Element) return Location is
      use Asis.Text;
      Source_Line : constant Asis.Program_Text := Line_Image (Lines (Elem, L.First_Line, L.First_Line)(L.First_Line));
   begin
      for C : Wide_Character of reverse Source_Line (1 .. L.First_Column - 1) loop
         if C > ' ' then
            return L;
         end if;
      end loop;
      return (File_Name => L.File_Name, First_Line => L.First_Line, First_Column => 1);
   end No_Indent;

   ---------------------
   -- Create_Location --
   ---------------------

   function Create_Location (File         : in Wide_String;
                             First_Line   : in Asis.Text.Line_Number;
                             First_Column : in Asis.Text.Character_Position) return Location
   is
   begin
      return (To_Unbounded_Wide_String (File), First_Line, First_Column);
   end Create_Location;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (E : in Asis.Element) return Location is
      use Asis.Compilation_Units, Asis.Elements, Asis.Text;

      S : constant Span := A4G_Bugs.Element_Span (E);
   begin
      if S = Nil_Span then
         return Null_Location;
      end if;

      return (To_Unbounded_Wide_String (Text_Name (Enclosing_Compilation_Unit (E))),
              S.First_Line,
              S.First_Column);
   end Get_Location;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (E : in Asis.Element_List) return Location is
   begin
      return Get_Location (E (E'First));
   end Get_Location;

   ----------------------
   -- Get_End_Location --
   ----------------------

   function Get_End_Location (E : in Asis.Element) return Location is
      use Asis.Compilation_Units, Asis.Elements, Asis.Text;

      S : constant Span := A4G_Bugs.Element_Span (E);
   begin
      if S = Nil_Span then
         return Null_Location;
      end if;

      return (To_Unbounded_Wide_String (Text_Name (Enclosing_Compilation_Unit (E))),
              S.Last_Line,
              S.Last_Column);
   end Get_End_Location;

   ----------------------
   -- Get_End_Location --
   ----------------------

   function Get_End_Location (E : in Asis.Element_List) return Location is
   begin
      return Get_End_Location (E (E'Last));
   end Get_End_Location;

   -----------------------------
   -- Get_Next_Token_Location --
   -----------------------------

   function Get_Next_Token_Location (E : in Asis.Element) return Location is
      use Ada.Strings, Ada.Strings.Wide_Fixed;
      use Asis.Text;
      Result : Location := Get_End_Location (E);
      L      : Line_Number := Result.First_Line;
      C      : Character_Position;
      Last   : Character_Position;
   begin
      Find_Token (Non_Comment_Image (Lines (E, L, L) (L)),
                  Separators,
                  From  => Result.First_Column + 1,
                  Test  => Outside,
                  First => C,
                  Last  => Last);
      while Last = 0 loop
         L := L + 1;
         Find_Token (Non_Comment_Image (Lines (E, L, L) (L)),
                     Separators,
                     Test  => Outside,
                     First => C,
                     Last  => Last);
      end loop;

      Result.First_Line   := L;
      Result.First_Column := C;
      return Result;
   end Get_Next_Token_Location;

   --------------------------------
   -- Get_Previous_Word_Location --
   --------------------------------

   Identifier_Chars : constant Ada.Strings.Wide_Maps.Wide_Character_Set
     := Ada.Strings.Wide_Maps.To_Set (Ranges => (('a', 'z'), ('A', 'Z'), ('0', '9'), ('_', '_')));

   function Get_Previous_Word_Location (E        : in Asis.Element;
                                        Matching : in Wide_String  := "";
                                        Starting : in Search_Start := From_Head;
                                        Skipping : in Natural      := 0)
                                        return Location
   is
      use Asis.Text;
      E_Span     : constant Span := A4G_Bugs.Element_Span (E);
      L          : Line_Number;
      Word_Start : Character_Position;
      Result     : Location;
      To_Skip    : Natural := Skipping;

      function Find_Word_Start (Line : in Wide_String) return Character_Position is
         use Ada.Strings.Wide_Maps;
         use Utilities;
         Stop  : Natural := Line'Last;
         Start : Positive;
      begin
         while Stop >= Line'First loop
            if Is_In (Line (Stop), Identifier_Chars) then
               Start := Stop;
               while  Start > Line'First and then Is_In (Line (Start - 1), Identifier_Chars) loop
                  Start := Start - 1;
               end loop;
               if To_Skip = 0 then
                  if Matching = "" or else To_Upper (Line (Start .. Stop)) = Matching then
                     return Character_Position (Start);
                  end if;
               else
                  To_Skip := To_Skip - 1;
               end if;
               Stop := Start - 1;
            else
               Stop := Stop - 1;
            end if;
         end loop;

         return 0;
      end Find_Word_Start;

      use type Asis.ASIS_Integer;   -- Gela-ASIS compatibility
   begin  -- Get_Previous_Word_Location
      case Starting is
         when From_Head =>
            L := E_Span.First_Line;
            Word_Start := Find_Word_Start (Non_Comment_Image (Lines (E, L, L) (L))
                                           (1 .. Positive (E_Span.First_Column) - 1));
         when From_Tail =>
            L := E_Span.Last_Line;
            Word_Start := Find_Word_Start (Non_Comment_Image (Lines (E, L, L) (L))
                                           (1 .. Positive (E_Span.Last_Column)));
      end case;
      while Word_Start = 0 loop
         L     := L - 1;
         Word_Start := Find_Word_Start (Non_Comment_Image (Lines (E, L, L)(L)));
      end loop;

      Result := Get_Location (E);
      Result.First_Line   := L;
      Result.First_Column := Word_Start;
      return Result;
   end Get_Previous_Word_Location;


   --------------------------------
   -- Get_Previous_Word_Location --
   --------------------------------

   function Get_Previous_Word_Location (L        : in Asis.Element_List;
                                        Matching : Wide_String := "";
                                        Starting : Search_Start := From_Head)
                                        return Location
   is
   begin
      case Starting is
         when From_Head =>
            return Get_Previous_Word_Location (L (L'First), Matching, From_Head);
         when From_Tail =>
            return Get_Previous_Word_Location (L (L'Last), Matching, From_Tail);
      end case;
   end Get_Previous_Word_Location;


   ----------------------------
   -- Get_Next_Word_Location --
   ----------------------------

   function Get_Next_Word_Location (E        : in Asis.Element;
                                    Matching : in Wide_String  := "";
                                    Starting : in Search_Start := From_Tail;
                                    Skipping : in Natural      := 0)
                                    return Location
   is
      use Asis.Text;
      E_Span     : constant Span := A4G_Bugs.Element_Span (E);
      L          : Line_Number;
      Word_Start : Character_Position;
      Result     : Location;
      To_Skip    : Natural := Skipping;

      function Find_Word_Start (Line : in Wide_String) return Character_Position is
         use Ada.Strings.Wide_Maps;
         use Utilities;
         Start : Positive := Line'First;
         Stop  : Positive;
      begin
         while Start <= Line'Last loop
            if Is_In (Line (Start), Identifier_Chars) then
               Stop := Start;
               while  Stop < Line'Last and then Is_In (Line (Stop + 1), Identifier_Chars) loop
                  Stop := Stop + 1;
               end loop;
               if To_Skip = 0 then
                  if Matching = "" or else To_Upper (Line (Start .. Stop)) = Matching then
                     return Character_Position (Start);
                  end if;
               else
                  To_Skip := To_Skip - 1;
               end if;
               Start := Stop + 1;
            else
               Start := Start + 1;
            end if;
         end loop;

         return 0;
      end Find_Word_Start;

   begin  -- Get_Next_Word_Location
      case Starting is
         when From_Head =>
            L := E_Span.First_Line;
         when From_Tail =>
            L := E_Span.Last_Line;
      end case;
      declare
         The_Line : constant Asis.Program_Text := Non_Comment_Image (Lines (E, L, L) (L));
      begin
         case Starting is
            when From_Head =>
               Word_Start := Find_Word_Start (The_Line (Positive (E_Span.First_Column) .. The_Line'Last));
            when From_Tail =>
               Word_Start := Find_Word_Start (The_Line (Positive (E_Span.Last_Column) + 1 .. The_Line'Last));
         end case;
      end;
      while Word_Start = 0 loop
         L     := L + 1;
         Word_Start := Find_Word_Start (Non_Comment_Image (Lines (E, L, L)(L)));
      end loop;

      Result := Get_Location (E);
      Result.First_Line   := L;
      Result.First_Column := Word_Start;
      return Result;
   end Get_Next_Word_Location;


   ----------------------------
   -- Get_Next_Word_Location --
   ----------------------------

   function Get_Next_Word_Location (L        : in Asis.Element_List;
                                    Matching : Wide_String := "";
                                    Starting : Search_Start := From_Tail)
                                    return Location
   is
   begin
      case Starting is
         when From_Head =>
            return Get_Next_Word_Location (L (L'First), Matching, From_Head);
         when From_Tail =>
            return Get_Next_Word_Location (L (L'Last), Matching, From_Tail);
      end case;
   end Get_Next_Word_Location;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (L : in Location) return Wide_String is
   begin
      return To_Wide_String (L.File_Name);
   end Get_File_Name;

   ----------------------
   -- Get_First_Column --
   ----------------------

   function Get_First_Column (L : in Location) return Asis.Text.Character_Position is
   begin
      return L.First_Column;
   end Get_First_Column;

   --------------------
   -- Get_First_Line --
   --------------------

   function Get_First_Line (L : in Location) return Asis.Text.Line_Number is
   begin
      return L.First_Line;
   end Get_First_Line;

   -----------
   -- Image --
   -----------

   function Image (L          : in Location;
                   Separator  : in Wide_Character := ':';
                   Quoted     : in Boolean        := False)
                   return Wide_String
   is
      use Ada.Characters.Handling;
      use Ada.Directories;
      use Utilities;

   begin  -- Image

      if L = Null_Location then
         Failure ("Image of null location");
      elsif L.File_Name = Null_Unbounded_Wide_String then
         if Quoted then
            return Quote (ASIS_Integer_Img (L.First_Column));
         else
            return ASIS_Integer_Img (L.First_Column);
         end if;
      else
         if Quoted then
            return
              Quote (if Long_File_Name.Value = On
                     then To_Wide_String (L.File_Name)
                     else To_Wide_String (Simple_Name (To_String (To_Wide_String (L.File_Name)))))
              & Separator
              & Quote (ASIS_Integer_Img (L.First_Line))
              & Separator
              & Quote (ASIS_Integer_Img (L.First_Column));
         else
            return
              (if Long_File_Name.Value = On
               then To_Wide_String (L.File_Name)
               else To_Wide_String (Simple_Name (To_String (To_Wide_String (L.File_Name)))))
              & Separator
              & ASIS_Integer_Img (L.First_Line)
              & Separator
              & ASIS_Integer_Img (L.First_Column);
         end if;
      end if;
   end Image;


   ----------------
   -- Safe_Image --
   ----------------

   function Safe_Image (L         : in Location;
                        Separator : in Wide_Character := ':')
                        return Wide_String
   is
   begin
      if L = Null_Location then
         return "unknown location";
      else
         return Image (L, Separator);
      end if;
   end Safe_Image;

   -----------
   -- Value --
   -----------

   function Value (S : in Wide_String) return Location is
      use Ada.Strings.Wide_Fixed, Asis.Text;

      Pos_Colon1 : Natural;
      Pos_Colon2 : Natural;
      Result : Location;
   begin
      Pos_Colon1 := Index (S, ":");
      if Pos_Colon1 = 0 then
         return (To_Unbounded_Wide_String (S), 0, 0);
      end if;

      Result.File_Name := To_Unbounded_Wide_String (S (S'First .. Pos_Colon1-1));

      Pos_Colon2 := Index (S (Pos_Colon1 + 1 .. S'Last), ":");
      if Pos_Colon2 = 0 then
         Pos_Colon2 := S'Last + 1;
      end if;
      Result.First_Line := Line_Number'Wide_Value (S (Pos_Colon1 + 1 .. Pos_Colon2 - 1));
      if Pos_Colon2 < S'Last then
         Result.First_Column := Character_Position'Wide_Value (S (Pos_Colon2+1 .. S'Last));
      end if;

      return Result;
   end Value;

   -----------------------
   -- Is_Long_File_Name --
   -----------------------

   function Is_Long_File_Name return Boolean is
   begin
      return Long_File_Name.Value = On;
   end Is_Long_File_Name;

begin  -- Framework.Locations
   Framework.Variables.Register (Long_File_Name'Access, Variable_Name => "LONG_FILE_NAME");
end Framework.Locations;
