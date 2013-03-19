----------------------------------------------------------------------
--  Framework - Package body                                        --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2008. The Ada --
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

-- Ada
with
  Ada.Strings,
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis.Compilation_Units,
  Asis.Elements;

-- Adalog
with
  Units_List,
  Utilities;

-- AdaControl
with
  Framework.Control_Manager;

package body Framework is

   -----------
   -- Image --
   -----------

   function Image (Entity : in Entity_Specification) return Wide_String is
      use Utilities;
   begin
      case Entity.Kind is
         when Box =>
            return "<>";
         when Equal =>
            return "=";
         when Regular_Id =>
            return To_Title (To_Wide_String (Entity.Specification));
         when All_Id =>
            return "all " & To_Title (To_Wide_String (Entity.Specification));
      end case;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Name : in Wide_String) return Entity_Specification is
      use Utilities;
   begin
      return (Kind          => Regular_Id,
              Specification => To_Unbounded_Wide_String (To_Upper (Name)));
   end Value;

   -------------------------------
   -- Entity_Specification_Kind --
   -------------------------------

   function Entity_Specification_Kind (Entity : in Entity_Specification) return Entity_Specification_Kinds is
   begin
      return Entity.Kind;
   end Entity_Specification_Kind;

   -------------
   -- Matches --
   -------------

   function Matches (Entity    : in Entity_Specification;
                     Name      : in Asis.Element;
                     Extend_To : in Extension_Set := No_Extension) return Boolean
   is
   -- This implementation of Matches is a bit violent, but it ensures consistency with Matching_Context
      use Framework.Control_Manager;

      Junk_Store : Context_Store;
      Result     : Boolean;
   begin
      if Entity.Kind = Box then
         return True;
      end if;

      Associate (Junk_Store, Entity, Root_Context'(null record));
      Result := Matching_Context (Junk_Store, Name, Extend_To) /= No_Matching_Context;
      Clear (Junk_Store);
      return Result;
   end Matches;

   ---------------------
   -- Create_Location --
   ---------------------

   function Create_Location (File         : in Wide_String;
                             First_Line   : in Natural;
                             First_Column : in Natural) return Location is
   begin
      return (To_Unbounded_Wide_String (File), First_Line, First_Column);
   end Create_Location;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (E : in Asis.Element) return Location is
      use Asis.Compilation_Units, Asis.Elements, Asis.Text;

      S : constant Span := Element_Span (E);
   begin
      if S = Nil_Span then
         return Null_Location;
      end if;

      return (To_Unbounded_Wide_String (Text_Name (Enclosing_Compilation_Unit (E))),
              S.First_Line,
              S.First_Column);
   end Get_Location;


   ----------------------
   -- Get_End_Location --
   ----------------------

   function Get_End_Location (E : in Asis.Element) return Location is
      use Asis.Compilation_Units, Asis.Elements, Asis.Text;

      S : constant Span := Element_Span (E);
   begin
      if S = Nil_Span then
         return Null_Location;
      end if;

      return (To_Unbounded_Wide_String (Text_Name (Enclosing_Compilation_Unit (E))),
              S.Last_Line,
              S.Last_Column);
   end Get_End_Location;


   --------------------------------
   -- Get_Previous_Word_Location --
   --------------------------------

   Identifier_Chars : constant Ada.Strings.Wide_Maps.Wide_Character_Set
     := Ada.Strings.Wide_Maps.To_Set (Ranges => (('a', 'z'), ('A', 'Z'), ('0', '9'), ('_', '_')));

   function Get_Previous_Word_Location (E        : in Asis.Element;
                                        Matching : Wide_String := "";
                                        Starting : Search_Start := From_Head)
                                        return Location is
      use Asis.Text;
      E_Span     : constant Span := Element_Span (E);
      L          : Line_Number;
      Word_Start : Character_Position;
      Result     : Location;

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
               if Matching = "" or else To_Upper (Line (Start .. Stop)) = Matching then
                  return Start;
               end if;
               Stop := Start - 1;
            else
               Stop := Stop - 1;
            end if;
         end loop;

         return 0;
      end Find_Word_Start;

   begin  -- Get_Previous_Word_Location
      case Starting is
         when From_Head =>
            L := E_Span.First_Line;
            Word_Start := Find_Word_Start (Non_Comment_Image (Lines (E, L, L) (L)) (1 .. E_Span.First_Column - 1));
         when From_Tail =>
            L := E_Span.Last_Line;
            Word_Start := Find_Word_Start (Non_Comment_Image (Lines (E, L, L) (L)) (1 .. E_Span.Last_Column));
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
                                    Matching : Wide_String := "";
                                    Starting : Search_Start := From_Tail)
                                    return Location
   is
      use Asis.Text;
      E_Span     : constant Span := Element_Span (E);
      L          : Line_Number;
      Word_Start : Character_Position;
      Result     : Location;

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
               if Matching = "" or else To_Upper (Line (Start .. Stop)) = Matching then
                  return Start;
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
               Word_Start := Find_Word_Start (The_Line (E_Span.First_Column + 1 .. The_Line'Last));
            when From_Tail =>
               Word_Start := Find_Word_Start (The_Line (E_Span.Last_Column + 1 .. The_Line'Last));
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

   function Get_First_Column (L : in Location) return Natural is
   begin
      return Natural (L.First_Column);
   end Get_First_Column;

   --------------------
   -- Get_First_Line --
   --------------------

   function Get_First_Line (L : in Location) return Natural is
   begin
      return Natural (L.First_Line);
   end Get_First_Line;

   -----------
   -- Image --
   -----------

   function Image (L          : in Location;
                   Short_Name : in Boolean := Default_Short_Name;
                   Separator  : in Wide_Character := ':')
                   return Wide_String
   is
      use Utilities;

      function Strip (Name : in Wide_String) return Wide_String is
      begin
         if not Short_Name then
            return Name;
         end if;

         for I in reverse Name'Range loop
            if Name (I) = '/' or Name (I) = '\' then
               return Name (I+1 .. Name'Last);
            end if;
         end loop;

         -- No path found
         return Name;
      end Strip;

   begin  -- Image
      if L = Null_Location then
         Failure ("Image of null location");
      elsif L.File_Name = Null_Unbounded_Wide_String then
         return Integer_Img (L.First_Column);
      else
         return Strip (To_Wide_String (L.File_Name))
           & Separator
           & Integer_Img (L.First_Line)
           & Separator
           & Integer_Img (L.First_Column);
      end if;
   end Image;


   ----------------
   -- Safe_Image --
   ----------------

   function Safe_Image (L          : in Location;
                        Short_Name : in Boolean := Default_Short_Name;
                        Separator  : in Wide_Character := ':')
                        return Wide_String
   is
   begin
      if L = Null_Location then
         return "unknown location";
      else
         return Image (L, Short_Name, Separator);
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


begin  -- Framework
   Units_List.Initialize (Adactl_Context'Access);
end Framework;
