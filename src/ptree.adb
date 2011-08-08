-------------------------------------------------------------------------------
-- Program PTREE                                                             --
-- (C) Copyright 2001 ADALOG                                                 --
-- Author: J-P. Rosen                                                        --
--                                                                           --
-- Prints a graphic representation of an ASIS tree                           --
-- with corresponding source                                                 --
--                                                                           --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                   Tel: +33 1 41 24 31 40                 --
--  19-21 rue du 8 mai 1945  Fax: +33 1 41 24 07 36                 --
--  94110 ARCUEIL            E-m: info@adalog.fr                    --
--  FRANCE                   URL: http://www.adalog.fr              --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
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
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
-------------------------------------------------------------------------------
with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded,
  Ada.Integer_Wide_Text_IO,
  Ada.Wide_Text_IO;

with   -- ASIS components
  Asis.Ada_Environments,
  Asis.Compilation_Units,
  Asis.Elements,
  Asis.Errors,
  Asis.Exceptions,
  Asis.Implementation,
  Asis.Iterator,
  Asis.Text;

with   -- Other reusable components
  Implementation_Options,
  Options_Analyzer;

procedure Ptree is
   use Asis;
   use Compilation_Units, Elements;

   type Location is record
      File_Name    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      First_Line   : Asis.Text.Line_Number        := 0;
      First_Column : Asis.Text.Character_Position := 0;
   end record;

   -- Text is indentent at least from Col_Base, then with a step of Col_Step
   Col_Base : constant := 30;
   Col_Step : constant := 10;

   Span_Option : Boolean;
   Unit_Spec   : Location;
   package Options is new Options_Analyzer (Binary_Options => "hsbS",
                                            Valued_Options => "p",
                                            Tail_Separator => "--");
   use Options;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
      use Ada.Wide_Text_IO;
   begin
      Put_Line ("PTREE: prints the logical nesting of ASIS elements for a unit");
      Put_Line ("Usage: ptree [-sS] [-p <project_file>] <unit>[:<line_number>[:<column_number>]] -- <ASIS_Options>");
      Put_Line ("   or: ptree -h");
      New_Line;

      Put_Line ("Options:");
      Put_Line ("   -h      prints this help message");
      Put_Line ("   -p file specify an emacs ada-mode project file (.adp)");
      Put_Line ("   -s      process specifications only");
      Put_Line ("   -S      print span of each element");
   end Print_Help;

   -----------
   -- Value --
   -----------

   function Value (S : Wide_String) return Location is
      use Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded, Asis.Text;

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

   --------------------------------------------------------------------------
   -- The analyzer                                                         --
   --------------------------------------------------------------------------

   subtype Info is Natural;

   Sep : constant Wide_String := Asis.Text.Delimiter_Image;

   procedure Put_Kind (Element : in Asis.Element) is
      use Ada.Wide_Text_IO;
   begin
      case Element_Kind (Element) is
         when Not_An_Element => -- ????
            null;
         when A_Pragma =>
            Put (" (");
            Put (Pragma_Kinds'Wide_Image (Pragma_Kind (Element)));
            Put (')');
         when A_Defining_Name =>
            Put (" (");
            Put (Defining_Name_Kinds'Wide_Image (Defining_Name_Kind (Element)));
            Put (')');
         when A_Declaration =>
            Put (" (");
            Put (Declaration_Kinds'Wide_Image (Declaration_Kind (Element)));
            Put (')');
         when A_Definition =>
            Put (" (");
            Put (Definition_Kinds'Wide_Image (Definition_Kind (Element)));
            Put (')');
         when An_Expression =>
            Put (" (");
            Put (Expression_Kinds'Wide_Image (Expression_Kind (Element)));
            Put (')');
         when An_Association =>
            Put (" (");
            Put (Association_Kinds'Wide_Image (Association_Kind (Element)));
            Put (')');
         when A_Statement =>
            Put (" (");
            Put (Statement_Kinds'Wide_Image (Statement_Kind (Element)));
            Put (')');
         when A_Path =>
            Put (" (");
            Put (Path_Kinds'Wide_Image (Path_Kind (Element)));
            Put (')');
         when A_Clause =>
            Put (" (");
            Put (Clause_Kinds'Wide_Image (Clause_Kind (Element)));
            Put (')');
         when An_Exception_Handler =>
            null;
      end case;
   end Put_Kind;

   procedure Put_Span (The_Span : Asis.Text.Span) is
      use Ada.Wide_Text_IO, Ada.Integer_Wide_Text_IO;
   begin
      if Span_Option then
         Put (" [");
         Put (The_Span.First_Line, Width => 1);
         Put (':');
         Put (The_Span.First_Column, Width => 1);
         Put (", ");
         Put (The_Span.Last_Line, Width => 1);
         Put (':');
         Put (The_Span.Last_Column, Width => 1);
         Put (']');
      end if;
   end Put_Span;

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Info)
   is
      pragma Unreferenced (Control);
      use Ada.Wide_Text_IO, Asis.Text, Ada.Strings, Ada.Strings.Wide_Fixed;
      The_Span : constant Span := Element_Span (Element);
   begin
      State := State + 1;

      -- Chech if in range
      if Unit_Spec.First_Line /= 0 then
         if The_Span.First_Line > Unit_Spec.First_Line or The_Span.Last_Line < Unit_Spec.First_Line then
            return;
         elsif Unit_Spec.First_Column /= 0 and then
           (The_Span.First_Line = Unit_Spec.First_Line
            and (The_Span.First_Column > Unit_Spec.First_Column or The_Span.Last_Column < Unit_Spec.First_Column))
         then
            return;
         end if;
      end if;

      for I in 1..State-1 loop
         Put ("| ");
      end loop;

      Put ("+-");
      Put (Element_Kinds'Wide_Image (Element_Kind (Element)));
      Put (": ");
      declare
         Source : constant Wide_String := Element_Image (Element);
         Start  : Natural := 1;
         Stop   : Natural;
         First_Line : Boolean := True;
      begin
         loop
            Stop := Index (Source(Start..Source'Last), Sep);
            if not First_Line then
               for I in 1..State loop
                  Put ("| ");
               end loop;
            end if;
            if Col < Col_Base then
               Set_Col (Col_Base);
            else
               Set_Col (Col_Base + (Col - Col_Base + Col_Step) / Col_Step * Col_Step);
            end if;
            if Stop = 0 then
               if First_Line then
                  Put (Trim (Source (Start..Source'Last), Both));
                  Put_Span (The_Span);
                  Put_Kind (Element);
               else
                  Put (Source (Start..Source'Last));
               end if;

               New_Line;
               exit;
            else
               if First_Line then
                  Put (Trim (Source (Start..Stop-1), Both));
                  Put_Span (The_Span);
                  Put_Kind (Element);
               else
                  Put (Source (Start..Stop-1));
               end if;
               Start := Stop + Sep'Length;
            end if;
            First_Line := False;

            New_Line;
         end loop;
      end;
   end Pre_Procedure;

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info)
   is
      pragma Unreferenced (Element, Control);
   begin
      State := State - 1;
   end Post_Procedure;

   procedure Traverse is new Asis.Iterator.Traverse_Element
     (Info, Pre_Procedure, Post_Procedure);

   My_Context     : Context;
   My_Unit        : Compilation_Unit;
   My_Declaration : Declaration;
   The_Control    : Traverse_Control := Continue;
   The_Info       : Info := 0;

   use Ada.Wide_Text_IO, Ada.Characters.Handling, Implementation_Options;
begin
   if Is_Present (Option => 'h') then
      Print_Help;
      return;
   end if;

   if Parameter_Count /= 1 then
      Print_Help;
      return;
   end if;
   Span_Option := Is_Present (Option => 'S');
   Unit_Spec   := Value (To_Wide_String (Parameter (1)));

   Implementation.Initialize;
   Ada_Environments.Associate (My_Context, "Ptree",
                               Parameters_String (Value (Option            => 'p',
                                                         Explicit_Required => True),
                                                  To_Wide_String (Options.Tail_Value)));
   Ada_Environments.Open (My_Context);

   declare
      use Ada.Strings.Wide_Unbounded;
      Unit_Name : constant Wide_String := To_Wide_String (Unit_Spec.File_Name);
   begin
      if Is_Present (Option => 's') then
         My_Unit := Library_Unit_Declaration (Unit_Name, My_Context);
      else
         My_Unit := Compilation_Unit_Body (Unit_Name, My_Context);
      end if;
   end;

   declare
      My_CC_List : constant Context_Clause_List
        := Context_Clause_Elements (Compilation_Unit => My_Unit,
                                    Include_Pragmas  => True) ;
   begin
      for I in My_CC_List'Range loop
         Traverse (My_CC_List (I), The_Control, The_Info);
      end loop;
   end;

   My_Declaration := Unit_Declaration (My_Unit);
   Traverse (My_Declaration, The_Control, The_Info);

   Ada_Environments.Close (My_Context);
   Ada_Environments.Dissociate (My_Context);
   Implementation.Finalize;
exception
   when Occur : Asis.Exceptions.ASIS_Failed =>
      case Asis.Implementation.Status is
         when Asis.Errors.Use_Error =>
            Ada.Wide_Text_IO.Put_Line ("Inconsistent tree, please remove *.adt files");
         when others =>
            Ada.Wide_Text_IO.Put_Line (To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));
      end case;

   when Occur : Options_Error =>
      Ada.Wide_Text_IO.Put_Line (To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));
      Print_Help;
end Ptree;
