----------------------------------------------------------------------
-- Program PTREE                                                    --
-- (C) Copyright 2001-2022 ADALOG                                   --
-- Author: J-P. Rosen                                               --
--                                                                  --
-- Prints a graphic representation of an ASIS tree                  --
-- with corresponding source                                        --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
-- ADALOG                                     --
-- 2 rue du Docteur Lombard                   --
-- 92441 Issy-Les-Moulineaux CEDEX  E-m: info@adalog.fr             --
-- FRANCE                           URL: https://www.adalog.fr/     --
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
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Unbounded,
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
  A4G_Bugs,
  Implementation_Options,
  Options_Analyzer,
  Project_File.Factory,
  Utilities;

procedure Ptree is
   use Asis;
   use Compilation_Units, Elements;

   Version : constant Wide_String := "1.1";  -- Assume previous unnumbered versions were 1.0

   Force_Spec : Boolean := False;

   Unit_Name    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   First_Line   : Asis.Text.Line_Number        := 0;
   Last_Line    : Asis.Text.Line_Number        := Asis.Text.Line_Number'Last;
   First_Column : Asis.Text.Character_Position := 0;
   I_Options    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   -- Text is indented at least from Col_Base, then with a step of Col_Step
   Col_Base : constant := 30;
   Col_Step : constant := 10;

   Span_Option : Boolean;
   package Options is new Options_Analyzer (Binary_Options => "hsbS",
                                            Valued_Options => "p",
                                            Tail_Separator => "--");
   use Options;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
      use Ada.Wide_Text_IO, Ada.Characters.Handling, Asis.Implementation;
   begin
      Put_Line ("ptree " & Version & " with " & ASIS_Implementor_Version);
      if Parameter_Count > 0 and then To_Upper (Parameter (1)) = "VERSION" then
         return;
      end if;

      Put_Line ("prints the logical nesting of ASIS elements for a unit");
      Put_Line ("Usage: ptree [-sS] [-p <project_file>] <unit>[:<line_number>[:<column_number>]] -- <ASIS_Options>");
      Put_Line ("   or: ptree -h [version]");
      New_Line;

      Put_Line ("Options:");
      Put_Line ("   -h      prints this help message (only version number if followed by ""version""");
      Put_Line ("   -p file specify a project file (.gpr or .adp)");
      Put_Line ("   -s      process specifications only");
      Put_Line ("   -S      print span of each element");
   end Print_Help;

   ---------------------
   -- Parse_Parameter --
   ---------------------

   procedure Parse_Parameter (S : Wide_String)is
      use Ada.Strings, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded, Ada.Strings.Wide_Maps;
      use Asis.Text;

      Unit_First : Natural;
      Pos_Colon1 : Natural;
      Pos_Colon2 : Natural;
      Pos_Dash   : Natural;

      function Make_Unit_Name (Name : Wide_String) return Wide_String is
         use Utilities;
         Pos_Dot : constant Natural := Index (Name, ".", Going => Backward);
      begin
         if Pos_Dot = 0 or Pos_Dot /= Name'Last - 3 then
            return Name (Unit_First .. Name'Last);
         end if;

         if To_Upper (Name (Name'Last - 2 .. Name'Last)) = "ADB" then
            return Translate (Name (Unit_First .. Name'Last - 4), To_Mapping ("-", "."));
         elsif To_Upper (Name (Name'Last - 2 .. Name'Last)) = "ADS" then
            Force_Spec := True;
            return Translate (Name (Unit_First .. Name'Last - 4), To_Mapping ("-", "."));
         else
            return Name (Unit_First .. Name'Last);
         end if;
      end Make_Unit_Name;

   begin  -- Parse_Parameter
      Unit_First := Index (S, Set => To_Set ("/\"), Going => Backward);
      if Unit_First = 0 then
         -- There is no directory separator
         Unit_First := S'First;
      else
         I_Options  := " -I" & To_Unbounded_Wide_String (S (S'First ..  Unit_First));
         Unit_First := Unit_First + 1;
      end if;

      Pos_Colon1 := Index (S (Unit_First .. S'Last), ":");
      if Pos_Colon1 = 0 then        -- <file>
         Unit_Name := To_Unbounded_Wide_String (Make_Unit_Name (S (Unit_First .. S'Last)));
         return;
      end if;

      Unit_Name  := To_Unbounded_Wide_String (Make_Unit_Name (S (Unit_First .. Pos_Colon1-1)));

      Pos_Colon2 := Index (S (Pos_Colon1 + 1 .. S'Last), ":");
      if Pos_Colon2 = 0 then                  -- <file>:<line>[-<line>]
         Pos_Dash := Index (S (Pos_Colon1 + 1 .. S'Last), "-");
         if Pos_Dash = 0 then                 -- <file>:<line>
            First_Line := Line_Number'Wide_Value (S (Pos_Colon1 + 1 .. S'Last));
            Last_Line  := First_Line;
         else
            if Pos_Dash = Pos_Colon1 + 1 then  -- <file>:-<line>
               First_Line := 1;
            else
               First_Line := Line_Number'Wide_Value (S (Pos_Colon1 + 1 .. Pos_Dash - 1));
            end if;
            if Pos_Dash = S'Last then          -- <file>:<line>-
               Last_Line := Asis.Text.Line_Number'Last;
            else                               -- <file>:<line>-<line>
               Last_Line := Line_Number'Wide_Value (S (Pos_Dash + 1 .. S'Last));
            end if;
         end if;
      else
         First_Line   := Line_Number'Wide_Value (S (Pos_Colon1 + 1 .. Pos_Colon2 - 1));
         Last_Line    := First_Line;
         First_Column := Character_Position'Wide_Value (S (Pos_Colon2+1 .. S'Last));
      end if;
   end Parse_Parameter;


   --------------------------------------------------------------------------
   -- The analyzer                                                         --
   --------------------------------------------------------------------------

   type Info is
      record
         Depth : Natural;
         Top_Active_Element : Asis.Element;
      end record;
   -- Depth is the current nesting of syntactic elements
   -- Top_Active_Element is the first encountered element that needs to be printed, everything inside it must also
   --    be printed.

   Sep : constant Wide_String := Asis.Text.Delimiter_Image;

   package ASIS_Integer_Wide_Text_IO is new Ada.Wide_Text_IO.Integer_IO (Asis.ASIS_Integer);

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
      use Ada.Wide_Text_IO, ASIS_Integer_Wide_Text_IO;
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
      The_Span : constant Span := A4G_Bugs.Element_Span (Element);
   begin
      State.Depth := State.Depth + 1;

      if Is_Nil (State.Top_Active_Element) then
         if First_Column = 0 then -- <line 1>[-<line 2]
            if   The_Span.First_Line not in First_Line .. Last_Line
              or The_Span.Last_Line  not in First_Line .. Last_Line
            then
               return;
            end if;
         else   -- <line>:<column>
            if The_Span.First_Line /= First_Line
              or else First_Column not in The_Span.First_Column .. The_Span.Last_Column
            then
               return;
            end if;
         end if;
         State.Top_Active_Element := Element;
      end if;

      for I in Natural range 1..State.Depth-1 loop
         Put ("| ");
      end loop;

      Put ("+-");
      Put (Element_Kinds'Wide_Image (Element_Kind (Element)));
      Put (':');
      declare
         Source : constant Wide_String := Element_Image (Element);
         Start  : Natural := 1;
         Stop   : Natural;
         Is_First_Line : Boolean := True;
         Offset : constant Natural := Index_Non_Blank (Source) - 1;
      begin
         loop
            Stop := Index (Source(Start..Source'Last), Sep);
            if not Is_First_Line then
               for I in Natural range 1..State.Depth loop
                  Put ("| ");
               end loop;
            end if;
            if Col < Col_Base then
               Set_Col (Col_Base);
            else
               Set_Col (Col_Base + (Col - Col_Base + Col_Step) / Col_Step * Col_Step);
            end if;
            if Stop = 0 then   -- last line
               if Is_First_Line then
                  Put (Trim (Source (Start..Source'Last), Both));
                  Put_Span (The_Span);
                  Put_Kind (Element);
               else
                  if Source (Start .. Start + Offset - 1) = (Start .. Start + Offset - 1 => ' ') then
                     Put (Source (Start + Offset .. Source'Last));
                  else  -- badly indented, give up
                     Put (Source (Start .. Source'Last));
                  end if;
               end if;

               New_Line;
               exit;
            end if;

            if Is_First_Line then
               Put (Trim (Source (Start .. Stop - 1), Both));
               Put_Span (The_Span);
               Put_Kind (Element);
            else
               if Source (Start .. Start + Offset -1) = (Start .. Start + Offset -1 => ' ') then
                  Put (Source (Start + Offset .. Stop - 1));
               else  -- badly indented, give up
                  Put (Source (Start .. Stop - 1));
               end if;
            end if;
            Start := Stop + Sep'Length;

            Is_First_Line := False;

            New_Line;
         end loop;
      end;
   end Pre_Procedure;

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info)
   is
      pragma Unreferenced (Control);
   begin
      State.Depth := State.Depth - 1;
      if Is_Equal (State.Top_Active_Element, Element) then
         State.Top_Active_Element := Nil_Element;
      end if;
   end Post_Procedure;

   procedure Traverse is new Asis.Iterator.Traverse_Element
     (Info, Pre_Procedure, Post_Procedure);

   My_Context     : Context;
   My_Unit        : Compilation_Unit;
   My_Declaration : Declaration;
   The_Control    : Traverse_Control := Continue;
   The_Info       : Info := (Depth => 0, Top_Active_Element => Nil_Element);

   use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;
   use Implementation_Options, Project_File, Utilities;
begin  -- Ptree
   if Is_Present (Option => 'h') then
      Print_Help;
      return;
   end if;

   if Parameter_Count /= 1 then
      Print_Help;
      return;
   end if;
   Implementation_Options.Default_F_Parameter := 'S';  -- -FS by default
   Span_Option := Is_Present (Option => 'S');
   begin
      Parse_Parameter (To_Wide_String (Parameter (1)));
   exception
      when Constraint_Error =>
         User_Message ("Illegal value for line or column specification");
         return;
   end;

   Implementation.Initialize (Initialize_String);
   Ada_Environments.Associate (My_Context, "Ptree",
                               Parameters_String (Factory.Corresponding_Project (Value (Option            => 'p',
                                                                                        Explicit_Required => True)),
                                                  To_Wide_String (Options.Tail_Value) & To_Wide_String (I_Options)));
   Ada_Environments.Open (My_Context);

   if Is_Present (Option => 's') or Force_Spec then
      My_Unit := Library_Unit_Declaration (To_Wide_String (Unit_Name), My_Context);
   else
      My_Unit := Compilation_Unit_Body (To_Wide_String (Unit_Name), My_Context);
   end if;

   declare
      My_CC_List : constant Context_Clause_List
        := Context_Clause_Elements (Compilation_Unit => My_Unit,
                                    Include_Pragmas  => True) ;
   begin
      for Cc : Asis.Context_Clause of My_CC_List loop
         Traverse (Cc, The_Control, The_Info);
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

   when Occur : Implementation_Error =>
      Ada.Wide_Text_IO.Put_Line (To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));

   when Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit =>
      Ada.Wide_Text_IO.Put_Line ("Unit " & To_Wide_String (Unit_Name) & " not found in context");

   when Occur : Options_Error =>
      Ada.Wide_Text_IO.Put_Line (To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));
      Print_Help;
end Ptree;
