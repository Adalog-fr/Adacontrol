----------------------------------------------------------------------
--  Print Full Name Image                                           --
--  (C) Copyright 2001, 2005 ADALOG                                 --
--  Author: J-P. Rosen                                              --
--                                                                  --
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
  Ada.Strings.Wide_Maps,
  Ada.Strings.Wide_Unbounded,
  Ada.Wide_Text_IO;

with   -- ASIS components
  Asis.Ada_Environments,
  Asis.Elements,
  Asis.Errors,
  Asis.Compilation_Units,
  Asis.Iterator,
  Asis.Implementation,
  Asis.Expressions,
  Asis.Exceptions,
  Asis.Text;

with   -- Other reusable components
  Implementation_Options,
  Options_Analyzer,
  Utilities;

with   -- What we test
  Thick_Queries;

procedure Pfni is
   use Asis, Asis.Expressions;
   use Compilation_Units, Elements;
   use Thick_Queries, Utilities;
   use Ada.Wide_Text_IO;


   Force_Spec : Boolean := False;
   Force_Full : Boolean := False;

   Overloading_Option : Boolean;
   Full_Option        : Boolean;
   Quiet_Option       : Boolean;

   Unit_Name    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   First_Line   : Asis.Text.Line_Number        := 0;
   Last_Line    : Asis.Text.Line_Number        := Asis.Text.Line_Number'Last;
   First_Column : Asis.Text.Character_Position := 0;
   I_Options    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

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

   begin
      Unit_First := Index (S, Set => To_Set ("/\"), Going => Backward);
      if Unit_First = 0 then
         -- There is no directory separator
         Unit_First := S'First;
      else
         I_Options  := " -I" & To_Unbounded_Wide_String (S (S'First ..  Unit_First));
         Unit_First := Unit_First + 1;
      end if;

      Pos_Colon1 := Index (S (Unit_First .. S'Last), ":");
      if Pos_Colon1 = 0 then
         Unit_Name := To_Unbounded_Wide_String (Make_Unit_Name (S (Unit_First .. S'Last)));
         return;
      end if;

      Force_Full := True;
      Unit_Name  := To_Unbounded_Wide_String (Make_Unit_Name (S (Unit_First .. Pos_Colon1-1)));

      Pos_Colon2 := Index (S (Pos_Colon1 + 1 .. S'Last), ":");
      if Pos_Colon2 = 0 then
         Pos_Dash := Index (S (Pos_Colon1 + 1 .. S'Last), "-");
         if Pos_Dash = 0 then
            First_Line := Line_Number'Wide_Value (S (Pos_Colon1 + 1 .. S'Last));
            Last_Line  := First_Line;
         else
            if Pos_Dash /= Pos_Colon1 + 1 then
               First_Line := Line_Number'Wide_Value (S (Pos_Colon1 + 1 .. Pos_Dash - 1));
            end if;
            if Pos_Dash /= S'Last then
               Last_Line  := Line_Number'Wide_Value (S (Pos_Dash   + 1 .. S'Last));
            end if;
         end if;
      else
         First_Line   := Line_Number'Wide_Value (S (Pos_Colon1 + 1 .. Pos_Colon2 - 1));
         Last_Line    := First_Line;
         First_Column := Character_Position'Wide_Value (S (Pos_Colon2+1 .. S'Last));
      end if;
   end Parse_Parameter;

   -------------
   -- Options --
   -------------

   package Options is new Options_Analyzer (Binary_Options => "dfhoqs",
                                            Valued_Options => "p",
                                            Tail_Separator => "--");
   use Options;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
   begin
      Put_Line ("PFNI : Print Full Name Image");
      Put_Line ("Usage: pfni [-sofdq] [-p <project-file>] <unit>[:<line_number>[:<column_number>]] -- <ASIS options>");
      Put_Line ("   or: pfni -h");
      New_Line;

      Put_Line ("Options:");
      Put_Line ("   -d      debug mode");
      Put_Line ("   -f      full output (each occurrence of names)");
      Put_Line ("   -h      prints this help message");
      Put_Line ("   -o      output overloading information");
      Put_Line ("   -p file specify an emacs ada-mode project file (.adp)");
      Put_Line ("   -q      don't repeat source line (quiet)");
      Put_Line ("   -s      process specifications only");
   end Print_Help;

   ------------------
   -- Adjust_Image --
   ------------------

   function Adjust_Image (Original : Wide_String) return Wide_String is
      -- Transform a Full_Name_Image according to the syntax we use externally.
      -- The differences with the string return by Full_Name_Image are:
      --   we use "return" rather than ":" for the return type of functions.
      --   we use "access" rather than "*" for access parameters
      use Ada.Strings.Wide_Fixed;

      Pos   : Natural;
      Start : Natural;
   begin
      Pos := Index (Original, ":");
      if Pos = 0 then
         -- Find a real * meaning "access", discard the "*" and "**" operators
         Start := Original'First;
         loop
            Pos := Index (Original (Start .. Original'Last), "*");

            if Pos = 0 then
               -- No * found
               return Original;

            elsif Original (Pos+1) = '"' then
               -- "*" operator
               Start := Pos+2;

            elsif Original (Pos+1) = '*' then
               -- "**" operator
               Start := Pos+3;

            else
               -- Real access parameter
               exit;
            end if;
         end loop;

         return
           Original (Original'First..Pos - 1) &
           " access " &
           Adjust_Image (Original (Pos + 1 .. Original'Last));

      else
         return
           Adjust_Image (Original (Original'First..Pos - 1)) &
           " return " &
           Adjust_Image (Original (Pos + 1 .. Original'Last));
      end if;
   end Adjust_Image;

   ----------------
   -- Print_Name --
   ----------------

   package Line_Number_IO is new Ada.Wide_Text_IO.Integer_IO (Asis.Text.Line_Number);

   Previous_Line : Asis.Text.Line_Number := 0;
   procedure Print_Name (The_Name : Asis.Element) is
      use Asis.Text, Ada.Strings, Ada.Strings.Wide_Fixed;
      The_Span : constant Span := Element_Span (The_Name);
   begin
      if The_Span.First_Line not in First_Line .. Last_Line
        or else (First_Column /= 0
                 and then First_Column not in The_Span.First_Column .. The_Span.Last_Column)
      then
         return;
      end if;

      if The_Span.First_Line /= Previous_Line then
         New_Line;
         if not Quiet_Option then
            declare
               use Line_Number_IO;
               The_Lines : constant Line_List := Lines (The_Name,
                                                        The_Span.First_Line,
                                                        The_Span.Last_Line);
            begin
               for I in The_Lines'Range loop
                  Put (I, Width => 4);
                  Put (": ");
                  Put_Line (Line_Image (The_Lines (I)));
               end loop;
            end;
         end if;
         Previous_Line := The_Span.Last_Line;
         Put ("==>> ");
      else
         Put (", ");
      end if;

      declare
         Result : constant Wide_String := Full_Name_Image (The_Name, Overloading_Option);
      begin
         if Result = "" then
            Put ("<<FNI not available for """ & Trim (Element_Image (The_Name), Both) & """>>");
         else
            Put (Adjust_Image (Result));
         end if;
      end;
   end Print_Name;

   --------------------------------------------------------------------------
   -- The analyzer                                                         --
   --------------------------------------------------------------------------

   type Info is null record;

   -------------------
   -- Pre_Procedure --
   -------------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Info)
   is
      pragma Unreferenced (State);
      use Asis.Text;
   begin
      case Element_Kind (Element) is
         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Attribute_Reference =>
                  if Full_Option then
                     Print_Name (Element);
                  end if;
                  Control := Abandon_Children;
               when An_Identifier | An_Enumeration_Literal | An_Operator_Symbol =>
                  if Full_Option then
                     Print_Name (Element);
                  end if;
               when A_Selected_Component =>
                  if Full_Option then
                     Print_Name (Selector (Element));
                  end if;
                  Control := Abandon_Children;
               when others =>
                  null;
            end case;
         when A_Defining_Name =>
            Print_Name (Element);
         when others =>
            null;
      end case;
   exception
      when others =>
         Put_Line (Asis.Text.Element_Image (Element));
         raise;
   end Pre_Procedure;

   --------------------
   -- Post_Procedure --
   --------------------

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Info)
   is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure;

   --------------
   -- Traverse --
   --------------

   procedure Traverse is new Asis.Iterator.Traverse_Element
     (Info, Pre_Procedure, Post_Procedure);

   My_Context     : Context;
   My_Unit        : Compilation_Unit;
   My_Declaration : Declaration;
   The_Control    : Traverse_Control := Continue;
   The_Info       : Info;

   use Ada.Characters.Handling, Asis.Exceptions;
   use Implementation_Options;
   use Ada.Strings.Wide_Unbounded;
begin  -- PFNI
   if Is_Present (Option => 'h') then
      Print_Help;
      return;
   end if;

   if Parameter_Count /= 1 then
      Print_Help;
      return;
   end if;

   Implementation_Options.Default_F_Parameter := 'S';  -- -FS by default
   begin
      Parse_Parameter (To_Wide_String (Parameter (1)));
   exception
      when Constraint_Error =>
         User_Message ("Illegal value for line or column specification");
         return;
   end;

   Overloading_Option := Is_Present (Option => 'o');
   Debug_Option       := Is_Present (Option => 'd');
   Full_Option        := Is_Present (Option => 'f') or Force_Full;
   Quiet_Option       := Is_Present (Option => 'q');

   Implementation.Initialize;
   Ada_Environments.Associate (My_Context,
                               "MARF",
                               Parameters_String (Value (Option => 'p', Explicit_Required => True),
                                                  To_Wide_String (Tail_Value) & To_Wide_String (I_Options)));
   Ada_Environments.Open (My_Context);

   if Is_Present (Option => 's') or Force_Spec then
      My_Unit := Library_Unit_Declaration (To_Wide_String (Unit_Name), My_Context);
   else
      My_Unit := Compilation_Unit_Body (To_Wide_String (Unit_Name), My_Context);
   end if;

   declare
      My_CC_List : constant Context_Clause_List
        := Context_Clause_Elements (Compilation_Unit => My_Unit, Include_Pragmas  => True) ;
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
   when Occur : Options_Error =>
      Put_Line (To_Wide_String (Ada.Exceptions.Exception_Message (Occur)));
      Print_Help;
   when ASIS_Failed =>
      case Asis.Implementation.Status is
         when Asis.Errors.Use_Error =>
            User_Message ("Inconsistent tree, please remove *.adt files");
         when others =>
            Asis_Exception_Messages;
            if Debug_Option then
               raise;
            end if;
      end case;
   when ASIS_Inappropriate_Compilation_Unit =>
      User_Message ("Unit " & To_Wide_String (Unit_Name) & " not found in context");
   when
     ASIS_Inappropriate_Context
     | ASIS_Inappropriate_Container
     | ASIS_Inappropriate_Element
     | ASIS_Inappropriate_Line
     | ASIS_Inappropriate_Line_Number
     =>
      Asis_Exception_Messages;
      if Debug_Option then
         raise;
      end if;

end Pfni;
