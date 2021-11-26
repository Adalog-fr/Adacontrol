----------------------------------------------------------------------
--  Adactl_Fix - Procedure body                                     --
--                                                                  --
--  This software is (c) Adalog 2017-2021.                               --
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
--                                                                  --
--  This  software is  distributed  in  the hope  that  it will  be --
--  useful,  but WITHOUT  ANY  WARRANTY; without  even the  implied --
--  warranty  of  MERCHANTABILITY   or  FITNESS  FOR  A  PARTICULAR --
--  PURPOSE.                                                        --
----------------------------------------------------------------------

with  -- Ada units
     Ada.Containers.Ordered_Maps,
     Ada.Containers.Vectors,
     Ada.Directories,
     Ada.Exceptions,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with  -- Adalog components
    Options_Analyzer,
    Utilities;

procedure Adactl_Fix is
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   Version : constant String    := "Adactl_Fix V1.1r2";
   Marker  : constant Character := '!';

   --------------------------------------------
   -- Global data
   --------------------------------------------
   Incorrect_Fix_File : exception;
   Parameter_Error    : exception;

   --
   -- Coordinate
   --
   type Coordinate is
      record
         Line : Ada.Text_IO.Count;
         Col  : Ada.Text_IO.Count;
      end record;
   function "<" (Left, Right : Coordinate) return Boolean is
      (Left.Line < Right.Line or else (Left.Line = Right.Line and Left.Col < Right.Col));
   function ">=" (Left, Right : Coordinate) return Boolean is
     (not (Left < Right));   --## Rule line off SIMPLIFIABLE_EXPRESSIONS ## This is the definition of "<"

   --
   -- Position
   --
   type Position is
      record
         File_Name : Unbounded_String;
         Start_Pos : Coordinate;
         End_Pos   : Coordinate;
      end record;
   function "<" (Left, Right : Position) return Boolean is
      (Left.File_Name < Right.File_Name
         or else (Left.File_Name = Right.File_Name
                    and (Left.Start_Pos < Right.Start_Pos
                         or else (Left.Start_Pos = Right.Start_Pos and Left.End_Pos < Right.End_Pos))));

   function Image (P : Position) return String is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      return To_String (P.File_Name)
             & ':' & Trim (P.Start_Pos.Line'Image, Left)
             & ':' & Trim (P.Start_Pos.Col'Image,  Left);
   end Image;

   --
   -- Fix
   --
   package String_Vector is new Ada.Containers.Vectors (Positive, Unbounded_String);
   use String_Vector;

   type Fix_Kind is (Insert, Replace, Refactor, Delete, Not_A_Fix);
   subtype Range_Fixes   is Fix_Kind range Replace .. Delete;
   subtype True_Fix_Kind is Fix_Kind range Insert .. Delete;
   type Fix_Descriptor (Kind : True_Fix_Kind := Insert) is
      record
         case Kind is
            when Insert | Replace =>
               First : Positive;
               Last  : Positive;
            when Refactor | Delete =>
               null;
         end case;
      end record;
   -- A constructor, because an aggregate can't be used when a discriminant governing a variant part is not static
   function Create (Kind : True_Fix_Kind; First : Natural := 0; Last : Natural := 0) return Fix_Descriptor;

   package Fix_Maps is new Ada.Containers.Ordered_Maps (Key_Type     => Position,
                                                        Element_Type => Fix_Descriptor);
   use Fix_Maps;

   --
   -- Options
   --
   package Options is new Options_Analyzer (Binary_Options => "dhv",
                                            Valued_Options => "o");
   Debug_Option   : Boolean := False;
   Verbose_Option : Boolean := False;
   Help_Option    : Boolean := False;
   Output_Name    : Unbounded_String := Null_Unbounded_String;

   --
   -- Global variables
   --

   -- Invariant: there are no overlapping fixes in Kept_Fixes
   Kept_Fixes      : Fix_Maps.Map;
   Replacements    : String_Vector.Vector;
   Conflicts_Found : Boolean := False;
   Input_File      : File_Type;
   Output_File     : File_Type;


   --------------------------------------------
   -- Subprograms
   --------------------------------------------

   ------------
   -- Create --
   ------------

   function Create (Kind : True_Fix_Kind; First : Natural := 0; Last : Natural := 0) return Fix_Descriptor is
   begin
      case Kind is
         when Insert =>
            return (Insert, First, Last);
         when Replace =>
            return (Replace, First, Last);
         when Refactor =>
            return (Kind => Refactor);
         when Delete =>
            return (Kind => Delete);
      end case;
   end Create;

   ----------
   -- Find --
   ----------

   function Find (Char : Character; From : Positive; Into : String) return Positive is
      -- Return Into'Last + 1 if not found
   begin
      for Inx in Positive range From .. Into'Last loop
         if Into (Inx) = Char then
            return Inx;
         end if;
      end loop;
      return Into'Last + 1;
   end Find;

   -------------
   -- Message --
   -------------

   procedure Message (Text : String; Partial : Boolean := False) is
   begin
      Put (Standard_Error, Text);
      if not Partial then
         New_Line (Standard_Error);
      end if;
   end Message;

   ----------
   -- Info --
   ----------

   procedure Info (Text : String; Partial : Boolean := False) is
   begin
      if Verbose_Option then
         Message (Text, Partial);
      end if;
   end Info;

   ---------------------
   -- Analyze_Options --
   ---------------------

   procedure Analyze_Options is
      use Options;
   begin
      Debug_Option   := Is_Present (Option => 'd');
      Verbose_Option := Is_Present (Option => 'v');
      Help_Option    := Is_Present (Option => 'h');

      if not Help_Option and then Parameter_Count = 0 then
         Message ("Parameter error: Name(s) of fix file(s) missing");
         raise Parameter_Error;
      end if;

      Output_Name := To_Unbounded_String (String'(Value (Option => 'o', Explicit_Required => True)));
   end Analyze_Options;

   -------------------
   -- Read_Fix_File --
   -------------------

   procedure Read_Fix_File (Fix_Name : String) is
      Fix_File       : File_Type;
      Repl_Start     : Positive;
      Fix_Pos        : Position;
      In_Replacement : Boolean := False;
      Kind           : Fix_Kind;

      procedure Fix_File_Error (Err_Col : Positive; Err_Message : String := "");
      pragma No_Return (Fix_File_Error);
      procedure Fix_File_Error (Err_Col : Positive; Err_Message : String := "") is
      begin
         if Err_Message /= "" then
            Message ("*** " & Err_Message);
         end if;

         raise Incorrect_Fix_File with Name (Fix_File) & ':'
                                     & Ada.Text_IO.Count'Image (Line (Fix_File)) & ':'
                                     & Positive'Image (Err_Col);
      end Fix_File_Error;

      procedure Parse (Source : in String; Pos : out Position; Kind : out Fix_Kind) is
      -- Expected syntax for a fix: <file name>:<Start line>:<Start col>:<Fix kind>[:<End line>:<End col>]
      -- Any line not matching this syntax returns Kind => Not_A_Fix
      --  (often diagnosed through the raising of Constraint_Error)
         Start : Positive;
         Stop  : Positive;

      begin
         --## rule off ASSIGNMENTS ## Must fill elements of record one by one, cannot use aggregate
         Start := Source'First;
         Stop  := Find (':', From => Start, Into => Source);
         if Stop = Start + 1 then
            -- Assume the ':' is from a drive letter
            Stop  := Find (':', From => Start + 2, Into => Source);
         end if;
         if Stop > Source'Last then
            Kind := Not_A_Fix;
            return;
         end if;
         Pos.File_Name := To_Unbounded_String (Source (Start .. Stop - 1));

         Start := Stop + 1;
         Stop  := Find (':', From => Start + 1, Into => Source);
         if Stop > Source'Last then
            Kind := Not_A_Fix;
            return;
         end if;
         Pos.Start_Pos.Line := Ada.Text_IO.Count'Value (Source (Start .. Stop - 1));

         Start := Stop + 1;
         Stop  := Find (':', From => Start + 1, Into => Source);
         if Stop > Source'Last then
            Kind := Not_A_Fix;
            return;
         end if;
         Pos.Start_Pos.Col := Ada.Text_IO.Count'Value (Source (Start .. Stop - 1));

         Start := Stop + 1;
         Stop  := Find (':', From => Start + 1, Into => Source);
         Kind := Fix_Kind'Value (Source (Start .. Stop - 1));

         -- If we are here, we can assume the line is really a fix message

         if Kind in Range_Fixes then
            Start := Stop + 1;
            Stop  := Find (':', From => Start + 1, Into => Source);
            Pos.End_Pos.Line := Ada.Text_IO.Count'Value (Source (Start .. Stop - 1));

            Start := Stop + 1;
            Stop  := Source'Last + 1;
            Pos.End_Pos.Col := Ada.Text_IO.Count'Value (Source (Start .. Stop - 1));
         else
            Pos.End_Pos := Pos.Start_Pos;
         end if;
         --## rule on ASSIGNMENTS
      exception
         when Constraint_Error =>
            Kind := Not_A_Fix;
            return;
      end Parse;

      procedure Add_Fix (Loc : Position; Fix : Fix_Descriptor) is
      -- Add fix to Kept_Fixes, unless there is an overlap.
      -- In case of overlap, keep the existing one, unless:
      --   - TBSL one is fully inside the other one: keep the inner one
      --   - Exactly one is an insertion whose position is equal to the end of the previous one
      --     or the start of the next one: keep both
      --   - one is a deletion that fully covers the other fix: keep the deletion
      -- Note: Refactor is ignored

         Previous_Curs : Fix_Maps.Cursor := Kept_Fixes.Floor (Loc);
         Previous_Pos  : Position;
         Previous_Fix  : Fix_Descriptor;
         Next_Curs     : Fix_Maps.Cursor := Kept_Fixes.Ceiling (Loc);
         Next_Pos      : Position;
         Next_Fix      : Fix_Descriptor;

      begin
         if Previous_Curs /= Fix_Maps.No_Element then
            Previous_Pos := Key (Previous_Curs);
            if Previous_Pos.File_Name = Loc.File_Name then
               Previous_Fix := Element (Previous_Curs);
               if Fix.Kind = Insert and Previous_Fix.Kind = Insert
                 and Loc.Start_Pos = Previous_Pos.Start_Pos
               then
                  -- Two inserts at the same position, ignore this one
                  Conflicts_Found := True;
                  Info ("    Conflict: two inserts at " & Image (Loc));
                  return;
               elsif Previous_Fix.Kind = Insert and Loc.Start_Pos = Previous_Pos.Start_Pos then
                  -- Only one is insert => OK
                  null;
               elsif Previous_Fix.Kind = Delete and then Previous_Pos.End_Pos >= Loc.End_Pos then
                  -- Already have a Delete fix that covers this fix => Ignore this fix
                  return;
               elsif Previous_Fix.Kind = Delete and then Fix.Kind = Delete
                     and then Previous_Pos.End_Pos >= Loc.Start_Pos
               then
                  -- Two overlaping Delete fixes => Replace by Delete covering both
                  -- Note: Previous does not extend farther than Loc, because it is caught by previous test
                  Kept_Fixes.Delete (Previous_Curs);
                  Kept_Fixes.Insert ((Loc.File_Name, Previous_Pos.Start_Pos, Loc.End_Pos), Create (Delete));
               elsif Previous_Pos.End_Pos >= Loc.Start_Pos then
                  -- Previous overlaps this one => ignore this one
                  Conflicts_Found := True;
                  Info ("    Conflict: overlap between " & Image (Previous_Pos) & " and " & Image (Loc));
                  return;
               end if;
            end if;
         end if;

         if Next_Curs /= Fix_Maps.No_Element then
            Next_Pos := Key (Next_Curs);
            if Next_Pos.File_Name = Loc.File_Name then
               Next_Fix := Element (Next_Curs);
               if Fix.Kind = Insert and Loc.Start_Pos = Next_Pos.Start_Pos then
                  -- Only one is insert => OK
                  null;
               elsif Fix.Kind = Delete and then Loc.End_Pos >= Next_Pos.End_Pos then
                  -- This is a Delete fix that covers a previous fix => remove previous
                  Kept_Fixes.Delete (Loc);
               elsif Next_Fix.Kind = Delete and then Fix.Kind = Delete
                 and then Loc.End_Pos >= Next_Pos.Start_Pos
               then
                  -- Two overlaping Delete fixes => Replace by Delete covering both
                  -- Note: Loc does not extend farther than Next, because it is caught by previous test
                  Kept_Fixes.Delete (Next_Curs);
                  Kept_Fixes.Insert ((Loc.File_Name, Loc.Start_Pos, Next_Pos.End_Pos), Create (Delete));
               elsif Loc.End_Pos >= Next_Pos.Start_Pos then
                  Conflicts_Found := True;
                  Info ("    Conflict: overlap between " & Image (Next_Pos) & " and " & Image (Loc));
                  return;
               end if;
            end if;
         end if;

         begin
            Kept_Fixes.Insert (Loc, Fix);
         exception
            when Constraint_Error =>
               -- Ooops, already have a fix with same location. Ignore this one
               Info ("    Conflict: already a fix at " & Image (Loc));
               Conflicts_Found := True;
         end;
      end Add_Fix;

   begin   -- Read_Fix_File
      Info ("*** Reading fixes from """ & Fix_Name & '"');

      Open (Fix_File, In_File, Fix_Name);

      loop    -- Exit on End_Error
         declare
            Line : constant String := Get_Line (Fix_File);
         begin
            if Line = "" then
               if In_Replacement then
                  Add_Fix (Fix_Pos, Create (Kind, Repl_Start, Replacements.Last_Index));
                  In_Replacement := False;
               end if;
            elsif Line (Line'First) = Marker then
               if In_Replacement then
                  Replacements.Append (To_Unbounded_String (Line (Line'First + 1 .. Line'Last)));
               else
                  Fix_File_Error (1, "Unexpected line format");
               end if;
            else
               if In_Replacement then
                  Add_Fix (Fix_Pos, Create (Kind, Repl_Start, Replacements.Last_Index));
                  In_Replacement := False;
               end if;

               Parse (Line, Fix_Pos, Kind);
               case Kind is
                  when Not_A_Fix =>
                     null;
                  when Delete =>
                     -- Add fix immediately, since there are no replacement lines to wait for
                     Add_Fix (Fix_Pos, Create (Kind));
                  when Refactor =>
                     null;
                  when Insert | Replace =>
                     In_Replacement := True;
                     Repl_Start     := Replacements.Last_Index + 1;
               end case;
            end if;
         end;
      end loop;
   exception
      when Name_Error =>
         Message ("*** " & Fix_Name & ": File not found");

      when End_Error =>
         if In_Replacement then
            Add_Fix (Fix_Pos, Create (Kind, Repl_Start, Replacements.Last_Index));
         end if;
         Close (Fix_File);
      when Occur : others =>
         if Is_Open (Fix_File) then
            Message ("*** " & Fix_Name & ':'
                     & Ada.Text_IO.Count'Image (Line (Fix_File)) & ':'
                     & Ada.Text_IO.Count'Image (Col (Fix_File)));
         end if;
         Message ("    " & Ada.Exceptions.Exception_Information (Occur));

         if Is_Open (Fix_File) then
            Close (Fix_File);
         end if;
         raise;
   end Read_Fix_File;

   ----------------
   -- Open_Files --
   ----------------

   -- Open input file and corresponding output file
   procedure Open_Files (Name : String) is
      use Ada.Directories;
   begin
      Info ("    Fixing " & Name);

      begin
         Open (Input_File, In_File, Name);
      exception
         when Ada.Text_IO.Name_Error =>
            raise;
         when others =>
            raise Incorrect_Fix_File with "Unable to open " & Name;
      end;

      if Output_Name /= Null_Unbounded_String then
         Create (Output_File, Out_File, To_String (Output_Name) & Simple_Name (Name));
         Info ("        to " & Ada.Text_IO.Name(Output_File));
         Set_Output (Output_File);
      end if;

   end Open_Files;

   -----------------
   -- Close_Files --
   -----------------

   -- Close input and output files
   procedure Close_Files is
   begin
      Close (Input_File);
      if Is_Open (Output_File) then
         Set_Output (Standard_Output);  -- Don't leave Current_Output on a closed file
         Close (Output_File);
      end if;
   end Close_Files;

   -----------------
   -- Print_Up_To --
   -----------------

   -- Copy input file to output file, up to Pos not included
   -- Pos.Col may be after the end of line, spaces added as necessary
   procedure Print_Up_To (Pos : Coordinate) is
      Char : Character;
   begin
      for L in Ada.Text_IO.Count range Line (Input_File) .. Pos.Line - 1 loop
         Put_Line (Get_Line (Input_File));
      end loop;
      for C in Ada.Text_IO.Count range Col (Input_File) .. Pos.Col - 1 loop
         if End_Of_Line (Input_File) then
            Char := ' ';
         else
            Get (Input_File, Char);
         end if;
         Put (Char);
      end loop;
   end Print_Up_To;

   -----------
   -- Print --
   -----------

   -- Print given Text to output file
   procedure Print (Text : String) is
   begin
      Put (Text);
   end Print;

   ----------------
   -- Print_Rest --
   ----------------

   -- Print up to end of input file
   procedure Print_Rest is
   begin
      loop
         Put_Line (Get_Line (Input_File));
      end loop;
   exception
      when End_Error =>
         null;
   end Print_Rest;


   -----------
   -- Break --
   -----------

   -- Insert line break into output file
   procedure Break is
   begin
      New_Line;
   end Break;

   -------------
   -- Skip_To --
   -------------

   -- Skip characters from input file, up to Pos included
   procedure Skip_To (Pos : in Coordinate) is
      Discarded : Character;
   begin
      Set_Line (Input_File, Pos.Line);
      if Col (Input_File) <= Pos.Col then
         Set_Col (Input_File, Pos.Col);
         -- We skip the last character by reading it, for the case where it is the last character in the line
         Get (Input_File, Discarded);
      end if;
   end Skip_To;

   --------------------------------------------
   -- Local data
   --------------------------------------------

   Fix_Pos      : Position;
   Previous_Pos : Position;
   Skipping_File : Unbounded_String := Null_Unbounded_String;
begin   --Adactl_Fix
   Analyze_Options;

   if Help_Option then
      Message (Version);
      Message ("usage: adactl_fix [-dhv] <fix-file> ... [-o <output-prefix>]");
      return;
   end if;

   for P in Natural range 1 .. Options.Parameter_Count loop
      Read_Fix_File (Options.Parameter (P));
   end loop;
   if First (Kept_Fixes) = Fix_Maps.No_Element then
      -- no file contained fixes
      return;
   end if;

   Previous_Pos := (Null_Unbounded_String, (0, 0), (0,0));
   for Current_Fix in Iterate (Kept_Fixes) loop
      Fix_Pos := Key (Current_Fix);
      if Fix_Pos.File_Name /= Skipping_File then
         begin
            if Fix_Pos.File_Name /= Previous_Pos.File_Name then
               if Previous_Pos /= (Null_Unbounded_String, (0, 0), (0, 0)) then
                  Print_Rest;
                  Close_Files;
               end if;
               Open_Files (To_String (Fix_Pos.File_Name));
            end if;

            Print_Up_To (Fix_Pos.Start_Pos);
            declare
               Fix : constant Fix_Descriptor := Element (Current_Fix);
            begin
               case Fix.Kind is
                  when Insert | Replace =>
                     Print (To_String (Replacements (Fix.First)));
                     for Line in Positive range Fix.First + 1 .. Fix.Last loop
                        Break;
                        Print (To_String (Replacements (Line)));
                     end loop;
                  when Delete | Refactor =>
                     null;
               end case;

               case Fix.Kind is
                  when Insert | Refactor =>
                     null;
                  when Replace | Delete =>
                     Skip_To (Fix_Pos.End_Pos);
               end case;
            end;
            Previous_Pos := Fix_Pos;
         exception
            when Name_Error =>
               Skipping_File := Fix_Pos.File_Name;
               Message ("*** Unable to open " & To_String (Skipping_File) & ", all corresponding fixes ignored");
         end;
      end if;
   end loop;
   if Previous_Pos /= (Null_Unbounded_String, (0, 0), (0, 0)) then
      Print_Rest;
      Close_Files;
   end if;

   if Conflicts_Found then
      Message ("*** Some fixes were not applied due to conflicts, run AdaControl again");
   end if;

exception
   when Parameter_Error =>
      null;   -- Message already printed
              -- TBSL: set return status to 1
   when Occur: Options.Options_Error =>
      Message ("*** Option error: " & Ada.Exceptions.Exception_Message (Occur));
   when Occur : Incorrect_Fix_File =>
      Message ("*** Incorrect_Fix_File: " & Ada.Exceptions.Exception_Message (Occur));
   when Occur : others =>
      Message ("*** Unexpected exception: " & Ada.Exceptions.Exception_Message (Occur));
      Utilities.Stack_Traceback (Occur);
end Adactl_Fix;
