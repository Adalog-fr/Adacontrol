----------------------------------------------------------------------
--  Framework - Package body                                        --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
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
  Ada.Strings.Wide_Fixed,
  Ada.Unchecked_Deallocation;

-- ASIS
with
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Units_List,
  Utilities;

package body Framework is
   Default_Context : constant Context_Node_Access
     := new Context_Node'(new Root_Context'Class'(No_Matching_Context), null);

   procedure Free is new Ada.Unchecked_Deallocation (Root_Context'Class, Context_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Context_Node, Context_Node_Access);

   procedure Release (List : in out Context_Node_Access) is

      Next : Context_Node_Access;
   begin
      while List /= null loop
         Next := List.Next;
         Clear (List.Value.all);
         Free (List.Value);
         Free (List);
         List := Next;
      end loop;
   end Release;

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

   -------------------
   -- Basic_Context --
   -------------------

   package body Basic is
      function New_Context (With_Type : in Control_Kinds; With_Label : in Wide_String) return Basic_Rule_Context is
      begin
         return (Ctl_Kind    => With_Type,
                 Ctl_Label   => To_Unbounded_Wide_String (With_Label),
                 With_Count  => False,
                 Count_Label => Null_Unbounded_Wide_String);
      end New_Context;

   end Basic;

   ---------------
   -- Rule_Type --
   ---------------

   function Control_Kind (Context : in Basic_Rule_Context) return Control_Kinds is
   begin
      return Context.Ctl_Kind;
   end Control_Kind;

   ----------------
   -- Rule_Label --
   ----------------

   function Control_Label (Context : in Basic_Rule_Context) return Wide_String is
   begin
      return To_Wide_String (Context.Ctl_Label);
   end Control_Label;

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

   function Matches (Name : in Asis.Element; Entity : in Entity_Specification) return Boolean is
      -- This implementation of Matches is a bit violent, but it ensures consistency with Matching_Context
      Junk_Store : Context_Store;
      Result     : Boolean;
   begin
      Associate (Junk_Store, Entity, Root_Context'(null record));
      Result := Matching_Context (Junk_Store, Name) /= No_Matching_Context;
      Clear (Junk_Store);
      return Result;
   end Matches;

   ---------------
   -- Associate --
   ---------------

   procedure Associate (Into          : in out Context_Store;
                        Specification : in     Entity_Specification;
                        Context       : in     Root_Context'Class;
                        Additive      : in     Boolean := False)
   is
      use Utilities;

      procedure Add_To_Map (Names : in out Context_Tree.Map) is
         use Context_Tree;
         Node        : Context_Node_Access := null;
         Current     : Context_Node_Access := null;
         Count_Label : Unbounded_Wide_String;
      begin
         if Is_Present (Names, Specification.Specification) then
            if Additive then
               Node := Fetch (Names, Specification.Specification);

               -- Check value not already in the list
               Current := Node;
               while Current /= null loop
                  if Current.Value.all = Context then
                     exit;
                  end if;
                  Current := Current.Next;
               end loop;
            else
               Current := Fetch (Names, Specification.Specification);
            end if;
         end if;

         -- Here, Current designates the context if already in Names,
         -- is null otherwise
         if Current = null then
            Current := new Context_Node'(new Root_Context'Class'(Context), Node);
         else
            -- Check for the special case to allow Count in addition to another rule type
            if Context not in Basic_Rule_Context'Class or Current.Value.all not in Basic_Rule_Context'Class then
               raise Already_In_Store;
            end if;

            if (Basic_Rule_Context (Context).Ctl_Kind = Count)
              = (Basic_Rule_Context (Current.Value.all).Ctl_Kind = Count)
            then
               -- Both are Count, or neither
               raise Already_In_Store;
            end if;

            -- One and only one is a Count
            if Basic_Rule_Context (Current.Value.all).Ctl_Kind = Count then
               Count_Label :=  Basic_Rule_Context (Current.Value.all).Ctl_Label;
               Free (Current.Value);
               Current.Value := new Root_Context'Class'(Context);
            else
               Count_Label := Basic_Rule_Context (Context).Ctl_Label;
            end if;
            Basic_Rule_Context (Current.Value.all).With_Count  := True;
            Basic_Rule_Context (Current.Value.all).Count_Label := Count_Label;
         end if;

         Add (Names, Specification.Specification, Current);
      end Add_To_Map;

   begin
      case Specification.Kind is
         when Regular_Id =>
            Add_To_Map (Into.Qualified_Names);
         when All_Id =>
            Add_To_Map (Into.Simple_Names);
         when others =>
            Failure ("Associate : bad kind");
      end case;
   end Associate;

   -----------------------
   -- Associate_Default --
   -----------------------

   procedure Associate_Default (Into    : in out Context_Store;
                                Context : in     Root_Context'Class) is
   begin
      if Into.Default /= null then
         raise Already_In_Store;
      end if;

      Into.Default := new Context_Node'(new Root_Context'Class'(Context), null);
   end Associate_Default;

   -------------
   -- Balance --
   -------------

   procedure Balance (Store : in out Context_Store) is
      use Context_Tree;
   begin
      Balance (Store.Simple_Names);
      Balance (Store.Qualified_Names);
   end Balance;

   -----------
   -- Clear --
   -----------

   procedure Clear   (Store : in out Context_Store) is
      procedure Clear is new Context_Tree.Generic_Clear_And_Release (Release);
   begin
      Clear (Store.Simple_Names);
      Clear (Store.Qualified_Names);
      Release (Store.Default);
      Store.Last_Returned := null;
      Store.Last_Name     := Null_Unbounded_Wide_String;
   end Clear;

   ----------------------
   -- Matching_Context --
   ----------------------

   function Matching_Context (Into : in Context_Store;
                              Name : in Asis.Element) return Root_Context'Class
   is
      use Context_Tree, Utilities, Thick_Queries;
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;

      function Any_Name_Image (E : Asis.Element) return Wide_String is
         use Asis.Declarations;
      begin
         if Element_Kind (Name) = A_Defining_Name then
            return Defining_Name_Image (E);
         else
            return A4G_Bugs.Name_Image (E);
         end if;
      end Any_Name_Image;

      Good_Name       : Asis.Element;
      Name_Enclosing  : Asis.Element;
      Name_Image      : Unbounded_Wide_String;
      Name_Extra      : Unbounded_Wide_String; -- Initialized to Null_Unbounded_Wide_String
      Result          : Context_Node_Access;
      Is_Predefined_Op: Boolean := False;
   begin
      if Is_Nil (Name) then
         return No_Matching_Context;
      end if;

      -- Find the place where the name is used (skipping all selections)
      Name_Enclosing := Enclosing_Element (Name);
      while Expression_Kind (Name_Enclosing) = A_Selected_Component loop
         Name_Enclosing := Enclosing_Element (Name_Enclosing);
      end loop;

      if Expression_Kind (Name) = A_Selected_Component then
         Good_Name := Selector (Name);
      else
         Good_Name := Name;
      end if;

      -- Special case for pragmas: they have no "name" in the Asis sense.
      if Element_Kind (Name) = A_Pragma then
         -- No overloading, no "all"
         Name_Image := To_Unbounded_Wide_String (To_Upper (Pragma_Name_Image (Good_Name)));
         Result     := Fetch (Into.Qualified_Names,
                              Name_Image,
                              Default_Value => Default_Context);

      -- Only "all" (without overloading) is acceptable for dispatching calls
      elsif Is_Dispatching_Call (Name_Enclosing)
        and then Is_Equal (Good_Name, Called_Simple_Name (Name_Enclosing))
      then
         Name_Image := To_Unbounded_Wide_String (To_Upper (A4G_Bugs.Name_Image (Good_Name)));
         Result     := Fetch (Into.Simple_Names,
                              Name_Image,
                              Default_Value => Default_Context);

      else
         if Element_Kind (Good_Name) = An_Expression then
            if Expression_Kind (Good_Name) = An_Attribute_Reference then
               while Expression_Kind (Good_Name) = An_Attribute_Reference loop
                  Name_Extra := ''' & To_Upper (Attribute_Name_Image (Good_Name)) & Name_Extra;
                  Good_Name  := Prefix (Good_Name);
               end loop;

               if Expression_Kind (Good_Name) = A_Selected_Component then
                  Good_Name := Selector (Good_Name);
               end if;
            end if;

            case Expression_Kind (Good_Name) is
               when An_Identifier | An_Enumeration_Literal =>
                  null;
               when An_Operator_Symbol =>
                  declare
                     Op_Decl : constant Asis.Element := Corresponding_Name_Declaration (Good_Name);
                  begin
                     if Is_Nil (Op_Decl) then
                        Is_Predefined_Op := True;
                     else
                        -- This should be a function !
                        case Declaration_Kind (Op_Decl) is
                           when A_Function_Declaration
                              | A_Function_Body_Declaration
                              | A_Function_Renaming_Declaration
                              | A_Generic_Function_Renaming_Declaration
                              | A_Function_Body_Stub
                              | A_Generic_Function_Declaration
                              | A_Function_Instantiation
                              | A_Formal_Function_Declaration
                                =>
                              Is_Predefined_Op := False;
                           when others =>
                              -- A4G_Bug, this known to happen only on predefined "&" operators
                              Is_Predefined_Op := True;
                              A4G_Bugs.Trace_Bug ("Matching_Context: Bad declaration of predefined operator");
                        end case;
                     end if;
                  end;
               when others =>
                  -- This can happen if we were given something that is not appropriate,
                  -- or for something dynamic used as the prefix of an attribute.
                  return No_Matching_Context;
            end case;
         end if;

         -- Search from most specific to least specific
         -- Predefined operators cannot be searched with profile

         -- Search without "all", with overloading
         if Is_Predefined_Op then
            Result := Default_Context;
         else
            Name_Image := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Good_Name, With_Profile => True)))
                          & Name_Extra;
            Result := Fetch (Into.Qualified_Names,
                             Name_Image,
                             Default_Value => Default_Context);
         end if;

         -- Search without "all", without overloading
         if Result.Value.all = No_Matching_Context then
            Name_Image := To_Unbounded_Wide_String (To_Upper
                                                    (Full_Name_Image
                                                     (Good_Name, With_Profile => False)))
                          & Name_Extra;
            Result := Fetch (Into.Qualified_Names,
                             Name_Image,
                             Default_Value => Default_Context);
         end if;

         -- Search with "all", with overloading
         if not Is_Predefined_Op and Result.Value.all = No_Matching_Context then
            Name_Image := To_Unbounded_Wide_String (To_Upper
                                                    (Any_Name_Image (Good_Name)
                                                   & Profile_Image (Good_Name, With_Profile => True)))
                          & Name_Extra;
            Result := Fetch (Into.Simple_Names,
                             Name_Image,
                             Default_Value => Default_Context);
         end if;

         -- Search with "all", without overloading
         if Result.Value.all = No_Matching_Context then
            Name_Image := To_Unbounded_Wide_String (To_Upper (Any_Name_Image (Good_Name)))
                          & Name_Extra;
            Result := Fetch (Into.Simple_Names,
                             Name_Image,
                             Default_Value => Default_Context);
         end if;

         -- For attribute references, search attribute name
         if Result.Value.all = No_Matching_Context
           and then Expression_Kind (Name) = An_Attribute_Reference
         then
            Name_Image := To_Unbounded_Wide_String (''' & To_Upper (Attribute_Name_Image (Name)));
            Result := Fetch (Into.Simple_Names,
                             Name_Image,
                             Default_Value => Default_Context);
        end if;

      end if;

      -- Last resort:
      -- Check default
      if Result.Value.all = No_Matching_Context then
         if Into.Default /= null then
            Result     := Into.Default;
            Name_Image := Null_Unbounded_Wide_String;
         end if;
      end if;

      Into.This.Self.Last_Returned := Result;
      Into.This.Self.Last_Name     := Name_Image;
      return Result.Value.all;
   end Matching_Context;

   -------------------------------
   -- Extended_Matching_Context --
   -------------------------------

   function Extended_Matching_Context (Into : in Context_Store;
                                       Name : in Asis.Element) return Root_Context'Class is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Thick_Queries;
      Good_Name        : Asis.Element;
      Name_Declaration : Asis.Declaration;
   begin
      if Is_Nil (Name) then
         return No_Matching_Context;
      elsif Expression_Kind (Name) = A_Selected_Component then
         Good_Name := Selector (Name);
      else
         Good_Name := Name;
      end if;

      -- 1) Check provided name
      declare
         Result : constant Root_Context'Class := Matching_Context (Into, Good_Name);
      begin
         if Result /= No_Matching_Context then
            return Result;
         end if;
      end;

      -- No extended context for attribute references
      -- (at least for the moment)
      if Expression_Kind (Good_Name) = An_Attribute_Reference then
         return No_Matching_Context;
      end if;

      if Element_Kind (Good_Name) = A_Defining_Name then
         Name_Declaration := Enclosing_Element (Good_Name);
      else
         Name_Declaration := Corresponding_Name_Declaration (Good_Name);
      end if;

      -- 2) if name is a renaming, try the ultimate name
      if Declaration_Kind (Name_Declaration) in A_Renaming_Declaration then
         declare
            Result : constant Root_Context'Class := Matching_Context (Into, Ultimate_Name (Good_Name));
         begin
            if Result /= No_Matching_Context then
               return Result;
            end if;
         end;
      end if;

      -- 3) if name is an instantiation, try the corresponding generic
      if Declaration_Kind (Name_Declaration) in A_Generic_Instantiation then
         declare
            Result : constant Root_Context'Class := Matching_Context (Into,
                                                                      Generic_Unit_Name (Name_Declaration));
         begin
            if Result /= No_Matching_Context then
               return Result;
            end if;
         end;
      end if;

      -- 4) if name is from an instantiation, try the corresponding generic element
      if Is_Part_Of_Instance (Name_Declaration) then
         declare
            Result : constant Root_Context'Class := Matching_Context (Into,
                                                                      Corresponding_Generic_Element (Good_Name));
         begin
            if Result /= No_Matching_Context then
               return Result;
            end if;
         end;
       end if;

       -- Nothing found
       return No_Matching_Context;
   end Extended_Matching_Context;

   ---------------------------
   -- Next_Matching_Context --
   ---------------------------

   function Next_Matching_Context (Into : in Context_Store) return Root_Context'Class is
   begin
      Into.This.Self.Last_Returned := Into.This.Self.Last_Returned.Next;
      if Into.This.Self.Last_Returned = null then
         return Default_Context.Value.all;
      else
         return Into.This.Self.Last_Returned.Value.all;
      end if;
   end Next_Matching_Context;

   ------------------------
   -- Last_Matching_Name --
   ------------------------

   function Last_Matching_Name (Into : in Context_Store) return Wide_String is
   begin
      return To_Wide_String (Into.Last_Name);
   end Last_Matching_Name;

   ------------
   -- Update --
   ------------

   procedure Update (Into          : in out Context_Store;
                     Context       : in     Root_Context'Class) is
   begin
      Into.Last_Returned.Value.all := Context;
   end Update;

   -----------------
   -- Association --
   -----------------

   function Association (Into          : in Context_Store;
                         Specification : in Entity_Specification) return Root_Context'Class is
      use Context_Tree, Utilities;
      Result : Context_Node_Access;
   begin
      case Specification.Kind is
         when Regular_Id =>
            Result := Fetch (Into.Qualified_Names, Specification.Specification, Default_Value => Into.Default);
         when All_Id =>
            Result := Fetch (Into.Simple_Names, Specification.Specification, Default_Value => Into.Default);
         when others =>
            Failure ("Association: bad kind");
      end case;

      if Result = null then
         Result := Default_Context;
      end if;

      Into.This.Self.Last_Returned := Result;
      Into.This.Self.Last_Name     := Specification.Specification;
      return Result.Value.all;
   end Association;

   -----------
   -- Clear --
   -----------

   procedure Clear (Context : in out Root_Context) is
      pragma Unreferenced (Context);
   begin
      null;
   end Clear;

   ----------------
   -- Dissociate --
   ----------------

   procedure Dissociate (From          : in out Context_Store;
                         Specification : in     Entity_Specification) is
      use Context_Tree, Utilities;
      Temp : Context_Node_Access;
   begin
      case Specification.Kind is
         when Regular_Id =>
            if not Is_Present (From.Qualified_Names, Specification.Specification) then
               raise Not_In_Store;
            end if;

            Temp := Fetch (From.Qualified_Names, Specification.Specification);
            Release (Temp);
            Delete (From.Qualified_Names, Specification.Specification);
         when All_Id =>
            if not Is_Present (From.Simple_Names, Specification.Specification) then
               raise Not_In_Store;
            end if;

            Temp := Fetch (From.Simple_Names, Specification.Specification);
            Release (Temp);
            Delete (From.Simple_Names, Specification.Specification);
         when others =>
            Failure ("Dissociate : bad kind");
      end case;
   end Dissociate;

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

   --------------------------------
   -- Get_Previous_Word_Location --
   --------------------------------

   Identifier_Chars : constant Ada.Strings.Wide_Maps.Wide_Character_Set
     := Ada.Strings.Wide_Maps.To_Set (Ranges => (('a', 'z'), ('A', 'Z'), ('0', '9'), ('_', '_')));

   function Get_Previous_Word_Location (E : in Asis.Element) return Location is
      use Asis.Text;
      E_Span : constant Span := Element_Span (E);
      L      : Line_Number;
      Start  : Character_Position;
      Result : Location;

      function Find_Word_Start (Line : in Wide_String) return Character_Position is
         use Ada.Strings.Wide_Maps;
      begin
         for I in reverse Line'Range loop
            if Is_In (Line (I), Identifier_Chars) then
               for J in reverse Positive range Line'First+1 .. I loop
                  if not Is_In (Line (J-1), Identifier_Chars) then
                     return J;
                  end if;
               end loop;
               return Line'First;
            end if;
         end loop;

         return 0;
      end Find_Word_Start;
   begin
      L := E_Span.First_Line;
      Start := Find_Word_Start (Non_Comment_Image (Lines (E, L, L)(L)) (1 .. E_Span.First_Column-1));
      while Start = 0 loop
         L     := L - 1;
         Start := Find_Word_Start (Non_Comment_Image (Lines (E, L, L)(L)));
      end loop;

      Result := Get_Location (E);
      Result.First_Line   := L;
      Result.First_Column := Start;
      return Result;
   end Get_Previous_Word_Location;

   ----------------------------
   -- Get_Next_Word_Location --
   ----------------------------

   function Get_Next_Word_Location (E : in Asis.Element) return Location is
      use Asis.Text;
      E_Span : constant Span := Element_Span (E);
      L      : Line_Number;
      Start  : Character_Position;
      Result : Location;

      function Find_Word_Start (Line : in Wide_String) return Character_Position is
         use Ada.Strings.Wide_Maps;
      begin
         for I in Line'Range loop
            if Is_In (Line (I), Identifier_Chars) then
               return I;
            end if;
         end loop;

         return 0;
      end Find_Word_Start;
   begin
      L := E_Span.Last_Line;
      declare
         The_Line : constant Asis.Program_Text := Non_Comment_Image (Lines (E, L, L) (L));
      begin
         Start := Find_Word_Start (The_Line (E_Span.Last_Column + 1 .. The_Line'Last));
      end;
      while Start = 0 loop
         L     := L + 1;
         Start := Find_Word_Start (Non_Comment_Image (Lines (E, L, L)(L)));
      end loop;

      Result := Get_Location (E);
      Result.First_Line   := L;
      Result.First_Column := Start;
      return Result;
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

   function Image (L : in Location; Short_Name : in Boolean := Default_Short_Name) return Wide_String is
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

   begin
      if L = Null_Location then
         Failure ("Image of null location");
      elsif L.File_Name = Null_Unbounded_Wide_String then
         return Integer_Img (L.First_Column);
      else
         return Strip (To_Wide_String (L.File_Name))
           & ":"
           & Integer_Img (L.First_Line)
           & ":"
           & Integer_Img (L.First_Column);
      end if;
   end Image;

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

   ---------------
   -- Is_Banned --
   ---------------

   function Is_Banned (Element : in Asis.Element; For_Rule : in Wide_String) return Boolean is
      use Asis.Declarations, Asis.Elements;
      Context : constant Root_Context'Class
        := Matching_Context (Inhibited, Names (Unit_Declaration (Enclosing_Compilation_Unit (Element)))(1));
   begin
      if Context = No_Matching_Context then
         return False;
      end if;

      declare
         Inhibit_Context : Inhibited_Rule renames Inhibited_Rule (Context);
      begin
         if To_Wide_String (Inhibit_Context.Rule_Name) = For_Rule
           or else To_Wide_String (Inhibit_Context.Rule_Name) = "ALL"
         then
            return Inhibit_Context.Is_Banned;
         end if;
         loop
            declare
               New_Context : constant Root_Context'Class := Next_Matching_Context (Inhibited);
            begin
               exit when New_Context = No_Matching_Context;
               if To_Wide_String (Inhibited_Rule (New_Context).Rule_Name) = For_Rule then
                  return Inhibited_Rule (New_Context).Is_Banned;
               end if;
            end;
         end loop;
      end;

      return False;
   end Is_Banned;

begin
   Units_List.Initialize (Adactl_Context'Access);
end Framework;
