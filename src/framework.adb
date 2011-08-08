----------------------------------------------------------------------
--  Framework - Package body                                        --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Code Cheker  is free software;  you can redistribute  it and/or --
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
  Ada.Strings.Wide_Fixed,
  Ada.Unchecked_Deallocation;

-- ASIS
with
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

package body Framework is
   Default_Context : constant Context_Node_Access
     := new Context_Node'(new Rule_Context'Class'(No_Matching_Context), null);

   procedure Free is new Ada.Unchecked_Deallocation (Rule_Context'Class, Context_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Context_Node, Context_Node_Access);

   ---------------
   -- Associate --
   ---------------

   procedure Associate (Into          : in out Context_Store;
                        Specification : in     Entity_Specification;
                        Context       : in     Rule_Context'Class;
                        Additive      : in     Boolean := False)
   is
      use Context_Tree;
      Node : Context_Node_Access := null;
   begin
      if Specification.Is_All then
         if Is_Present (Into.Simple_Names, Specification.Specification) then
            if Additive then
               Node := Fetch (Into.Simple_Names, Specification.Specification);
            else
               raise Already_In_Store;
            end if;
         end if;

         Add (Into.Simple_Names,
              Specification.Specification,
              new Context_Node'(new Rule_Context'Class'(Context), Node));
      else
         if Is_Present (Into.Qualified_Names, Specification.Specification) then
            if Additive then
               Node := Fetch (Into.Qualified_Names, Specification.Specification);
            else
               raise Already_In_Store;
            end if;
         end if;

         Add (Into.Qualified_Names,
              Specification.Specification,
              new Context_Node'(new Rule_Context'Class'(Context), Node));
      end if;
   end Associate;

   -----------------------
   -- Associate_Default --
   -----------------------

   procedure Associate_Default (Into    : in out Context_Store;
                                Context : in     Rule_Context'Class) is
   begin
      Into.Default := new Context_Node'(New Rule_Context'Class'(Context), null);
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

   ----------------------
   -- Matching_Context --
   ----------------------

   function Matching_Context (Into : Context_Store;
                              Name : Asis.Element) return Rule_Context'Class
   is
      use Context_Tree, Utilities, Thick_Queries;
      use Asis, Asis.Elements, Asis.Declarations, Asis.Expressions;

      Name_Definition : Defining_Name;
      Name_Image      : Unbounded_Wide_String;
      Result          : Context_Node_Access;
   begin

      -- Special case for pragmas: they have no "name" in the Asis sense.
      if Element_Kind (Name) = A_Pragma then
         -- No overloading, no "all"
         Name_Image := To_Unbounded_Wide_String (To_Upper (Pragma_Name_Image (Name)));

         Result := Fetch (Into.Qualified_Names,
                          Name_Image,
                          Default_Value => Default_Context);

         -- Similarly for attribute references.
      elsif Expression_Kind (Name) = An_Attribute_Reference then
         -- No overloading, no "all"
         Name_Image := To_Unbounded_Wide_String (To_Upper
                                                   (Expressions.Name_Image
                                                      (A4G_Bugs.Attribute_Designator_Identifier
                                                         (Name))));

         Result := Fetch (Into.Qualified_Names,
                          Name_Image,
                          Default_Value => Default_Context);

      else
         -- Not a pragma or attribute => must be a name
         if Element_Kind (Name) = A_Defining_Name then
            Name_Definition := Name;
         else
            case Expression_Kind (Name) is
               when A_Selected_Component =>
                  Name_Definition := Corresponding_Name_Definition (Selector (Name));

               when An_Identifier | An_Operator_Symbol | An_Enumeration_Literal =>
                  Name_Definition := Corresponding_Name_Definition (Name);

                  if Is_Nil (Name_Definition) then
                     --  Must be an implicitely declared operator symbol, for which
                     --  the implementation does not provide a "fake" declaration.
                     --  We do not handle these for the moment
                     Into.This.Self.Last_Returned := Default_Context;
                     Into.This.Self.Last_Name     := Null_Unbounded_Wide_String;
                     return No_Matching_Context;
                  end if;

               when An_Explicit_Dereference =>
                  -- This is always dynamic, and therefore can't match anything
                  return No_Matching_Context;

               when others =>
                  Failure ("Matching_Context: not a name", Name);
            end case;
         end if;

         -- Search from most specific to least specific

         -- Search without "all", with overloading
         Name_Image := To_Unbounded_Wide_String (To_Upper
                                                   (Full_Name_Image
                                                      (Name_Definition, With_Profile => True)));
         Result := Fetch (Into.Qualified_Names,
                          Name_Image,
                          Default_Value => Default_Context);

         -- Search without "all", without overloading
         if Result.Value.all = No_Matching_Context then
            Name_Image := To_Unbounded_Wide_String
              (To_Upper (Full_Name_Image (Name_Definition, With_Profile => False)));
            Result := Fetch (Into.Qualified_Names,
                             Name_Image,
                             Default_Value => Default_Context);
         end if;

         -- Search with "all", with overloading
         if Result.Value.all = No_Matching_Context then
            Name_Image := To_Unbounded_Wide_String
              (To_Upper (Defining_Name_Image (Name_Definition)
                           & Profile_Image (Name_Definition,
                                            With_Profile => True)));
            Result := Fetch (Into.Simple_Names,
                             Name_Image,
                             Default_Value => Default_Context);
         end if;

         -- Search with "all", without overloading
         if Result.Value.all = No_Matching_Context then
            Name_Image := To_Unbounded_Wide_String
              (To_Upper (Defining_Name_Image (Name_Definition)));
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

   ---------------------------
   -- Next_Matching_Context --
   ---------------------------

   function Next_Matching_Context (Into : Context_Store) return Rule_Context'Class is
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

   function Last_Matching_Name (Into : Context_Store) return Wide_String is
   begin
      return To_Wide_String (Into.Last_Name);
   end Last_Matching_Name;

   ------------
   -- Update --
   ------------

   procedure Update (Into          : in out Context_Store;
                     Context       : in     Rule_Context'Class) is
   begin
      Into.Last_Returned.Value.all := Context;
   end Update;

   -----------------
   -- Association --
   -----------------

   function Association (Into          : in Context_Store;
                         Specification : in Entity_Specification) return Rule_Context'Class is
      use Context_Tree;
      Result : Context_Node_Access;
   begin
      if Specification.Is_All then
         if Is_Present (Into.Simple_Names, Specification.Specification) then
            Result := Fetch (Into.Simple_Names, Specification.Specification);
         else
            raise Not_In_Store;
         end if;

      else
         if Is_Present (Into.Qualified_Names, Specification.Specification) then
            Result := Fetch (Into.Qualified_Names, Specification.Specification);
         else
            raise Not_In_Store;
         end if;
      end if;

      Into.This.Self.Last_Returned := Result;
      Into.This.Self.Last_Name     := Specification.Specification;
      return Result.Value.all;
   end Association;

   ----------------
   -- Dissociate --
   ----------------

   procedure Dissociate (From          : in out Context_Store;
                         Specification : in     Entity_Specification) is
      use Context_Tree;
      Temp : Context_Node_Access;
   begin
      if Specification.Is_All then
         if Is_Present (From.Simple_Names, Specification.Specification) then
            Temp := Fetch (From.Simple_Names, Specification.Specification);
            Free (Temp.Value);
            Free (Temp);
            Delete (From.Simple_Names, Specification.Specification);
         else
            raise Not_In_Store;
         end if;

      else
         if Is_Present (From.Qualified_Names, Specification.Specification) then
            Temp := Fetch (From.Qualified_Names, Specification.Specification);
            Free (Temp.Value);
            Free (Temp);
            Delete (From.Qualified_Names, Specification.Specification);
         else
            raise Not_In_Store;
         end if;
      end if;
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

   -------------------
   -- File_Location --
   -------------------

   function File_Location (E : in Asis.Element) return Wide_String is
      use Asis.Elements;
      use Asis.Compilation_Units;
   begin
      return Text_Name (Enclosing_Compilation_Unit (E));
   end File_Location;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (E : in Asis.Element) return Location is
      use Ada.Strings.Wide_Unbounded;
      use Asis.Text;

      S : constant Span := Element_Span (E);
   begin
      return (To_Unbounded_Wide_String (File_Location (E))
              , S.First_Line, S.First_Column);
   end Get_Location;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (L : in Location) return Wide_String is
      use Ada.Strings.Wide_Unbounded;
   begin
      return To_Wide_String (L.File_Name);
   end Get_File_Name;

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

   function Image (L : in Location) return Wide_String is
      use Ada.Strings;
      use Ada.Strings.Wide_Unbounded;
      use Ada.Strings.Wide_Fixed;
      use Asis.Text;
   begin
      if L.File_Name = Null_Unbounded_Wide_String then
         return Trim (Character_Position_Positive'Wide_Image (L.First_Column), Left);
      else
         return To_Wide_String (L.File_Name)
           & ":"
           & Trim (Line_Number_Positive'Wide_Image (L.First_Line), Left)
           & ":"
           & Trim (Character_Position_Positive'Wide_Image (L.First_Column), Left);
      end if;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (S : Wide_String) return Location is
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

end Framework;
