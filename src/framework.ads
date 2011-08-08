----------------------------------------------------------------------
--  Framework - Package specification                               --
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
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis,
  Asis.Text;

--  Adalog
with
  Binary_Map;

pragma Elaborate_All (Binary_Map);

package Framework is

   --
   --  General types for rules
   --

   type Rule_Types is (Check, Search, Count);

   --------------------------------------
   --  Location                        --
   --------------------------------------

   -- A location is the position of an element in a file

   type Location is private;
   Null_Location : constant Location;

   function Create_Location (File         : in Wide_String;
                             First_Line   : in Natural;
                             First_Column : in Natural) return Location;
   function Get_Location (E : in Asis.Element) return Location;
   -- Returns location of an element

   function Get_File_Name (L : in Location) return Wide_String;
   -- Returns location file name

   function Get_First_Line (L : in Location) return Natural;
   -- Returns location first line

   function Image (L : in Location) return Wide_String;
   -- Returns image of a location
   -- i.e. file:1:1

   function Value (S : Wide_String) return Location;
   -- Returns location value of a string
   -- raises Constraint_Error for an incorrect input string


   --------------------------------------
   --  Entity_Specification            --
   --------------------------------------

   -- An Entity_Specification is the structure that corresponds to
   -- the specification of an Ada entity in the command language

   type Entity_Specification is private;
   function Image (Entity : Entity_Specification) return Wide_String;
   function Value (Name : Wide_String) return Entity_Specification;
   function Is_Box (Entity : Entity_Specification) return Boolean;

   --------------------------------------
   --  Rule_Context                    --
   --------------------------------------

   -- A context is a rule-specific information associated
   -- to an entity specification

   type Rule_Context is tagged null record;
   procedure Clear (Context : in out Rule_Context);
   -- The default (inherited) Clear does nothing.
   -- Redefine clear if you extend Rule_Context with fields (like maps
   -- or access value) that need finalization when the context store is cleared.

   Empty_Context       : constant Rule_Context := (null record);
   No_Matching_Context : constant Rule_Context'Class;

   -- A simple context is what most rules will need
   type Simple_Context is new Rule_Context with
      record
         Rule_Type  : Rule_Types;
         Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      end record;

   --------------------------------------
   --  Context_Store                   --
   --------------------------------------

   -- A context_store associates a context to a specific entity specification

   type Context_Store is limited private;
   Already_In_Store : exception;
   Not_In_Store     : exception; -- Raised by Dissociate and Association only

   procedure Balance (Store : in out Context_Store);
   procedure Clear   (Store : in out Context_Store);

   procedure Associate (Into          : in out Context_Store;
                        Specification : in     Entity_Specification;
                        Context       : in     Rule_Context'Class;
                        Additive      : in     Boolean := False);
   -- If Additive is False, only one context can be associated to the specification
   --    (or Already_In_Store is raised)
   -- If Additive is True, several contexts can be associated to a specification

   procedure Associate_Default (Into    : in out Context_Store;
                                Context : in     Rule_Context'Class);
   -- If a default context is defined, it will be returned by Matching_Context if
   -- the name is not matched, instead of No_Matching_Context.

   function Matching_Context (Into : Context_Store;
                              Name : Asis.Element) return Rule_Context'Class;
   -- Retrieves the context associated to the element if there is a match
   -- Returns No_Matching_Context otherwise (including if Name is a Nil_Element).
   --
   -- Appropriate Element_Kinds for Name:
   --   A_Pragma (condition searched on pragma name)
   --   A_Defining_Name
   --   An_Expression
   --
   -- Appropriate Expression_Kinds:
   --      A_Selected_Component (condition searched on the selector)
   --      An_Identifier
   --      An_Attribute_Reference (condition searched on Name'Attribute)
   --
   -- Matches are, in decreasing order of priority:
   --   The name matches with overloading
   --   The name matches without overloading
   --   The name matches an "all" association with overloading
   --   The name matches an "all" association without overloading
   --   There is a default association (matches everything)

   function Extended_Matching_Context (Into : Context_Store;
                                       Name : Asis.Element) return Rule_Context'Class;
   -- Same as Matching_Context, but extends the search to corresponding generics if Name is
   -- an instantiation or part of an instantiation
   -- Restricted to identifiers.

   function Next_Matching_Context (Into : Context_Store) return Rule_Context'Class;
   --  Use to retrieve other contexts of an additive association
   --  Returns the default (or No_Matching_Context) when exhausted

   function Last_Matching_Name (Into : Context_Store) return Wide_String;
   -- Name that found the context in the last query to Matching_Context

   procedure Update (Into    : in out Context_Store;
                     Context : in     Rule_Context'Class);
   -- Updates context last returned by Matching_Context or Association

   function  Association (Into          : in Context_Store;
                          Specification : in Entity_Specification) return Rule_Context'Class;
   -- Returns the first Context associated to the specification
   -- (currently used only for non-additive associations; this may change in the future)

   procedure Dissociate (From          : in out Context_Store;
                         Specification : in     Entity_Specification);
   -- Removes context associated to specification

private
   use Ada.Strings.Wide_Unbounded;

   --
   -- Location
   --

   type Location is record
      File_Name    : Unbounded_Wide_String;
      First_Line   : Asis.Text.Line_Number        := 0;
      First_Column : Asis.Text.Character_Position := 0;
   end record;
   Null_Location : constant Location := (Null_Unbounded_Wide_string, 0, 0);


   --
   -- Entity_Specification
   --

   type Entity_Specification (Is_Box : Boolean := False) is
      record
         case Is_Box is
            when True =>
               null;
            when False =>
               Is_All        : Boolean;
               Specification : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         end case;
      end record;

   --
   -- Context
   --

   --  This way of defining No_Matching_Context ensures that it cannot
   --  be used for anything else than comparisons.
   type Not_Found_Context is new Rule_Context with null record;
   No_Matching_Context : constant Rule_Context'Class
     := Not_Found_Context'(null record);

   --
   -- Context_Store
   --

   type Context_Access is access Rule_Context'Class;
   type Context_Node;
   type Context_Node_Access is access Context_Node;
   type Context_Node is
      record
         Value : Context_Access;
         Next  : Context_Node_Access;
      end record;
   package Context_Tree is new Binary_Map
     (Key_Type   => Unbounded_Wide_String,
      Value_Type => Context_Node_Access);

   type Auto_Pointer (Self : access Context_Store) is limited null record;
   -- Rosen trick strikes again...

   type Context_Store is
      record
         This             : Auto_Pointer (Context_Store'access);
         Simple_Names     : Context_Tree.Map;
         Qualified_Names  : Context_Tree.Map;
         Default          : Context_Node_Access;
         Last_Returned    : Context_Node_Access;
         Last_Name        : Unbounded_Wide_String;
      end record;

end Framework;
