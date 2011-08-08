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

   Version : constant Wide_String := "1.8r7";

   -------------------------------------------------------------------
   -- General dimensioning constants                                --
   -------------------------------------------------------------------

   -- These constants define limits about what "reasonable" programs may contain.
   -- They can be used by rules to limit some capabilities.
   -- These limits are arbitrary and can be changed at will, no other change is needed.

   Max_Controls_For_Rule : constant := 100;
   -- For rules that need an upper bound to the number of times they can
   -- be specified in a control

   Max_Parameters : constant := 30;
   -- Maximum number of parameters declared by a subprogram or an entry

   Max_Loop_Nesting : constant := 20;
   -- Maximum depth of nested loops

   Max_Scopes : constant := 50;
   -- Maximum depth of scopes nesting.


   -------------------------------------------------------------------
   -- The ASIS context                                              --
   -------------------------------------------------------------------

   Adactl_Context : aliased Asis.Context;

   -------------------------------------------------------------------
   --  General types for rules                                      --
   -------------------------------------------------------------------

   type Control_Index is range 0 ..  Max_Controls_For_Rule;
   type Control_Index_Set is array (Control_Index range 1 .. 32) of Boolean; -- Purposedly limited
   pragma Pack (Control_Index_Set);

   type Control_Kinds is (Check, Search, Count);
   type Control_Kinds_Set is array (Control_Kinds) of Boolean;
   pragma Pack (Control_Kinds_Set);
   Empty_Control_Kinds_Set : constant Control_Kinds_Set := (others => False);

   type Uncheckable_Kinds is (False_Positive, False_Negative, Missing_Unit);
   subtype Uncheckable_Consequence is Uncheckable_Kinds range False_Positive .. False_Negative;

   -------------------------------------------------------------------
   --  Location                                                     --
   -------------------------------------------------------------------

   -- A location is the position of an element in a file

   type Location is private;
   Null_Location : constant Location;

   function Create_Location (File         : in Wide_String;
                             First_Line   : in Natural;
                             First_Column : in Natural) return Location;
   function Get_Location (E : in Asis.Element) return Location;
   -- Returns location of an element

   function Get_Previous_Word_Location (E : in Asis.Element) return Location;
   -- Returns the location of the first "word" (identifier of keyword) that immediately
   -- precedes E.

   function Get_Next_Word_Location (E : in Asis.Element) return Location;
   -- Returns the location of the first "word" (identifier of keyword) that immediately
   -- follows E.

   function Get_File_Name (L : in Location) return Wide_String;
   -- Returns location file name

   function Get_First_Line (L : in Location) return Natural;
   -- Returns location first line

   function Get_First_Column (L : in Location) return Natural;
   -- Returns location first column

   Default_Short_Name : Boolean := False;
   function Image (L : in Location; Short_Name : in Boolean := Default_Short_Name) return Wide_String;
   -- Returns image of a location
   -- i.e. file:1:1
   -- If Short_Name = True, strip File name from any path

   function Value (S : in Wide_String) return Location;
   -- Returns location value of a string
   -- raises Constraint_Error for an incorrect input string


   -------------------------------------------------------------------
   --  Entity_Specification                                         --
   -------------------------------------------------------------------

   -- An Entity_Specification is the structure that corresponds to
   -- the specification of an Ada entity in the command language

   type Entity_Specification is private;
   type Entity_Specification_Kinds is (Box, Equal, Regular_Id, All_Id);

   function Entity_Specification_Kind (Entity : in Entity_Specification) return Entity_Specification_Kinds;

   function Image   (Entity : in Entity_Specification) return Wide_String;
   function Value   (Name   : in Wide_String)          return Entity_Specification;
   function Matches (Name   : in Asis.Element; Entity : in Entity_Specification) return Boolean;
   -- Appropriate element kinds for Matches:
   --   like Matching_Context, see below


   -------------------------------------------------------------------
   --  Contexts                                                     --
   -------------------------------------------------------------------

   -- A context is a rule-specific information

   type Root_Context is tagged null record;

   procedure Clear (Context : in out Root_Context);
   -- The default (inherited) Clear does nothing.
   -- Redefine clear if you extend Rule_Context with fields (like maps
   -- or access value) that need finalization when the context store is cleared.

   Empty_Context       : constant Root_Context := (null record);
   No_Matching_Context : constant Root_Context'Class;

   -- A basic context is what most rules need
   -- It simply (logically) holds the Control_Kind and Control_Label, but we need extra
   -- mechanisms to allow specifying "Count" in addition to any "Search" or "Check"
   -- without causing double definitions in the context store. Therefore, this has to
   -- be private, and a constructor is provided.
   type Basic_Rule_Context is new Root_Context with private;

   package Basic is
      -- This package to prevent this operations from being primitive.
      -- Since New_Context could have been called Basic_New_Context, it is not
      -- really annoying to have to write Basic.New_Context...
      function New_Context (With_Type : in Control_Kinds; With_Label : in Wide_String) return Basic_Rule_Context;
   end Basic;
   function Control_Kind  (Context : in Basic_Rule_Context) return Control_Kinds;
   function Control_Label (Context : in Basic_Rule_Context) return Wide_String;


   -------------------------------------------------------------------
   --  Context_Store                                                --
   -------------------------------------------------------------------

   -- A context_store associates a context to a specific entity specification
   -- (or by cheating a little bit) to any string.

   type Context_Store is limited private;
   Already_In_Store : exception;
   Not_In_Store     : exception; -- Raised by Dissociate and Association only

   procedure Balance (Store : in out Context_Store);
   procedure Clear   (Store : in out Context_Store);

   procedure Associate (Into          : in out Context_Store;
                        Specification : in     Entity_Specification;
                        Context       : in     Root_Context'Class;
                        Additive      : in     Boolean := False);
   -- If Additive is False, only one context can be associated to the specification
   --    (or Already_In_Store is raised)
   -- If Additive is True, several /different/ contexts can be associated to a specification
   --    (Already_In_Store is raised if the same context value is associated twice)

   procedure Associate_Default (Into    : in out Context_Store;
                                Context : in     Root_Context'Class);
   -- If a default context is defined, it will be returned by Matching_Context if
   -- the name is not matched, instead of No_Matching_Context.
   -- Already_In_Store is raised if a default is already associated

   function Matching_Context (Into : in Context_Store;
                              Name : in Asis.Element) return Root_Context'Class;
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

   function Extended_Matching_Context (Into : in Context_Store;
                                       Name : in Asis.Element) return Root_Context'Class;
   -- Same as Matching_Context, but extends the search to the original name for renamings
   -- and corresponding generics if Name is an instantiation or part of an instantiation

   function Next_Matching_Context (Into : in Context_Store) return Root_Context'Class;
   --  Use to retrieve other contexts of an additive association
   --  Returns the default (or No_Matching_Context) when exhausted

   function Last_Matching_Name (Into : in Context_Store) return Wide_String;
   -- Name that found the context in the last query to Matching_Context or Association

   procedure Update (Into    : in out Context_Store;
                     Context : in     Root_Context'Class);
   -- Updates context last returned by Matching_Context or Association

   function  Association (Into          : in Context_Store;
                          Specification : in Entity_Specification) return Root_Context'Class;
   -- Returns the first Context associated to the specification
   -- (currently used only for non-additive associations; this may change in the future)
   --  Returns the default (or No_Matching_Context) when not found

   procedure Dissociate (From          : in out Context_Store;
                         Specification : in     Entity_Specification);
   -- Removes context associated to specification


   -------------------------------------------------------------------
   --  Banned entities                                              --
   -------------------------------------------------------------------

   function Is_Banned (Element : in Asis.Element; For_Rule : in Wide_String) return Boolean;
   -- Returns True if Element is declared within a banned unit for rule For_Rule.
   -- A banned unit is one which is the target of an inhibit all command.

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
   Null_Location : constant Location := (Null_Unbounded_Wide_String, 0, 0);


   --
   -- Entity_Specification
   --

   type Entity_Specification (Kind : Entity_Specification_Kinds := Regular_Id) is
      record
         case Kind is
            when Box | Equal =>
               null;
            when Regular_Id | All_Id =>
               Specification : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         end case;
      end record;

   --
   -- Context
   --

   --  This way of defining No_Matching_Context ensures that it cannot
   --  be used for anything else than comparisons.
   type Not_Found_Context is new Root_Context with null record;
   No_Matching_Context : aliased constant Root_Context'Class := Not_Found_Context'(null record);

   type Basic_Rule_Context is new Root_Context with
      record
         Ctl_Kind    : Control_Kinds;
         Ctl_Label   : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         With_Count  : Boolean;
         Count_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      end record;

   --
   -- Context_Store
   --

   type Context_Access is access Root_Context'Class;
   type Context_Node;
   type Context_Node_Access is access Context_Node;
   type Context_Node is
      record
         Value : Context_Access;
         Next  : Context_Node_Access;
      end record;
   package Context_Tree is new Binary_Map (Key_Type   => Unbounded_Wide_String,
                                           Value_Type => Context_Node_Access);

   type Auto_Pointer (Self : access Context_Store) is limited null record;
   -- Rosen trick strikes again...

   type Context_Store is
      record
         This             : Auto_Pointer (Context_Store'Access);
         Simple_Names     : Context_Tree.Map;
         Qualified_Names  : Context_Tree.Map;
         Default          : Context_Node_Access;
         Last_Returned    : Context_Node_Access;
         Last_Name        : Unbounded_Wide_String;
      end record;

   --
   -- Inhibition
   --

   type Inhibited_Rule is new Root_Context with
      record
         Rule_Name : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Is_Banned : Boolean;
      end record;
   Inhibited : Framework.Context_Store;

end Framework;
