----------------------------------------------------------------------
--  Framework.Control_Manager - Package specification               --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2008.           --
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
  Ada.Strings.Wide_Unbounded;

-- Adalog
private with
  Binary_Map;

package Framework.Control_Manager is

   -------------------------------------------------------------------
   --  Contexts                                                     --
   -------------------------------------------------------------------

   -- A context is a rule-specific information

   type Root_Context is tagged null record;

   function Equivalent_Values (Left, Right : Root_Context) return Boolean;
   -- Compares the significative parts of a contex (i.e. those that must be unique
   -- for an additive association - see below)
   -- Default is regular equality

   procedure Clear (Context : in out Root_Context);
   -- The default (inherited) Clear does nothing.
   -- Redefine clear if you extend Root_Context with fields (like maps
   -- or access value) that need finalization when the context store is cleared.

   No_Matching_Context : constant Root_Context'Class;

   -- A basic context is what most rules need
   -- It simply (logically) holds the Control_Kind and Control_Label, but we need extra
   -- mechanisms to allow specifying "Count" in addition to any "Search" or "Check"
   -- without causing double definitions in the context store. Therefore, this has to
   -- be private, and a constructor is provided.
   type Basic_Rule_Context is new Root_Context with
      record
         Ctl_Kind    : Control_Kinds;
         Ctl_Label   : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         With_Count  : Boolean;
         Count_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      end record;

   package Basic is
      -- This package to prevent these operations from being primitive.
      -- Since New_Context could have been called Basic_New_Context, it is not
      -- really annoying to have to write Basic.New_Context...
      function New_Context (With_Type : in Control_Kinds; With_Label : in Wide_String) return Basic_Rule_Context;

      -- This allows to merge a "Count" context with a non-"Count" context.
      -- Returns True if the merge is possible, False otherwise (if both or neither are "Count"
      function Merge_Context (Context    : in out Basic_Rule_Context;
                              With_Type  : in     Control_Kinds;
                              With_Label : in     Wide_String) return Boolean;
   end Basic;

   Null_Context : constant Root_Context := (null record);
   -- For rules that need a simple set of entities, use a Context_Store
   -- of Null_Context


   -------------------------------------------------------------------
   --  Context_Store                                                --
   -------------------------------------------------------------------

   -- A context_store associates a context to a specific entity specification
   -- (or by cheating a little bit) to any string.

   type Context_Store is limited private;
   Already_In_Store : exception;
   Not_In_Store     : exception; -- Raised by Dissociate and Association only

   procedure Balance  (Store : in out Context_Store);
   procedure Clear    (Store : in out Context_Store);
   function  Is_Empty (Store : in     Context_Store) return Boolean;

   procedure Associate (Into          : in out Context_Store;
                        Specification : in     Entity_Specification;
                        Context       : in     Root_Context'Class;
                        Additive      : in     Boolean := False);
   -- If Additive is False, only one context can be associated to the specification
   --    (or Already_In_Store is raised)
   -- If Additive is True, several /different/ contexts can be associated to a specification
   --    (Already_In_Store is raised if the same context value is associated twice)

   function Matching_Context (Into      : in Context_Store;
                              Name      : in Asis.Element;
                              Extend_To : in Extension_Set := No_Extension) return Root_Context'Class;
   -- Retrieves the context associated to the element if there is a match
   -- Returns No_Matching_Context otherwise (including if Name is a Nil_Element).
   -- If Extend_To (Instances), extends the search to corresponding generic element if Name is an instantiation
   --    or part of an instantiation
   -- If Extend_To (Renaming), extends the search to the original name for renamings
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


   function Last_Matching_Name (Into : in Context_Store) return Wide_String;
   -- Name that found the context in the last query to Matching_Context or Association
   -- "" if the last query was not succesfull

   type Matching_Kind is (Original, Renamed, Instance, From_Instance, None);
   function Last_Matching_Kind (Into : in Context_Store) return Matching_Kind;
   -- Kind of match in the last query to Matching_Context or Association
   -- None if the last query was not succesfull

   procedure Update (Into    : in out Context_Store;
                     Context : in     Root_Context'Class);
   -- Updates context last returned by Matching_Context or Association

   function  Association (Into          : in Context_Store;
                          Specification : in Entity_Specification) return Root_Context'Class;
   -- Returns the first Context associated to the specification
   --  Returns No_Matching_Context when not found

   function  Association (Into : in Context_Store;
                          Key  : in Wide_String) return Root_Context'Class;
   -- Idem, but works with plain strings rather than entities.
   -- Especially useful when contexts are associated to "keys" rather than entities

   procedure Dissociate (From          : in out Context_Store;
                         Specification : in     Entity_Specification);
   -- Removes context associated to specification

   -- Iterator for additive associations
   -- Must be initialized by a Create from an instantiation of
   -- Framework.Control_Manager.Generic_Context_Iterator (hence the unknown discriminant)
   type Context_Iterator (<>) is private;

   procedure Reset              (Iter      : in out Context_Iterator;
                                 Name      : in     Asis.Element;
                                 Extend_To : in     Extension_Set := No_Extension);
   procedure Reset              (Iter      : in out Context_Iterator; Name : in Entity_Specification);
   function  Value              (Iter      : in     Context_Iterator) return Root_Context'Class;
   function  Last_Matching_Name (Iter      : in     Context_Iterator) return Wide_String;
   procedure Next               (Iter      : in out Context_Iterator);
   function  Is_Exhausted       (Iter      : in     Context_Iterator) return Boolean;

   type Iterator_Position is private;
   procedure Save (Iter     : in Context_Iterator; Into : out Iterator_Position);
   function Value (Position : in Iterator_Position) return Root_Context'Class;

private

   --
   -- Control_ID
   --

   type Control_ID is new Natural;

   --
   -- Context
   --

   --  This way of defining No_Matching_Context ensures that it cannot
   --  be used for anything else than comparisons.
   type Not_Found_Context is new Root_Context with null record;
   No_Matching_Context : aliased constant Root_Context'Class := Not_Found_Context'(null record);

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

   type Context_Store is limited
      record
         This             : Auto_Pointer (Context_Store'Access);
         Simple_Names     : Context_Tree.Map;
         Qualified_Names  : Context_Tree.Map;
         Last_Returned    : Context_Node_Access;
         Last_Name        : Unbounded_Wide_String;
         Last_Kind        : Matching_Kind;
      end record;

   type Context_Iterator is access all Context_Store;
   type Iterator_Position is new Context_Access;

end Framework.Control_Manager;
