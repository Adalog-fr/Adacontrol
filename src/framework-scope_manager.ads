----------------------------------------------------------------------
--  Framework.Scope_Manager - Package specification                 --
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

--  This package provides facilities for rules that need to manage
--  information associated to scopes. A scope is a construct that can
--  contain declarations.
--
--  Note that when processing context clauses, the current scope is the one
--  of the following library unit. This is what the user would expect,
--  although from an ASIS point of view, the associated construct has not
--  yet been entered.

-- Asis
with
  Asis;
package Framework.Scope_Manager is
   type Iterator_Mode is (All_Scopes, Unit_Scopes, Current_Scope_Only);

   type Scope_Range is range 0 .. Max_Scopes;
   -- Level 0 is useful as a special value.
   -- The value returned by Current_Depth is always at least 1.

   type Scope_List is array (Scope_Range range <>) of Asis.Element;

   type Declaration_Origin is (Same_Unit, Specification, Parent);
   -- Declaration_Origin is Specification if the current scope is a body and the info
   -- comes from the corresponding specification.

   function Current_Depth   return Scope_Range;
   function Current_Scope   return Asis.Element;
   function Enclosing_Scope return Asis.Element;
   function Active_Scopes   return Scope_List;
   function In_Private_Part (Scope : Scope_Range := Current_Depth) return Boolean;
   function In_Context_Clauses return Boolean;
   function Is_Current_Scope_Global return Boolean;
   -- Current scope is global if itself and all enclosing scopes are all
   -- packages or generic packages

   type Scoping_Procedure is access procedure (Scope : Asis.Element);
   -- This declaration is for use in the private part of Scoped_Store,
   -- no use for the users of this package. (No harm either).

   generic
      type Data (<>) is private;
      with function Equivalent_Keys (L, R : Data) return Boolean is "=";
   package Scoped_Store is
      -- This package manages user data that are to be associated to a scope.
      -- It is managed as a stack. Data associated to a scope are automatically
      -- deleted when the scope is exited.
      -- Data pushed when processing a package spec, a generic package spec, a
      -- task spec or a protected spec is temporarily removed at the end of the spec,
      -- and restored at the beginning of the corresponding body. It is deleted at the
      -- end of the body, unless it is a compilation unit.

      procedure Push (Info : in Data);
      -- Adds Info on top of stack, associated to current scope
      procedure Push_Enclosing (Info : in Data);
      -- Adds Info associated to the enclosing scope of the current scope
      -- If Current_Scope is a library unit, the info is associated to the scope level 0,
      -- and the corresponding Current_Data_Scope returns Nil_Element

      -- Iterator
      -- Data are returned by Get_Current_Data from top to bottom but not removed
      -- from the stack.
      --
      -- In the first form of Reset, the iterator is set to the top of the statck.
      -- If the mode of Reset is Current_Scope_Only, only data associated to the current scope
      -- are returned by the iterator.
      -- If the mode of Reset is Unit_Scopes, only data associated to the scope of the current
      -- compilation unit and above are returned.
      -- If the mode is All_Scopes, all data are returned.
      --
      -- In the second form of Reset, the iterator is initialized on the data with
      -- Equivalent_Keys to the provided Info (Data_Available returns False if not found).
      -- If the mode of Reset is Current_Scope_Only, only data associated to the scope
      -- of Info are returned by the iterator.
      -- If the mode of Reset is Unit_Scopes, only data associated to the scope
      -- of the compilation unit of Info and above are returned by the iterator.
      -- If the mode is All_Scopes, all data from Info to the bottom of the stack are returned.
      --
      -- Continue is like the second form of Reset, starting from the current position.
      --
      -- Next moves to the next element. If the iterator is exhausted (i.e.
      -- Data_Available is False), it raises Constraint_Error.
      --
      -- After a call to Delete_Current, the iterator moves to the next position.
      --
      -- Is_Current_Transmitted_From_Spec is true iff the current scope is a body
      -- and the data originated from the corresponding specification.
      --
      -- It is possible to add new data with Push while iterating; since they are added
      -- on top (i.e. above the current position of the iterator), it does not
      -- change the behaviour of the iterator.
      -- The same does not hold when adding data with Push_Enclosing, which should therefore
      -- not be used while iterating.

      procedure Reset (Mode : Iterator_Mode);
      procedure Reset (Info : Data; Mode : Iterator_Mode);
      procedure Continue (Mode : Iterator_Mode);
      procedure Next;
      function  Data_Available     return Boolean;
      function  Current_Data       return Data;
      function  Current_Data_Level return Scope_Range;
      function  Current_Data_Scope return Asis.Element;
      function  Current_Origin     return Declaration_Origin;
      procedure Update_Current (Info : in Data);
      procedure Delete_Current;

   private
      -- The following declarations are here because they are not allowed
      -- in a generic body.

      procedure Enter_Scope   (Scope : Asis.Element);
      procedure Enter_Private (Scope : Asis.Element);
      procedure Exit_Scope    (Scope : Asis.Element);
      procedure Clear_All     (Scope : Asis.Element);
      -- The parameters of Enter_Private and Clear_All are not used,
      -- they are here just to match the profile.

      Enter_Access   : constant Scoping_Procedure := Enter_Scope'Access;
      Private_Access : constant Scoping_Procedure := Enter_Private'Access;
      Exit_Access    : constant Scoping_Procedure := Exit_Scope'Access;
      Clear_Access   : constant Scoping_Procedure := Clear_All'Access;
   end Scoped_Store;

   ----------------------------------------------------------------------------
   --
   --  Declarations below this line are for the use of the framework
   --

   procedure Enter_Unit  (Unit  : in Asis.Compilation_Unit);
   procedure Enter_Scope (Scope : in Asis.Element; Is_Unit : Boolean := False);
   procedure Enter_Private_Part;
   procedure Exit_Unit   (Unit  : in Asis.Compilation_Unit);
   procedure Exit_Scope  (Scope : in Asis.Element; Force : Boolean := False);
   procedure Exit_Context_Clauses;

   procedure Reset;
   -- Cleans up all active scope and all Scoped_Store data
   -- To be used only in the case of a premature termination due
   -- to an unexpected exception.

end Framework.Scope_Manager;
