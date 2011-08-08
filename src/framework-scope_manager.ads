----------------------------------------------------------------------
--  Framework.Scope_Manager - Package specification                 --
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
   type Iterator_Mode is (All_Scopes, Current_Scope_Only);

   Max_Scopes : constant := 50;
   -- Maximum depth of scopes nesting.
   -- The value is arbitrary, but should be (largely) sufficient.
   -- Can be safely increased by just changing the constant if necessary.

   type Scope_Range is range 0 .. Max_Scopes;
   -- Level 0 is useful as a special value.
   -- The value returned by Current_Depth is always at least 1.

   function Current_Depth return Scope_Range;
   function Current_Scope return Asis.Element;
   function Active_Scopes return Asis.Element_List;

   -- The ASIS element at depth Current_Depth

   -- The following declaration is for use in the private part of Scoped_Data,
   -- no use for the users of this package. (No harm either).
   type Scoping_Procedure is access procedure (Scope : Asis.Element);

   generic
      type Data (<>) is private;
   package Scoped_Store is
      -- This package manages user data that are to be associated to a scope.
      -- It is managed as a stack. Data associated to a scope are automatically
      -- deleted when the scope is exited.
      -- Data pushed when processing a package spec, a generic package spec, a
      -- task spec or a protected spec is temporarily removed at the end of the spec,
      -- and restored at the beginning of the corresponding body. It is deleted at the
      -- end of the body.

      procedure Push (Info : in Data);
      -- Adds Info on top of stack, associated to current scope

      -- Iterator
      -- Data are returned by Get_Current_Data from top to bottom but not removed
      -- from the stack.
      --
      -- If the mode of Reset is Current_Scope_Only, only data associated to the
      -- current scope are returned by the iterator. If the mode is All_Scopes,
      -- all data are returned.
      --
      -- Next moves to the next element. If the iterator is exhausted (i.e.
      -- Data_Available is False), it raises Constraint_Error.
      --
      -- After a call to Delete_Current, the iterator moves to the next position.
      --
      -- Is_Current_Transmitted_From_Spec is true iff the current scope is a body
      -- and the data originated from the corresponding specification.
      --
      -- It is possible to add new data while iterating; since they are added
      -- on top (i.e. above the current position of the iterator), it does not
      -- change the behaviour of the iterator.

      procedure Reset (Mode : Iterator_Mode);
      procedure Next;
      function  Data_Available                   return Boolean;
      function  Get_Current_Data                 return Data;
      function  Is_Current_Transmitted_From_Spec return Boolean;
      procedure Update_Current (Info : in Data);
      procedure Delete_Current;

   private
      -- The following declarations are here because they are not allowed
      -- in a generic body.

      procedure Enter_Scope  (Scope : Asis.Element);
      procedure Exit_Scope   (Scope : Asis.Element);
      procedure Clear_All    (Scope : Asis.Element);
      -- The parameter of Clear_All is not used, it is there just to
      -- match the profile.

      Enter_Access : constant Scoping_Procedure := Enter_Scope'Access;
      Exit_Access  : constant Scoping_Procedure := Exit_Scope'Access;
      Clear_Access : constant Scoping_Procedure := Clear_All'Access;
   end Scoped_Store;

   --
   --  Declarations below this line are for the use of the framework
   --

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit);
   procedure Enter_Scope (Scope : Asis.Element);
   procedure Exit_Scope  (Scope : Asis.Element);

   procedure Reset;
   -- Cleans up all active scope and all Scoped_Store data
   -- To be used only in the case of a premature termination due
   -- to an unexpected exception.

end Framework.Scope_Manager;
