----------------------------------------------------------------------
--  Framework.Symbol_Table - Package specification                  --
--                                                                  --
--  This software is (c) Adalog 2004-2007.                          --
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

-- ASIS
with
  Asis;

package Framework.Symbol_Table is

   type Root_Content is tagged limited null record;
   -- This declaration is here for use in the private part of the generic,
   -- no use for the user

   Max_Instances : constant := 10;
   -- Max allowed number of instantiations of Data_Access.
   -- Exceeding this number will immediately result in Failure
   -- If necessary, just increase the above constant, no other change required.


   --
   -- Declarations common to all instantiations
   --
   type Scope_Kinds is (Declaration_Scope, Visibility_Scope, All_Scopes);
   subtype Usage_Scope_Kinds is Scope_Kinds range Declaration_Scope .. Visibility_Scope;
   -- The Declaration scope is the scope where the entity is declared
   -- The Visibility scope is the outermost scope where the entity is visible
   -- They are different for entities declared in package specs and formal parameters
   -- The global scope (where library elements are declared) is represented by Nil_Element

   Not_In_Table   : exception;
   Delete_Current : exception;
   -- Works like Binary_Map.Delete_Current


   -- Instantiate this generic to allow to associate any data with any declared element.
   -- Data associated to an element are automatically freed when the (visibility) scope of
   -- the element is exited. Note that this happens *after* calling Framework.Plugs.Exit_Scope,
   -- therefore rules that need to inspect how an entity has been used can do so by plugging
   -- some processing into Exit_Scope.
   generic
      type Content is private;
   package Data_Access is
      procedure Store (Element : Asis.Element; Content_Value : Content);
      function  Fetch (Element : Asis.Element) return Content;
      -- Raises Not_In_Table if not present
      function  Fetch (Element : Asis.Element; Default : Content) return Content;
      -- Returns Default if not present
      procedure Delete (Element : Asis.Element);

      function Is_Present (Element : Asis.Element) return Boolean;
      function Scope_Of   (Element : Asis.Element; Scope_Kind : Usage_Scope_Kinds := Declaration_Scope)
                           return Asis.Element;
      -- Raises Not_In_Table if not present

      procedure Clear;
      -- Remove all data stored by this package

      -- Apply Action to all entities whose declaration or visibility scope is the current scope
      -- Raise Delete_Current if the current node is to be removed.
      generic
         with procedure Action (Entity : Asis.Defining_Name; Content_Value : in out Content);
      procedure On_Every_Entity_From_Scope (Scope_Kind : Scope_Kinds);
   private
      type Content_Hook is new Root_Content with
         record
            The_Content : Content;
         end record;
   end Data_Access;

   ----------------------------------------------------------------------------------------
   --
   --  Declarations below this line are for the use of the framework
   --

   procedure Exit_Scope (Element : in Asis.Element);
   -- Clean-up elements from current scope

end Framework.Symbol_Table;
