----------------------------------------------------------------------
--  Framework.Symbol_Table - Package specification                  --
--                                                                  --
--  This software  is (c) Adalog  2004-2007. The Ada  Controller is --
--  free software;  you can redistribute it and/or  modify it under --
--  terms of  the GNU  General Public License  as published  by the --
--  Free Software Foundation; either version 2, or (at your option) --
--  any later version.   This unit is distributed in  the hope that --
--  it will be  useful, but WITHOUT ANY WARRANTY;  without even the --
--  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
--  PURPOSE.  See the GNU  General Public License for more details. --
--  You  should have  received a  copy  of the  GNU General  Public --
--  License distributed  with this  program; see file  COPYING.  If --
--  not, write to  the Free Software Foundation, 59  Temple Place - --
--  Suite 330, Boston, MA 02111-1307, USA.                          --
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

-- ASIS
with
  Asis;

-- Adalog
with
  Framework.Scope_Manager;

package Framework.Symbol_Table is

   type Root_Content is tagged limited null record;
   -- This declaration is here for use in the private part of the generic,
   -- no use for the user

   Max_Instances : constant := 5;
   -- Max allowed number of instantiations of Data_Access.
   -- Exceeding this number will immediately result in Failure
   -- If necessary, just increase the above constant, no other change required.

   Not_In_Table : exception;


   -- Instantiate this generic to allow to associate any data with any declared element.
   -- Data associated to an element are automatically freed when the scope of the element is
   -- exited. Note that this happens *after* calling Framework.Plugs.Exit_Scope, therefore
   -- rules that need to inspect how an entity has been used can do so by plugging some
   -- processing into Exit_Scope.
   generic
      type Content is private;
   package Data_Access is
      procedure Store (Element : Asis.Element; Content_Value : Content);
      function  Fetch (Element : Asis.Element) return Content;
      -- Raises Not_In_Table if not present
      function  Fetch (Element : Asis.Element; Default : Content) return Content;
      -- Returns Default if not present

      function Is_Present (Element : Asis.Element) return Boolean;
      function Depth_Of (Element : Asis.Element) return Framework.Scope_Manager.Scope_Range;
      -- Raises Not_In_Table if not present

      procedure Clear;
      -- Remove all data stored by this package

      -- Apply Action to all entities declared within the current scope
      generic
         with procedure Action (Entity : Asis.Defining_Name; Content_Value : in out Content);
      procedure On_Every_Entity_From_Scope;
   private
      type Content_Hook is new Root_Content with
         record
            The_Content : Content;
         end record;
   end Data_Access;

   --
   --  Declarations below this line are for the use of the framework
   --

   procedure Exit_Scope (Element : in Asis.Element);
   -- Clean-up elements from current scope

end Framework.Symbol_Table;
