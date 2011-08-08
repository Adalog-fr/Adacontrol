----------------------------------------------------------------------
--  Framework.Queries - Package specification                       --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2007. The Ada --
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

-- Asis
with
  Asis;

package Framework.Queries is
   -- This package contain high level ASIS queries, but unlike
   -- Thick_Queries, it is application dependant, i.e. the queries
   -- may refer to the Framework.

   function Enclosing_Package_Name (Rule_Id : Wide_String; N : in Asis.Name) return Wide_String;
   -- If N is declared immediately within a package specification, returns the Full_Name_Image
   -- of the package (in uppercase)
   -- Otherwise, returns ""
   --
   -- Cannot be made application independant, because it calls Uncheckable in some cases.

   function System_Value (Name : Wide_String) return Asis.Declaration;
   -- Returns the declaration of the element of package System whose name is passed as parameter.
   -- Provided name must be all upper-case.
   -- It is a Failure if the name is not found.
   -- If this function is called from elaboration code, put a pragma Elaborate_All (Framework.Queries)
   --
   -- Cannot be made application independant, because it needs the Asis context from Framework.
end Framework.Queries;
