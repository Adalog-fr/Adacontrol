----------------------------------------------------------------------
--  Framework.Queries - Package specification                       --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2012.           --
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
   Ada.Wide_Characters.Handling,
   Ada.Strings.Wide_Unbounded;

-- Asis
with
  Asis;

-- Adalog
with
  Thick_Queries;

package Framework.Queries is
   -- This package contain high level ASIS queries, but unlike
   -- Thick_Queries, it is application dependant, i.e. the queries
   -- may refer to the Framework.

   function To_Key (Name : Asis.Name) return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String is
     (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (Thick_Queries.Full_Name_Image (Name,
                                                                                          With_Profile => True)));
   -- To use Name as a key in a binary map or other data structure. Works fine to compare elements, but depends on
   -- casing

   function To_Key_Upper (Name : Asis.Name) return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String is
     (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (Ada.Wide_Characters.Handling.To_Upper
                                                           (Thick_Queries.Full_Name_Image (Name,
                                                                                           With_Profile => True))));
   -- Same as before, but the key is forced to upper case. Slightly less efficient, but necessary when the name
   -- has to be matched against something whose casing is not known (like a name provided by the user in a rule)

   function Enclosing_Package_Name (Rule_Id : Wide_String; N : in Asis.Name) return Wide_String;
   -- If N is declared immediately within a package specification, returns the Full_Name_Image
   -- of the package (with profile).
   -- Otherwise, returns ""
   --
   -- Cannot be made application independant, because it calls Uncheckable in some cases.

   procedure Init_Standard (A_Unit : Asis.Compilation_Unit);
   -- For some mysterious reason (A4G bug?) Standard cannot be retrieved normally using
   -- Library_Unit_Declaration. Since we need it for Standard_Value below, we retrieve it
   -- as the parent unit of some compilation unit.
   -- Therefore, this procedure must be called with any compilation unit before calling
   -- Standard_Value

   function Standard_Value (Name : Wide_String) return Asis.Declaration;
   -- Returns the declaration of the element of package Standard whose name is passed as parameter.
   -- Character literals defined in Standard are not accessible however (there are too many of them)
   -- Provided name must be all upper-case.
   -- It is a Failure if the name is not found.
   -- If this function is called from elaboration code, put a pragma Elaborate_All (Framework.Queries)
   --
   -- Cannot be made application independant, because it needs the Asis context from Framework.

   function System_Value (Name : Wide_String) return Asis.Declaration;
   -- Returns the declaration of the element of package System whose name is passed as parameter.
   -- Provided name must be all upper-case.
   -- It is a Failure if the name is not found.
   -- If this function is called from elaboration code, put a pragma Elaborate_All (Framework.Queries)
   --
   -- Cannot be made application independant, because it needs the Asis context from Framework.

   function System_Value (Name : Wide_String) return Thick_Queries.Extended_Biggest_Int;
   -- Same as previous one, but returns the value when we know it is integer.

end Framework.Queries;
