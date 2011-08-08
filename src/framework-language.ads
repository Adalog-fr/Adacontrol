----------------------------------------------------------------------
--  Framework.Language - Package specification                      --
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

package Framework.Language is
   --  Process the language used by rules files
   --  Syntax:
   --  [<label> ":"] "check"|"search" <Name> ["(" <parameter> {"," <parameter>}")"] ";"
   --  Ada-like comments (--) and Shell-like comments (#) are allowed.

   function Parameter_Exists return Boolean;
   --  Returns true if there are parameters left to parse

   --  Following functions return the next parameter, or raise Syntax_Error
   --  They have side effects (i.e. they advance to the next parameter)
   function Get_Integer_Parameter return Integer;
   function Get_String_Parameter  return Wide_String;
   function Get_Entity_Parameter  return Entity_Specification;

   -- Procedure to be called by rules if there is something wrong with the
   -- parameters
   procedure Parameter_Error (Message : Wide_String);
   pragma No_Return (Parameter_Error);

   function Adjust_Image (Original : Wide_String) return Wide_String;
   -- Transform a Full_Name_Image according to the syntax we use externally.
   -- The differences with the string return by Full_Name_Image are:
   --   we use "return" rather than ":" for the return type of functions.
   --   we use "access" rather than "*" for access parameters


   --
   --  Declarations below this line are for the use of the framework
   --

   --  Compile a set of rules
   procedure Compile_File   (Name   : String);
   procedure Compile_String (Source : String);

   function Rule_Exists return Boolean;

end Framework.Language;
