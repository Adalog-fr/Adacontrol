----------------------------------------------------------------------
--  Framework.Language - Package specification                      --
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

package Framework.Language is

   --  Process the language used by rules files
   --  Syntax:
   --  <Program> ::= {<commmand> ";"}
   --  <command> ::= [ <label> ":" ] "check"|"search"|"count <Name>
   --                       [ "(" {<modifier>} <parameter> {"," {<modifier>} <parameter>}")" ]
   --              | "quit"
   --              | "go"
   --              | "help"  [ "all" | <name>{,<name>} ]
   --              | "clear" [ <name> {,<name>} ]
   --              | "source" <file>
   --  Ada-like comments (--) and Shell-like comments (#) are allowed.

   function Source_Location return Location;
   -- Location of the current token in the rule file

   function Parameter_Exists return Boolean;
   --  Returns true if there are parameters left to parse
   function Is_Integer_Parameter return Boolean;
   --  Returns true if next parameter is an integer
   function Is_Float_Parameter return Boolean;
   --  Returns true if next parameter is a float

   --  Following functions return the next parameter, or raise Syntax_Error
   --  They have side effects (i.e. they advance to the next parameter)
   function Get_Integer_Parameter return Integer;
   function Get_Float_Parameter   return Float;
   function Get_String_Parameter  return Wide_String;
   function Get_Entity_Parameter  return Entity_Specification;

   -- Following function returns True if the current token is True_KW
   -- and False if the current token is False_KW (the token is consumed)
   -- Otherwise, returns Default and does not change the current token
   -- True_KW and False_KW must be given in upper-case.
   -- An empty string is allowed for either of them, meaning there is no
   -- corresponding keyword
   function Get_Modifier (True_KW  : Wide_String;
                          False_KW : Wide_String := "";
                          Default  : Boolean     := False) return Boolean;

   -- A new version of Get_Modifier that allow multiple choice for the modifier.
   -- This version is generic since it is possible to use any enumerated type to
   -- control the behavior of the program.
   -- Default is the default value returned when no modifier has been matched.
   -- Prefix is the prefix used for the enumeration since it may be necessary to
   -- use reserved Ada keywords for the modifier.
   generic
      type Index is (<>);
   function Get_Enumerated_Modifier
     (Default : in Index       := Index'First;
      Prefix  : in Wide_String := "")
     return Index;

   --  The following package can be instantiated to parse "flag"
   --  parameters (keywords). The flags are the 'Image of the values
   --  of type Flags, with the initial Prefix removed (if not "").
   --  This allows having flags that are the same as Ada keywords
   --  If Allow_Any is False, it is an error if the current token is
   --    not a flag. Otherwise, if the current token is not a flag,
   --    Flags'First is returned and the current token is not consumed
   --    in order to retrieve it with other Get_XXX_Parameter functions,
   --    unless it is itself Flags'First (which makes an error)
   --
   -- WARNING !!
   -- If you instantiate this package immediately inside a library package,
   -- you must put a "pragma Elaborate (Framework.Language);", or circularity
   -- will result.
   generic
      type Flags is (<>);
      Prefix : Wide_String := "";
   package Flag_Utilities is
      function Get_Flag_Parameter (Allow_Any : Boolean) return Flags;

      function Image (Item : Flags) return Wide_String;
      procedure Help_On_Flags (Header      : Wide_String := "";
                               Footer      : Wide_String := "";
                               Extra_Value : Wide_String := "");
   end Flag_Utilities;

   -- Procedure to be called by rules if there is something wrong with the
   -- parameters
   procedure Parameter_Error (Message : Wide_String);
   procedure Parameter_Error (Message : Wide_String; Position : Location);
   pragma No_Return (Parameter_Error);

   function Adjust_Image (Original : Wide_String) return Wide_String;
   -- Transform a Full_Name_Image according to the syntax we use externally.
   -- The differences with the string return by Full_Name_Image are:
   --   we use "return" rather than ":" for the return type of functions.
   --   we use "access" rather than "*" for access parameters


   ---------------------------------------------------------------
   --
   --  Declarations below this line are for the use of the framework
   --

   --  Compile and execute a set of commands
   procedure Execute (Command_String : Wide_String);

   function Go_Command_Found return Boolean;
   --  Returns true if the last compiled command was "Go;"

   function Had_Failure return Boolean;
   --  Returns true if an exception was raised by a "Go" command

   function Had_Errors return Boolean;
   --  Returns true if there was a syntax error in a rule file

private
   -- Declarations for child units:
   Failure_Occurred    : Boolean := False;
   Rule_Error_Occurred : Boolean := False;
   procedure Compile;
end Framework.Language;
