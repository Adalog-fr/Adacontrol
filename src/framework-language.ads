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
with  -- Adalog
   Thick_Queries;
package Framework.Language is

   --  Process the language used by rules files
   --  Syntax:
   --  <Program> ::= {<commmand> ";"}
   --  <command> ::= [ <label> ":" ] "check"|"search"|"count" <Name>
   --                       [ "(" {<modifier>} <parameter> {"," {<modifier>} <parameter>}")" ]
   --              | "clear" "all" | <name> {,<name>}
   --              | "go"
   --              | "help"  [ "all" | "list" | <name>{,<name>} ]
   --              | "inhibit" "all" | <rule_name> "(" [ "all" <unit> {, "all" <unit>} ")"
   --              | "message" <any_string>
   --              | "quit"
   --              | "set" "format" <format_name> |
   --                      "output" <file_name>   |
   --                      "statistics" <level>   |
   --                      "trace" <file_name>    |
   --                      "debug" | "ignore" | "verbose" | "warning"  "on" | "off"
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
   function Get_Integer_Parameter (Min : Thick_Queries.Biggest_Int := Thick_Queries.Biggest_Int'First;
                                   Max : Thick_Queries.Biggest_Int := Thick_Queries.Biggest_Int'Last)
                                   return Thick_Queries.Biggest_Int;
   function Get_Integer_Parameter (Min : Integer := Integer'First;
                                   Max : Integer := Integer'Last)
                                   return Integer;
   function Get_Float_Parameter   return Float;
   function Get_Name_Parameter    return Wide_String;
   -- Returns either a single identifier or an attribute (possibly complex, i.e. 'Base'First)
   -- A string is considered as an operator's name, and returned with its quotes
   -- In any case, returned name is in upper case
   function Get_String_Parameter  return Wide_String;
   -- The parameter must be a quoted string. The delimiter quotes are not returned, and any
   -- enclosed doubled quotes are reduced to a single one.
   function Get_Entity_Parameter (Allow_Extended : Boolean := False) return Entity_Specification;
   function Get_File_Parameter    return Wide_String;
   -- If the parameter is not an absolute file name, it is made relative to the
   -- directory of the rules file, or to the current directory if there is none

   -- Following function returns True if the current token is True_KW
   -- and False if the current token is False_KW (the token is consumed)
   -- Otherwise, returns Default and does not change the current token
   -- True_KW and False_KW must be given in upper-case.
   -- An empty string is allowed for either of them, meaning there is no
   -- corresponding keyword
   function Get_Modifier (True_KW  : Wide_String;
                          False_KW : Wide_String := "";
                          Default  : Boolean     := False) return Boolean;

   --  The following two packages can be instantiated to parse modifiers and "flag"
   --  parameters (keywords). The flags are the 'Image of the values
   --  of the generic formal type, with the initial Prefix removed (if not "").
   --  This allows having modifiers or flags that are the same as Ada keywords
   --
   -- WARNING !!
   -- If you instantiate one of these packages immediately inside a library package,
   -- you must put a "pragma Elaborate (Framework.Language);", or circularity
   -- will result.

   generic
      type Modifiers is (<>);
      Prefix : in Wide_String := "";
   package Modifier_Utilities is
      function Get_Modifier (Required : Boolean) return Modifiers;
      -- If not Required and no modifier given, returns Modifiers'First
      function Image (Item : Modifiers) return Wide_String;
      procedure Help_On_Modifiers (Header      : Wide_String := "";
                                   Footer      : Wide_String := "";
                                   Extra_Value : Wide_String := "");

      type Unconstrained_Modifier_Set is array (Modifiers range <>) of Boolean;
      subtype Modifier_Set is Unconstrained_Modifier_Set (Modifiers);
      Empty_Set : constant Modifier_Set := (others => False);
      Full_Set  : constant Modifier_Set := (others => True);

      function Get_Modifier_Set return Modifier_Set;
      function Image (Set     : Unconstrained_Modifier_Set;
                      Default : Unconstrained_Modifier_Set := Empty_Set) return Wide_String;
      -- Image of all modifiers in Set, separated and terminated by a single space
      -- "" for empty set and Default
   end Modifier_Utilities;

   generic
      type Flags is (<>);
      Prefix : Wide_String := "";
   package Flag_Utilities is
      function Get_Flag_Parameter (Allow_Any : Boolean) return Flags;
      --  If Allow_Any is False, it is an error if the current token is
      --    not a flag. Otherwise, if the current token is not a flag,
      --    Flags'First is returned and the current token is not consumed
      --    in order to retrieve it with other Get_XXX_Parameter functions,
      --    unless it is itself Flags'First (which makes an error)

      function Image (Item : Flags) return Wide_String;
      procedure Help_On_Flags (Header      : Wide_String := "";
                               Footer      : Wide_String := "";
                               Extra_Value : Wide_String := "");
   end Flag_Utilities;

   -- Procedure to be called by rules if there is something wrong with the
   -- parameters
   procedure Parameter_Error (Rule : Wide_String; Message : Wide_String);
   procedure Parameter_Error (Rule : Wide_String; Message : Wide_String; Position : Location);
   pragma No_Return (Parameter_Error);

   function Adjust_Image (Original : Wide_String) return Wide_String;
   -- Transform a Full_Name_Image according to the syntax we use externally.
   -- The differences with the string return by Full_Name_Image are:
   --   we use "return" rather than ":" for the return type of functions.
   --   we use "access" rather than "*" for access parameters


   ------------------------------------------------------------------
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

   procedure Syntax_Error (Message : Wide_String; Position : Location);
   pragma No_Return (Syntax_Error);

end Framework.Language;
