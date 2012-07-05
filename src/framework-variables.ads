----------------------------------------------------------------------
--  Framework.Variables - Package specification                     --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2012. The Ada --
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

package Framework.Variables is
   --
   -- Management of rules variables
   --

   type Writer_Access is access procedure (Value : Wide_String);
   type Reader_Access is access function return Wide_String;
   -- Used only for private part of Register_XX_Variable, useless (but harmless) for user

   -- When Instantiating the following generics, Rule_Name and Variable_Name are expected
   -- to be given in UPPER_CASE
   generic
      Variable      : in out Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      Rule_Name     : in     Wide_String := "";
      Variable_Name : in     Wide_String;
   package Register_String_Variable is
      procedure Help_On_Variable;
   private
      function  Value_Image return Wide_String;
      procedure Writer (Val : in Wide_String);
      Reader_Ptr : constant Reader_Access := Value_Image'Access;
      Writer_Ptr : constant Writer_Access := Writer'Access;
   end Register_String_Variable;

   generic
      type Variable_Type is (<>);
      Variable      : in out Variable_Type;
      Rule_Name     : in     Wide_String := "";
      Variable_Name : in     Wide_String;
   package Register_Discrete_Variable is
      procedure Help_On_Variable;
   private
      function  Value_Image return Wide_String;
      procedure Writer (Val : in Wide_String);
      Reader_Ptr : constant Reader_Access := Value_Image'Access;
      Writer_Ptr : constant Writer_Access := Writer'Access;
   end Register_Discrete_Variable;

   generic
      type Variable_Type is (<>);
      Variable      : in out Variable_Type;
      Rule_Name     : in     Wide_String := "";
      Variable_Name : in     Wide_String;
   package Register_Integer_Variable is
      procedure Help_On_Variable;
   private
      function  Value_Image return Wide_String;
      procedure Writer (Val : in Wide_String);
      Reader_Ptr : constant Reader_Access := Value_Image'Access;
      Writer_Ptr : constant Writer_Access := Writer'Access;
   end Register_Integer_Variable;

   -- For cases where simple assignment is not sufficient, provide the necessary behaviour in
   -- procedure Set_Variable. If Value is incorrect, the procedure shall raise Constraint_Error.
   generic
      with procedure Set_Variable (Val : Wide_String);
      with function  Variable_Value return Wide_String;
      Rule_Name     : in     Wide_String := "";
      Variable_Name : in     Wide_String;
   package Register_Special_Variable is
   private
      -- Relay subprograms needed, can't take 'Access of formal procedure
      function  Value_Image return Wide_String;
      procedure Writer (Val : in Wide_String);
      Reader_Ptr : constant Reader_Access := Value_Image'Access;
      Writer_Ptr : constant Writer_Access := Writer'Access;
   end  Register_Special_Variable;

   -- Shared type for variables:
   type Switch is (Off, On);

private
   ---------------------------------------------------------------------
   --
   --  Declarations below this line are for the use of the framework
   --
   procedure Initialize;

   procedure Set_Variable (Rule_Id : in Wide_String; Variable : in Wide_String; Val : in Wide_String);
   No_Such_Variable : exception;

   type Name_List is array (Natural range <>) of Unbounded_Wide_String;
   function All_Variables return Name_List;

   function Fetch (Variable : Unbounded_Wide_String) return Wide_String;

end Framework.Variables;
