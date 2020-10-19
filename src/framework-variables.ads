----------------------------------------------------------------------
--  Framework.Variables - Package specification                     --
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
  Ada.Strings.Wide_Unbounded;

package Framework.Variables is
   pragma Elaborate_Body;

   --
   -- Management of rules variables
   --


   --
   -- Root type for rules variables
   --

   type Object is abstract tagged null record;
   subtype Class is Object'Class;
   type Class_Access is access all Class;

   -- Methods:
   procedure Set (Variable : in out Variables.Object; To : Wide_String)      is abstract;
   function  Value_Image (Variable : in Variables.Object) return Wide_String is abstract;
   function  All_Values  (Variable : in Variables.Object) return Wide_String is abstract;

   -- Class_Wide subprograms:
   procedure Register (The_Variable  : Variables.Class_Access;
                       Variable_Name : Wide_String);
   -- Variable_Name is expected to be given in UPPER_CASE
   -- For rule variables, the name should be Rule_Id & ".XXX"
   procedure Help_On_Variable (Variable_Name : Wide_String);


   --
   -- Rule variables that accept the Min/Max modifier
   --
   type Bounding_Kind is (Exact, Min, Max);
   type Boundable_Object is abstract new Object with null record;
   not overriding procedure Set (Variable : in out Boundable_Object;
                                 To       : Wide_String;
                                 Bounding : Bounding_Kind) is abstract;


   --
   -- Generic generators for subclasses
   --

   generic
      type Value_Type is (<>);
   package Discrete_Type is
      type Object is new Variables.Object with
         record
            Value : Value_Type;
         end record;
      overriding procedure Set (Variable : in out Discrete_Type.Object; To : Wide_String);
      overriding function  Value_Image (Variable : in Discrete_Type.Object) return Wide_String;
      overriding function  All_Values  (Variable : in Discrete_Type.Object) return Wide_String;
   end Discrete_Type;

   generic
      type Value_Type is range <>;
   package Integer_Type is
      type Object is new Variables.Boundable_Object with
         record
            Value : Value_Type;
         end record;
      overriding procedure Set (Variable : in out Integer_Type.Object; To : Wide_String);
      overriding procedure Set (Variable : in out Integer_Type.Object; To : Wide_String; Bounding : Bounding_Kind);
      overriding function  Value_Image (Variable : in Integer_Type.Object) return Wide_String;
      overriding function  All_Values  (Variable : in Integer_Type.Object) return Wide_String;
   end Integer_Type;

   ---------------------------------------------------------------------
   --
   --  Declarations below this line are for the use of the framework
   --
   procedure Initialize;

   procedure Set_Variable (Variable : in Wide_String; Val : in Wide_String; Bounding : Bounding_Kind := Exact);
   No_Such_Variable : exception;
   Exact_Required   : exception;

   type Name_List is array (Natural range <>) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   function All_Variables return Name_List;

   function Fetch (Variable : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String) return Wide_String;

end Framework.Variables;
