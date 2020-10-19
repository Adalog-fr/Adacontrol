----------------------------------------------------------------------
--  Framework.Variables.Shared_Types - Package specification        --
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

package Framework.Variables.Shared_Types is
   --
   -- Concrete class packages for various kinds of rule variables
   --

   package String_Type is
      type Object is new Variables.Object with
         record
            Value : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         end record;
      overriding procedure Set         (Variable : in out String_Type.Object; To : Wide_String);
      overriding function  Value_Image (Variable : in     String_Type.Object) return Wide_String;
      overriding function  All_Values  (Variable : in     String_Type.Object) return Wide_String;
   end String_Type;

   type Switch is (Off, On);
   package Switch_Type is new Discrete_Type (Switch);

   type Extended_Switch is (Off, On, Inverted);
   package Extended_Switch_Type is new Discrete_Type (Extended_Switch);

   type Verbosity is (Compact, Detailed);
   package Verbosity_Type is new Discrete_Type (Verbosity);

   type Extra_Infos is (None, Compact, Detailed, Root_Detailed);
   package Extra_Infos_Type is new Discrete_Type (Extra_Infos);

   package Natural_Type is new Integer_Type (Natural);
end Framework.Variables.Shared_Types;
