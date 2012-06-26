----------------------------------------------------------------------
--  Framework.Variables - Package body                              --
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

-- Adalog
with
   Utilities,
   Binary_Map;

package body Framework.Variables is

   package Writers_List is new Binary_Map (Unbounded_Wide_String, Writer_Access);
   Writers_Map : Writers_List.Map;


   ------------------
   -- Variable_Key --
   ------------------

   function Variable_Key (Rule_Name : Wide_String; Variable_Name : Wide_String) return Unbounded_Wide_String is
   -- Utility for the Register_XX_Variable packages
      use Utilities;
   begin
      if Rule_Name = "" then
         return To_Unbounded_Wide_String (To_Upper (Variable_Name));
      else
         return To_Unbounded_Wide_String (To_Upper (Rule_Name & '.' & Variable_Name));
      end if;
   end Variable_Key;

   ------------------------------
   -- Register_String_Variable --
   ------------------------------

   package body Register_String_Variable is
      procedure Writer (Value : in Wide_String) is
      begin
         Variable := To_Unbounded_Wide_String (Value);
      end Writer;
      use Utilities, Writers_List;
   begin
      Add (Writers_Map, Variable_Key (Rule_Name, Variable_Name), Writer_Ptr);
   end Register_String_Variable;

   --------------------------------
   -- Register_Discrete_Variable --
   --------------------------------

   package body Register_Discrete_Variable is
      procedure Writer (Value : in Wide_String) is
      begin
         if Value = "" then
            Variable := Variable_Type'Last;
         else
            Variable := Decode (Value);
         end if;
      end Writer;
      use Utilities, Writers_List;
   begin
      Add (Writers_Map, Variable_Key (Rule_Name, Variable_Name), Writer_Ptr);
   end Register_Discrete_Variable;

   -------------------------------
   -- Register_Special_Variable --
   -------------------------------

   package body Register_Special_Variable is
      procedure Writer (Value : in Wide_String) is
      begin
         Set_Variable (Value);
      end Writer;
      use Utilities, Writers_List;
   begin
      Add (Writers_Map, Variable_Key (Rule_Name, Variable_Name), Writer_Ptr);
   end Register_Special_Variable;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable (Rule_Id : in Wide_String; Variable : in Wide_String; Value : in Wide_String) is
      use Utilities, Writers_List;
   begin
      Fetch (Writers_Map, Variable_Key (Rule_Id, Variable)) (Value);
   exception
      when Not_Present =>
         -- This exception not visible to clients, transform it
         raise No_Such_Variable;
   end Set_Variable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Writers_List;
   begin
      Balance (Writers_Map);
   end Initialize;

end Framework.Variables;
