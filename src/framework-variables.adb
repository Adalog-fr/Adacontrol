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

   type Operations is
      record
         Read  : Reader_Access;
         Write : Writer_Access;
      end record;
   package Variables_CB is new Binary_Map (Unbounded_Wide_String, Operations);
   Call_Backs : Variables_CB.Map;

   Number_Of_Variables : Natural := 0;

   ------------------
   -- Variable_Key --
   ------------------

   function Variable_Key (Rule_Name : Wide_String; Variable_Name : Wide_String) return Wide_String is
   -- Utility for the Register_XX_Variable packages
   begin
      if Rule_Name = "" then
         return Variable_Name;
      else
         return Rule_Name & '.' & Variable_Name;
      end if;
   end Variable_Key;

   ------------------------------
   -- Register_String_Variable --
   ------------------------------

   package body Register_String_Variable is
      procedure Help_On_Variable is
         use Utilities;
      begin
         User_Message (To_Title (Variable_Key (Rule_Name, Variable_Name)) & ": ""<string>"" (" & Value_Image & ')');
      end Help_On_Variable;

      function  Value_Image return Wide_String is
      begin
         return '"' & To_Wide_String (Variable) & '"';
      end Value_Image;

      procedure Writer (Val : in Wide_String) is
      begin
         Variable := To_Unbounded_Wide_String (Val);
      end Writer;
      use Variables_CB;
   begin  -- Register_String_Variable
      Add (Call_Backs,
           To_Unbounded_Wide_String (Variable_Key (Rule_Name, Variable_Name)),
           (Reader_Ptr, Writer_Ptr));
      Number_Of_Variables := Number_Of_Variables + 1;
   end Register_String_Variable;

   --------------------------------
   -- Register_Discrete_Variable --
   --------------------------------

   package body Register_Discrete_Variable is
      procedure Help_On_Variable is
         use Utilities;
      begin
         User_Message (To_Title (Variable_Key (Rule_Name, Variable_Name)) & ": ", Stay_On_Line => True);
         for V in Variable_Type range Variable_Type'First .. Variable_Type'Pred (Variable_Type'Last) loop
            User_Message (To_Title (Variable_Type'Wide_Image (V)) & ", ", Stay_On_Line => True);
         end loop;
         User_Message (To_Title (Variable_Type'Wide_Image (Variable_Type'Last)), Stay_On_Line => True);
         User_Message ( " (" & Value_Image & ')');
      end Help_On_Variable;

      function  Value_Image return Wide_String is
      begin
         return Variable_Type'Wide_Image (Variable);
      end Value_Image;

      procedure Writer (Val : in Wide_String) is
      begin
         if Val = "" then
            Variable := Variable_Type'Last;
         else
            Variable := Variable_Type'Wide_Value (Val);
         end if;
      end Writer;
      use Variables_CB;
   begin  -- Register_Discrete_Variable
      Add (Call_Backs,
           To_Unbounded_Wide_String (Variable_Key (Rule_Name, Variable_Name)),
           (Reader_Ptr, Writer_Ptr));
      Number_Of_Variables := Number_Of_Variables + 1;
   end Register_Discrete_Variable;

   -------------------------------
   -- Register_Integer_Variable --
   -------------------------------

   package body Register_Integer_Variable is
      procedure Help_On_Variable is
         use Utilities;
      begin
         User_Message (To_Title (Variable_Key (Rule_Name, Variable_Name)) & ": "
                       & Variable_Type'Wide_Image (Variable_Type'First)
                       & " .. "
                       & Variable_Type'Wide_Image (Variable_Type'Last)
                       & " (" & Value_Image & ')');
      end Help_On_Variable;

      function  Value_Image return Wide_String is
      begin
         return Variable_Type'Wide_Image (Variable);
      end Value_Image;

      procedure Writer (Val : in Wide_String) is
      begin
         if Val = "" then
            Variable := Variable_Type'Last;
         else
            Variable := Variable_Type'Wide_Value (Val);
         end if;
      end Writer;
      use Variables_CB;
   begin  -- Register_Integer_Variable
      Add (Call_Backs,
           To_Unbounded_Wide_String (Variable_Key (Rule_Name, Variable_Name)),
           (Reader_Ptr, Writer_Ptr));
      Number_Of_Variables := Number_Of_Variables + 1;
   end Register_Integer_Variable;

   -------------------------------
   -- Register_Special_Variable --
   -------------------------------

   package body Register_Special_Variable is
      function  Value_Image return Wide_String is
      begin
         return Variable_Value;
      end Value_Image;


      procedure Writer (Val : in Wide_String) is
      begin
         Set_Variable (Val);
      end Writer;
      use Variables_CB;
   begin  -- Register_Special_Variable
      Add (Call_Backs,
           To_Unbounded_Wide_String (Variable_Key (Rule_Name, Variable_Name)),
           (Reader_Ptr, Writer_Ptr));
      Number_Of_Variables := Number_Of_Variables + 1;
   end Register_Special_Variable;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable (Rule_Id : in Wide_String; Variable : in Wide_String; Val : in Wide_String) is
      use Utilities, Variables_CB;
   begin
      Fetch (Call_Backs,
             To_Unbounded_Wide_String (To_Upper (Variable_Key (Rule_Id, Variable)))).Write (Val);
   exception
      when Not_Present =>
         -- This exception not visible to clients, transform it
         raise No_Such_Variable;
   end Set_Variable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Variables_CB;
   begin
      Balance (Call_Backs);
   end Initialize;

   -------------------
   -- All_Variables --
   -------------------

   function All_Variables return Name_List is
      Result : Name_List (1 .. Number_Of_Variables);
      Inx    : Natural := 0;
      procedure Add_One (Key : Unbounded_Wide_String; Value : in out Operations) is
         pragma Unreferenced (Value);
      begin
         Inx := Inx + 1;
         Result (Inx) := Key;
      end Add_One;

      procedure Add_All is new Variables_CB.Iterate (Add_One);
   begin
      Add_All (Call_Backs);
      return Result;
   end All_Variables;

   -----------
   -- Fetch --
   -----------

   function Fetch (Variable : Unbounded_Wide_String) return Wide_String is
      use Variables_CB;
   begin
      return Fetch (Call_Backs, Variable).Read.all;
   exception
      when Not_Present =>
         raise No_Such_Variable;
   end Fetch;
end Framework.Variables;
