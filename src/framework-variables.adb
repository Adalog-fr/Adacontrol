----------------------------------------------------------------------
--  Framework.Variables - Package body                              --
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

-- Adalog
with
   Utilities,
   Binary_Map;

package body Framework.Variables is

   package Variables_Map is new Binary_Map (Unbounded_Wide_String, Class_Access);
   Variables_Table : Variables_Map.Map;

   Number_Of_Variables : Natural := 0;

   ------------------
   -- Variable_Key --
   ------------------

   function Variable_Key (Variable_Name : Wide_String) return Unbounded_Wide_String is
      use Utilities;
   begin
      return To_Unbounded_Wide_String (To_Upper (Variable_Name));
   end Variable_Key;

   --------------
   -- Register --
   --------------

   procedure Register (The_Variable : Variables.Class_Access; Variable_Name : Wide_String) is
      use Variables_Map;
   begin
      Add (Variables_Table,
           Variable_Key (Variable_Name),
           The_Variable);
      Number_Of_Variables := Number_Of_Variables + 1;
   end Register;

   ----------------------
   -- Help_On_Variable --
   ----------------------

   procedure Help_On_Variable (Variable_Name : Wide_String) is
      use Utilities, Variables_Map;
      Variable : constant Variables.Class := Fetch (Variables_Table, Variable_Key (Variable_Name)).all;
   begin
      User_Message (To_Title (Variable_Name),     Stay_On_Line => True);
      User_Message (": " & All_Values (Variable), Stay_On_Line => True);
      User_Message (" = " & Value_Image (Variable));
   end Help_On_Variable;

   -------------------
   -- Discrete_Type --
   -------------------

   package body Discrete_Type is
      function  All_Values  (Variable : in Discrete_Type.Object) return Wide_String is
         pragma Unreferenced (Variable);
         use Utilities;
         Buffer : Unbounded_Wide_String;
      begin
         for V in Value_Type range Value_Type'First .. Value_Type'Pred (Value_Type'Last) loop
            Append (Buffer, To_Title (Value_Type'Wide_Image (V)) & ", ");
         end loop;
         return
           '('
           & To_Wide_String (Buffer)
           & To_Title (Value_Type'Wide_Image (Value_Type'Last))
           & ')';
      end All_Values;

      function  Value_Image (Variable : in Discrete_Type.Object) return Wide_String is
         use Utilities;
      begin
         return To_Title (Value_Type'Wide_Image (Variable.Value));
      end Value_Image;

      procedure Set (Variable : in out Discrete_Type.Object; To : Wide_String) is
      begin
         if To = "" then
            Variable.Value := Value_Type'Last;
         else
            Variable.Value := Value_Type'Wide_Value (To);
         end if;
      end Set;
   end Discrete_Type;

   ------------------
   -- Integer_Type --
   ------------------

   package body Integer_Type is
      function  All_Values  (Variable : in Integer_Type.Object) return Wide_String is
         pragma Unreferenced (Variable);
         First_Image : constant Wide_String := Value_Type'Wide_Image (Value_Type'First);
      begin
         return
             First_Image (2 .. First_Image'Last)   -- Damn initial space!
           & " .."
           & Value_Type'Wide_Image (Value_Type'Last);
      end All_Values;

      function  Value_Image (Variable : in Integer_Type.Object) return Wide_String is
      begin
         return Value_Type'Wide_Image (Variable.Value);
      end Value_Image;

      procedure Set (Variable : in out Integer_Type.Object; To : Wide_String) is
      begin
         Set (Variable, To, Exact);
      end Set;

      procedure Set (Variable : in out Integer_Type.Object; To : Wide_String; Bounding : Bounding_Kind) is
         Val : Value_Type;
      begin
         if To = "" then
            Variable.Value := Value_Type'Last;
         else
            Val := Value_Type'Wide_Value (To);
            case Bounding is
               when Exact =>
                  Variable.Value := Val;
               when Min =>
                  if Variable.Value < Val then
                     Variable.Value := Val;
                  end if;
               when Max =>
                  if Variable.Value > Val then
                     Variable.Value := Val;
                  end if;
            end case;
         end if;
      end Set;
   end Integer_Type;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable (Variable : in Wide_String; Val : in Wide_String; Bounding : Bounding_Kind := Exact) is
      use Variables_Map;
   begin
      -- Block required to catch Not_Present from declaration:
      declare
         Var : constant Variables.Class_Access := Fetch (Variables_Table, Variable_Key (Variable));
      begin
         if Var.all in Boundable_Object'Class then
            Set (Boundable_Object'Class (Var.all), Val, Bounding);
         elsif Bounding = Exact then
            Set (Var.all, Val);
         else
            raise Exact_Required;
         end if;
      end;
   exception
      when Not_Present =>
         -- This exception not visible to clients, transform it
         raise No_Such_Variable;
   end Set_Variable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Variables_Map;
   begin
      Balance (Variables_Table);
   end Initialize;

   -------------------
   -- All_Variables --
   -------------------

   function All_Variables return Name_List is
      Result : Name_List (1 .. Number_Of_Variables);
      Inx    : Natural := 0;
      procedure Add_One (Key : Unbounded_Wide_String; Info : in out Class_Access) is
         pragma Unreferenced (Info);
      begin
         Inx := Inx + 1;
         Result (Inx) := Key;
      end Add_One;

      procedure Add_All is new Variables_Map.Iterate (Add_One);
   begin  -- All_Variables
      Add_All (Variables_Table);
      return Result;
   end All_Variables;

   -----------
   -- Fetch --
   -----------

   function Fetch (Variable : Unbounded_Wide_String) return Wide_String is
      use Variables_Map;
   begin
      return Value_Image (Fetch (Variables_Table, Variable).all);
   exception
      when Not_Present =>
         raise No_Such_Variable;
   end Fetch;
end Framework.Variables;
