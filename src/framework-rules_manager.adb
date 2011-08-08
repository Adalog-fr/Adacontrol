----------------------------------------------------------------------
--  Framework.Rules_Manager - Package body                          --
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

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Utilities,
  Binary_Map;

-- Pragmas
pragma Elaborate_All (Utilities);
pragma Elaborate_All (Binary_Map);

package body Framework.Rules_Manager is

   Last_Rule_Name   : Wide_String (1..50);  -- 50 arbitrary, but largely sufficient
   Last_Rule_Length : Natural := 0;
   Max_Name_Length  : Natural := 0;

   Nb_Rules : Natural := 0;

   type Element is record
      Help     : Help_Procedure;
      Add_Use  : Add_Use_Procedure;
      Command  : Command_Procedure;
      Prepare  : Prepare_Procedure;
      Finalize : Finalize_Procedure;
   end record;

   package Rule_List is new Binary_Map (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
                                        Element);
   Rule_Map : Rule_List.Map;

   --------------
   -- Register --
   --------------

   procedure Register (Rule     : Wide_String;
                       Help     : Help_Procedure;
                       Add_Use  : Add_Use_Procedure;
                       Command  : Command_Procedure;
                       Prepare  : Prepare_Procedure  := null;
                       Finalize : Finalize_Procedure := null) is
      use Utilities, Ada.Strings.Wide_Unbounded;
   begin
      if Help = null or Add_Use = null or Command = null then
         Failure ("Missing Help, Add_Use or Clear procedure");
      end if;

      Rule_List.Add (Rule_Map,
                     To_Unbounded_Wide_String (To_Upper (Rule)),
                     (Help, Add_Use, Command, Prepare, Finalize));
      Nb_Rules := Nb_Rules + 1;
      if Rule'Length > Max_Name_Length then
         Max_Name_Length := Rule'Length;
      end if;
   end Register;

   -----------
   -- Enter --
   -----------

   procedure Enter (Rule : Wide_String) is
   begin
      Last_Rule_Name (1..Rule'Length) := Rule;
      Last_Rule_Length                := Rule'Length;
   end Enter;

   ---------------
   -- Last_Rule --
   ---------------

   function Last_Rule return Wide_String is
   begin
      return Last_Rule_Name (1 .. Last_Rule_Length);
   end Last_Rule;

   ------------------
   -- Is_Rule_Name --
   ------------------

   function Is_Rule_Name (Rule : Wide_String) return Boolean is
      use Rule_List;
   begin
      return Is_Present (Rule_Map, To_Unbounded_Wide_String (Rule));
   end Is_Rule_Name;

   ----------
   -- Help --
   ----------

   procedure Help (Rule_Id : in Wide_String) is
      use Ada.Strings.Wide_Unbounded, Utilities, Rule_List;

      Rule_Name : constant Unbounded_Wide_String := To_Unbounded_Wide_String (To_Upper (Rule_Id));
   begin
      Fetch (Rule_Map, Rule_Name).Help.all;
   exception
      when Not_Present =>
         Error ("Unknown rule: " & To_Wide_String (Rule_Name));
   end Help;

   --------------
   -- Help_All --
   --------------

   procedure Help_All is
      procedure One_Help (Key : in Unbounded_Wide_String; Value : in out Element) is
         pragma Unreferenced (Key);
         use Utilities;
      begin
         Value.Help.all;
         User_Message ("");
      end One_Help;

      procedure Help_Iterate is new Rule_List.Iterate (One_Help);
   begin
      Help_Iterate (Rule_Map);
   end Help_All;

   ----------------
   -- Help_Names --
   ----------------

   procedure Help_Names is
      use Utilities;
      Spaces : constant Unbounded_Wide_String := 3 * ' ';
      Line : Unbounded_Wide_String := Spaces;

      procedure One_Name (Key : in Unbounded_Wide_String; Value : in out Element) is
         pragma Unreferenced (Value);
         Name : constant Wide_String := To_Wide_String (Key);
      begin
         if Length (Line) + Max_Name_Length + 1 >= 80 then
            User_Message (To_Wide_String (Line));
            Line := Spaces;
         end if;
         Append (Line, To_Title (Name) & (Max_Name_Length - Name'Length +1) * ' ');
      end One_Name;

      procedure Help_Iterate is new Rule_List.Iterate (One_Name);

   begin
      Help_Iterate (Rule_Map);
      User_Message (To_Wide_String (Line));
   end Help_Names;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types;
                      Rule_Name : in Wide_String) is
      use Rule_List;
   begin
      -- Existence of rule checked by the language
      Fetch (Rule_Map, To_Unbounded_Wide_String (Rule_Name)).Add_Use (Label, Rule_Type);
   end Add_Use;

   -----------------
   -- Prepare_All --
   -----------------

   procedure Prepare_All is
      procedure Prepare_One (Key : in Unbounded_Wide_String; Value : in out Element) is
         pragma Unreferenced (Key);
      begin
         if Value.Prepare /= null then
            Value.Prepare.all;
         end if;
      end Prepare_One;

      procedure Iterate_On_Prepare is new Rule_List.Iterate (Prepare_One);
   begin
      Rule_List.Balance (Rule_Map);
      Iterate_On_Prepare (Rule_Map);
   end Prepare_All;

   ------------------
   -- Finalize_All --
   ------------------

   procedure Finalize_All is
      procedure Finalize_One (Key : in Unbounded_Wide_String; Value : in out Element) is
         pragma Unreferenced (Key);
      begin
         if Value.Finalize /= null then
            Value.Finalize.all;
         end if;
      end Finalize_One;

      procedure Iterate_On_Finalize is new Rule_List.Iterate (Finalize_One);
   begin
      Iterate_On_Finalize (Rule_Map);
   end Finalize_All;

   -----------------
   -- Command_All --
   -----------------

   procedure Command_All (Action : Rule_Action) is
      procedure One_Command (Key : in Unbounded_Wide_String; Value : in out Element) is
         pragma Unreferenced (Key);
      begin
         Value.Command (Action);
      end One_Command;

      procedure Command_Iterate is new Rule_List.Iterate (One_Command);
   begin
      Command_Iterate (Rule_Map);
  end Command_All;

  -------------
  -- Command --
  -------------

  procedure Command (Rule_Id : in Wide_String; Action : Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Utilities, Rule_List;

      Rule_Name : constant Unbounded_Wide_String := To_Unbounded_Wide_String (To_Upper (Rule_Id));
   begin
      Fetch (Rule_Map, Rule_Name).Command (Action);
   exception
      when Not_Present =>
         Error ("Unknown rule: " & To_Wide_String (Rule_Name));
  end Command;

  ---------------------
  -- Number_Of_Rules --
  ---------------------

  function Number_Of_Rules return Natural is
  begin
     return Nb_Rules;
  end Number_Of_Rules;

end Framework.Rules_Manager;
