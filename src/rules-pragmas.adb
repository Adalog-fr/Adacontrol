----------------------------------------------------------------------
--  Rules.Pragmas - Package body                                    --
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

-- ASIS
with
  Asis.Elements;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Pragmas is
  use Framework, Ada.Strings.Wide_Unbounded;

   Rule_Used             : Boolean := False;
   Nonstandard_Used      : Boolean := False;
   Save_Used             : Boolean;
   Save_Nonstandard_Used : Boolean;

   Rule_Uses : Context_Store;
   Nonstandard_Type  : Rule_Types;
   Nonstandard_Label : Unbounded_Wide_String;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): <pragma names>");
      User_Message ("Control usage of specific pragmas");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Utilities, Framework.Language;

   begin
      if  not Parameter_Exists then
         Parameter_Error ("At least one parameter required for rule " & Rule_Id);
      end if;

      while Parameter_Exists loop
         declare
            Pragma_Name : constant Wide_String := To_Upper (Get_String_Parameter);
         begin
            if Pragma_Name = "NONSTANDARD" then
               if Nonstandard_Used then
                  raise Already_In_Store;
               end if;
               Nonstandard_Used  := True;
               Nonstandard_Type  := Rule_Type;
               Nonstandard_Label := To_Unbounded_Wide_String (Label);
            else
               Associate (Rule_Uses,
                          Specification => Value (Pragma_Name),
                          Context =>  Simple_Context'(Rule_Type  => Rule_Type,
                                                      Rule_Label => To_Unbounded_Wide_String (Label)));
            end if;
         exception
            when Already_In_Store =>
               Parameter_Error (Pragma_Name & " is already used in rule " & Rule_Id);
         end;
      end loop;

      Rule_Used := True;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used        := False;
            Nonstandard_Used := False;
            Clear (Rule_Uses);
         when Suspend =>
            Save_Used             := Rule_Used;
            Rule_Used             := False;
            Save_Nonstandard_Used := Nonstandard_Used;
            Nonstandard_Used      := False;
         when Resume =>
            Rule_Used := Save_Used;
            Nonstandard_Used := Save_Nonstandard_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Rule_Uses);
   end Prepare;

   --------------------
   -- Process_Pragma --
   --------------------

   procedure Process_Pragma (Pragma_Element : in Asis.Pragma_Element) is
      use Asis, Asis.Elements, Ada.Strings.Wide_Unbounded, Utilities, Framework.Reports;

   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Nonstandard_Used
        and then Pragma_Kind (Pragma_Element) in An_Implementation_Defined_Pragma .. An_Unknown_Pragma
      then
         Report (Rule_Id,
                 To_Wide_String (Nonstandard_Label),
                 Nonstandard_Type,
                 Get_Location (Pragma_Element),
                 "use of non-standard pragma """ & Pragma_Name_Image (Pragma_Element) & '"');
      end if;

      declare
         Current_Context : Rule_Context'Class := Matching_Context (Rule_Uses, Pragma_Element);
      begin
         if Current_Context = No_Matching_Context then
            return;
         end if;

         Report (Rule_Id,
                 To_Wide_String (Simple_Context (Current_Context).Rule_Label),
                 Simple_Context (Current_Context).Rule_Type,
                 Get_Location (Pragma_Element),
                 "use of pragma """ & To_Title (Last_Matching_Name (Rule_Uses)) & '"');
      end;
   end Process_Pragma;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access,
                                     Prepare => Prepare'Access);
end Rules.Pragmas;
