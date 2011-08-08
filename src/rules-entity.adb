----------------------------------------------------------------------
--  Rules.Entity - Package body                                     --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Code Cheker  is free software;  you can redistribute  it and/or --
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
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Entity is
   use Framework;

   Rule_Used : Boolean := False;

   Entities  : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): Entity names");
      User_Message ("This rule can be used to check/search for the occurrence of any Ada entity");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if not Parameter_Exists then
         Parameter_Error ("At least one parameter required for rule " & Rule_Id);
      end if;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Entities, Entity, Simple_Context'(Rule_Type,
                                                         To_Unbounded_Wide_String (Label)));
         exception
            when Already_In_Store =>
               Parameter_Error ("Entity already given for rule " & Rule_Id
                                  & ": " & To_Wide_String (Entity.Specification));
         end;
      end loop;

      Rule_Used  := True;
   end Add_Use;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Entities);
   end Prepare;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Element : in Asis.Expression) is
      use Ada.Strings.Wide_Unbounded;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         use Framework.Reports, Utilities;
         Current_Context : Rule_Context'Class := Matching_Context (Entities, Element);
      begin
         if Current_Context = No_Matching_Context then
            return;
         end if;

         Report (Rule_Id,
                 To_Wide_String (Simple_Context (Current_Context).Rule_Label),
                 Simple_Context (Current_Context).Rule_Type,
                 Get_Location (Element),
                 "use of element """ & To_Title (Last_Matching_Name (Entities)) & '"');
      end;

   end Process_Identifier;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                           Help    => Help'Access,
                           Prepare => Prepare'Access,
                           Add_Use => Add_Use'Access);
end Rules.Entity;
