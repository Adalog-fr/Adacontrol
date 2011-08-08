----------------------------------------------------------------------
--  Rules.Other_Dependencies - Package body                         --
--                                                                  --
--  This software  is (c) SAGEM DS and  Adalog  2004-2006.  The Ada --
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

-- ASIS
with
  Asis.Clauses;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Other_Dependencies is
   use Framework;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;
   Context   : Basic_Rule_Context;

   Allowed_Entities  : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): <Unit name>");
      User_Message ("Control semantic dependencies (with clauses) to units other than those indicated");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language;

   begin
      if Rule_Used then
         Parameter_Error (Rule_Id, "rule already specified");
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Allowed_Entities, Entity, Empty_Context);
         exception
            when Already_In_Store =>
               Parameter_Error (Rule_Id, "entity already given: " & Image (Entity));
         end;
      end loop;

      Rule_Used := True;
      Context   := Basic.New_Context (Rule_Type, Label);
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Clear (Allowed_Entities);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Allowed_Entities);
   end Prepare;

   -------------------------
   -- Process_With_Clause --
   -------------------------

   procedure Process_With_Clause (Clause : in Asis.Clause) is
      use Asis.Clauses;
      use Framework.Reports, Utilities;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Names : constant Asis.Name_List := Clause_Names (Clause);
      begin
         for N in Names'Range loop
            if Extended_Matching_Context (Allowed_Entities, Names (N)) = No_Matching_Context then
               Report (Rule_Id,
                       Context,
                       Get_Location (Names (N)),
                       "unit depends on """ & To_Title (Last_Matching_Name (Allowed_Entities)) & '"');
            end if;
         end loop;
      end;
   end Process_With_Clause;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access,
                                              Prepare => Prepare'Access);
end Rules.Other_Dependencies;
