----------------------------------------------------------------------
--  Rules.Entities - Package body                                   --
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

-- Asis
with
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Entities is
   use Framework;

   -- Algorithm:
   --
   -- This rule was intended to be the simplest one: each time an identifier is encountered,
   -- just check if it is associated to a context.
   --
   -- Alas! Nothing is simple when renamings come into play.
   -- If the identifier is declared by renaming, every record field and variable name which
   -- is part of the renamed expression is "used", but not parts that are evaluated by the
   -- elaboration of the declaration. In short, if we see:
   --    Ren := 1;
   -- and Ren is declared by:
   --    Ren : Integer renames Integer'(Pack.Tab(I).F);
   -- we must consider that when Ren is used, Tab (a variable) is used, and F (a record field)
   -- is used, but not Pack (a package) nor I (evaluated by the elaboration of the renaming),
   -- nor Integer (evaluated by the elaboration of the renaming).

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Searched_Entities  : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): <Entity name>");
      User_Message ("Control occurrences of any Ada entity");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;

   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Searched_Entities, Entity, Basic.New_Context (Ctl_Kind, Ctl_Label));
         exception
            when Already_In_Store =>
               Parameter_Error (Rule_Id, "entity already given: " & Image (Entity));
         end;
      end loop;

      Rule_Used := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Clear (Searched_Entities);
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
      Balance (Searched_Entities);
   end Prepare;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Element : in Asis.Expression) is

      Decl : Asis.Declaration;
      Expr : Asis.Expression;

      procedure Check (Identifier : Asis.Expression) is
         use Asis, Asis.Elements, Asis.Expressions;
         use Framework.Reports, Thick_Queries, Utilities;

         Current_Context : constant Root_Context'Class
           := Matching_Context (Searched_Entities, Identifier, Extend_To => (Instance => True, Renaming => False));
      begin
         if Current_Context /= No_Matching_Context then
            Report (Rule_Id,
                    Current_Context,
                    Get_Location (Element),
                    "use of element """ & To_Title (Last_Matching_Name (Searched_Entities)) & '"');
         end if;

         if Expression_Kind (Identifier) = An_Attribute_Reference then
            return;
         end if;

         Decl := Corresponding_Name_Declaration (Identifier);
         case Declaration_Kind (Decl) is
            when An_Object_Renaming_Declaration =>
               Expr := A4G_Bugs.Renamed_Entity (Decl);
               loop
                  case Expression_Kind (Expr) is
                     when An_Identifier
                        | An_Operator_Symbol
                        | An_Enumeration_Literal
                          =>
                        Check (Expr);
                        exit;
                     when A_Selected_Component =>
                        Check (Selector (Expr));
                        Expr := Prefix (Expr);
                     when An_Explicit_Dereference =>
                        -- everything left of the dereference is "used" at the place of the renaming
                        exit;
                     when A_Slice
                        | An_Indexed_Component
                          =>
                        -- The indexing expression is "used" at the place of the renanming,
                        -- not when using the renamed entity
                        Expr := Prefix (Expr);
                     when A_Function_Call =>
                        -- The function is "used" at the place of the renanming,
                        -- not when using the renamed entity
                        exit;
                     when An_Attribute_Reference =>
                        Check (Expr);
                        exit;
                     when A_Type_Conversion =>
                        -- Allowed for tagged types
                        Expr := Converted_Or_Qualified_Expression (Expr);
                     when others =>
                        Failure ("Entities: unexpected expression in renaming", Expr);
                  end case;
               end loop;
            when An_Exception_Renaming_Declaration
               | A_Package_Renaming_Declaration
               | A_Generic_Package_Renaming_Declaration
               | A_Generic_Procedure_Renaming_Declaration
               | A_Generic_Function_Renaming_Declaration
                 =>
               -- Nothing dynamic here
               Check (Simple_Name (A4G_Bugs.Renamed_Entity (Decl)));
            when A_Procedure_Renaming_Declaration
               | A_Function_Renaming_Declaration
                 =>
               Expr := A4G_Bugs.Renamed_Entity (Decl);
               case Expression_Kind (Expr) is
                  when An_Explicit_Dereference =>
                     -- everything left of the dereference is "used" at the place of the renanming
                     null;
                  when An_Indexed_Component =>
                     -- This happens when a procedure renames an entry of an entry family
                     -- The index is "used" at the place of the renanming
                     Expr := Prefix (Expr);
                  when others =>
                     Check (Simple_Name (Expr));
               end case;
            when others =>
               -- including Not_A_Declaration for predefined stuff
               null;
         end case;
      end Check;
   begin   -- Process_Identifier
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check (Element);
   end Process_Identifier;

begin  -- Rules.Entities
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Entities;
