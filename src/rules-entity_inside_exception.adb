----------------------------------------------------------------------
--  Rules.Entity_Inside_Exception - Package body                    --
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

-- Asis
with
  Asis,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;


-- Pragmas
pragma Elaborate_All (Asis.Iterator);

package body Rules.Entity_Inside_Exception is
   use Framework;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Entities : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): <Entity name>");
      User_Message ("Control occurrences of an entity inside an exception handler.");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if  not Parameter_Exists then
         Parameter_Error ("At least one parameter required for rule " & Rule_Id);
      end if;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Entities, Entity, Simple_Context'(Rule_Type, To_Unbounded_Wide_String (Label)));
         exception
            when Already_In_Store =>
               Parameter_Error (Image (Entity) & " is already used in rule " & Rule_Id);
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
            Rule_Used := False;
            Clear (Entities);
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
      Balance (Entities);
   end Prepare;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Identifier : in Asis.Identifier) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Reports, Utilities;

      Identifier_Location   : Location;
      Use_Context           : constant Rule_Context'Class
        := Matching_Context (Entities, Identifier);
   begin
      if Use_Context /= No_Matching_Context then
         Identifier_Location := Get_Location (Identifier);
         Report (Rule_Id,
                 To_Wide_String (Simple_Context (Use_Context).Rule_Label),
                 Simple_Context (Use_Context).Rule_Type,
                 Identifier_Location,
                 "use of """ & To_Title (Last_Matching_Name (Entities)) & '"');
      end if;
   end Process_Identifier;

   --------------
   -- Traverse --
   --------------

   type State_Information is null record;

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out State_Information);

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out State_Information);

   procedure Traverse is new Asis.Iterator.Traverse_Element
     (State_Information, Pre_Procedure, Post_Procedure);

   -------------------
   -- Pre_Procedure --
   -------------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out State_Information) is
      use Asis;
      use Asis.Elements, Asis.Expressions;
   begin
      case Element_Kind (Element) is
         when A_Pragma =>
            -- Do not traverse pragmas
            Control := Abandon_Children;

         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Identifier =>
                  Process_Identifier (Element);

               when An_Attribute_Reference =>
                  Process_Identifier (Element);  -- Check the attribute itself

                  -- Traverse manually the left branch only, in order to avoid processing
                  -- the attribute identifier
                  Traverse (Prefix (Element), Control, State);
                  if Control /= Terminate_Immediately then
                     Control := Abandon_Children;
                  end if;

              when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end Pre_Procedure;

   --------------------
   -- Post_Procedure --
   --------------------

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out State_Information) is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure;

   -------------------------------
   -- Process_Exception_Handler --
   -------------------------------

   procedure Process_Exception_Handler (Handler : in Asis.Exception_Handler) is
      use Asis;

      Control : Traverse_Control  := Continue;
      State   : State_Information;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Traverse (Handler, Control, State);
   end Process_Exception_Handler;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access,
                                     Prepare => Prepare'Access);
end Rules.Entity_Inside_Exception;
