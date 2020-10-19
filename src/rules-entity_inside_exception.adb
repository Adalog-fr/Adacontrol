----------------------------------------------------------------------
--  Rules.Entity_Inside_Exception - Package body                    --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
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

-- Asis
with
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;
pragma Elaborate_All (Asis.Iterator);

-- Adalog
with
  Thick_Queries,
  Utilities;

package body Rules.Entity_Inside_Exception is
   use Framework, Framework.Control_Manager;

   type Entity_Context is new Basic_Rule_Context with
      record
         Is_Not : Boolean;
      end record;

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
      User_Message ("Control occurrences of an entity inside an exception handler.");
      User_Message;
      User_Message ("Parameter(s): [not] calls | entry_calls | <Entity name>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;
      Is_Not : Boolean;
   begin
      if  not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      while Parameter_Exists loop
         Is_Not := Get_Modifier ("NOT");
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Entities, Entity, Entity_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Is_Not));
         exception
            when Already_In_Store =>
               Parameter_Error (Rule_Id, Image (Entity) & " already used");
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

   --------------
   -- Traverse --
   --------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Null_State);

   procedure Traverse is new Asis.Iterator.Traverse_Element (Null_State, Pre_Procedure, Null_State_Procedure);

   -------------------
   -- Pre_Procedure --
   -------------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            State   : in out Null_State)
   is

      procedure Check (Identifiers : in Asis.Expression_List) is
         use Framework.Locations, Framework.Reports;
         use Thick_Queries, Utilities;
         Found  : Boolean := False;
         C_Kind : Callable_Kinds;
      begin
         for Id: Asis.Expression of Identifiers loop
            declare
               Use_Context : constant Root_Context'Class := Matching_Context (Entities, Id);
            begin
               if Use_Context /= No_Matching_Context then
                  Found := True;
                  if not Entity_Context (Use_Context).Is_Not then
                     Report (Rule_Id,
                             Use_Context,
                             Get_Location (Element),
                             "use of """ & To_Title (Last_Matching_Name (Entities)) & '"');
                  end if;
               end if;
            end;
         end loop;

         if not Found then
            C_Kind := Callable_Kind (Element);
            if C_Kind in An_Entry_Callable then
               declare
                  Calls_Context : constant Root_Context'Class := Association (Entities, "ENTRY_CALLS");
               begin
                  if Calls_Context /= No_Matching_Context then
                     Found := True;
                     if not Entity_Context (Calls_Context).Is_Not then
                        Report (Rule_Id,
                                Calls_Context,
                                Get_Location (Element),
                                "entry call to " & Full_Name_Image (Element));
                     end if;
                  end if;
               end;
            end if;

            if not Found and C_Kind /= Not_A_Callable then
               declare
                  Calls_Context : constant Root_Context'Class := Association (Entities, "CALLS");
               begin
                  if Calls_Context /= No_Matching_Context then
                     if not Entity_Context (Calls_Context).Is_Not then
                        Report (Rule_Id,
                                Calls_Context,
                                Get_Location (Element),
                                "call to " & Full_Name_Image (Element));
                     end if;
                  end if;
               end;
            end if;
         end if;
      end Check;

      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
   begin   -- Pre_Procedure
      case Element_Kind (Element) is
         when A_Definition =>
            case Definition_Kind (Element) is
               when An_Aspect_Specification =>
                  -- 2012, ignored for the moment
                  Control := Abandon_Children;
               when others =>
                  null;
            end case;

         when A_Pragma =>
            -- Do not traverse pragmas
            Control := Abandon_Children;

         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Identifier
                  | An_Operator_Symbol
                  | An_Enumeration_Literal
                    =>
                  Check (Used_Identifiers (Element));

               when An_Attribute_Reference =>
                  Check ((1 => Element));  -- Check the attribute itself

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

   -------------------------------
   -- Process_Exception_Handler --
   -------------------------------

   procedure Process_Exception_Handler (Handler : in Asis.Exception_Handler) is
      use Asis;

      Control : Traverse_Control  := Continue;
      State   : Null_State;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Traverse (Handler, Control, State);
   end Process_Exception_Handler;

begin  -- Rules.Entity_Inside_Exception
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Entity_Inside_Exception;
