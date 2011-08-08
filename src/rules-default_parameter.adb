----------------------------------------------------------------------
--  Rules.Default_Parameter - Package body                          --
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
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Binary_Map,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);
package body Rules.Default_Parameter is
   use Framework, Utilities;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   type Usage_Kind is (Used, Not_Used);
   subtype Key_Used is Usage_Kind range Used .. Used;

   package Usage_Kind_Utilities is new Framework.Language.Flag_Utilities (Key_Used);
   use Usage_Kind_Utilities;

   type Usage_Rec is new Basic_Rule_Context with
      record
         Active     : Boolean;
      end record;
   type Usage_Tab is array (Usage_Kind) of Usage_Rec;


   package Parameter_Tree is new Binary_Map
     (Key_Type   => Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
      Value_Type => Usage_Tab,
      "<"        => Ada.Strings.Wide_Unbounded."<",
      ">"        => Ada.Strings.Wide_Unbounded.">");

   type Entity_Context is new Root_Context with
      record
         Formals_Map : Parameter_Tree.Map;
      end record;
   procedure Clear (Context : in out Entity_Context);

   Entities : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Parameter 1: <Subprogram or generic name>");
      User_Message  ("Parameter 2: <Formal parameter name>");
      Help_On_Flags ("Parameter 3: [not]");
      User_Message  ("Control subprogram calls or generic instantiations that use (or not)");
      User_Message  ("the default value for a given parameter");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

      Entity : Entity_Specification;
      Formals_Map : Parameter_Tree.Map;
   begin
      if not Parameter_Exists then
         Parameter_Error ("Missing subprogram or generic name for rule " & Rule_Id);
      end if;
      Entity := Get_Entity_Parameter;

      if not Parameter_Exists then
         Parameter_Error ("Missing formal name for rule " & Rule_Id);
      end if;

      declare
         Formal      : constant Wide_String := To_Upper (Get_String_Parameter);
         Usage       : Usage_Kind;
         Affirmative : Boolean;
      begin
         if Parameter_Exists then
            Affirmative := not Get_Modifier ("NOT");
            Usage       := Get_Flag_Parameter (Allow_Any => False);
            if not Affirmative then
               Usage := Not_Used;
            end if;
         else
            Usage := Used;
         end if;

         begin
            Associate (Entities, Entity, Entity_Context'(Formals_Map => Formals_Map));
         exception
            when Already_In_Store =>
               null;
         end;

         -- Note: Maps have object (reference) semantics
         Formals_Map := Entity_Context (Association (Entities, Entity)).Formals_Map;
         declare
            Val : Usage_Tab := Parameter_Tree.Fetch (Formals_Map,
                                                     To_Unbounded_Wide_String (Formal),
                                                     Default_Value => (others =>
                                                                         (Basic.New_Context (Search, "") with False)));
         begin
            Val (Usage) := (Basic.New_Context (Rule_Type, Label) with True);
            Parameter_Tree.Add (Formals_Map, To_Unbounded_Wide_String (Formal), Val);
            Update (Entities, Entity_Context'(Formals_Map => Formals_Map));
         exception
            when Already_In_Store =>
               Parameter_Error ("Formal already specified: " & Formal);
         end;
      end;
      Rule_Used  := True;
   end Add_Use;

   -----------
   -- Clear --
   -----------

   procedure Clear (Context : in out Entity_Context) is
      use Parameter_Tree;
   begin
      Clear (Context.Formals_Map);
   end Clear;

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
      -- We do not balance the trees for each formal.
      -- Since we do not expect more than 1 or 2 entries in each...
   end Prepare;

   -----------------------------------
   -- Process_Call_Or_Instantiation --
   -----------------------------------

   procedure Process_Call_Or_Instantiation (Element : in Asis.Element) is
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Elements, Asis.Expressions, Asis.Declarations, Asis.Statements;
      use Parameter_Tree, Thick_Queries;

      function Get_Association_List (E : in Asis.Element) return Asis.Association_List is
      begin
         if Expression_Kind (Element) = A_Function_Call then
            return Function_Call_Parameters (E, Normalized => True);
         elsif Statement_Kind (Element) = A_Procedure_Call_Statement or
           Statement_Kind (Element) = An_Entry_Call_Statement
         then
            return Call_Statement_Parameters (E, Normalized => True);
         elsif Declaration_Kind (Element) in A_Generic_Instantiation
           or Declaration_Kind (Element) = A_Formal_Package_Declaration
         then
            return Generic_Actual_Part (E, Normalized => True);
         else
            Failure ("Unexpected element in Process for Default_Parameter", Element);
         end if;
      end Get_Association_List;

      Name : Asis.Expression;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Element_Kind (Element) is
         when A_Declaration =>
            -- Must be A_Generic_Instantiation or A_Formal_Package_Declaration
            Name := Generic_Unit_Name (Element);
         when An_Expression =>
            -- Must be A_Function_Call
            Name := Called_Simple_Name (Element);
            -- Avoid using Called_Simple_Name for (non attribute) functions due to ASIS bug
            if Expression_Kind (Name) /= An_Attribute_Reference then
               declare
                  Called : constant Asis.Declaration := A4G_Bugs.Corresponding_Called_Function (Element);
               begin
                  if Is_Nil (Called) then
                     -- Some predefined stuff
                     Name := Nil_Element;
                  else
                     Name := Names (Called)(1);
                  end if;
               end;
            end if;
         when others =>
            -- Must be a procedure or entry call
            Name := Called_Simple_Name (Element);
      end case;

      declare
         use Framework.Reports;
         Current_Context : constant Root_Context'Class := Matching_Context (Entities, Name);
      begin
         if Current_Context = No_Matching_Context then
            return;
         end if;

         declare
            Associations    : constant Asis.Association_List := Get_Association_List (Element);
            Formal          : Asis.Element;
            Formals_Context : Entity_Context renames Entity_Context (Current_Context);
            Usage           : Usage_Tab;
         begin
            for I in Associations'Range loop
               Formal := Formal_Parameter (Associations (I));
               if Is_Present (Formals_Context.Formals_Map,
                              To_Unbounded_Wide_String (To_Upper (Defining_Name_Image (Formal))))
               then
                  Usage := Fetch (Formals_Context.Formals_Map,
                                  To_Unbounded_Wide_String (To_Upper
                                                            (Defining_Name_Image (Formal))));
                  if Usage (Used).Active then
                     if Is_Defaulted_Association (Associations (I)) then
                        Report (Rule_Id,
                                Usage (Used),
                                Get_Location (Element),
                                "default use of formal """ & Defining_Name_Image (Formal) & '"');
                     end if;
                  end if;

                  if Usage (Not_Used).Active then
                     if not Is_Defaulted_Association (Associations (I)) then
                        Report (Rule_Id,
                                Usage (Not_Used),
                                Get_Location (Element),
                                "non default use of formal """ & Defining_Name_Image (Formal) & '"');
                     end if;
                  end if;
               end if;
            end loop;
         end;
      end;
   end Process_Call_Or_Instantiation;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access,
                                              Prepare => Prepare'Access);
end Rules.Default_Parameter;
