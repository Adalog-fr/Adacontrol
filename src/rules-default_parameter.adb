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
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Binary_Map,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Default_Parameter is
   use Ada.Strings.Wide_Unbounded;
   use Framework, Framework.Control_Manager, Utilities;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   type Usage_Kind is (Used, Positional, Not_Used);
   package Usage_Kind_Utilities is new Framework.Language.Flag_Utilities (Usage_Kind);

   type Entity_Kind is (E_Name, E_Calls, E_Instantiations);
   package Entity_Kind_Utilities is new Framework.Language.Flag_Utilities (Entity_Kind, Prefix => "E_");

   type Usage_Rec is new Basic_Rule_Context with
      record
         Active : Boolean;
      end record;
   type Usage_Tab is array (Usage_Kind) of Usage_Rec;
   Empty_Usage : constant Usage_Tab := (others => (Basic_Rule_Context with Active => False));

   package Parameter_Tree is new Binary_Map (Key_Type   => Unbounded_Wide_String,
                                             Value_Type => Usage_Tab);

   type Entity_Context is new Root_Context with
      record
         Formals_Map : Parameter_Tree.Map;
      end record;
   procedure Clear (Context : in out Entity_Context);

   Entities : Context_Store;

   Key_All               : constant Unbounded_Wide_String := To_Unbounded_Wide_String ("ALL");
   Entity_Calls          : constant Entity_Specification  := Value ("CALLS");
   Entity_Instantiations : constant Entity_Specification  := Value ("INSTANTIATIONS");

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control subprogram calls or generic instantiations that use the default for a");
      User_Message  ("given defaulted parameter, or provide it, positionally or using any notation");
      User_Message;
      Entity_Kind_Utilities.Help_On_Flags (Header      => "Parameter 1:",
                                           Extra_Value => "<Subprogram or generic name>");
      User_Message  ("Parameter 2: <Formal parameter name> | all");
      Usage_Kind_Utilities.Help_On_Flags ("Parameter 3:");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Usage_Kind_Utilities, Entity_Kind_Utilities;

      Entity      : Entity_Specification;
      E_Kind      : Entity_Kind;

      procedure Update (Ent : Entity_Specification; Form : Wide_String; Usg : Usage_Kind) is
         Formals_Map : Parameter_Tree.Map;
      begin
         begin
            Associate (Entities, Ent, Entity_Context'(Formals_Map => Formals_Map));
         exception
            when Already_In_Store =>
               null;
         end;

         -- Note: Maps have object (reference) semantics
         Formals_Map := Entity_Context (Association (Entities, Ent)).Formals_Map;
         declare
            Val : Usage_Tab := Parameter_Tree.Fetch (Formals_Map,
                                                     To_Unbounded_Wide_String (Form),
                                                     Default_Value => Empty_Usage);
         begin
            if Val (Usg).Active then
               Parameter_Error (Rule_Id, "this combination of parameters already specified");
            end if;
            Val (Usg) := (Basic.New_Context (Ctl_Kind, Ctl_Label) with True);
            Parameter_Tree.Add (Formals_Map, To_Unbounded_Wide_String (Form), Val);
            Update (Entities, Entity_Context'(Formals_Map => Formals_Map));
         end;
      end Update;

   begin  -- Add_Control
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "missing subprogram or generic name");
      end if;

      E_Kind := Get_Flag_Parameter (Allow_Any => True);
      if E_Kind = E_Name then
         Entity := Get_Entity_Parameter;
      end if;
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "missing formal name");
      end if;

      declare
         Formal : constant Wide_String := Get_Name_Parameter;
         -- Note: "ALL" is handled as a regular name
         Usage : Usage_Kind;
      begin
         if Parameter_Exists then
            Usage := Get_Flag_Parameter (Allow_Any => False);
         else
            Parameter_Error (Rule_Id, "usage kind expected");
         end if;

         case E_Kind is
            when E_Name =>
               Update (Entity, Formal, Usage);
            when E_Calls =>
               Update (Entity_Calls, Formal, Usage);
            when E_Instantiations =>
               Update (Entity_Instantiations, Formal, Usage);
         end case;
      end;
      Rule_Used  := True;
   end Add_Control;

   -------------
   -- Or_Else --
   -------------

   function Or_Else (L, R : Usage_Tab) return Usage_Tab is
      Result : Usage_Tab;
   begin
      for U in Usage_Kind loop
         if L (U).Active then
            Result (U) := L (U);
         else
            Result (U) := R (U);
         end if;
      end loop;
      return Result;
   end Or_Else;

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
      use Asis, Asis.Elements, Asis.Declarations;
      use Thick_Queries;

      Name : Asis.Expression;

      function Get_Formals_List return Asis.Element_List is
      begin
         if Expression_Kind (Name) = An_Attribute_Reference then
            -- Calls to attributes must be ignored, since they have no formal name (and no default value)
            return Nil_Element_List;
         elsif Expression_Kind (Element) = A_Function_Call then
            return Called_Profile (Element);
         elsif Statement_Kind (Element) = A_Procedure_Call_Statement or
           Statement_Kind (Element) = An_Entry_Call_Statement
         then
            return Called_Profile (Element);
         elsif Declaration_Kind (Element) in A_Generic_Instantiation
           or Declaration_Kind (Element) = A_Formal_Package_Declaration
         then
            return Generic_Formal_Part (A4G_Bugs.Corresponding_Name_Declaration
                                        (Simple_Name
                                         (Ultimate_Name
                                          (Generic_Unit_Name (Element)))));
         else
            Failure ("Unexpected element in Process for Default_Parameter", Element);
         end if;
      end Get_Formals_List;

      procedure Check (Formal : Asis.Expression; Name_Context, All_Context : Root_Context'Class)
      is
         use Asis.Expressions;
         use Framework.Reports, Parameter_Tree;

         Formal_Key : constant Unbounded_Wide_String
           := To_Unbounded_Wide_String (To_Upper (Defining_Name_Image (Formal)));
         Usage         : Usage_Tab        := Empty_Usage;
         Actual        : constant Asis.Expression := Actual_Expression (Element, Formal, Return_Default => False);
         Is_Defaulted  : constant Boolean := Is_Nil (Actual);
         Is_Positional : constant Boolean := not Is_Defaulted
                                             and then Is_Nil (Formal_Parameter (Enclosing_Element (Actual)));

      begin
         -- Build Usage in order of preferences:
         --    Sp_Name, Formal_Name
         --    Sp_Name, Positional
         --    Sp_Name, All
         --    All,     Formal_Name
         --    All,     Positional
         --    All,     All
         if Name_Context /= No_Matching_Context then
            if Is_Present (Entity_Context (Name_Context).Formals_Map, Formal_Key) then
               Usage := Fetch (Entity_Context (Name_Context).Formals_Map, Formal_Key);
            end if;
            if Is_Present (Entity_Context (Name_Context).Formals_Map, Key_All) then
               Usage := Or_Else (Usage, Fetch (Entity_Context (Name_Context).Formals_Map, Key_All));
            end if;
         end if;
         if All_Context /= No_Matching_Context then
            if Is_Present (Entity_Context (All_Context).Formals_Map, Formal_Key) then
               Usage := Or_Else (Usage, Fetch (Entity_Context (All_Context).Formals_Map, Formal_Key));
            end if;
            if Is_Present (Entity_Context (All_Context).Formals_Map, Key_All) then
               Usage := Or_Else (Usage, Fetch (Entity_Context (All_Context).Formals_Map, Key_All));
            end if;
         end if;

         if Usage (Used).Active then
            if Is_Defaulted then
               Report (Rule_Id,
                       Usage (Used),
                       Get_Location (Element),
                       "default use of formal """ & Defining_Name_Image (Formal) & '"');
            end if;
         end if;

         if Usage (Positional).Active then
            if Is_Positional then
               Report (Rule_Id,
                       Usage (Used),
                       Get_Location (Element),
                       "use of defaulted formal """ & Defining_Name_Image (Formal) & """ with positional association");
            end if;
         end if;

         if Usage (Not_Used).Active then
             if not Is_Defaulted then
               Report (Rule_Id,
                       Usage (Not_Used),
                       Get_Location (Element),
                       "non default use of formal """ & Defining_Name_Image (Formal) & '"');
            end if;
         end if;
      end Check;

      Entity_All : Entity_Specification;
   begin  -- Process_Call_Or_Instantiation
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Element_Kind (Element) is
         when A_Declaration =>
            -- Must be A_Generic_Instantiation or A_Formal_Package_Declaration
            Name       := Generic_Unit_Name (Element);
            Entity_All := Entity_Instantiations;
         when An_Expression =>
            -- Must be A_Function_Call
            Name       := Called_Simple_Name (Element);
            Entity_All := Entity_Calls;
         when others =>
            -- Must be a procedure or entry call
            Name       := Called_Simple_Name (Element);
            Entity_All := Entity_Calls;
      end case;

      declare
         Name_Context : constant Root_Context'Class := Matching_Context (Entities,
                                                                         Name,
                                                                         Extend_To => All_Extensions);
         All_Context  : constant Root_Context'Class := Control_Manager.Association (Entities, Entity_All);
      begin
         if Name_Context = No_Matching_Context and All_Context = No_Matching_Context then
            return;
         end if;

         declare
            Formals : constant Asis.Element_List := Get_Formals_List;
         begin
            for I in Formals'Range loop
               case Element_Kind (Formals (I)) is
                  when A_Clause =>
                     -- Use clause in generic formal part
                     null;
                  when A_Declaration =>
                     case Declaration_Kind (Formals (I)) is
                        when A_Parameter_Specification
                           | A_Formal_Object_Declaration
                             =>
                           if not Is_Nil (Initialization_Expression (Formals (I))) then
                              declare
                                 Formal_Names : constant Asis.Expression_List := Names (Formals (I));
                              begin
                                 for F in Formal_Names'Range loop
                                    Check (Formal_Names (F), Name_Context, All_Context);
                                 end loop;
                              end;
                           end if;
                        when A_Formal_Procedure_Declaration
                           | A_Formal_Function_Declaration
                             =>
                           case Default_Kind (Formals (I)) is
                              when Not_A_Default =>
                                 Failure ("Not_A_Default");
                              when A_Name_Default
                                 | A_Box_Default
                                   =>
                                 Check (Names (Formals (I))(1), Name_Context, All_Context);
                              when A_Nil_Default =>
                                 null;

                              -- Depending on A4G version, the following branch may cover 0 values, causing
                              -- a warning
                              pragma Warnings (Off);
                              when others =>  -- Ada 2005: A_Null_Default
                                 null;
                              pragma Warnings (On);
                           end case;
                        when others =>
                           -- Others cases have no possible default value
                           null;
                     end case;
                  when others =>
                     Failure ("Bad formal", Formals (I));
               end case;
            end loop;
         end;
      end;
   end Process_Call_Or_Instantiation;

begin  -- Rules.Default_Parameter
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Default_Parameter;
