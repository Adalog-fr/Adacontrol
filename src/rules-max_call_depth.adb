----------------------------------------------------------------------
--  Rules.Max_Call_Depth - Package body                             --
--                                                                  --
--  This software  is (c) SAGEM DS and  Adalog  2004-2006.  The Ada --
--  Controller  is  free software;  you can redistribute  it and/or --
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
  Asis.Iterator;

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

package body Rules.Max_Call_Depth is
   use Framework, Ada.Strings.Wide_Unbounded;

   -- Algorithm:
   --
   -- The function Max_Call_Depth computes the maximum depth of a *call*, i.e. it returns
   -- at least 1.
   -- Since the call depth is a property of the callable entity, the value is kept in the
   -- Call_Depths map to avoid analyzing the same callable entity twice.
   -- The map is *not* reset between runs, since it is a static property, once you have it,
   -- it won't change depending on parameters!
   --
   -- The "call depth" is defined as the number of frames pushed on the stack, therefore:
   --   - A task entry call counts always for 1, irrespectively of what happens in the accept body,
   --     since the accept is executed on a different stack. Of course, the same does /not/ apply
   --     to protected entries.
   --   - Similarly, calls during the elaboration of task bodies are not counted.
   --   - Calls to non-statically determinable callable entities (access to SP, dispatching calls,
   --     calls to imported SP) are deemed to have a depth of 1, short of a better solution.
   --   - Operands of a call do not add extra depth, i.e. a call to P(F(X)) has a depth of 1 (operands
   --     are evaluated by the caller before the call, so there is only one frame stacked at a time)
   --   - Calls happening during the elaboration of nested packages must be counted, but not calls
   --     that are part of any other nested program unit.
   --   - Calls that are part of the elaboration of types, subtypes, and objects must be counted,
   --     but not those that are part of the default expression of components and discriminants.
   --     Strictly speaking, calls that appear as part of per-object constraints should not be counted
   --     here, but at the place where an object is defined; we'll forget about this because it is
   --     not worth the complication.
   --
   -- We don't need to do anything special for generics, since we are starting from calls, they will
   -- always refer to (parts of) instantiations.

   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Ctl_Labels : array (Control_Kinds) of Unbounded_Wide_String;

   Infinite : constant Natural := Natural'Last;
   Unused   : constant Integer := -1;
   Depths   : array (Control_Kinds) of Integer := (others => Unused);
   -- Depth that triggers the message, i.e. allowed depth + 1

   type Called_Kind is (Enumerated, Recursive, Regular);
   type Entity_Descriptor is
      record
         Kind  : Called_Kind;
         Depth : Natural;
      end record;
   package Depth_Map is new Binary_Map (Unbounded_Wide_String, Entity_Descriptor);
   Call_Depths : Depth_Map.Map;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter: <Allowed depth> | finite");
      User_Message ("Control maximum call depth");
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

      if Depths (Ctl_Kind) /= Unused then
         Parameter_Error (Rule_Id, "rule already specified");
      end if;

      if Is_Integer_Parameter then
         Depths (Ctl_Kind) := Get_Integer_Parameter (Min => 0) + 1;
         -- + 1 since we store the depth wich is an error
      else
         declare
            Param : constant Wide_String := Get_Name_Parameter;
         begin
            if Param /= "FINITE" then
               Parameter_Error (Rule_Id, "depth or ""finite"" expected for parameter");
            end if;
            Depths (Ctl_Kind) := Infinite;
         end;
      end if;

      if Parameter_Exists then
         Parameter_Error (Rule_Id, "only one parameter allowed");
      end if;

      Ctl_Labels (Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
      Rule_Used             := True;
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
            Depths    := (others => Unused);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ----------------
   -- Call_Depth --
   ----------------

   function Entity_Call_Depth (Decl : Asis.Declaration; Call : Asis.Element) return Natural;

   function Call_Depth (Call : Asis.Element) return Natural is
      -- Computes the depth of a call, including itself
      use Asis, Asis.Elements;
      use Depth_Map, Framework.Reports, Thick_Queries;

      Called       : constant Asis.Expression := Ultimate_Name (Called_Simple_Name (Call));
      Called_Name  : Unbounded_Wide_String;
      Called_Descr : Call_Descriptor;
      Called_Depth : Entity_Descriptor;
   begin
      if not Is_Nil (Called) then
         Called_Name := To_Unbounded_Wide_String (Full_Name_Image (Called, With_Profile => True));
         if Is_Present (Call_Depths, Called_Name) then
            Called_Depth := Fetch (Call_Depths, Called_Name);
            case Called_Depth.Kind is
               when Recursive =>
                  return Infinite;
               when Enumerated =>
                  return 0;
               when Regular =>
                  return Called_Depth.Depth + 1;
            end case;
         end if;
      end if;

      Called_Descr := Corresponding_Call_Description (Call);
      case Called_Descr.Kind is
         when An_Attribute_Call | A_Predefined_Entity_Call =>
            -- Assume these functions have a depth of 0
            Called_Depth := (Regular, 0);

         when A_Dereference_Call | A_Dispatching_Call =>
            if Is_Part_Of_Instance (Call) then
               -- Unfortunately, Corresponding_Generic_Element does not work on call.
               -- Let the message reference the instantiation instead
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Ultimate_Enclosing_Instantiation (Call)),
                            "Dispatching call, call to predefined or dynamic entity in generic; "
                            & "assuming depth of 1");
            else
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Call),
                            "Dispatching call, call to predefined or dynamic entity; assuming depth of 1");
            end if;
            -- Short of knowing, assume depth of 1
            -- Return directly, since there is no name to add to Call_Depths in this case
            return 1;

         when A_Regular_Call =>
            if Declaration_Kind (Called_Descr.Declaration) = An_Enumeration_Literal_Specification then
               -- Do not even count these as calls
               Called_Depth := (Enumerated, 0);
            else
               -- Normal case
               Called_Depth := (Regular, Entity_Call_Depth (Called_Descr.Declaration, Call));
               if Called_Depth.Depth = Infinite then
                  Called_Depth.Kind := Recursive;
               end if;
            end if;
      end case;

      Add (Call_Depths, Called_Name, Called_Depth);

      case Called_Depth.Kind is
         when Recursive =>
            return Infinite;
         when Enumerated =>
            return 0;
         when Regular =>
            return Called_Depth.Depth + 1;
      end case;
   end Call_Depth;

   -----------------------
   -- Entity_Call_Depth --
   -----------------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            Count   : in out Natural);
   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             Count   : in out Natural);
   procedure Traverse is new Asis.Iterator.Traverse_Element (Natural, Pre_Procedure, Post_Procedure);
   -- Computes the maximum depth of all calls encountered in the body.

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            Count   : in out Natural) is
      use Thick_Queries, Utilities;
      use Asis, Asis.Declarations, Asis.Elements;

      Temp : Asis.Element;
   begin
      case Element_Kind (Element) is
         when An_Expression =>
            case Expression_Kind (Element) is
               when A_Function_Call =>
                  Count := Natural'Max (Count, Call_Depth (Element));
                  if Count = Infinite then
                     Control := Terminate_Immediately;
                  end if;
               when others =>
                  null;
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_Procedure_Call_Statement
                    | An_Entry_Call_Statement
                    =>
                  Count := Natural'Max (Count, Call_Depth (Element));
                  if Count = Infinite then
                     Control := Terminate_Immediately;
                  end if;
               when others =>
                  null;
            end case;

         when A_Declaration =>
            case Declaration_Kind (Element) is
               when Not_A_Declaration =>
                  Failure ("not a declaration");
               when An_Ordinary_Type_Declaration
                  | A_Task_Type_Declaration
                  | A_Protected_Type_Declaration
                  | A_Private_Type_Declaration
                  | A_Private_Extension_Declaration
                  | A_Subtype_Declaration
                    =>
                  -- Traverse the definition, but not the discriminant part
                  Temp := Type_Declaration_View (Element);
                  if not Is_Nil (Temp) then
                     -- Temp is nil for an empty task type declaration (task T;)
                     Traverse (Temp, Control, Count);
                  end if;
                  Control := Abandon_Children;
               when An_Incomplete_Type_Declaration
                  | A_Deferred_Constant_Declaration
                  | An_Integer_Number_Declaration
                  | A_Real_Number_Declaration
                  | An_Enumeration_Literal_Specification
                  | A_Discriminant_Specification
                  | A_Procedure_Declaration
                  | A_Function_Declaration
                  | A_Procedure_Body_Declaration
                  | A_Function_Body_Declaration
                  | A_Task_Body_Declaration
                  | A_Protected_Body_Declaration
                  | An_Entry_Declaration
                  | An_Entry_Body_Declaration
                  | A_Body_Stub
                  | An_Exception_Declaration
                  | A_Choice_Parameter_Specification
                  | A_Generic_Declaration
                    =>
                  -- Nothing interesting here for us
                  Control := Abandon_Children;
               when A_Variable_Declaration
                  | A_Constant_Declaration
                  | A_Single_Task_Declaration
                  | A_Single_Protected_Declaration
                  | A_Loop_Parameter_Specification
                  | A_Package_Declaration
                  | A_Generic_Instantiation
                    =>
                  -- Let's recurse normally
                  null;
               when A_Renaming_Declaration =>
                  -- Traverse only the renamed entity (not the new name)
                  Traverse (A4G_Bugs.Renamed_Entity (Element), Control, Count);
                  Control := Abandon_Children;
               when A_Package_Body_Declaration =>
                  -- Recurse normally if it is not a generic body
                  if Is_Generic_Unit (Element) then
                     Control := Abandon_Children;
                  end if;
               when A_Component_Declaration =>
                  -- Traverse the declaration, but not the initialization expression
                  Traverse (Object_Declaration_View (Element), Control, Count);
                  Control := Abandon_Children;
               when A_Parameter_Specification
                  | An_Entry_Index_Specification
                  | A_Formal_Declaration
                    =>
                  -- Should not happen since we don't traverse the corresponding parent node
                  Failure ("Unexpected declaration: "
                           & Declaration_Kinds'Wide_Image (Declaration_Kind (Element)), Element);
               when others =>
                  -- Ada 2005 declaration kinds
                  null;
            end case;

         when others =>
            null;
      end case;
   end Pre_Procedure;

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             Count   : in out Natural) is
      pragma Unreferenced (Element, Control, Count);
   begin
      null;
   end Post_Procedure;

   function Entity_Call_Depth (Decl : Asis.Declaration; Call : Asis.Element) return Natural is
      -- The call depth of an entity is the maximum of all calls inside it, i.e.:
      -- returns 0 if Decl is the declaration of a callable_entity that calls nothing
      -- returns 1 if Decl is the declaration of a callable_entity that calls only entities of depth 0
      --    ...
      -- returns Infinite if Decl is the declaration of a callable_entity that is directly or indirectly recursive
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Depth_Map, Framework.Reports, Thick_Queries, Utilities;

      Called_Name : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Full_Name_Image
                                                                                (Ultimate_Name (Names (Decl)(1)),
                                                                                 With_Profile => True));
      Called_Body    : Asis.Declaration;
      Renaming       : Asis.Expression;
      Control        : Traverse_Control := Continue;
      Count          : Natural          := 0;
      Is_Uncheckable : Boolean := False;
      Result         : Entity_Descriptor  := (Regular, 0);
   begin
      -- Called_Body walks the structure until we find the real body corresponding to Decl
      -- So, it is really the called body only after this loop!
      Called_Body := Decl;

      -- This loop is exited from the tests at the top, or when an acceptable body is found
      loop
         if Is_Uncheckable or Is_Nil (Called_Body) then
            exit;
         end if;
         if Is_Banned (Called_Body, Rule_Id) then
            Is_Uncheckable := True;
            exit;
         end if;

         case Declaration_Kind (Called_Body) is
            when A_Procedure_Declaration
               | A_Function_Declaration
               | A_Generic_Procedure_Declaration
               | A_Generic_Function_Declaration
               | A_Procedure_Instantiation
               | A_Function_Instantiation
                 =>
               Called_Body := Corresponding_Body (Called_Body);
            when An_Entry_Declaration  =>
               if Is_Task_Entry (Called_Body) then
                  -- A task entry => not followed
                  Called_Body := Nil_Element;
               else
                  Called_Body := Corresponding_Body (Called_Body);
               end if;
            when A_Procedure_Body_Declaration
              | A_Function_Body_Declaration
              | An_Entry_Body_Declaration
              =>
               -- A real body (at last!)
               exit;
            when A_Procedure_Body_Stub
              | A_Function_Body_Stub
              =>
               Called_Body := Corresponding_Subunit (Called_Body);
            when A_Procedure_Renaming_Declaration
              | A_Function_Renaming_Declaration
              =>
               Renaming := Simple_Name (A4G_Bugs.Renamed_Entity (Called_Body));
               case Expression_Kind (Renaming) is
                  when An_Explicit_Dereference =>
                     -- renaming of not static stuff
                     Is_Uncheckable := True;
                  when An_Indexed_Component =>
                     -- This can happen when renaming a member of an entry family as a procedure
                     -- (sigh)
                     Renaming := Simple_Name (Prefix (Renaming));
                  when An_Attribute_Reference
                    | An_Enumeration_Literal
                    =>
                     Called_Body := Nil_Element;
                  when others =>
                     null;
               end case;
               Called_Body := A4G_Bugs.Corresponding_Name_Declaration (Renaming);
            when A_Formal_Function_Declaration
               | A_Formal_Procedure_Declaration
                 =>
               Is_Uncheckable := True;
            when Not_A_Declaration =>
               -- this should happen only when the body is given by a pragma import
               Assert (Element_Kind (Called_Body) = A_Pragma, "Entity_Call_Depth: not a declaration or pragma");
               Is_Uncheckable := True;
            when others =>
               Failure ("not a callable entity declaration", Called_Body);
         end case;
      end loop;

      if Is_Uncheckable then
         -- predefined stuff, call from access to SP or dispatching call (sigh!), banned unit
         -- or imported SP
         -- the best we can do is to consider that it calls nothing

         if Is_Part_Of_Instance (Call) then
            -- Unfortunately, Corresponding_Generic_Element does not work on call.
            -- Let the message reference the instantiation instead
            Uncheckable (Rule_Id,
                         False_Negative,
                         Get_Location (Ultimate_Enclosing_Instantiation (Call)),
                         "Call to predefined, dynamic, formal, imported, or inhibited entity in generic; "
                           & "assuming depth of 1");
         else
            Uncheckable (Rule_Id,
                         False_Negative,
                         Get_Location (Call),
                         "Call to predefined, dynamic, formal, imported, or inhibited entity; assuming depth of 1");
         end if;
         Count := 0;
      elsif Is_Nil (Called_Body) then
         -- Task entry, attribute, literal
         -- Has no body, but we assume a depth of 0
         Count := 0;
      else
         -- Initialize to Infinite before traversing. This way, if it is truly recursive,
         -- it will be found in the map and the result will be Infinite.
         Add (Call_Depths, Called_Name, (Recursive, Infinite));

         -- We cannot directly traverse the whole body, since bodies are discarded
         -- We traverse all the parts manually (except formal parameters, of course)
         -- Of course, we can stop the traversal as soon as we determine that the
         -- SP is recursive.
         declare
            Recursivity_Found : exception;
         begin
            declare
               Body_Decls : constant Asis.Declaration_List := Body_Declarative_Items (Called_Body);
            begin
               for I in Body_Decls'Range loop
                  Traverse (Body_Decls (I), Control, Count);
                  if Count = Infinite then
                     raise Recursivity_Found;
                  end if;
               end loop;
            end;
            declare
               Body_Stats: constant Asis.Statement_List := Body_Statements (Called_Body);
            begin
               for I in Body_Stats'Range loop
                  Traverse (Body_Stats (I), Control, Count);
                  if Count = Infinite then
                     raise Recursivity_Found;
                  end if;
               end loop;
            end;
            declare
               Body_Handlers: constant Asis.Exception_Handler_List := Body_Exception_Handlers (Called_Body);
            begin
               for I in Body_Handlers'Range loop
                  Traverse (Body_Handlers (I), Control, Count);
                  if Count = Infinite then
                     raise Recursivity_Found;
                  end if;
               end loop;
            end;
         exception
            when Recursivity_Found =>
               Result := (Recursive, Infinite);
         end;
      end if;
      Result.Depth := Count;

      Add (Call_Depths, Called_Name, Result);
      return Count;
   end Entity_Call_Depth;


   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Element) is
      Depth : Natural;

      procedure Do_Report (Ctl_Kind : Control_Kinds) is
         use Framework.Reports, Utilities;
      begin
         if Depth = Infinite then
             Report (Rule_Id,
                    To_Wide_String (Ctl_Labels (Ctl_Kind)),
                    Ctl_Kind,
                    Get_Location (Call),
                    "Call to recursive entity");
        else
            Report (Rule_Id,
                    To_Wide_String (Ctl_Labels (Ctl_Kind)),
                    Ctl_Kind,
                    Get_Location (Call),
                    "Call has a depth of " & Integer_Img (Depth));
         end if;
      end Do_Report;

   begin  -- Process_Call
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Depth := Call_Depth (Call);
      if Depths (Check) /= Unused and then Depth >= Depths (Check) then
         Do_Report (Check);
      elsif Depths (Search) /= Unused and then Depth >= Depths (Search) then
         Do_Report (Search);
      end if;

      if Depths (Count) /= Unused and then Depth >= Depths (Count) then
         Do_Report (Count);
      end if;

   end Process_Call;

begin  -- Rules.Max_Call_Depth
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Max_Call_Depth;
