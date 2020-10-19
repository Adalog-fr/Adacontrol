----------------------------------------------------------------------
--  Rules.Max_Call_Depth - Package body                             --
--                                                                  --
--  This software is (c) SAGEM DS and Adalog 2004-2006.             --
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

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Errors,
  Asis.Exceptions,
  Asis.Expressions,
  Asis.Implementation,
  Asis.Iterator;

-- Adalog
with
  A4G_Bugs,
  Binary_Map,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Queries,
  Framework.Variables,
  Framework.Variables.Shared_Types;

package body Rules.Max_Call_Depth is
   use Ada.Strings.Wide_Unbounded;
   use Framework, Framework.Variables, Framework.Variables.Shared_Types;

   -- Algorithm:
   --
   -- The function Call_Depth computes the maximum depth of a *call*, i.e. it returns
   -- at least 1.
   -- Since the call depth is a property of the callable entity, the value is kept in the
   -- Call_Depths map to avoid analyzing the same callable entity twice.
   -- "Forced" entities provided as parameters are kept in a separate context store (Forced_Entities), and
   -- entered into Call_Depths as they are encountered.
   --
   -- The Call_Depths map is reset between runs only if there are forced entities; otherwise, it is a static
   -- property, once you have it, it won't change.
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

   Infinite : constant Asis.ASIS_Natural := Asis.ASIS_Natural'Last;
   Unused   : constant Asis.ASIS_Integer := Asis.ASIS_Integer'Val(-1);
   Depths   : array (Control_Kinds) of Asis.ASIS_Integer := (others => Unused);
   -- Depth that triggers the message, i.e. allowed depth + 1

   type Called_Kind is (Regular, Inline, Recursive, Banned, Formal, Unavailable, Unknown, Dynamic);
   subtype Unexplored is Called_Kind range Banned .. Unavailable;
   -- Regular .. Imported are really properties of the called entity
   -- Dynamic .. Unknown are properties of the call.
   -- But it's not worth making two different types for this subtility
   --
   -- A value in Unexplored is returned by Call_Depth if it is a direct call to a subprogram with the
   -- corresponding property.
   -- If the call is to something that indirectly calls an Unexplored SP, the returned kind is Unknown.

   type Depth_Descriptor is
      record
         Kind  : Called_Kind;
         Depth : Asis.ASIS_Natural;
         -- Infinite      for Recursive
         -- Actual depth  for Regular
         -- Minimum depth for unknown
      end record;
   package Depth_Map is new Binary_Map (Unbounded_Wide_String, Depth_Descriptor);
   Call_Depths : Depth_Map.Map;

   Forced_Entities : Control_Manager.Context_Store;

   -- Rule variables
   Count_Expr_Fun_Calls : aliased Switch_Type.Object := (Value => On);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control maximum call depth");
      User_Message;
      User_Message ("Parameter (1): <Allowed depth> | finite");
      User_Message ("Parameter (2..): <Forced entity>");
      User_Message;
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Count_Expr_Fun_Calls");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Framework.Control_Manager;
      use Depth_Map;

      use type Asis.ASIS_Integer;   -- Gela-ASIS compatibility
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one parameter required");
      end if;

      if Depths (Ctl_Kind) /= Unused then
         Parameter_Error (Rule_Id, "rule already specified");
      end if;

      if Is_Integer_Parameter then
         -- We limit max to Infinite-2 so that there can be no confusion with Infinite after adding 1.
         -- Should be more than enough anyway...
         Depths (Ctl_Kind) := Get_Integer_Parameter (Min => 0, Max => Infinite-2) + 1;
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
         -- Forced entities provided: cannot keep previous Call_Depths
         Clear (Call_Depths);

         while Parameter_Exists loop
            Associate (Forced_Entities, Get_Entity_Parameter, Null_Context);
         end loop;
      end if;

      Ctl_Labels (Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
      Rule_Used             := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager, Framework.Control_Manager;
      use Depth_Map;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Depths    := (others => Unused);
            if not Is_Empty (Forced_Entities) then
               -- we had forced entities, need to clear Call_Depths
               Clear (Call_Depths);
            end if;
            Clear (Forced_Entities);
            Count_Expr_Fun_Calls := (Value => On);
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
      use Framework.Control_Manager;
   begin
      Balance (Forced_Entities);
   end Prepare;


   ------------------------
   -- Report_Uncheckable --
   ------------------------

   procedure Report_Uncheckable (Call : Asis.Element; Message : Wide_String; Assumed : Asis.ASIS_Natural) is
      use Asis.Elements;
      use Framework.Locations, Framework.Reports, Thick_Queries, Utilities;
   begin
      if Is_Part_Of_Instance (Call) then
         -- Unfortunately, Corresponding_Generic_Element does not work on call.
         -- Let the message reference the instantiation instead
         Uncheckable (Rule_Id,
                      False_Negative,
                      Get_Location (Ultimate_Enclosing_Instantiation (Call)),
                      Message & " in generic; assuming depth of " & ASIS_Integer_Img (Assumed));
      else
         Uncheckable (Rule_Id,
                      False_Negative,
                      Get_Location (Call),
                      Message & "; assuming depth of " & ASIS_Integer_Img (Assumed));
      end if;
   end Report_Uncheckable;

   ----------------
   -- Call_Depth --
   ----------------

   function Entity_Call_Depth (Decl : Asis.Declaration) return Depth_Descriptor;

   function Call_Depth (Call : Asis.Element) return Depth_Descriptor is
   -- Computes the depth of a call, including itself
      use Asis, Asis.Elements;
      use Depth_Map, Framework.Control_Manager, Framework.Queries, Thick_Queries;

      Called       : constant Asis.Expression := Ultimate_Name (Called_Simple_Name (Call));
      Called_Name  : Unbounded_Wide_String;
      Called_Descr : Call_Descriptor;
      Called_Depth : Depth_Descriptor;
   begin
      if Is_Nil (Called) then
         return (Dynamic, 1);
      end if;

      Called_Name := To_Key (Called);
      if Is_Present (Call_Depths, Called_Name) then
         Called_Depth := Fetch (Call_Depths, Called_Name);

      elsif Matching_Context (Forced_Entities, Called, Extend_To => All_Extensions) /= No_Matching_Context then
         Called_Depth := (Regular, 0);
         Add (Call_Depths, Called_Name, Called_Depth);   -- will be found faster next time

      else
         Called_Descr := Corresponding_Call_Description (Call);
         case Called_Descr.Kind is
            when An_Attribute_Call =>
               -- Short of knowing, assume they are implemented with a regular call, with no further calls
               Called_Depth := (Regular, 0);

            when A_Predefined_Entity_Call =>
               -- Assume these are generated in-line
               Called_Depth := (Inline, 0);

            when A_Dereference_Call | A_Dispatching_Call =>
               -- Short of knowing, assume depth of 1
               -- Return directly, since there is no name to add to Call_Depths in this case
               return (Dynamic, 1);

            when An_Enumeration_Literal =>
               -- Do not even count these as calls
               Called_Depth := (Inline, 0);

            when A_Regular_Call =>
               -- Normal case
               Called_Depth := Entity_Call_Depth (Called_Descr.Declaration);
               if Called_Depth.Kind = Regular
                 and then Declaration_Kind (Called_Descr.Declaration) = An_Expression_Function_Declaration
                 and then Count_Expr_Fun_Calls.Value = Off
               then
                  Called_Depth.Kind := Inline;
               end if;
         end case;

         -- This may seem redundant with the call to Add in Entity_Call_Depth, but it isn't if
         -- the Called_Name is a renaming, since we register here the new name, and Entity_Call_Depth
         -- does the same for the ultimate name. Granted, for regular calls it is added twice, but this
         -- happens only once.
         Add (Call_Depths, Called_Name, Called_Depth);
      end if;

      case Called_Depth.Kind is
         when Inline | Recursive | Dynamic =>
            return Called_Depth;
         when Regular | Unexplored | Unknown =>
            -- All cases where something is actually called (although we may not know very well what)
            return (Called_Depth.Kind, Called_Depth.Depth + 1);
      end case;
   end Call_Depth;

   -----------------------
   -- Entity_Call_Depth --
   -----------------------

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            Descr   : in out Depth_Descriptor);
   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             Descr   : in out Depth_Descriptor);
   procedure Traverse is new Asis.Iterator.Traverse_Element (Depth_Descriptor, Pre_Procedure, Post_Procedure);
   -- Computes the maximum depth of all calls encountered in the body.

   procedure Pre_Procedure (Element : in     Asis.Element;
                            Control : in out Asis.Traverse_Control;
                            Descr   : in out Depth_Descriptor)
   is
      use Thick_Queries, Utilities;
      use Asis, Asis.Declarations, Asis.Elements;

      Temp       : Asis.Element;
      This_Descr : Depth_Descriptor;
   begin
      case Element_Kind (Element) is
         when An_Expression =>
            case Expression_Kind (Element) is
               when A_Function_Call =>
                  This_Descr := Call_Depth (Element);
                  case This_Descr.Kind is
                     when Recursive =>
                        Descr := This_Descr;
                        -- No need to investigate any further
                        Control := Terminate_Immediately;
                     when Regular | Inline =>
                        -- If Descr.Kind = Unknown, it stays this way
                        Descr.Depth := Asis.ASIS_Natural'Max (Descr.Depth, This_Descr.Depth);
                     when Unexplored | Unknown | Dynamic =>
                        -- All cases where the body is unknown are turned to Unknown at this point
                        Descr := (Unknown, Asis.ASIS_Natural'Max (Descr.Depth, This_Descr.Depth));
                  end case;
               when others =>
                  null;
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_Procedure_Call_Statement
                    | An_Entry_Call_Statement
                    =>
                  This_Descr := Call_Depth (Element);
                  case This_Descr.Kind is
                     when Recursive =>
                        Descr := This_Descr;
                        -- No need to investigate any further
                        Control := Terminate_Immediately;
                     when Regular | Inline =>
                        -- If Descr.Kind = Unknown, it stays this way
                        Descr.Depth := Asis.ASIS_Natural'Max (Descr.Depth, This_Descr.Depth);
                     when Unexplored | Unknown | Dynamic =>
                        -- All cases where the body is unknown are turned to Unknown at this point
                        Descr := (Unknown, Asis.ASIS_Natural'Max (Descr.Depth, This_Descr.Depth));
                  end case;
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
                  if not Is_Nil (Temp)
                     and then Access_Type_Kind (Temp) not in Access_To_Subprogram_Definition
                  then
                     -- Temp is nil for an empty task type declaration (task T;)
                     -- We're not supposed to traverse formal parameters that are part of access to SP
                        Traverse (Temp, Control, Descr);
                  end if;
                  Control := Abandon_Children;
               when An_Incomplete_Type_Declaration
                  | A_Tagged_Incomplete_Type_Declaration
                  | A_Deferred_Constant_Declaration
                  | An_Integer_Number_Declaration
                  | A_Real_Number_Declaration
                  | An_Enumeration_Literal_Specification
                  | A_Discriminant_Specification
                  | A_Procedure_Declaration
                  | A_Null_Procedure_Declaration
                  | A_Function_Declaration
                  | An_Expression_Function_Declaration   -- Ada 2012
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
                  Traverse (Renamed_Entity (Element), Control, Descr);
                  Control := Abandon_Children;
               when A_Package_Body_Declaration =>
                  -- Recurse normally if it is not a generic body
                  if Is_Generic_Unit (Element) then
                     Control := Abandon_Children;
                  end if;
               when A_Component_Declaration =>
                  -- Traverse the declaration, but not the initialization expression
                  Traverse (Object_Declaration_View (Element), Control, Descr);
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

         when A_Definition =>
            case Definition_Kind (Element) is
               when An_Aspect_Specification =>
                  -- 2012, ignored for the moment
                  Control := Abandon_Children;
               when An_Access_Definition =>
                  -- Nothing can make a call here, and traversing it would make problems with
                  -- access to subprograms (we assume the formals are not traversed)
                  Control := Abandon_Children;
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;

   exception
      when Asis.Exceptions.ASIS_Failed =>
         declare
            use Asis.Errors, Asis.Implementation;
         begin
            if Status /= Not_Implemented_Error then
               raise;
            end if;

            -- Not_Implemented_Error
            -- Presumably a use of a "non official" construct (conditional expression...)
            -- This is known to happen in recent versions of the GNAT run-time
            -- Short of any other solution, consider it does not include any call
            -- (i.e. do nothing)
         end;
   end Pre_Procedure;

   procedure Post_Procedure (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             Descr   : in out Depth_Descriptor)
   is
      pragma Unreferenced (Element, Control, Descr);
   begin
      null;
   end Post_Procedure;

   function Entity_Call_Depth (Decl : Asis.Declaration) return Depth_Descriptor is
   -- The call depth of an entity is the maximum of all calls inside it, i.e.:
   -- returns 0 if Decl is the declaration of a callable_entity that calls nothing
   -- returns 1 if Decl is the declaration of a callable_entity that calls only entities of depth 0
   --    ...
   -- returns Infinite if Decl is the declaration of a callable_entity that is directly or indirectly recursive
   --
   -- Precondition: Decl is the declaration of a real subprogram, not of a renaming
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Depth_Map, Framework.Queries, Framework.Rules_Manager, Thick_Queries, Utilities;

      Called_Name : constant Unbounded_Wide_String := To_Key (Names (Decl)(1));
      Called_Body : Asis.Declaration;
      Control     : Traverse_Control := Continue;
      Result      : Depth_Descriptor;

      procedure Analyze_Body is
         Recursivity_Found : exception;
      begin
         -- Initialize to Infinite before traversing. This way, if it is truly recursive,
         -- it will be found in the map and the result will be Infinite.
         Add (Call_Depths, Called_Name, (Recursive, Infinite));
         Result := (Regular, 0);

         -- We cannot directly traverse the whole body, since bodies are discarded
         -- We traverse all the parts manually (except formal parameters, of course)
         -- Of course, we can stop the traversal as soon as we determine that the
         -- SP is recursive.
         for Body_Decl : Asis.Declaration of Body_Declarative_Items (Called_Body) loop
            Traverse (Body_Decl, Control, Result);
            if Result.Kind = Recursive then
               raise Recursivity_Found;
            end if;
         end loop;
         for Stmt : Asis.Statement of Body_Statements (Called_Body) loop
            Traverse (Stmt, Control, Result);
            if Result.Kind = Recursive then
               raise Recursivity_Found;
            end if;
         end loop;
         for Hndlr : Asis.Exception_Handler of Body_Exception_Handlers (Called_Body) loop
            Traverse (Hndlr, Control, Result);
            if Result.Kind = Recursive then
               raise Recursivity_Found;
            end if;
         end loop;

      exception
         when Recursivity_Found =>
            Result := (Recursive, Infinite);
      end Analyze_Body;

   begin  -- Entity_Call_Depth
      -- Called_Body walks the structure until we find the real body corresponding to Decl
      -- So, it is really the called body only after this loop!
      Called_Body := Decl;

      loop
         if Is_Nil (Called_Body) then
            -- body not in context
            Result := (Unavailable, 0);
            exit;
         elsif Element_Kind (Called_Body) = A_Pragma or Definition_Kind (Called_Body) = An_Aspect_Specification then
            -- body given by a pragma (or aspect) import
            Result := (Unavailable, 0);
            exit;
         elsif Is_Banned (Called_Body, Rule_Id) then
            Result := (Banned, 0);
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
               begin
                  Called_Body := Corresponding_Body (Called_Body);
               exception
                  when Asis.Exceptions.ASIS_Inappropriate_Element =>
                     A4G_Bugs.Trace_Bug ("Max_Call_Depth: call of SP from formal package");
                     Result := (Unavailable, 0);
                     exit;
               end;

            when A_Null_Procedure_Declaration =>
               Result := (Regular, 0);
               exit;

            when An_Expression_Function_Declaration =>   -- Ada 2012
               -- Like Analyze_Body, on the result expression
               Add (Call_Depths, Called_Name, (Recursive, Infinite));
               Result := (Regular, 0);
               Traverse (Result_Expression (Called_Body), Control, Result);
               exit;

            when An_Entry_Declaration  =>
               if Is_Task_Entry (Called_Body) then
                  -- A task entry => not followed
                  Result := (Regular, 0);
                  exit;
               end if;
               Called_Body := Corresponding_Body (Called_Body);

            when A_Procedure_Body_Declaration
              | A_Function_Body_Declaration
              | An_Entry_Body_Declaration
              =>
               -- A real body (at last!)
               Analyze_Body;
               exit;

            when A_Procedure_Body_Stub
               | A_Function_Body_Stub
               =>
               Called_Body := Corresponding_Subunit (Called_Body);

            when A_Procedure_Renaming_Declaration
               | A_Function_Renaming_Declaration
               =>
               Called_Body := Simple_Name (Renamed_Entity (Called_Body));
               if Expression_Kind (Called_Body) = An_Explicit_Dereference then
                  Result := (Dynamic, 1);
                  exit;
               end if;
               Called_Body := Corresponding_Name_Declaration (Called_Body);

            when A_Formal_Function_Declaration
               | A_Formal_Procedure_Declaration
                 =>
               Result := (Formal, 0);
               exit;

            when others =>
               Failure ("not a callable entity declaration", Called_Body);
         end case;
      end loop;

      Add (Call_Depths, Called_Name, Result);
      return Result;
   end Entity_Call_Depth;


   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Element) is
      Descr : Depth_Descriptor;

      procedure Do_Report (Ctl_Kind : Control_Kinds) is
         use Framework.Locations, Framework.Reports, Utilities;
      begin
         case Descr.Kind is
            when Regular | Inline =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Ctl_Kind)),
                       Ctl_Kind,
                       Get_Location (Call),
                       "Call has a depth of " & ASIS_Integer_Img (Descr.Depth));
            when Dynamic =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Ctl_Kind)),
                       Ctl_Kind,
                       Get_Location (Call),
                       "Dynamic or dispatching call has a depth of at least " & ASIS_Integer_Img (Descr.Depth));
            when Unexplored | Unknown =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Ctl_Kind)),
                       Ctl_Kind,
                       Get_Location (Call),
                       "Call has a depth of at least " & ASIS_Integer_Img (Descr.Depth));
            when Recursive =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Ctl_Kind)),
                       Ctl_Kind,
                       Get_Location (Call),
                       "Call to recursive entity");
         end case;
      end Do_Report;

      use type Asis.ASIS_Integer;   --## rule line off Unnecessary_Use_Clause Reduceable_Scope ## Gela compatibility
   begin  -- Process_Call
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Descr := Call_Depth (Call);
      if Depths (Check) /= Unused and then Descr.Depth >= Depths (Check) then
         Do_Report (Check);
      elsif Depths (Search) /= Unused and then Descr.Depth >= Depths (Search) then
         Do_Report (Search);
      else
         case Descr.Kind is
            when Unknown =>
               Report_Uncheckable (Call, "depth unknown for some elements in call chain", Descr.Depth);
            when Dynamic =>
               Report_Uncheckable (Call, "dynamic or dispatching call", Descr.Depth);
            when Banned =>
               Report_Uncheckable (Call, "call to a inhibited subprogram", Descr.Depth);
            when Formal =>
               Report_Uncheckable (Call, "call to a generic formal subprogram", Descr.Depth);
            when Unavailable =>
               Report_Uncheckable (Call,
                                   "call to a subprogram whose body is not available (imported, predefined...)",
                                   Descr.Depth);
            when Regular | Inline | Recursive =>
               null;
         end case;
      end if;

      if Depths (Count) /= Unused and then Descr.Depth >= Depths (Count) then
         Do_Report (Count);
      end if;

   end Process_Call;

begin  -- Rules.Max_Call_Depth
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
   Framework.Variables.Register (Count_Expr_Fun_Calls'Access,
                                 Variable_Name => Rule_Id & ".COUNT_EXPR_FUN_CALLS");
end Rules.Max_Call_Depth;
