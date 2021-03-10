----------------------------------------------------------------------
--  Rules.Object_Declarations.NRT_Utilities - Package body          --
--                                                                  --
--  This software is (c) Adalog 2004-2018.                          --
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
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Binary_Map,
  Scope_Manager,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Reports;

package body Rules.Object_Declarations.NRT_Utilities is
   use Ada.Strings.Wide_Unbounded;
   use Framework;

   package Dependents_Set is new Binary_Map (Unbounded_Wide_String, Asis.Defining_Name);

   type Suspect_Info is
      record
         Object       : Asis.Defining_Name;
         Cont         : Control_Manager.Basic_Rule_Context;
         Type_Name    : Unbounded_Wide_String;
         Dependings   : Dependents_Set.Map;
         Dependents   : Dependents_Set.Map;
         Depend_Level : Scope_Manager.Scope_Range;
      end record;
   package Suspect_Map is new Binary_Map (Unbounded_Wide_String, Suspect_Info);
   All_Suspects : Suspect_Map.Map;

   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Object    : Asis.Defining_Name;
                        Cont      : Control_Manager.Basic_Rule_Context;
                        Type_Name : Wide_String)
   is
      use Asis, Asis.Elements;
      use Framework.Locations, Framework.Reports, Utilities;
   begin
      case Declaration_Kind (Enclosing_Element (Object)) is
         when A_Constant_Declaration =>
            Report (Rule_Id,
                    Cont,
                    Get_Location (Object),
                    "Constant declaration of not required type " & Type_Name);
         when A_Variable_Declaration =>
               Report (Rule_Id,
                       Cont,
                       Get_Location (Object),
                       "Variable declaration of not required type " & Type_Name);
         when A_Parameter_Specification =>
               Report (Rule_Id,
                       Cont,
                       Get_Location (Object),
                       "Parameter declaration of not required type " & Type_Name);
         when A_Loop_Parameter_Specification =>
             Report (Rule_Id,
                     Cont,
                     Get_Location (Object),
                     "Loop parameter declaration of not required type " & Type_Name);
         when An_Element_Iterator_Specification =>
            Report (Rule_Id,
                    Cont,
                    Get_Location (Object),
                    "Loop parameter declaration of not required type " & Type_Name);
         when others =>
            Failure ("Report: bad declaration", Enclosing_Element (Object));
      end case;
   end Do_Report;


   -----------------------------
   -- Subtype_Or_Type_Context --
   -----------------------------

   function Subtype_Or_Type_Context (Store   : Framework.Control_Manager.Context_Store;
                                     St_Name : Asis.Element) return Framework.Control_Manager.Root_Context'Class
   is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Control_Manager, Thick_Queries, Utilities;
      Ctxt : constant Root_Context'Class := Matching_Context (Store, St_Name);
      Decl : Asis.Declaration;
   begin
      if Ctxt /= No_Matching_Context then
         return Ctxt;
      end if;

      -- Second chance
      case Attribute_Kind (St_Name) is
         when A_Base_Attribute =>
            -- Retry without 'Base
            return Subtype_Or_Type_Context (Store, Strip_Attributes (St_Name));
         when A_Class_Attribute =>
            -- Nothing else to check, T'Class is not T !
            return No_Matching_Context;
         when Not_An_Attribute =>
            -- Regular case: check first subtype
            if Element_Kind (St_Name) = A_Defining_Name then
               Decl := Enclosing_Element (St_Name);
            else
               Decl := Corresponding_Name_Declaration (Simple_Name (St_Name));
            end if;
            return Matching_Context (Store, Names (A4G_Bugs.Corresponding_First_Subtype
                                     (Corresponding_Full_Type_Declaration (Decl))) (1));
         when others =>
            Failure ("Object_Declarations: unknown type attribute", St_Name);
      end case;
   end Subtype_Or_Type_Context;

   -------------------
   -- Make_Required --
   -------------------

   procedure Make_Required (Ident : Asis.Element) is
      use Suspect_Map, Thick_Queries;
      Ident_State : Variable_State;
      Full_Info   : Suspect_Info;
      Ident_Key   : Unbounded_Wide_String;

      procedure Make_One_Dependent_Required (Key : Unbounded_Wide_String; Value : in out  Asis.Defining_Name) is
         pragma Unreferenced (Key);
      begin
         Make_Required (Value);
      end Make_One_Dependent_Required;
      procedure Make_All_Dependents_Required is new Dependents_Set.Iterate (Make_One_Dependent_Required);
   begin  -- Make_Required
      if Active_Suspect_Variables.Is_Present (Ident) then
         -- Ident may not be there if discovered required after exiting its scope
         Ident_State := Active_Suspect_Variables.Fetch (Ident);
         if Ident_State = Required then
            -- We already knew it...
            return;
         end if;
         Ident_State := Required;
         Active_Suspect_Variables.Store (Ident, Ident_State);
      end if;

      Ident_Key := To_Unbounded_Wide_String (Full_Name_Image (Ident));
      if not Is_Present (All_Suspects, Ident_Key) then
         -- May have been already identified as required, and removed from All_Suspects
         return;
      end if;
      Full_Info := Fetch (All_Suspects, Ident_Key);
      Make_All_Dependents_Required (Full_Info.Dependents);

      Dependents_Set.Clear (Full_Info.Dependings);
      Dependents_Set.Clear (Full_Info.Dependents);
      Delete (All_Suspects, Ident_Key);
   end Make_Required;


   --------------------
   -- Make_Dependent --
   --------------------

   procedure Make_Dependent (Obj : Asis.Name; On : Asis.Name) is
      use Dependents_Set, Scope_Manager, Suspect_Map, Thick_Queries;
      On_State   : Variable_State;
      Full_Info  : Suspect_Info;
      On_Key     : Unbounded_Wide_String;
      Ident_Key  : Unbounded_Wide_String;
      On_Level   : Scope_Range;

      procedure Update_Level (Of_Info : in out Suspect_Info;
                              Value   : Scope_Range)
      is
         Local_Info : Suspect_Info := Of_Info; -- Variable required to call Update_Dependents_Level;

         procedure Update_One_Dependent (Key : Unbounded_Wide_String; Elem : in out Asis.Element) is
            pragma Unreferenced (Elem);

            Depend_Info : Suspect_Info;
         begin
            Depend_Info := Fetch (All_Suspects, Key);
            Update_Level (Depend_Info, Value);
            Add (All_Suspects, Key, Depend_Info);
         end Update_One_Dependent;
         procedure Update_Dependents_Level is new Dependents_Set.Iterate (Update_One_Dependent);
      begin  -- Update_Level
         if Of_Info.Depend_Level <= Value then
            return;
         end if;
         Of_Info.Depend_Level := Value;

         Update_Dependents_Level (Local_Info.Dependents);
      end Update_Level;

   begin  -- Make_Dependent
      if not Active_Suspect_Variables.Is_Present (On) then
         return;
      end if;

      On_State := Active_Suspect_Variables.Fetch (On);
      if On_State = Required then
         Make_Required (Obj);
         return;
      end if;

      -- Here, create dependency between Obj and On
      On_Key    := To_Unbounded_Wide_String (Full_Name_Image (On));
      if not Is_Present (All_Suspects, On_Key) then
         -- This happens when an object is referenced from outside its declaration scope (formal parameter)
         -- The rule does not define this as a dependency
         return;
      end if;
      Ident_Key := To_Unbounded_Wide_String (Full_Name_Image (Obj));
      Full_Info := Fetch (All_Suspects, On_Key);
      On_Level  := Full_Info.Depend_Level;
      Add (Full_Info.Dependents, Ident_Key, Obj);
      Add (All_Suspects, On_Key, Full_Info);

      Full_Info := Fetch (All_Suspects, Ident_Key);
      Add (Full_Info.Dependings, On_Key, On);
      Update_Level (Full_Info, On_Level);
      Add (All_Suspects, Ident_Key, Full_Info);
   end Make_Dependent;


   ------------------------
   -- Check_Not_Required --
   ------------------------

   procedure Check_Not_Required (Name : Asis.Name) is
   -- Name is known to be not required if it does not depend any more on other variables, or only on variables
   -- declared in the same scope (to break circularities).
   -- Report, and see if it clears its own dependents
      use Scope_Manager;
      use Dependents_Set, Suspect_Map, Thick_Queries;

      Name_Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Full_Name_Image (Name));

      procedure Process_One_Dependent (Key : Unbounded_Wide_String; Info : in out Asis.Element) is
         Dep_Info : Suspect_Info;
      begin
         if not Is_Present (All_Suspects, Key) then
            -- previously found as required
            return;
         end if;
         Dep_Info := Fetch (All_Suspects, Key);
         Delete (Dep_Info.Dependings, Name_Key);
         Add (All_Suspects, Key, Dep_Info);
         Check_Not_Required (Info);
      end Process_One_Dependent;
      procedure Process_All_Dependents is new Dependents_Set.Iterate (Process_One_Dependent);

      This_Info : Suspect_Info;
   begin  -- Check_Not_Required
      if not Is_Present (All_Suspects, Name_Key) then
         return;
      end if;
      This_Info := Fetch (All_Suspects, Name_Key);
      if Is_Empty (This_Info.Dependings) or else This_Info.Depend_Level >= Current_Depth then
         Do_Report (This_Info.Object, This_Info.Cont, To_Wide_String (This_Info.Type_Name));

         -- Delete from all suspects before processing dependents to protect from circularities
         Delete (All_Suspects, Name_Key);
         Process_All_Dependents (This_Info.Dependents);
         Clear (This_Info.Dependents);
         Clear (This_Info.Dependings);
      end if;
   end Check_Not_Required;


   ------------------
   -- Is_Requiring --
   ------------------

   function Is_Requiring (Expr : Asis.Expression; Of_Type : Asis.Declaration) return Boolean is
   -- Is there someting in Expr that forces the type for the enclosing expression?
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
      Good_Expr : Asis.Expression := Expr;
   begin
      if Element_Kind (Good_Expr) = An_Association then
         Good_Expr := Actual_Parameter (Good_Expr);
      end if;

      case Expression_Kind (Good_Expr) is
         when An_Attribute_Reference =>
            case Attribute_Kind (Good_Expr) is
               when A_First_Attribute | A_Last_Attribute =>
                  return Type_Category (Prefix (Good_Expr)) = An_Array_Type;
               when others =>
                  return False;
            end case;
         when A_Function_Call =>
            declare
               Called : constant Asis.Expression := Called_Simple_Name (Good_Expr);
            begin
               if Is_Nil (Called) then -- dereference...
                  return False;
               elsif Corresponding_Call_Description (Good_Expr).Kind = A_Predefined_Entity_Call then
                  if Expression_Kind (Prefix (Good_Expr)) = An_Operator_Symbol then
                     return (for some Param of Actual_Parameters (Good_Expr) => Is_Requiring (Param, Of_Type));
                  else
                     return False;
                  end if;
               elsif Expression_Kind (Called) = An_Attribute_Reference then
                  return False;
               elsif Ultimate_Origin (Called) = An_Application_Unit then
                  return False;
               else
                  declare
                     Result_Profile : constant Profile_Entry := Types_Profile
                       (Corresponding_Called_Function (Good_Expr)).Result_Type;
                  begin
                     if Result_Profile.Access_Form = Not_An_Access_Definition then
                        -- we don't care about anonymous types
                        return Is_Equal (Ultimate_Type_Declaration (Of_Type),
                                         Enclosing_Element (Result_Profile.General_Name.Name));
                     else
                        return False;
                     end if;
                  end;
               end if;
            end;
         when A_Parenthesized_Expression =>
            return Is_Requiring (Expression_Parenthesized (Good_Expr), Of_Type);
         when A_Qualified_Expression =>
            return Is_Requiring (Converted_Or_Qualified_Expression (Good_Expr), Of_Type);
         when others =>   -- Including a type conversion!
            return False;
      end case;
   end Is_Requiring;

   ------------------
   -- Make_Suspect --
   ------------------

   procedure Make_Suspect (Name : Asis.Name;
                           Cont      : Framework.Control_Manager.Basic_Rule_Context;
                           Type_Name : Wide_String)
   is
      use Thick_Queries;
   begin
      Active_Suspect_Variables.Store (Name, Suspect);
      Suspect_Map.Add (All_Suspects,
                       To_Unbounded_Wide_String (Full_Name_Image (Name)),
                       (Object       => Name,
                        Cont         => Cont,
                        Type_Name    => To_Unbounded_Wide_String (Type_Name),
                        Dependings   => Dependents_Set.Empty_Map,
                        Dependents   => Dependents_Set.Empty_Map,
                        Depend_Level => Scope_Manager.Current_Depth));
   end Make_Suspect;

   ----------------
   -- Is_Suspect --
   ----------------

   function Is_Suspect (Ident : Asis.Name) return Boolean is
   begin
      if not Active_Suspect_Variables.Is_Present (Ident) then
         return False;
      end if;

      -- If it is already required, it's no more suspect...
      return Active_Suspect_Variables.Fetch (Ident) /= Required;
   end Is_Suspect;


end Rules.Object_Declarations.NRT_Utilities;
