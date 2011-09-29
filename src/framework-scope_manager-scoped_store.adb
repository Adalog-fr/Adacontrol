----------------------------------------------------------------------
--  Framework.Scope_Manager.Scoped_Store - Package body             --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2008. The Ada --
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
  Ada.Strings.Wide_Fixed,
  Ada.Unchecked_Deallocation;

-- Adalog
with
  Binary_Map,
  Thick_Queries;

-- Asis
with
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements;

separate (Framework.Scope_Manager)
package body Scoped_Store is
   use Utilities;

   Is_Active : Boolean := False;

   type Data_Access is access Data;

   -- Linked list of Data, LIFO order
   type Node;
   type Link is access Node;
   type Node is
      record
         Next    : Link;
         Scope   : Scope_Range;
         Origin  : Declaration_Origin;
         Content : Data_Access;  -- Content is null for a deleted node
      end record;
   procedure Free is new Ada.Unchecked_Deallocation (Node, Link);

   Head           : Link;  -- Head (textually last) declaration
   Parents_Head   : Link;  -- Where Head was after restoring parent units context
   Unit_Spec_Head : Link;  -- Where Head was when a compilation unit package body is entered
   -- (After restoring parents and spec)

   type Spec_Kind is (Public_Library, Private_Library, Not_Library);
   type Spec_Save is
      record
         Kind         : Spec_Kind;
         Visible_Head : Link;
         Visible_Tail : Link;
         Private_Head : Link;
         Private_Tail : Link;
      end record;
   Unit_Info : Spec_Save;
   -- Context of the current compilation unit
   -- Kind is initialized in Enter_Unit
   -- Visible_Head is initialized in Enter_Private
   -- other fields are set at Exit_Scope
   -- All pointers are set to null until initialized


   package Spec_Maps is new Binary_Map (Key_Type   => Unbounded_Wide_String,
                                        Value_Type => Spec_Save);
   Spec_Store : Spec_Maps.Map;

   --
   -- Iterator:
   --
   Current      : Link;
   -- Invariant: Current is the current element of the iterator
   --            Current never designates a deleted node (whose Content is null)
   Previous     : Link;
   Current_Mode : Iterator_Mode;
   Final_Scope  : Scope_Range;


   ----------
   -- Free --
   ----------

   procedure Free (Ptr : in out Data_Access) is
      procedure Real_Free is new Ada.Unchecked_Deallocation (Data, Data_Access);
   begin
      if Ptr = null then
         return;
      end if;

      Clear (Ptr.all);
      Real_Free (Ptr);
   end Free;


   ----------
   -- Push --
   ----------

   procedure Push (Info : in Data) is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      Head := new Node'(Next    => Head,
                        Scope   => Scope_Top,
                        Origin  => Same_Unit,
                        Content => new Data'(Info));
   end Push;

   --------------------
   -- Push_Enclosing --
   --------------------

   procedure Push_Enclosing (Info : in Data) is
      Insert_Ptr    : Link := Head;
      Before_Insert : Link := null;
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      while Insert_Ptr /= null
        and then (Insert_Ptr.Content = null       -- a deleted node
                  or Insert_Ptr.Scope = Scope_Top)
      loop
         Before_Insert := Insert_Ptr;
         Insert_Ptr    := Insert_Ptr.Next;
      end loop;
      if Insert_Ptr = Head then
         Head := new Node'(Next    => Head,
                           Scope   => Scope_Top - 1,
                           Origin  => Same_Unit,
                           Content => new Data'(Info));
      else
         Before_Insert.Next := new Node'(Next    => Insert_Ptr,
                                         Scope   => Scope_Top - 1,
                                         Origin  => Same_Unit,
                                         Content => new Data'(Info));
      end if;
   end Push_Enclosing;

   -----------
   -- Reset --
   -----------

   procedure Reset (Mode : Iterator_Mode) is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      Current_Mode := Mode;
      Current      := Head;
      Previous     := null;
      while Current /= null and then Current.Content = null loop -- Skip deleted nodes
         Previous := Current;
         Current  := Current.Next;
      end loop;

      case Mode is
         when All_Scopes =>
            Final_Scope := 0;
         when Unit_Scopes =>
            -- NB: there must be at least one unit scope
            Final_Scope := Scope_Top;
            while not Scope_Stack (Final_Scope).Is_Unit loop
               Final_Scope := Final_Scope - 1;
            end loop;
         when Current_Scope_Only =>
            Final_Scope := Scope_Top;
      end case;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Info : Data; Mode : Iterator_Mode) is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      Current      := Head;
      Previous     := null;
      Current_Mode := Mode;
      while Current /= null
        and then (Current.Content = null    -- a deleted node
                  or else not Equivalent_Keys (Info, Current.Content.all))
      loop
         Previous := Current;
         Current  := Current.Next;
      end loop;
      Continue (Mode);
   end Reset;

   --------------
   -- Continue --
   --------------

   procedure Continue (Mode : Iterator_Mode) is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      if Current /= null then
         case Mode is
            when All_Scopes =>
               Final_Scope := 0;
            when Unit_Scopes =>
               -- NB: there must be at least one unit scope
               Final_Scope := Current.Scope;
               while not Scope_Stack (Final_Scope).Is_Unit loop
                  Final_Scope := Final_Scope - 1;
               end loop;
            when Current_Scope_Only =>
               Final_Scope := Current.Scope;
         end case;
      end if;
   end Continue;

   ------------------
   -- Current_Data --
   ------------------

   function Current_Data return Data is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      return Current.Content.all;
   end Current_Data;

   ------------------------
   -- Current_Data_Level --
   ------------------------

   function Current_Data_Level return Scope_Range is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      return Current.Scope;
   end Current_Data_Level;

   ------------------------
   -- Current_Data_Scope --
   ------------------------

   function Current_Data_Scope return Asis.Element is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      if Current.Scope = 0 then
         return Asis.Nil_Element;
      end if;

      return Scope_Stack (Current.Scope).Element;
   end Current_Data_Scope;

   --------------------
   -- Current_Origin --
   --------------------

   function Current_Origin return Declaration_Origin is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      return Current.Origin;
   end Current_Origin;

   ----------
   -- Next --
   ----------

   procedure Next is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      if Current = null then
         raise Constraint_Error;
      end if;

      case Current_Mode is
         when All_Scopes =>
            null;
         when Unit_Scopes | Current_Scope_Only =>
            if Current.Scope < Final_Scope then
               raise Constraint_Error;
            end if;
      end case;

      loop
         Previous := Current;
         Current  := Current.Next;
         exit when Current = null or else Current.Content /= null; -- not a deleted node
      end loop;
   end Next;

   --------------------
   -- Data_Available --
   --------------------

   function Data_Available return Boolean is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      if Current = null then
         return False;
      end if;

      case Current_Mode is
         when All_Scopes =>
            return True;

         when Unit_Scopes | Current_Scope_Only =>
            return Current.Scope >= Final_Scope;
      end case;
   end Data_Available;

   --------------------
   -- Update_Current --
   --------------------

   procedure Update_Current (Info : in Data) is
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      -- Deallocate and reallocate, since new data can have
      -- a different constraint
      Free (Current.Content);
      Current.Content := new Data'(Info);
   end Update_Current;

   --------------------
   -- Delete_Current --
   --------------------

   procedure Delete_Current is
      -- If the current node is from a spec or a parent unit, we may have pointers to it from Spec_Store
      -- In this case, we just mark the node as deleted by having a null Content (ensured by Free)
      -- Otherwise, the node is effectively removed from the list and freed.
      Deleted_Node : Link := Current;
   begin
      if not Is_Active then
         Failure (Inactive_Message);
      end if;

      Free (Deleted_Node.Content);

      Current := Current.Next;

      if Deleted_Node.Origin = Same_Unit then
         if Previous = null then
            Head := Current;
         else
            Previous.Next := Current;
         end if;

         -- Fix global pointers that might designate the deleted node
         if Unit_Spec_Head = Deleted_Node then
            Unit_Spec_Head := Current;
         end if;
         if Parents_Head = Deleted_Node then
            Parents_Head := Current;
         end if;
         if Unit_Info.Visible_Head = Deleted_Node then
            Unit_Info.Visible_Head := Current;
         end if;
         if Unit_Info.Private_Head = Deleted_Node then
            Unit_Info.Private_Head := Current;
         end if;
         -- Other pointers of Unit_Info are not yet initialized at that point

         Free (Deleted_Node);
      end if;

      -- Current might still designate a deleted node
      while Current /= null and then Current.Content = null loop
         Previous := Current;
         Current  := Current.Next;
      end loop;
   end Delete_Current;


   ----------------
   -- Enter_Unit --
   ----------------

   procedure Enter_Unit (Scope : Asis.Element) is
      procedure Restore_Parent_Context (Unit_Name : in Wide_String; Is_Private : in Boolean) is
         -- Copy context from parent unit if any
         use Spec_Maps, Ada.Strings, Ada.Strings.Wide_Fixed;

         Inx_Dot     : constant Natural := Index (Unit_Name, ".", Going => Backward);
         Parent_Name : constant Wide_String := Unit_Name (Unit_Name'First .. Inx_Dot - 1);
         Parent_Info : Spec_Save;
         Cur_Link    : Link;
      begin
         if Inx_Dot = 0 then
            -- Not a child unit
            return;
         end if;

         -- This is a child unit
         Parent_Info := Fetch (Spec_Store, To_Unbounded_Wide_String (Parent_Name));
         Restore_Parent_Context (Parent_Name,
                                 Is_Private => Parent_Info.Kind = Private_Library);

         if Parent_Info.Visible_Head /= null then
            Parent_Info.Visible_Tail.Next := Head;
            Head                          := Parent_Info.Visible_Head;

            Cur_Link := Parent_Info.Visible_Head;
            loop
               Cur_Link.Origin := Parent;
               Cur_Link.Scope  := 0;
               exit when Cur_Link = Parent_Info.Visible_Tail;
               Cur_Link := Cur_Link.Next;
            end loop;
         end if;

         if Is_Private and Parent_Info.Private_Head /= null then
            Parent_Info.Private_Tail.Next := Head;
            Head                          := Parent_Info.Private_Head;

            Cur_Link := Parent_Info.Private_Head;
            loop
               Cur_Link.Origin := Parent;
               Cur_Link.Scope  := 0;
               exit when Cur_Link = Parent_Info.Private_Tail;
               Cur_Link := Cur_Link.Next;
            end loop;
         end if;
      exception
         when Not_Present =>
            Utilities.Trace ("Parent not found in Restore_Parent_Context for unit "  --## rule line off No_Trace
                             & Unit_Name);
            raise;
      end Restore_Parent_Context;

      use Asis.Compilation_Units, Asis.Elements;
   begin  -- Enter_Unit
      Head := null;
      if Unit_Is_Private then
         Unit_Info := (Kind => Private_Library, others => null);
      else
         Unit_Info := (Kind => Public_Library, others => null);
      end if;

      -- Reestablish data saved from parent if any
      Restore_Parent_Context (To_Upper (Unit_Full_Name (Enclosing_Compilation_Unit (Scope))),
                              Unit_Is_Private);
      Parents_Head := Head;
   end Enter_Unit;


   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope (Scope : Asis.Element) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Thick_Queries;

      Cur_Link   : Link;
   begin   -- Enter_Scope
      case Declaration_Kind (Scope) is
         when A_Package_Body_Declaration
            | A_Protected_Body_Declaration
            | A_Task_Body_Declaration
              =>
            -- Restore data saved from spec if any
            declare
               use Spec_Maps;
               Info       : Spec_Save;
               Scope_Name : constant Unbounded_Wide_String
                 := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Names (Scope) (1), With_Profile => True)));
            begin
               if Is_Present (Spec_Store, Scope_Name) then
                  Info := Fetch (Spec_Store, Scope_Name);
                  if Info.Visible_Head /= null then
                     Info.Visible_Tail.Next := Head;
                     Head                   := Info.Visible_Head;

                     Cur_Link := Info.Visible_Head;
                     loop
                        Cur_Link.Origin := Specification;
                        Cur_Link.Scope  := Scope_Top;     -- May have been put to 0 if it is a parent
                        exit when Cur_Link = Info.Visible_Tail;
                        Cur_Link := Cur_Link.Next;
                     end loop;
                  end if;

                  if Info.Private_Head /= null then
                     Info.Private_Tail.Next := Head;
                     Head                   := Info.Private_Head;

                     Cur_Link := Info.Private_Head;
                     loop
                        Cur_Link.Origin := Specification;
                        Cur_Link.Scope  := Scope_Top;     -- May have been put to 0 if it is a parent
                        exit when Cur_Link = Info.Private_Tail;
                        Cur_Link := Cur_Link.Next;
                     end loop;
                  end if;

                  -- If it is not a library package, we can delete spec information
                  -- Do not delete information from library package specs, must be kept for children
                  -- Note that only a package body can be at depth 1 here (others cannot be compilation units)
                  if Scope_Top /= 1 then
                     Delete (From => Spec_Store, Key => Scope_Name);
                  end if;
               end if;
            end;
            if Scope_Top = 1 then
               -- Can only be a library package body
               Unit_Spec_Head := Head;
            end if;
         when A_Procedure_Body_Declaration -- Possible child units
            | A_Function_Body_Declaration
              =>
            -- Restore data from spec (if any)
            -- These certainly have no private part!
            declare
               use Spec_Maps;
               Scope_Name : constant Unbounded_Wide_String
                 := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Names (Scope) (1),
                                                                         With_Profile => True)));
               Info : Spec_Save;
            begin
               if Is_Present (Spec_Store, Scope_Name) then
                  Info := Fetch (Spec_Store, Scope_Name);
                  if Info.Visible_Head /= null then
                     Info.Visible_Tail.Next := Head;
                     Head                   := Info.Visible_Head;

                     Cur_Link := Info.Visible_Head;
                     loop
                        Cur_Link.Origin := Specification;
                        Cur_Link.Scope  := Scope_Top;     -- May have been put to 0 if it is a parent
                        exit when Cur_Link = Info.Visible_Tail;
                        Cur_Link := Cur_Link.Next;
                     end loop;
                  end if;

                  -- A subprogram has no children, therefore we can safely delete information
                  Delete (From => Spec_Store, Key => Scope_Name);
               end if;
            end;

         when others =>
            null;
      end case;
   end Enter_Scope;


   -------------------
   -- Enter_Private --
   -------------------

   procedure Enter_Private (Scope : Asis.Element) is

      procedure Restore_Parent_Private_Context (Unit_Name : in Wide_String) is
         -- Copy context from parent private part if any
         use Spec_Maps, Ada.Strings, Ada.Strings.Wide_Fixed;

         Inx_Dot     : constant Natural := Index (Unit_Name, ".", Going => Backward);
         Parent_Name : constant Wide_String := To_Upper (Unit_Name (Unit_Name'First .. Inx_Dot - 1));
         Parent_Info : Spec_Save;
         Cur_Link    : Link;
      begin
         if Inx_Dot = 0 then
            -- Not a child unit
            return;
         end if;

         -- This is a child unit
         Parent_Info := Fetch (Spec_Store, To_Unbounded_Wide_String (Parent_Name));
         if Parent_Info.Kind = Private_Library then
            -- Parent is private => all upper private parts have been loaded
            return;
         end if;
         Restore_Parent_Private_Context (Parent_Name);

         if Parent_Info.Private_Head /= null then
            Parent_Info.Private_Tail.Next := Head;
            Head := Parent_Info.Private_Head;

            Cur_Link := Parent_Info.Private_Head;
            loop
               Cur_Link.Origin := Parent;
               Cur_Link.Scope  := 0;
               exit when Cur_Link = Parent_Info.Private_Tail;
               Cur_Link := Cur_Link.Next;
            end loop;
         end if;

      exception
         when Not_Present =>
            Utilities.Trace ("Parent not found in Restore_Parent_Private_Context: " --## rule line off No_Trace
                              & Parent_Name);
            raise;
      end Restore_Parent_Private_Context;

      use Asis.Compilation_Units, Asis.Elements;
   begin  -- Enter_Private
      -- This is called only for compilation units
      -- Note that the scope is necessarily a [generic] package here,
      -- since these are the only compilation units with a private part.
      if Head /= null and then Head.Origin = Same_Unit then
         Unit_Info.Visible_Head := Head;
      else
         Unit_Info.Visible_Head := null;
      end if;

      -- If we are in a public child,
      -- restore info from private part of the parent if any
      if Unit_Info.Kind = Public_Library then
         Restore_Parent_Private_Context (Unit_Full_Name (Enclosing_Compilation_Unit (Scope)));
      end if;
   end Enter_Private;


   ----------------
   -- Exit_Scope --
   ----------------

   procedure Exit_Scope (Scope : Asis.Element) is
      use Asis, Asis.Declarations, Asis.Elements, Thick_Queries, Spec_Maps;

      procedure Delete_Data (Stop_At : Link) is
         -- Stop_At = pointer to first node to keep
         --           null to delete everything belonging to this scope
         Cur_Link        : Link := Head;
         Local_Scope_Top : Scope_Range := Scope_Top;
      begin

         -- Data linked to (pseudo) scope 0 must be deleted together with
         -- data from scope 1.
         if Scope_Top = 1 then
            Local_Scope_Top := 0;
         end if;

         while Cur_Link /= Stop_At
           and then Cur_Link.Scope >= Local_Scope_Top
           and then Cur_Link.Origin /= Parent
         loop
            Head := Cur_Link.Next;
            Free (Cur_Link.Content);
            Free (Cur_Link);
            Cur_Link := Head;
         end loop;
      end Delete_Data;

      Cur_Link   : Link;
      Scope_Kind : constant Asis.Declaration_Kinds := Declaration_Kind (Scope);
   begin  -- Exit_Scope

      case Scope_Kind is
         when A_Procedure_Declaration
            | A_Function_Declaration
            | A_Package_Declaration

            | A_Protected_Type_Declaration
            | A_Single_Protected_Declaration
            | A_Single_Task_Declaration
            | A_Task_Type_Declaration

            | A_Generic_Declaration
            | A_Generic_Instantiation
              =>
            if Scope_Top = 1
              and then (Scope_Kind = A_Package_Declaration
                        or Scope_Kind = A_Generic_Package_Declaration
                        or Scope_Kind = A_Package_Instantiation)
            then
               -- Library package specification
               -- Save data for the corresponding body and child units
               -- even if the list is empty, since we must keep whether the unit
               -- is public or private.

               -- Check if data were pushed from private part, but beware that elements from
               -- the private part of parents are inserted between our private part and our
               -- visible part
               if Head /= null
                 and then Head.Origin /= Parent
                 and then Head /= Unit_Info.Visible_Head
               then
                  Unit_Info.Private_Head := Head;
                  Unit_Info.Private_Tail := Unit_Info.Private_Head;
                  while Unit_Info.Private_Tail.Next /= null
                    and then Unit_Info.Private_Tail.Next.Origin /= Parent
                    and then Unit_Info.Private_Tail.Next /= Unit_Info.Visible_Head
                  loop
                     Unit_Info.Private_Tail := Unit_Info.Private_Tail.Next;
                  end loop;
               end if;

               if Unit_Info.Visible_Head /= null then
                  -- data were pushed from the visible part
                  Unit_Info.Visible_Tail := Unit_Info.Visible_Head;
                  while Unit_Info.Visible_Tail.Next /= null
                    and then Unit_Info.Visible_Tail.Next.Origin /= Parent
                  loop
                     Unit_Info.Visible_Tail := Unit_Info.Visible_Tail.Next;
                  end loop;
               end if;
               Add (To    => Spec_Store,
                    Key   => To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Names (Scope) (1),
                                                                                  With_Profile => True))),
                    Value => Unit_Info);

            else -- Not a library package spec
               if Head = null or else Head.Scope /= Scope_Top then
                  -- No data => nothing to do
                  return;
               end if;

               -- Save data for the corresponding body only
               -- We don't need to make the difference between visible and private part here,
               -- hence we keep the whole chain (including the private part) into Visible_Head
               Cur_Link := Head;
               while Cur_Link.Next /= null
                 and then Cur_Link.Next.Origin /= Parent
                 and then Cur_Link.Next.Scope = Scope_Top
               loop
                  Cur_Link := Cur_Link.Next;
               end loop;
               Add (To    => Spec_Store,
                    Key   => To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Names (Scope) (1),
                                                                                  With_Profile => True))),
                    Value => (Kind         => Not_Library,
                              Visible_Head => Head,
                              Visible_Tail => Cur_Link,
                              others       => null));
               Head := Cur_Link.Next;
            end if;

         when A_Package_Body_Declaration =>
            -- If a compilation unit, free only the data from the body
            -- (Info from the spec may be necessary for children)
            if Scope_Top = 1 then
               Delete_Data (Stop_At => Unit_Spec_Head);

               -- Data not deleted are referenced from Spec_Store, but must not
               -- be kept on the stack
               Head := Unit_Spec_Head;
            else
               Delete_Data (Stop_At => null);
            end if;

         when others =>
            Delete_Data (Stop_At => null);
      end case;
   end Exit_Scope;

   ---------------
   -- Clear_All --
   ---------------

   procedure Clear_All (Scope : Asis.Element) is
      pragma Unreferenced (Scope);
      Temp : Link;
   begin
      if not Clear_Stay_Active then
         Is_Active := False;
      end if;

      while Head /= null loop
         Temp := Head;
         Head := Head.Next;
         Free (Temp.Content);
         Free (Temp);
      end loop;
      Spec_Maps.Clear (Spec_Store);
   end Clear_All;

   procedure Activate is
   begin
      Unit_Procs    := new Scoping_Node'(Unit_Access,    Unit_Procs);
      Scope_Procs   := new Scoping_Node'(Scope_Access,   Scope_Procs);
      Private_Procs := new Scoping_Node'(Private_Access, Private_Procs);
      Exit_Procs    := new Scoping_Node'(Exit_Access,    Exit_Procs);
      Clear_Procs   := new Scoping_Node'(Clear_Access,   Clear_Procs);

      Is_Active := True;
   end Activate;
end Scoped_Store;

