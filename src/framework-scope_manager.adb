----------------------------------------------------------------------
--  Framework.Scope_Manager - Package body                          --
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
  Ada.Unchecked_Deallocation;

-- Adalog
with
  Binary_Map,
  Thick_Queries;

-- Adactl
with
  Utilities;

-- Asis
with
  Asis.Declarations,
  Asis.Elements;

package body Framework.Scope_Manager is
   --
   -- Management of the scopes stack
   --

   subtype Scope_Index is Scope_Range range 1 .. Scope_Range'Last;

   Scope_Stack : array (Scope_Index) of Asis.Element;
   Scope_Top   : Scope_Range := 0;

   --
   -- Linked list of Enter_Procs, Exit_Procs and Clear_Procs:
   --
   type Scoping_Node;
   type Scoping_Link is access Scoping_Node;

   type Scoping_Node is
      record
         Proc : Scoping_Procedure;
         Next : Scoping_Link;
      end record;

   Enter_Procs : Scoping_Link;
   Exit_Procs  : Scoping_Link;
   Clear_Procs : Scoping_Link;

   ------------------
   --Current_Depth --
   ------------------

   function Current_Depth return Scope_Range is
   begin
      return Scope_Top;
   end Current_Depth;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Asis.Element is
   begin
      return Scope_Stack (Scope_Top);
   end Current_Scope;

   ---------------------
   -- Enclosing_Scope --
   ---------------------

   function Enclosing_Scope return Asis.Element is
      use Asis;
   begin
      if Scope_Top = 1 then
         return Nil_Element;
      else
         return Scope_Stack (Scope_Top-1);
      end if;
   end Enclosing_Scope;

   -------------------
   -- Active_Scopes --
   -------------------

   function Active_Scopes return Asis.Element_List is
      use Asis;
      Result : Asis.Element_List (1 ..  List_Index (Scope_Top));
   begin
      for I in Result'Range loop
         Result (I) := Scope_Stack (Scope_Range (I));
      end loop;
      return Result;
   end Active_Scopes;

   ------------------
   -- Scoped_Store --
   ------------------

   package body Scoped_Store is

      type Data_Access is access Data;
      procedure Free is new Ada.Unchecked_Deallocation (Data, Data_Access);

      type Node;
      type Link is access Node;
      type Node is
         record
            Next        : Link;
            Scope       : Scope_Range;
            Transmitted : Boolean;
            Content     : Data_Access;
         end record;
      procedure Free is new Ada.Unchecked_Deallocation (Node, Link);

      Head : Link;

      type Spec_Save is
         record
            Head : Link;
            Tail : Link;
         end record;
      package Spec_Maps is new Binary_Map (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
                                           Spec_Save);
      Spec_Store : Spec_Maps.Map;

      -- Iterator:
      Current      : Link;
      Previous     : Link;
      Current_Mode : Iterator_Mode;


      ----------
      -- Push --
      ----------

      procedure Push (Info : in Data) is
      begin
         Head := new Node'(Next        => Head,
                           Scope       => Scope_Top,
                           Transmitted => False,
                           Content     => new Data'(Info));
         if Current = Head.Next then
            -- Iterator was on first element (but is no more), update previous
            Previous := Head;
         end if;
      end Push;

      --------------------
      -- Push_Enclosing --
      --------------------

      procedure Push_Enclosing (Info : in Data) is
         Insert_Ptr    : Link := Head;
         Before_Insert : Link := null;
      begin
         while Insert_Ptr /= null and then Insert_Ptr.Scope = Scope_Top loop
            Before_Insert := Insert_Ptr;
            Insert_Ptr    := Insert_Ptr.Next;
         end loop;
         if Insert_Ptr = Head then
            Head := new Node'(Next        => Head,
                              Scope       => Scope_Top-1,
                              Transmitted => False,
                              Content     => new Data'(Info));
            if Current = Head.Next then
               -- Iterator was on first element (but is no more), update previous
               Previous := Head;
            end if;
         else
            Before_Insert.Next := new Node'(Next        => Insert_Ptr,
                                            Scope       => Scope_Top-1,
                                            Transmitted => False,
                                            Content     => new Data'(Info));
            if Current = Insert_Ptr then
               -- Iterator was on insertion point, update previous
               Previous := Before_Insert.Next;
            end if;
         end if;
      end Push_Enclosing;

      -----------
      -- Reset --
      -----------

      procedure Reset (Mode : Iterator_Mode) is
      begin
         Current     := Head;
         Previous    := null;

         Current_Mode := Mode;
      end Reset;

      ------------------
      -- Current_Data --
      ------------------

      function Current_Data return Data is
      begin
         return Current.Content.all;
      end Current_Data;

      ------------------------
      -- Current_Data_Scope --
      ------------------------

      function Current_Data_Scope return Asis.Element is
      begin
         if Current.Scope = 0 then
            return Asis.Nil_Element;
         else
            return Scope_Stack (Current.Scope);
         end if;
      end Current_Data_Scope;

      --------------------------------------
      -- Is_Current_Transmitted_From_Spec --
      --------------------------------------

      function Is_Current_Transmitted_From_Spec return Boolean is
      begin
         return Current.Transmitted;
      end Is_Current_Transmitted_From_Spec;

      ----------
      -- Next --
      ----------

      procedure Next is
      begin
         if Current = null then
            raise Constraint_Error;
         end if;

         case Current_Mode is
            when All_scopes =>
               null;
            when Current_Scope_Only =>
               if Scope_Top /= Current.Scope then
                  raise Constraint_Error;
               end if;
         end case;

         Previous := Current;
         Current  := Current.Next;
      end Next;

      --------------------
      -- Data_Available --
      --------------------

      function Data_Available return Boolean is
      begin
         if Current = null then
            return False;
         end if;

         case Current_Mode is
            when All_Scopes =>
               return True;
            when Current_Scope_Only =>
               return Current.Scope = Scope_Top;
         end case;
      end Data_Available;

      --------------------
      -- Delete_Current --
      --------------------

      procedure Delete_Current is
         Old_Current  : Link := Current;
         Old_Previous : constant Link := Previous;
      begin
         Next;
         -- Raises Constraint_Error if Current = null => OK

         if Head = Old_Current then
            Head := Old_Current.Next;
         else
            Old_Previous.Next := Old_Current.Next;
         end if;
         if Previous = Old_Current then
            Previous := Old_Previous;
         end if;

         Free (Old_Current.Content);
         Free (Old_Current);
      end Delete_Current;

      --------------------
      -- Update_Current --
      --------------------

      procedure Update_Current (Info : in Data) is
      begin
         -- Deallocate and reallocate, since new data can have
         -- a different constraint
         Free (Current.Content);
         Current.Content := new Data'(Info);
      end Update_Current;

      -----------------
      -- Enter_Scope --
      -----------------

      procedure Enter_Scope (Scope : Asis.Element) is
         use Asis, Asis.Declarations, Asis.Elements, Thick_Queries, Ada.Strings.Wide_Unbounded;
      begin
         -- Reestablish data saved from spec if any
         case Declaration_Kind (Scope) is
            when
              A_Package_Body_Declaration |
              A_Protected_Body_Declaration |
              A_Task_Body_Declaration
              =>
               declare
                  use Spec_Maps;
                  Scope_Name : constant Unbounded_Wide_String
                    := To_Unbounded_Wide_String (Full_Name_Image (Names (Scope)(1)));
                  Info : Spec_Save;
               begin
                  if Is_Present (Spec_Store, Scope_Name) then
                     Info := Fetch (Spec_Store, Scope_Name);
                     Info.Tail.Next := Head;
                     Head := Info.Head;
                     Delete (From => Spec_Store,
                             Key  => Scope_Name);
                  end if;
               end;
            when others =>
               null;
         end case;
      end Enter_Scope;

      ----------------
      -- Exit_Scope --
      ----------------

      procedure Exit_Scope (Scope : Asis.Element) is
         use Asis, Asis.Declarations, Asis.Elements, Thick_Queries, Spec_Maps;
         Current : Link := Head;
      begin
         if Current = null or else Current.Scope /= Scope_Top then
            -- No data => nothing to do
            return;
         end if;

         case Declaration_Kind (Scope) is
            when
              A_Package_Declaration |
              A_Protected_Type_Declaration |
              A_Single_Protected_Declaration |
              A_Single_Task_Declaration |
              A_Task_Type_Declaration |
              A_Generic_Package_Declaration
              =>
               -- Save data for the corresponding body
               loop
                  Current.Transmitted := True;
                  exit when Current.Next = null or else Current.Next.Scope /= Scope_Top;
                  Current := Current.Next;
               end loop;
               Add (To    => Spec_Store,
                    Key   =>To_Unbounded_Wide_String (Full_Name_Image (Names (Scope)(1))),
                    Value => (Head => Head, Tail => Current));
               Head := Current.Next;
               Current.Next := null;
            when others =>
               -- Delete data
               while Current /= null and then Current.Scope = Scope_Top loop
                  Head := Current.Next;
                  Free (Current.Content);
                  Free (Current);
                  Current := Head;
               end loop;
         end case;
      end Exit_Scope;

      ---------------
      -- Clear_All --
      ---------------

      procedure Clear_All (Scope : Asis.Element) is
         pragma Unreferenced (Scope);
         Temp : Link;
      begin
         while Head /= null loop
            Temp := Head;
            Head := Head.Next;
            Free (Temp.Content);
            Free (Temp);
         end loop;
      end Clear_All;

   begin
      Enter_Procs := new Scoping_Node'(Enter_Access, Enter_Procs);
      Exit_Procs  := new Scoping_Node'(Exit_Access,  Exit_Procs);
      Clear_Procs := new Scoping_Node'(Clear_Access, Clear_Procs);
   end Scoped_Store;

   ----------------
   -- Enter_Unit --
   ----------------

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      -- For a compilation unit, we activate the scope here
      -- in order to put declarations from the context clauses
      -- into the scope of the unit
      use Asis.Elements;
   begin
      Enter_Scope (Unit_Declaration (Unit));
   end Enter_Unit;

   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope (Scope : Asis.Element) is
      use Utilities, Asis.Elements;
      Current : Scoping_Link := Enter_Procs;
   begin
      if Scope_Top /= 0 and then Is_Equal (Scope_Stack (Scope_Top), Scope) then
         -- Do not recreate the scope if it was created for the
         -- compilation unit (see above).
         return;
      elsif Scope_Top = Scope_Index'Last then
         Failure ("Maximum scope nesting reached");
      end if;

      Scope_Top := Scope_Top + 1;
      Scope_Stack (Scope_Top) := Scope;

      while Current /= null loop
         Current.Proc (Scope);
         Current := Current.Next;
      end loop;
   end Enter_Scope;

   ----------------
   -- Exit_Scope --
   ----------------

   procedure Exit_Scope (Scope : Asis.Element) is
      Current : Scoping_Link := Exit_Procs;
   begin
      while Current /= null loop
         Current.Proc (Scope);
         Current := Current.Next;
      end loop;

      Scope_Top := Scope_Top - 1;
      if Scope_Top = 0 then
         -- Clean-up any remaining stuff linked to scope 0
         Reset;
      end if;
   end Exit_Scope;

   -----------
   -- Reset --
   -----------

   procedure Reset is
       Current : Scoping_Link := Clear_Procs;
   begin
      Scope_Top := 0;

      while Current /= null loop
         Current.Proc (Asis.Nil_Element);
         Current := Current.Next;
      end loop;
  end Reset;

end Framework.Scope_Manager;
