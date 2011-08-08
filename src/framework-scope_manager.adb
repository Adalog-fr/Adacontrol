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
  Ada.Strings.Wide_Fixed,
  Ada.Unchecked_Deallocation;

-- Adalog
with
  A4G_Bugs,
  Binary_Map,
  Thick_Queries;

-- Adactl
with
  Utilities;

-- Asis
with
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements;

package body Framework.Scope_Manager is

   --
   -- Implementation notes:
   --
   -- This unit relies on the fact that a parent unit is always processed before its children.
   -- this is ensured by the options' analyzer. See Adactl_Options.Add_Unit.
   --
   -- Note that a "unit" scope is not necessarily at level 1, in the case of subunits.


   --
   -- Management of the scopes stack
   --

   subtype Scope_Index is Scope_Range range 1 .. Scope_Range'Last;
   type Scope_Data is
      record
         Element    : Asis.Element;
         In_Private : Boolean;
         Is_Unit    : Boolean; -- True if this is the "main" scope of a compilation unit
      end record;

   Scope_Stack            : array (Scope_Index) of Scope_Data;
   Scope_Top              : Scope_Range := 0;
   Unit_Is_Private        : Boolean;
   Inside_Context_Clauses : Boolean;
   Non_Package_Depth      : Scope_Range := 0;
   -- Depth of first scope which is not a [generic] package

   --
   -- Linked list of Enter_Procs, Private_Procs, Exit_Procs and Clear_Procs:
   --
   type Scoping_Node;
   type Scoping_Link is access Scoping_Node;

   type Scoping_Node is
      record
         Proc : Scoping_Procedure;
         Next : Scoping_Link;
      end record;

   Enter_Procs   : Scoping_Link;
   Private_Procs : Scoping_Link;
   Exit_Procs    : Scoping_Link;
   Clear_Procs   : Scoping_Link;

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
      return Scope_Stack (Scope_Top).Element;
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
         return Scope_Stack (Scope_Top-1).Element;
      end if;
   end Enclosing_Scope;

   -------------------
   -- Active_Scopes --
   -------------------

   function Active_Scopes return Scope_List is
      Result : Scope_List (1 ..  Scope_Top);
   begin
      for I in Result'Range loop
         Result (I) := Scope_Stack (Scope_Range (I)).Element;
      end loop;
      return Result;
   end Active_Scopes;


   --------------------------------------------------
   -- Management of user data associated to scopes --
   --------------------------------------------------

   ------------------
   -- Scoped_Store --
   ------------------

   package body Scoped_Store is

      type Data_Access is access Data;
      procedure Free is new Ada.Unchecked_Deallocation (Data, Data_Access);

      -- Linked list of Data, LIFO order
      type Node;
      type Link is access Node;
      type Node is
         record
            Next    : Link;
            Scope   : Scope_Range;
            Origin  : Declaration_Origin;
            Content : Data_Access;
         end record;
      procedure Free is new Ada.Unchecked_Deallocation (Node, Link);

      Head              : Link;  -- Head (textually last) declaration
      Unit_Head         : Link;  -- id. for the current compilation unit
      Unit_Visible_Head : Link;  -- id. for the visible part of the current compilation unit

      type Spec_Save is
         record
            Head         : Link;
            Visible_Head : Link;
            Tail         : Link;
         end record;
      Parent_Info : Spec_Save;

      package Spec_Maps is new Binary_Map (Key_Type   => Unbounded_Wide_String,
                                           Value_Type => Spec_Save);
      Spec_Store : Spec_Maps.Map;

      -- Iterator:
      Current      : Link;
      Previous     : Link;
      Current_Mode : Iterator_Mode;
      Final_Scope  : Scope_Range;


      ----------
      -- Push --
      ----------

      procedure Push (Info : in Data) is
      begin
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
         while Insert_Ptr /= null and then Insert_Ptr.Scope = Scope_Top loop
            Before_Insert := Insert_Ptr;
            Insert_Ptr    := Insert_Ptr.Next;
         end loop;
         if Insert_Ptr = Head then
            Head := new Node'(Next    => Head,
                              Scope   => Scope_Top-1,
                              Origin  => Same_Unit,
                              Content => new Data'(Info));
         else
            Before_Insert.Next := new Node'(Next    => Insert_Ptr,
                                            Scope   => Scope_Top-1,
                                            Origin  => Same_Unit,
                                            Content => new Data'(Info));
         end if;
      end Push_Enclosing;

      -----------
      -- Reset --
      -----------

      procedure Reset (Mode : Iterator_Mode) is
      begin
         Current      := Head;
         Previous     := null;
         Current_Mode := Mode;
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
         Current      := Head;
         Previous     := null;
         Current_Mode := Mode;
         while Current /= null and then not Equivalent_Keys (Info, Current.Content.all) loop
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
         return Current.Content.all;
      end Current_Data;

      ------------------------
      -- Current_Data_Level --
      ------------------------

      function  Current_Data_Level return Scope_Range is
      begin
         return Current.Scope;
      end Current_Data_Level;

      ------------------------
      -- Current_Data_Scope --
      ------------------------

      function Current_Data_Scope return Asis.Element is
      begin
         if Current.Scope = 0 then
            return Asis.Nil_Element;
         else
            return Scope_Stack (Current.Scope).Element;
         end if;
      end Current_Data_Scope;

      --------------------
      -- Current_Origin --
      --------------------

      function Current_Origin return Declaration_Origin is
      begin
         return Current.Origin;
      end Current_Origin;

      ----------
      -- Next --
      ----------

      procedure Next is
      begin
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

            when Unit_Scopes | Current_Scope_Only =>
               return Current.Scope >= Final_Scope;
         end case;
      end Data_Available;

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

      -------------------
      --Delete_Current --
      -------------------

      procedure Delete_Current is
         Deleted_Node : Link := Current;
      begin
         Current := Current.Next;
         if Previous = null then
            Head := Current;
         else
            Previous.Next := Current;
         end if;
         Free (Deleted_Node.Content);
         Free (Deleted_Node);
      end Delete_Current;

      -----------------
      -- Enter_Scope --
      -----------------

      procedure Enter_Scope (Scope : Asis.Element) is
         use Asis, Asis.Compilation_Units, Asis.Declarations, Asis.Elements;
         use Thick_Queries, Ada.Strings, Ada.Strings.Wide_Fixed;

         procedure Restore_Parent_Context is
            -- Copy context from parent unit if any
            -- The parent is always a package, and hence has no profile; the Full_Name_Image
            -- is equal to the Unit_Full_Name
            use Spec_Maps;
            Unit_Name : constant Wide_String := Unit_Full_Name (Enclosing_Compilation_Unit (Scope));
            Inx_Dot   : constant Natural     := Index (Unit_Name, ".", Going => Backward);
            Tail      : Link;
         begin
            Parent_Info := (others => null);
            if Inx_Dot /= 0 then
               -- This is a child unit
               if Is_Present (Spec_Store, To_Unbounded_Wide_String (Unit_Name (1 .. Inx_Dot - 1))) then
                  Parent_Info := Fetch (Spec_Store, To_Unbounded_Wide_String (Unit_Name (1 .. Inx_Dot - 1)));
                  if Unit_Is_Private then
                     Head := new Node'(Next    => Parent_Info.Head.Next,
                                       Scope   => Parent_Info.Head.Scope,
                                       Origin  => Parent,
                                       Content => new Data'(Parent_Info.Head.Content.all));
                  else
                     if Parent_Info.Visible_Head = null then
                        -- This can happen if all data belong to the private part of the parent
                        Head := null;
                     else
                        Head := new Node'(Next    => Parent_Info.Visible_Head.Next,
                                          Scope   => Parent_Info.Visible_Head.Scope,
                                          Origin  => Parent,
                                          Content => new Data'(Parent_Info.Visible_Head.Content.all));
                     end if;
                  end if;

                  if Head /= null then
                     -- Deep copy the chain
                     Tail := Head;
                     while Tail.Next /= null loop
                        Tail.Next := new Node'(Next    => Tail.Next.Next,
                                               Scope   => Tail.Next.Scope,
                                               Origin  => Parent,
                                               Content => new Data'(Tail.Next.Content.all));
                        Tail := Tail.Next;
                     end loop;
                  end if;
               end if;
            end if;
         end Restore_Parent_Context;

      begin
         case Declaration_Kind (Scope) is
            when A_Package_Body_Declaration
              | A_Protected_Body_Declaration
              | A_Task_Body_Declaration
              =>
               -- Restore data saved from spec if any
               -- (this includes data from parent units)
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

                     -- Do not delete information from library package specs, must be kept for children
                     -- Note that only a package body can be at depth 1 (others cannot be compilation units)
                     if Scope_Top = 1 then
                        Unit_Head := Head;
                     else
                        Delete (From => Spec_Store, Key => Scope_Name);
                     end if;
                  elsif Scope_Top = 1 then
                     Unit_Head := null;
                  end if;
               end;

            when A_Package_Declaration  -- Possible child units
              | A_Procedure_Declaration
              | A_Function_Declaration
              | A_Renaming_Declaration
              | A_Generic_Declaration
              | A_Generic_Instantiation
              =>
               -- Reestablish data saved from parent if any
               if Scope_Top = 1 then
                  Unit_Visible_Head := null;
                  Restore_Parent_Context;
               end if;

            when A_Procedure_Body_Declaration -- Possible child units
              | A_Function_Body_Declaration
              =>
               if Scope_Top = 1 and then Is_Nil (Corresponding_Declaration (Scope)) then
                  -- Restore data saved from parent if there is no corresponding spec
                  Unit_Visible_Head := null;
                  Restore_Parent_Context;
               else
                  -- Restore data from spec
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

                        -- A subprogram has no children, therefore we can safely delete information
                        Delete (From => Spec_Store, Key => Scope_Name);
                     end if;
                  end;
              end if;

            when others =>
               null;
         end case;
      end Enter_Scope;


      -------------------
      -- Enter_Private --
      -------------------

      procedure Enter_Private (Scope : Asis.Element) is
         pragma Unreferenced (Scope);
         Tail      : Link;
         Save_Head : constant Link := Head;
      begin
         if Scope_Top = 1 then
            -- Note that the scope is necessarily a [generic] package here,
            -- since these are the only compilation units with a private part.
            Unit_Visible_Head := Head;

            -- If we are in a public child,
            -- deep copy info from private part of the parent if any
            if not Unit_Is_Private and then Parent_Info.Visible_Head /= Parent_Info.Head then
               -- Parent_Info.Head is not null, because if it were,
               -- Parent_Info.Visible_Head would also be null
               Head := new Node'(Next    => Parent_Info.Head.Next,
                                 Scope   => Parent_Info.Head.Scope,
                                 Origin  => Parent,
                                 Content => new Data'(Parent_Info.Head.Content.all));
               Tail := Head;
               while Tail.Next /= Parent_Info.Visible_Head loop
                 Tail.Next := new Node'(Next    => Tail.Next.Next,
                                         Scope   => Tail.Next.Scope,
                                         Origin  => Parent,
                                         Content => new Data'(Tail.Next.Content.all));
                  Tail := Tail.Next;
               end loop;
               Tail.Next := Save_Head;
            end if;
         end if;
      end Enter_Private;


      ----------------
      -- Exit_Scope --
      ----------------

      procedure Exit_Scope (Scope : Asis.Element) is
         use Asis, Asis.Declarations, Asis.Elements, Thick_Queries, Spec_Maps;

         procedure Delete_Data (Stop_At : Link) is
            -- Stop_At = pointer to first node to keep
            --           null to delete the whole chain
            Cur_Link        : Link := Head;
            Local_Scope_Top : Scope_Range := Scope_Top;
         begin
            -- Data linked to (pseudo) scope 0 must be deleted together with
            -- data from scope 1.
            if Scope_Top = 1 then
               Local_Scope_Top := 0;
            end if;

            while Cur_Link /= Stop_At and then Cur_Link.Scope >= Local_Scope_Top loop
               Head := Cur_Link.Next;
               Free (Cur_Link.Content);
               Free (Cur_Link);
               Cur_Link := Head;
            end loop;
         end Delete_Data;

         Cur_Link : Link := Head;
      begin
         if Cur_Link = null or else Cur_Link.Scope /= Scope_Top then
            -- No data => nothing to do
            return;
         end if;

         case Declaration_Kind (Scope) is
            when A_Procedure_Declaration
              | A_Function_Declaration
              | A_Package_Declaration
              | A_Protected_Type_Declaration
              | A_Single_Protected_Declaration
              | A_Single_Task_Declaration
              | A_Task_Type_Declaration
              | A_Generic_Package_Declaration
              =>
               -- Save data for the corresponding body
               loop
                  if Cur_Link.Origin = Same_Unit then
                     Cur_Link.Origin := Specification;
                  end if;
                  exit when Cur_Link.Next = null or else Cur_Link.Next.Scope /= Scope_Top;
                  Cur_Link := Cur_Link.Next;
               end loop;
               Add (To    => Spec_Store,
                    Key   => To_Unbounded_Wide_String (Full_Name_Image (Names (Scope)(1))),
                    Value => (Head => Head, Visible_Head => Unit_Visible_Head, Tail => Cur_Link));
               Head         := Cur_Link.Next;
               Cur_Link.Next := null;

            when A_Package_Body_Declaration =>
               -- If a compilation unit, free only the data from the body
               -- (Info from the spec may be necessary for children)
               if Scope_Top = 1 then
                  Delete_Data (Stop_At => Unit_Head);

                  -- Data not deleted are referenced from Spec_Store, but must not
                  -- be kept on the stack
                  Head := null;
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
         while Head /= null loop
            Temp := Head;
            Head := Head.Next;
            Free (Temp.Content);
            Free (Temp);
         end loop;
      end Clear_All;

   begin  -- Scoped_Store
      Enter_Procs   := new Scoping_Node'(Enter_Access,   Enter_Procs);
      Private_Procs := new Scoping_Node'(Private_Access, Private_Procs);
      Exit_Procs    := new Scoping_Node'(Exit_Access,    Exit_Procs);
      Clear_Procs   := new Scoping_Node'(Clear_Access,   Clear_Procs);
   end Scoped_Store;

   ------------------------
   -- Enter_Private_Part --
   ------------------------

   procedure Enter_Private_Part is
      -- Note that this procedure is called after processing a visible part,
      -- even if there is no explicit private part (see Ruler)
      Current : Scoping_Link;
   begin
      Scope_Stack (Scope_Top).In_Private := True;

      Current := Private_Procs;
      while Current /= null loop
         Current.Proc (Scope_Stack (Scope_Top).Element);
         Current := Current.Next;
      end loop;
   end Enter_Private_Part;

   ----------------
   -- Enter_Unit --
   ----------------

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      -- For a compilation unit, we activate the scope here
      -- in order to put declarations from the context clauses
      -- into the scope of the unit
      use Asis, Asis.Elements;
   begin
      Unit_Is_Private        := A4G_Bugs.Unit_Class (Unit) in A_Private_Declaration .. A_Private_Body;
      Inside_Context_Clauses := True;

      Enter_Scope (Unit_Declaration (Unit), Is_Unit => True);
   end Enter_Unit;

   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope (Scope : Asis.Element; Is_Unit : Boolean := False) is
      use Utilities, Asis, Asis.Elements;
      Current : Scoping_Link;
   begin
      if Scope_Top /= 0 and then Is_Equal (Scope_Stack (Scope_Top).Element, Scope) then
         -- Do not recreate the scope if it was created for the
         -- compilation unit (see above).
         return;
      elsif Scope_Top = Scope_Index'Last then
         Failure ("Maximum scope nesting reached");
      end if;

      Scope_Top := Scope_Top + 1;
      Scope_Stack (Scope_Top) := (Element => Scope, In_Private => False, Is_Unit => Is_Unit);
      case Declaration_Kind (Scope) is
         when A_Package_Declaration
           | A_Generic_Package_Declaration
           | A_Package_Body_Declaration
           =>
            null;
         when others =>
            if Non_Package_Depth = 0 then
               Non_Package_Depth := Scope_Top;
            end if;
      end case;

      Current := Enter_Procs;
      while Current /= null loop
         Current.Proc (Scope);
         Current := Current.Next;
      end loop;
   end Enter_Scope;

   ----------------
   -- Exit_Scope --
   ----------------

   procedure Exit_Scope (Scope : Asis.Element; Force : Boolean := False) is
      Current : Scoping_Link;
   begin
      -- Delay exit from the unit-level scope until the unit is exited
      -- to allow Exit_Unit from rules to still have scoping information
      if Scope_Stack (Scope_Top).Is_Unit and not Force then
         return;
      end if;

      Current := Exit_Procs;
      while Current /= null loop
         Current.Proc (Scope);
         Current := Current.Next;
      end loop;

      if Scope_Top = Non_Package_Depth then
         -- exiting from the first non-package scope
         Non_Package_Depth := 0;
      end if;
      Scope_Top := Scope_Top - 1;
   end Exit_Scope;

   ---------------
   -- Exit_Unit --
   ---------------

   procedure Exit_Unit (Unit  : in Asis.Compilation_Unit) is
      use Asis.Elements;
   begin
      -- Now is the time to exit the top-most scope (see above)
      Exit_Scope (Unit_Declaration (Unit), Force => True);
   end Exit_Unit;

   --------------------------
   -- Exit_Context_Clauses --
   --------------------------

   procedure Exit_Context_Clauses is
   begin
      Inside_Context_Clauses := False;
   end Exit_Context_Clauses;

   ---------------------
   -- In_Private_Part --
   ---------------------

   function In_Private_Part (Scope : Scope_Range := Current_Depth) return Boolean is
   begin
      return Scope_Stack (Scope).In_Private;
   end In_Private_Part;

   ------------------------
   -- In_Context_Clauses --
   ------------------------

   function In_Context_Clauses return Boolean is
   begin
      return Inside_Context_Clauses;
   end In_Context_Clauses;

   -----------------------------
   -- Is_Current_Scope_Global --
   -----------------------------

   function Is_Current_Scope_Global return Boolean is
   begin
      return Non_Package_Depth = 0;
   end Is_Current_Scope_Global;

   -------------------------------
   -- Is_Enclosing_Scope_Global --
   -------------------------------

   function Is_Enclosing_Scope_Global return Boolean is
   begin
      return Non_Package_Depth = Scope_Top;
   end Is_Enclosing_Scope_Global;

   -----------
   -- Reset --
   -----------

   procedure Reset is
       Current : Scoping_Link;
   begin
      Scope_Top       := 0;
      Non_Package_Depth := 0;

      Current := Clear_Procs;
      while Current /= null loop
         Current.Proc (Asis.Nil_Element);
         Current := Current.Next;
      end loop;
   end Reset;

end Framework.Scope_Manager;
