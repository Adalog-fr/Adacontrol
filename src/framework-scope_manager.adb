----------------------------------------------------------------------
--  Framework.Scope_Manager - Package body                          --
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
  Ada.Unchecked_Deallocation;

-- Adalog
with
  A4G_Bugs;

-- Adactl
with
  Utilities;

-- Asis
with
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
   -- Warning: this is a very delicate unit. If you think that it should be improved, or that
   -- things should be made differently, please write to rosen@adalog.fr before attempting anything.


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
   Non_Package_Depth      : Scope_Range := 0;
   -- Depth of first scope which is not a [generic] package

   type Unit_Location is (Inside_Context_Clauses, After_Context_Clauses, Inside_Unit);
   Unit_State : Unit_Location;

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

   Unit_Procs    : Scoping_Link;
   Scope_Procs   : Scoping_Link;
   Private_Procs : Scoping_Link;
   Exit_Procs    : Scoping_Link;
   Clear_Procs   : Scoping_Link;

   --------------
   -- Is_Scope --
   --------------

   function Is_Scope (Element : Asis.Element) return Boolean is
      use Asis, Asis.Elements;
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Declaration
                  | A_Procedure_Declaration
                  | An_Entry_Declaration
                  | A_Package_Declaration
                  | A_Generic_Procedure_Declaration
                  | A_Generic_Function_Declaration
                  | A_Generic_Package_Declaration
                  | A_Formal_Procedure_Declaration
                  | A_Formal_Function_Declaration
                  | A_Package_Body_Declaration
                  | A_Task_Type_Declaration
                  | A_Single_Task_Declaration
                  | A_Protected_Type_Declaration
                  | A_Single_Protected_Declaration
                  | A_Task_Body_Declaration
                  | A_Protected_Body_Declaration
                  | An_Entry_Body_Declaration
                  | A_Procedure_Body_Declaration
                  | A_Function_Body_Declaration
                  | A_Package_Renaming_Declaration
                  | A_Procedure_Renaming_Declaration
                  | A_Function_Renaming_Declaration
                  | A_Generic_Package_Renaming_Declaration
                  | A_Generic_Procedure_Renaming_Declaration
                  | A_Generic_Function_Renaming_Declaration
                  | A_Package_Instantiation
                  | A_Procedure_Instantiation
                  | A_Function_Instantiation
                    =>
                  return True;
               when others =>
                  return False;
            end case;
         when A_Statement =>
            case Statement_Kind (Element) is
               when A_For_Loop_Statement
                  | A_Block_Statement
                  | An_Accept_Statement
                    =>
                  return True;
               when others =>
                  return False;
            end case;
         when An_Exception_Handler =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Scope;

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
      use Asis;
   begin
      if Scope_Top = 0 then
         return Nil_Element;
      end if;

      return Scope_Stack (Scope_Top).Element;
   end Current_Scope;

   ---------------------
   -- Enclosing_Scope --
   ---------------------

   function Enclosing_Scope return Asis.Element is
      use Asis;
   begin
      if Scope_Top = Scope_Stack'First then
         return Nil_Element;
      end if;

      return Scope_Stack (Scope_Top - 1).Element;
   end Enclosing_Scope;

   -------------------
   -- Active_Scopes --
   -------------------

   function Active_Scopes return Scope_List is
      Result : Scope_List (1 ..  Scope_Top);
   begin
      for I in Result'Range loop
         Result (I) := Scope_Stack (I).Element;
      end loop;
      return Result;
   end Active_Scopes;


   ------------------
   -- Scoped_Store --
   ------------------

   Inactive_Message  : constant Wide_String := "Call of scoped_store operation in inactive state";

   Clear_Stay_Active : Boolean := False;
   -- This is set to True only when performing an emergency reset due to an internal error.
   -- Not very pretty to communicate through global variables, but we do not want to complicate
   -- the regular case just for this (normally never happening) case.

   package body Scoped_Store is separate;

   ----------------
   -- Enter_Unit --
   ----------------

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      use Asis, Asis.Elements;
      Current : Scoping_Link;
      Scope   : constant Asis.Declaration := Unit_Declaration (Unit);
   begin
      Unit_State := Inside_Context_Clauses;

      -- A separate unit (scope_top /= 0, since Enter_Scope has not yet been called)
      -- must be handled as part of its parent
      if Scope_Top = 0 then
         Unit_Is_Private := A4G_Bugs.Unit_Class (Unit) in A_Private_Declaration .. A_Private_Body;
         Current := Unit_Procs;
         while Current /= null loop
            Current.Proc (Scope);
            Current := Current.Next;
         end loop;
      end if;

      -- For a compilation unit, we activate the scope here
      -- in order to put declarations from the context clauses
      -- into the scope of the unit
      Enter_Scope (Scope, Is_Unit => True);
   end Enter_Unit;

   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope (Scope : Asis.Element; Is_Unit : Boolean := False) is
      use Utilities, Asis, Asis.Elements;
      Current : Scoping_Link;
   begin
      if Unit_State = After_Context_Clauses then
         -- Do not recreate the scope if it was created for the
         -- compilation unit (see above).
         Scope_Top  := Scope_Top + 1;
         Unit_State := Inside_Unit;
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

      Current := Scope_Procs;
      while Current /= null loop
         Current.Proc (Scope);
         Current := Current.Next;
      end loop;
   end Enter_Scope;

   ------------------------
   -- Enter_Private_Part --
   ------------------------

   procedure Enter_Private_Part is
      -- Note that this procedure is called after processing a visible part,
      -- even if there is no explicit private part (see Ruler)
      Current : Scoping_Link;
   begin
      Scope_Stack (Scope_Top).In_Private := True;

      if Scope_Top = 1 then
         -- Call Enter_Private of the scope manager only for private parts
         -- of compilation units
         Current := Private_Procs;
         while Current /= null loop
            Current.Proc (Scope_Stack (Scope_Top).Element);
            Current := Current.Next;
         end loop;
      end if;
   end Enter_Private_Part;

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
      Unit_State := After_Context_Clauses;
      Scope_Top := Scope_Top -1;
   end Exit_Context_Clauses;

   ---------------------
   -- In_Private_Part --
   ---------------------

   function In_Private_Part (Scope : Scope_Range := Current_Depth) return Boolean is
   begin
      return Scope /= 0 and then Scope_Stack (Scope).In_Private;
   end In_Private_Part;

   ------------------------
   -- In_Context_Clauses --
   ------------------------

   function In_Context_Clauses return Boolean is
   begin
      return Unit_State = Inside_Context_Clauses;
   end In_Context_Clauses;

   -----------------------------
   -- Is_Current_Scope_Global --
   -----------------------------

   function Is_Current_Scope_Global return Boolean is
   begin
      return Scope_Top = 0 or else Non_Package_Depth = 0;
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

   procedure Reset (Deactivate : Boolean) is
      procedure Free_List (L : in out Scoping_Link) is
         procedure Free is new Ada.Unchecked_Deallocation (Scoping_Node, Scoping_Link);

         Del : Scoping_Link;
      begin
         while L /= null loop
            Del := L;
            L   := L.Next;
            Free (Del);
         end loop;
      end Free_List;

      Current : Scoping_Link;
   begin  -- Reset
      Scope_Top         := 0;
      Non_Package_Depth := 0;
      Clear_Stay_Active := not Deactivate;

      Current := Clear_Procs;
      while Current /= null loop
         Current.Proc (Asis.Nil_Element);
         Current := Current.Next;
      end loop;

      if Deactivate then
         Free_List (Unit_Procs);
         Free_List (Scope_Procs);
         Free_List (Private_Procs);
         Free_List (Exit_Procs);
         Free_List (Clear_Procs);
      end if;
   end Reset;

end Framework.Scope_Manager;
