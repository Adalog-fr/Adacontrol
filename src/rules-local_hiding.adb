----------------------------------------------------------------------
--  Rules.Local_Hiding - Package body                               --
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

-- Asis
with
  Asis.Clauses,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;

package body Rules.Local_Hiding is
   use Framework, Utilities;

   -- Algorithm:
   -- This rule is quite easy, since most of the work is done by Scoped_Store
   -- Each time a defining identifier is encountered, all scopes are searched for the same identifier
   -- The name used is an overloaded name for overloaded entities, therefore overloaded entities with
   -- different profiles are *not* considered hiding.
   -- The defining identifier is then added to the current scope.
   -- The only difficulty is that when we find the defining name of -say- a procedure, the current scope
   -- is already the procedure itself, but the name must be attached to where the procedure is declared,
   -- i.e. the enclosing scope.

   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Rule_Type  : Rule_Types;
   Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type Identifier_Data (Length : Positive) is
      record
         Name       : Wide_String (1..Length);
         Short_Last : Positive;
         Elem       : Asis.Element;
      end record;
   package Visible_Identifiers is new Framework.Scope_Manager.Scoped_Store (Identifier_Data);

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): none");
      User_Message ("Control occurrences of local identifiers that hide an outer identical name");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if  Parameter_Exists then
         Parameter_Error ("No parameter for rule " & Rule_Id);
      end if;

      if Rule_Used then
         Parameter_Error (Rule_Id & ": this rule can be specified only once");
      else
         Rule_Type  := Rule_Use_Type;
         Rule_Label := To_Unbounded_Wide_String (Label);
         Rule_Used  := True;
      end if;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := False;
            Rule_Label := Null_Unbounded_Wide_String;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ---------------------------
   -- Process_Defining_Name --
   ---------------------------

   procedure Process_Defining_Name (Name : in Asis.Defining_Name) is
      use Thick_Queries, Asis, Asis.Elements, Asis.Declarations;

      Scope : Asis.Element;

      -- This function detects names that are not to be processed by this rule, since
      -- they do not hide anything nor can they be hidden:
      --   - Names of record components (but not protected components)
      --   - Names of formal parameters in a SP renaming declaration
      --   - Names of formal parameters in the declaration of an access to SP type
      function Not_An_Appropriate_Name return Boolean is
         Decl           : constant Asis.Declaration := Enclosing_Element (Name);
         Decl_Enclosing : constant Asis.Element     := Enclosing_Element (Decl);
      begin
         case Declaration_Kind (Decl) is
            when A_Component_Declaration =>
               return Definition_Kind (Decl_Enclosing) /= A_Protected_Definition;
            when A_Parameter_Specification =>
               case Element_Kind (Decl_Enclosing) is
                  when A_Definition =>
                     return True;
                  when A_Declaration =>
                     return Declaration_Kind (Enclosing_Element (Decl)) in A_Renaming_Declaration;
                  when others =>
                     Failure ("Unexpected enclosing of name declaration", Decl_Enclosing);
               end case;
            when others =>
               return False;
         end case;
      end Not_An_Appropriate_Name;

      -- This function returns True if Left and Right are the same scopes,
      -- or if Left is a body and Right is the corresponding specification
      function Are_Equivalent_Scopes (Left, Right : Asis.Element) return Boolean is
      begin
         if Is_Equal (Left, Right) then
            return True;
         end if;

         case Element_Kind (Left) is
            when A_Statement
              | An_Exception_Handler
              =>
               return False;
            when others =>
               if Is_Nil (Corresponding_Declaration (Left)) then
                  -- No corresponding specification
                  return False;
               else
                  return Is_Equal (Corresponding_Declaration (Left), Right);
               end if;
         end case;
      end Are_Equivalent_Scopes;

   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Not_An_Appropriate_Name then
         return;
      end if;

      declare
         use Framework.Reports, Framework.Scope_Manager, Asis.Elements;
         use Ada.Strings.Wide_Unbounded;
         Short_Name    : constant Wide_String := To_Upper (Defining_Name_Image (Name));
         Full_Name     : constant Wide_String := Short_Name & Profile_Image (Name, With_Profile => False);
         Callable_Name : constant Boolean     := Is_Callable_Construct (Name);
         Is_Scope_Name : constant Boolean     := Is_Equal (Enclosing_Element (Name), Current_Scope);
         -- Is_Scope_Name is True if Name is the defining name for the current scope
         -- => it belongs to the enclosing scope

         function Is_Same (Check : Identifier_Data) return Boolean is
            -- If both are callable entities, compare with profiles
            -- otherwise, compare without profile
         begin
            if Callable_Name then
               if Is_Callable_Construct (Check.Elem) then
                  return Check.Name = Full_Name;
               else
                  return Check.Name (1 .. Check.Short_Last) = Short_Name;
               end if;
            else
               return Check.Name (1 .. Check.Short_Last) = Short_Name;
            end if;
         end Is_Same;


      begin
         Visible_Identifiers.Reset (All_Scopes);
         while Visible_Identifiers.Data_Available loop
            if Is_Same (Visible_Identifiers.Current_Data) then

               -- Discard the case where we find a name declared within a scope equivalent
               -- to the scope of Name:
               -- this corresponds for example to a spec and corresponding body, incomplete
               -- type and corresponding full declarations...
               -- These must correspond to the same entity, otherwise it would not be allowed
               -- by the compiler.
               if Is_Scope_Name then
                  Scope := Enclosing_Scope;
               else
                  Scope := Current_Scope;
               end if;
               if not Are_Equivalent_Scopes (Scope, Visible_Identifiers.Current_Data_Scope) then
                  Report (Rule_Id,
                          To_Wide_String (Rule_Label),
                          Rule_Type,
                          Get_Location (Name),
                          '"' & Framework.Language.Adjust_Image (To_Title (Full_Name))
                            & """ hides declaration at "
                            & Image (Get_Location (Visible_Identifiers.Current_Data.Elem)));
                  exit;
               end if;
            end if;
            Visible_Identifiers.Next;
         end loop;

         if Is_Scope_Name then
            -- This is the defining name of the program unit that defines the current scope
            -- it must be associated to the enclosing scope
            Visible_Identifiers.Push_Enclosing ((Full_Name'Length,
                                                 Full_Name,
                                                 Short_Name'Length,
                                                 Name));
         else
            Visible_Identifiers.Push ((Full_Name'Length,
                                       Full_Name,
                                       Short_Name'Length,
                                       Name));
         end if;
      end;
   end Process_Defining_Name;

   -------------------------
   -- Process_With_Clause --
   -------------------------

   procedure Process_With_Clause (With_Clause : Asis.Clause) is
      -- Names in with clauses cannot hide anything, but can be hidden
      use Asis, Asis.Clauses, Asis.Elements, Asis.Expressions, Thick_Queries;

      All_Names : constant Asis.Element_List := Clause_Names (With_Clause);
      Name      : Asis.Expression;
      Current   : Asis.Expression;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      for I in All_Names'Range loop
         Current := All_Names (I);
         loop
            case Expression_Kind (Current) is
               when An_Identifier =>
                  Name := Current;
               when A_Selected_Component =>
                  Name := Selector (Current);
               when others =>
                  Failure ("Unexpected name in with clause", All_Names (I));
            end case;
            declare
               Short_Name : constant Wide_String := To_Upper (Name_Image (Name));
               Full_Name  : constant Wide_String := Short_Name & Profile_Image (Name, With_Profile => False);
            begin
               -- Bind names to scope 0
               Visible_Identifiers.Push_Enclosing ((Full_Name'Length, Full_Name, Short_Name'Length, Name));
            end;
            exit when Expression_Kind (Current) = An_Identifier;
            Current := Prefix (Current);
         end loop;
      end loop;
   end Process_With_Clause;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access);
end Rules.Local_Hiding;
