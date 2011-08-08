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

-- Asis
with
  Asis.Clauses,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Scope_Manager;
pragma Elaborate (Framework.Language);

package body Rules.Local_Hiding is
   use Framework, Framework.Control_Manager, Utilities;

   -- Algorithm:
   -- This rule is quite easy, since most of the work is done by Scoped_Store
   -- Each time a defining identifier is encountered, all scopes are searched for the same identifier
   -- The name used is an overloaded name for overloaded entities, therefore overloaded entities with
   -- different profiles are *not* considered hiding.
   -- The defining identifier is then added to the current scope.
   -- The only difficulty is that when we find the defining name of -say- a procedure, the current scope
   -- is already the procedure itself, but the name must be attached to where the procedure is declared,
   -- i.e. the enclosing scope.

   type Extended_Subrules is (Strict, Overloading, Overloading_Short);
   subtype Subrules is Extended_Subrules range Extended_Subrules'First .. Extended_Subrules'Pred (Overloading_Short);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Extended_Subrules);

   type Modifiers is (Not_Operator, Not_Enumeration);
   package Modifiers_Flag_Utilities is new Framework.Language.Modifier_Utilities (Modifiers);

   type Rule_Usage is array (Subrules) of Boolean;
   Not_Used : constant Rule_Usage := (others => False);

   Rule_Used    : Rule_Usage := Not_Used;
   Save_Used    : Rule_Usage;
   Rule_Context : array (Subrules) of Basic_Rule_Context;

   Include_Op           : Rule_Usage;
   Include_Enum         : Rule_Usage;
   Overloading_Is_Short : Boolean;

   type Identifier_Data (Length : Positive) is
      record
         Name           : Wide_String (1..Length);
         Short_Last     : Positive;
         Elem           : Asis.Element;
         Is_Callable    : Boolean;
         Is_Enumeration : Boolean;
      end record;
   procedure Clear (Item : in out Identifier_Data) is  -- null proc
      pragma Unreferenced (Item);
   begin
      null;
   end Clear;
   package Visible_Identifiers is new Framework.Scope_Manager.Scoped_Store (Identifier_Data);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Subrules_Flag_Utilities, Modifiers_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags     ("Parameter(s): <exceptions> ", Footer => "(default = strict)");
      Help_On_Modifiers ("<exceptions>:");
      User_Message ("Control occurrences of local identifiers that hide or overload an identical name");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities, Modifiers_Flag_Utilities;
      Subrule         : Extended_Subrules;
      Modif_Specified : Modifier_Set;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Modif_Specified := Get_Modifier_Set;
            Subrule := Get_Flag_Parameter (Allow_Any => False);
            case Subrule is
               when Overloading =>
                  Overloading_Is_Short := False;
               when Overloading_Short =>
                  Subrule := Overloading;
                  Overloading_Is_Short := True;
               when others =>
                  null;
            end case;
            if Rule_Used (Subrule) then
               Parameter_Error (Rule_Id, "subrule already specified");
            end if;
            Rule_Used    (Subrule) := True;
            Rule_Context (Subrule) := Basic.New_Context (Ctl_Kind, Ctl_Label);
            Include_Op   (Subrule) := not Modif_Specified (Not_Operator);
            Include_Enum (Subrule) := not Modif_Specified (Not_Enumeration);
         end loop;
      else
         Rule_Used    (Strict) := True;
         Rule_Context (Strict) := Basic.New_Context (Ctl_Kind, Ctl_Label);
         Include_Op   (Strict) := True;
      end if;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := Not_Used;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Not_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      if Rule_Used /= Not_Used then
         Visible_Identifiers.Activate;
      end if;
   end Prepare;

   ---------------------------
   -- Process_Defining_Name --
   ---------------------------

   procedure Process_Defining_Name (Name : in Asis.Defining_Name) is
      use Asis, Asis.Elements, Asis.Declarations;
      use Thick_Queries, Framework.Scope_Manager;

      Scope      : Asis.Element;
      First_Name : Asis.Defining_Name;

      function Not_An_Appropriate_Name return Boolean is
         -- This function detects names that are not to be processed by this rule, since
         -- they do not hide anything nor can they be hidden:
         --   - Names of record components (but not protected components), including discriminants
         --   - Names of formal parameters in a SP renaming declaration
         --   - Names of formal parameters in the declaration of an access to SP type
         Decl           : constant Asis.Declaration := Enclosing_Element (Name);
         Decl_Enclosing : constant Asis.Element     := Enclosing_Element (Decl);
      begin
         case Declaration_Kind (Decl) is
            when A_Component_Declaration =>
               return Definition_Kind (Decl_Enclosing) /= A_Protected_Definition;
            when A_Discriminant_Specification =>
               return True;
            when A_Parameter_Specification =>
               case Element_Kind (Decl_Enclosing) is
                  when A_Definition =>
                     return True;
                  when A_Declaration =>
                     return Declaration_Kind (Enclosing_Element (Decl)) in A_Renaming_Declaration;
                  when A_Statement =>
                     -- This happens only for parameters of accept statements
                     return False;
                  when others =>
                     Failure ("Unexpected enclosing of name declaration", Decl_Enclosing);
               end case;
            when others =>
               return False;
         end case;
      end Not_An_Appropriate_Name;

   begin   -- Process_Defining_Name
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Not_An_Appropriate_Name then
         return;
      end if;

      First_Name := First_Defining_Name (Name);
      declare
         use Framework.Reports;
         function Enclosing_Scope (N : Asis.Element) return Asis.Element is
            Result : Asis.Element := Enclosing_Element (N);
         begin
            while Defining_Name_Kind (Result) = A_Defining_Expanded_Name loop
               Result := Enclosing_Element (Result);
            end loop;
            return Result;
         end Enclosing_Scope;

         Short_Name     : constant Wide_String := To_Upper (Defining_Name_Image (Name));
         Full_Name      : constant Wide_String := Short_Name & To_Upper (Profile_Image (Name, With_Profile => False));
         Callable_Name  : constant Boolean     := Is_Callable_Construct (Name);
         Is_Enumeration : constant Boolean     := Declaration_Kind (Enclosing_Element (Name))
                                                  = An_Enumeration_Literal_Specification;
         Is_Scope_Name  : constant Boolean     := Is_Equal (Enclosing_Scope (Name), Current_Scope);
         -- Is_Scope_Name is True if Name is the defining name for the current scope
         -- => it belongs to the enclosing scope
         Already_There  : Boolean;
         Overload_Count : Natural := 0;
         Overload_Last  : Asis.Element;

         type Hiding_Kinds is (Hides, Overloads, Not_Hiding);
         function Hiding_Kind (Check : Identifier_Data) return Hiding_Kinds is
         begin
            if not Callable_Name or else not Check.Is_Callable then
               -- At least one is not a callable entity => no overloading
               if Check.Name (1 .. Check.Short_Last) = Short_Name then
                  return Hides;
               else
                  return Not_Hiding;
               end if;
            elsif Check.Name = Full_Name then
               -- Overloadable and names match with profile
               return Hides;
            elsif Check.Name (1 .. Check.Short_Last) = Short_Name then
               -- Overloadable and names match without profile
               return Overloads;
            else
               return Not_Hiding;
            end if;
         end Hiding_Kind;

      begin
         if Is_Scope_Name then
            Scope := Enclosing_Scope;
         else
            Scope := Current_Scope;
         end if;

         Already_There := False;
         -- If scope is nil, it is the defining name of a library unit
         -- => cannot hide anything
         if not Is_Nil (Scope) then
            Visible_Identifiers.Reset (All_Scopes);

            while Visible_Identifiers.Data_Available loop
               -- Discard the case where we find the definition of another view
               -- of the same entity
               if Is_Equal (First_Name, Visible_Identifiers.Current_Data.Elem) then
                  Already_There := True;
               else
                  case Hiding_Kind (Visible_Identifiers.Current_Data) is
                     when Hides =>
                        if Rule_Used (Strict)
                          and (Include_Op   (Strict) or Short_Name (1) /= '"')
                          and (Include_Enum (Strict)
                               or not Is_Enumeration or not Visible_Identifiers.Current_Data.Is_Enumeration)
                        then
                           Report (Rule_Id,
                                   Rule_Context (Strict),
                                   Get_Location (Name),
                                   '"' & Framework.Language.Adjust_Image (To_Title (Full_Name))
                                   & """ hides declaration at "
                                   & Image (Get_Location (Visible_Identifiers.Current_Data.Elem)));
                        end if;
                     when Overloads =>
                        if Rule_Used (Overloading)
                          and (Include_Op   (Overloading) or Short_Name (1) /= '"')
                          and (Include_Enum (Overloading)
                               or not Is_Enumeration or not Visible_Identifiers.Current_Data.Is_Enumeration)
                        then
                           if Overloading_Is_Short then
                              Overload_Count := Overload_Count + 1;
                              Overload_Last  := Visible_Identifiers.Current_Data.Elem;
                           else
                              Report (Rule_Id,
                                      Rule_Context (Overloading),
                                      Get_Location (Name),
                                      '"' & Framework.Language.Adjust_Image (To_Title (Full_Name))
                                      & """ overloads declaration at "
                                      & Image (Get_Location (Visible_Identifiers.Current_Data.Elem)));
                           end if;
                        end if;
                     when Not_Hiding =>
                        null;
                  end case;
               end if;
               Visible_Identifiers.Next;
            end loop;

            if Overload_Count /= 0 then
               -- Short form of reports of overloading, issued after the loop
               Report (Rule_Id,
                       Rule_Context (Overloading),
                       Get_Location (Name),
                       '"' & Framework.Language.Adjust_Image (To_Title (Full_Name))
                       & """ overloads "
                       & Integer_Img (Overload_Count)
                       & " declaration(s), last at "
                       & Image (Get_Location (Overload_Last)));
            end if;
         end if;

         if not Already_There then
            if Is_Scope_Name then
               -- This is the defining name of the program unit that defines the current scope
               -- it must be associated to the enclosing scope
               Visible_Identifiers.Push_Enclosing ((Full_Name'Length,
                                                    Full_Name,
                                                    Short_Name'Length,
                                                    First_Name,
                                                    Is_Callable    => Callable_Name,
                                                    Is_Enumeration => Is_Enumeration));
            else
               Visible_Identifiers.Push ((Full_Name'Length,
                                          Full_Name,
                                          Short_Name'Length,
                                          First_Name,
                                          Is_Callable    => Callable_Name,
                                          Is_Enumeration => Is_Enumeration));
            end if;
         end if;
      end;
   end Process_Defining_Name;

   -------------------------
   -- Process_With_Clause --
   -------------------------

   procedure Process_With_Clause (With_Clause : in Asis.Clause) is
      -- Names in with clauses cannot hide anything, but can be hidden
      use Asis, Asis.Clauses, Asis.Elements, Asis.Expressions, Thick_Queries;
   begin
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         All_Names : constant Asis.Element_List := Clause_Names (With_Clause);
         Name      : Asis.Expression;
         Current   : Asis.Expression;
      begin
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
                  Short_Name : constant Wide_String := To_Upper (A4G_Bugs.Name_Image (Name));
                  Full_Name  : constant Wide_String := Short_Name & Profile_Image (Name, With_Profile => False);
               begin
                  -- Bind names to scope 0
                  Visible_Identifiers.Push_Enclosing ((Full_Name'Length,
                                                       Full_Name,
                                                       Short_Name'Length,
                                                       Name,
                                                       Is_Callable    => Is_Callable_Construct (Name),
                                                       Is_Enumeration => False));
               end;
               exit when Expression_Kind (Current) = An_Identifier;
               Current := Prefix (Current);
            end loop;
         end loop;
      end;
   end Process_With_Clause;

begin  -- Rules.Local_Hiding
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Local_Hiding;
