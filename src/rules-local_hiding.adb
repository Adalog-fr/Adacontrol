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
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;
pragma Elaborate (Framework.Language);

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

   type Subrules is (Strict, Overloading, Both);
   subtype True_Subrules is Subrules range Strict .. Overloading;
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Rule_Usage is array (True_Subrules) of Boolean;
   Not_Used : constant Rule_Usage := (others => False);

   Rule_Used    : Rule_Usage := Not_Used;
   Save_Used    : Rule_Usage;
   Rule_Context : array (True_Subrules) of Basic_Rule_Context;

   type Identifier_Data (Length : Positive) is
      record
         Name        : Wide_String (1..Length);
         Short_Last  : Positive;
         Elem        : Asis.Element;
         Other_Elem  : Asis.Element;
         Is_Callable : Boolean;
      end record;
   package Visible_Identifiers is new Framework.Scope_Manager.Scoped_Store (Identifier_Data);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags ("Parameter(s): ", Footer => "(default = strict)");
      User_Message ("Control occurrences of local identifiers that hide an outer identical name");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Framework.Language, Subrules_Flag_Utilities;
      Subrule : Subrules;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Subrule := Get_Flag_Parameter (Allow_Any => False);
            case Subrule is
               when True_Subrules =>
                  if Rule_Used (Subrule) then
                     Parameter_Error (Rule_Id, "subrule already specified");
                  end if;
                  Rule_Used    (Subrule) := True;
                  Rule_Context (Subrule) := Basic.New_Context (Rule_Use_Type, Label);
               when Both =>
                  if Rule_Used /= (True_Subrules => False) then
                     Parameter_Error (Rule_Id, "subrule already specified");
                  end if;
                  Rule_Used    := (others => True);
                  Rule_Context := (others => Basic.New_Context (Rule_Use_Type, Label));
            end case;
         end loop;
      else
         Rule_Used    (Strict) := True;
         Rule_Context (Strict) := Basic.New_Context (Rule_Use_Type, Label);
      end if;
   end Add_Use;

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
      if Rule_Used /= (True_Subrules => False) then
         Visible_Identifiers.Activate;
      end if;
   end Prepare;

   ---------------------------
   -- Process_Defining_Name --
   ---------------------------

   procedure Process_Defining_Name (Name : in Asis.Defining_Name) is
      use Asis, Asis.Elements, Asis.Declarations;
      use Thick_Queries, Framework.Scope_Manager;

      Scope : Asis.Element;

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

      function Other_Part (Elem : Asis.Defining_Name) return Asis.Element is
         -- Returns the declaration of the completion of Elem if any
         Decl : Asis.Declaration;
      begin
         Decl := Enclosing_Element (Elem);
         case Declaration_Kind (Decl) is
            when A_Private_Type_Declaration
               | A_Private_Extension_Declaration
               | An_Incomplete_Type_Declaration
                 =>
               return Corresponding_Type_Declaration (Decl);
            when A_Deferred_Constant_Declaration =>
               return Corresponding_Constant_Declaration (Elem);
            when A_Function_Declaration
               | A_Generic_Package_Declaration
               | A_Generic_Procedure_Declaration
               | A_Generic_Function_Declaration
               | A_Package_Declaration
               | A_Procedure_Declaration
               | A_Single_Task_Declaration
               | A_Task_Type_Declaration
               | A_Protected_Type_Declaration
               | A_Single_Protected_Declaration
                 =>
               return Corresponding_Body (Decl);
            when An_Entry_Declaration =>
               if Is_Task_Entry (Decl) then
                  -- Task entries have no body...
                  return Nil_Element;
               else
                  return Corresponding_Body (Decl);
               end if;
            when others =>
               return Nil_Element;
         end case;
      end Other_Part;

   begin   -- Process_Defining_Name
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Not_An_Appropriate_Name then
         return;
      end if;

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

         Short_Name    : constant Wide_String := To_Upper (Defining_Name_Image (Name));
         Full_Name     : constant Wide_String := Short_Name & Profile_Image (Name, With_Profile => False);
         Callable_Name : constant Boolean     := Is_Callable_Construct (Name);
         Is_Scope_Name : constant Boolean     := Is_Equal (Enclosing_Scope (Name), Current_Scope);
         -- Is_Scope_Name is True if Name is the defining name for the current scope
         -- => it belongs to the enclosing scope
         Already_There : Boolean;

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
               if Is_Equal (Visible_Identifiers.Current_Data.Other_Elem,
                            Enclosing_Element (Name))
               then
                  Already_There := True;
               else
                  case Hiding_Kind (Visible_Identifiers.Current_Data) is
                     when Hides =>
                        if Rule_Used (Strict) then
                           Report (Rule_Id,
                                   Rule_Context (Strict),
                                   Get_Location (Name),
                                   '"' & Framework.Language.Adjust_Image (To_Title (Full_Name))
                                   & """ hides declaration at "
                                   & Image (Get_Location (Visible_Identifiers.Current_Data.Elem)));
                        end if;
                     when Overloads =>
                        if Rule_Used (Overloading) then
                           Report (Rule_Id,
                                   Rule_Context (Overloading),
                                   Get_Location (Name),
                                   '"' & Framework.Language.Adjust_Image (To_Title (Full_Name))
                                   & """ overloads declaration at "
                                   & Image (Get_Location (Visible_Identifiers.Current_Data.Elem)));
                        end if;
                     when Not_Hiding =>
                        null;
                  end case;
               end if;
               Visible_Identifiers.Next;
            end loop;
         end if;

         if not Already_There then
            if Is_Scope_Name then
               -- This is the defining name of the program unit that defines the current scope
               -- it must be associated to the enclosing scope
               Visible_Identifiers.Push_Enclosing ((Full_Name'Length,
                                                    Full_Name,
                                                    Short_Name'Length,
                                                    Name,
                                                    Other_Part (Name),
                                                    Callable_Name));
            else
               Visible_Identifiers.Push ((Full_Name'Length,
                                          Full_Name,
                                          Short_Name'Length,
                                          Name,
                                          Other_Part (Name),
                                          Callable_Name));
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
                  Short_Name : constant Wide_String := To_Upper (Name_Image (Name));
                  Full_Name  : constant Wide_String := Short_Name & Profile_Image (Name, With_Profile => False);
               begin
                  -- Bind names to scope 0
                  Visible_Identifiers.Push_Enclosing ((Full_Name'Length, Full_Name,
                                                       Short_Name'Length, Name,
                                                       Nil_Element,
                                                       Is_Callable_Construct (Name)));
               end;
               exit when Expression_Kind (Current) = An_Identifier;
               Current := Prefix (Current);
            end loop;
         end loop;
      end;
   end Process_With_Clause;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB    => Help'Access,
                                     Add_Use_CB => Add_Use'Access,
                                     Command_CB => Command'Access,
                                     Prepare_CB => Prepare'Access);
end Rules.Local_Hiding;
