----------------------------------------------------------------------
--  Rules.Local_Hiding - Package body                               --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
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

-- Asis
with
  Asis.Clauses,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Scope_Manager,
  String_Matching,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Pattern_Queues,
  Framework.Pattern_Queues_Matchers,
  Framework.Variables,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);

package body Rules.Local_Hiding is
   use Framework, Framework.Control_Manager, Utilities, Framework.Variables.Shared_Types;

   -- Algorithm:
   -- This rule is quite easy, since most of the work is done by Scoped_Store
   -- Each time a defining identifier is encountered, all scopes are searched for the same identifier
   -- The name used is an overloaded name for overloaded entities, therefore overloaded entities with
   -- different profiles are *not* considered hiding.
   -- The defining identifier is then added to the current scope.
   -- The only difficulty is that when we find the defining name of -say- a procedure, the current scope
   -- is already the procedure itself, but the name must be attached to where the procedure is declared,
   -- i.e. the enclosing scope.

   type Subrules is (Strict, Overloading);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Modifiers is (Not_Operator, Not_Enumeration, Not_Identical_Renaming, Not_Different_Families);
   package Modifiers_Flag_Utilities is new Framework.Language.Modifier_Utilities (Modifiers);

   type Declaration_Families is (Data,         -- Constant, Variable, Named number, Parameter, Component...
                                 Types,        -- Including task and protected types
                                 Subprograms,  -- Procedure, Function, Entry (and instantiations thereof)
                                 Packages,     -- including formal packages, instantiations
                                 Generics,
                                 Exceptions,
                                 Labels        -- True labels, block and loop names
                                );

   type Rule_Usage is array (Subrules) of Boolean;
   Not_Used : constant Rule_Usage := (others => False);

   Rule_Used    : Rule_Usage := Not_Used;
   Save_Used    : Rule_Usage;
   Rule_Context : array (Subrules) of Basic_Rule_Context;

   Include_Op         : Rule_Usage;
   Include_Enum       : Rule_Usage;
   Ignore_Families    : Boolean;     -- Simple boolean since not allowed for Overloading
   Exclude_Renaming   : Boolean;     -- Simple boolean since not allowed for Overloading
   Allowed_Patterns   : array (Subrules) of Framework.Pattern_Queues.Queue;

   type Identifier_Data (Length : Positive) is
      record
         Name           : Wide_String (1..Length);
         Short_Last     : Positive;
         Elem           : Asis.Element;
         Is_Callable    : Boolean;
         Is_Enumeration : Boolean;
         Family         : Declaration_Families;
      end record;
   package Visible_Identifiers is new Scope_Manager.Scoped_Store (Identifier_Data);

   -- Rule variables
   Overloading_Report : aliased Verbosity_Type.Object := (Value => Detailed);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Subrules_Flag_Utilities, Modifiers_Flag_Utilities, Framework.Variables;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control occurrences of local identifiers that hide or overload an identical name");
      User_Message;
      Help_On_Flags ("Parameter(1): <exceptions> ", Footer => "(default = strict)");
      User_Message  ("Parameter(2..): ""<Allowed Pattern>""");
      Help_On_Modifiers ("<exceptions>:");
      User_Message;
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Overloading_Report");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities, Modifiers_Flag_Utilities;
      Subrule         : Subrules;
      Modif_Specified : Modifier_Set;
   begin
      if Parameter_Exists then
         Modif_Specified := Get_Modifier_Set;
         Subrule := Get_Flag_Parameter (Allow_Any => False);
         if Rule_Used (Subrule) then
            Parameter_Error (Rule_Id, "subrule already specified");
         end if;
         Rule_Used    (Subrule) := True;
         Rule_Context (Subrule) := Basic.New_Context (Ctl_Kind, Ctl_Label);
         Include_Op   (Subrule) := not Modif_Specified (Not_Operator);
         Include_Enum (Subrule) := not Modif_Specified (Not_Enumeration);
         case Subrule is
            when Overloading =>
               if Modif_Specified (Not_Identical_Renaming) then
                  Parameter_Error (Rule_Id, "Not_Identical_Renaming cannot be specified with Overloading");
               end if;
               if Modif_Specified (Not_Different_Families) then
                  Parameter_Error (Rule_Id, "Not_Different_Families cannot be specified with Overloading");
               end if;
            when Strict =>
               Exclude_Renaming :=     Modif_Specified (Not_Identical_Renaming);
               Ignore_Families  := not Modif_Specified (Not_Different_Families);
         end case;

         while Parameter_Exists loop
            if Is_String_Parameter then
               declare
                  use Framework.Pattern_Queues, String_Matching;
                  Pat : constant Wide_String := Get_String_Parameter;
               begin
                  Append (Allowed_Patterns (Subrule), Compile (Pat, Ignore_Case => True));
               exception
                  when Pattern_Error =>
                     Parameter_Error (Rule_Id, "Incorrect pattern: " & Pat);
               end;
            else
               Parameter_Error (Rule_Id, "Pattern string expected");
            end if;
         end loop;
      else
         Rule_Used    (Strict) := True;
         Rule_Context (Strict) := Basic.New_Context (Ctl_Kind, Ctl_Label);
         Include_Op   (Strict) := True;
         Include_Enum (Strict) := True;
         Exclude_Renaming      := False;
         Ignore_Families       := True;
      end if;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager, Framework.Pattern_Queues;
   begin
      case Action is
         when Clear =>
            Rule_Used  := Not_Used;
            for Q : Framework.Pattern_Queues.Queue of Allowed_Patterns loop
               Clear (Q);
            end loop;
            Overloading_Report := (Value => Detailed);
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

   ------------------------
   -- Declaration_Family --
   ------------------------

   function Declaration_Family (Name : Asis.Element) return Declaration_Families is
   -- Expected elements:
   -- - A_Name
   -- - A_Defining_Name
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
      Decl : Asis.Declaration := Name;
   begin
      loop  -- Loop to get rid of defining expanded names
         case Element_Kind (Decl) is
            when An_Expression =>
               Decl := Corresponding_Name_Declaration (Simple_Name (Decl));
            when A_Defining_Name =>
               Decl := Enclosing_Element (Decl);
            when A_Declaration =>
               exit;
            when A_Statement =>
               -- This happens only for block and loop names or labels
               return Labels;
            when others =>
               Failure ("Declaration_Family: not a (defining) name", Decl);
         end case;
      end loop;

      case Declaration_Kind (Decl) is
         when An_Object_Declaration
            | A_Number_Declaration
            | An_Enumeration_Literal_Specification
            | A_Parameter_Specification
            | A_Component_Declaration
            | A_Discriminant_Specification
            | A_Loop_Parameter_Specification
            | A_Generalized_Iterator_Specification
            | An_Element_Iterator_Specification
            | An_Object_Renaming_Declaration
            | A_Return_Variable_Specification
            | A_Return_Constant_Specification
            | An_Entry_Index_Specification
            | A_Choice_Parameter_Specification
            | A_Formal_Object_Declaration
            =>
            return Data;

         when A_Type_Declaration
            | A_Subtype_Declaration
            | A_Formal_Type_Declaration
            | A_Formal_Incomplete_Type_Declaration
            =>
            return Types;

         when A_Procedure_Declaration
            | A_Null_Procedure_Declaration
            | A_Function_Declaration
            | An_Expression_Function_Declaration
            | An_Entry_Declaration
            | A_Procedure_Body_Declaration
            | A_Function_Body_Declaration
            | An_Entry_Body_Declaration
            | A_Procedure_Instantiation
            | A_Function_Instantiation
            | A_Procedure_Body_Stub
            | A_Function_Body_Stub
            | A_Formal_Procedure_Declaration
            | A_Formal_Function_Declaration
            | A_Procedure_Renaming_Declaration
            | A_Function_Renaming_Declaration
            =>
            return Subprograms;

         when A_Package_Declaration
            | A_Package_Body_Declaration
            | A_Package_Instantiation
            | A_Package_Renaming_Declaration
            | A_Package_Body_Stub
            | A_Formal_Package_Declaration
            | A_Formal_Package_Declaration_With_Box
            =>
            return Packages;

         when A_Generic_Declaration
            | A_Generic_Package_Renaming_Declaration
            | A_Generic_Procedure_Renaming_Declaration
            | A_Generic_Function_Renaming_Declaration
            =>
            return Generics;

         when An_Exception_Declaration
            | An_Exception_Renaming_Declaration
            =>
            return Exceptions;

         when A_Task_Body_Declaration
            | A_Protected_Body_Declaration
            | A_Task_Body_Stub
            | A_Protected_Body_Stub
            =>
            return Declaration_Family (Corresponding_Declaration (Decl));
         when Not_A_Declaration =>
            Failure ("Declaration_Family: Not_A_Declaration", Decl);
      end case;
   end Declaration_Family;

   ---------------------------
   -- Process_Defining_Name --
   ---------------------------

   procedure Process_Defining_Name (Name : in Asis.Defining_Name) is
      use Asis, Asis.Elements, Asis.Declarations;
      use Thick_Queries, Scope_Manager;

      Scope      : Asis.Element;
      First_Name : Asis.Defining_Name;

      function Not_An_Appropriate_Name return Boolean is
      -- This function detects names that are not to be processed by this rule, since
      -- they do not hide anything nor can they be hidden:
      --   - Names of record components (but not protected components), including discriminants
      --   - Names of formal parameters in a SP renaming declaration
      --   - Names of formal parameters in the declaration of an access to SP type
      -- or because they are identical renamings with the appropriate option
         use Asis.Expressions;

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
            when A_Renaming_Declaration =>
               if Exclude_Renaming then
                  declare
                     Renamed : constant Asis.Expression := Simple_Name (Renamed_Entity (Decl));
                  begin
                     return Expression_Kind (Renamed) = An_Identifier -- Not an indexed component...
                       and then To_Upper (Defining_Name_Image (Name)) = To_Upper (Name_Image (Renamed));
                  end;
               else
                  return False;
               end if;
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
         use Framework.Locations, Framework.Reports;
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

         use Framework.Pattern_Queues_Matchers;
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
                          and then (Include_Op   (Strict) or Short_Name (1) /= '"')
                          and then (Include_Enum (Strict)
                                    or not Is_Enumeration or not Visible_Identifiers.Current_Data.Is_Enumeration)
                          and then (Ignore_Families
                                    or Declaration_Family (First_Name) =  Visible_Identifiers.Current_Data.Family)
                          and then not Match_Any (Defining_Name_Image (First_Name), Allowed_Patterns (Strict))
                        then
                           Report (Rule_Id,
                                   Rule_Context (Strict),
                                   Get_Location (Name),
                                   '"' & Adjust_Image (To_Title (Full_Name))
                                   & """ hides declaration at "
                                   & Image (Get_Location (Visible_Identifiers.Current_Data.Elem)));
                        end if;
                     when Overloads =>
                        if Rule_Used (Overloading)
                          and then (Include_Op   (Overloading) or Short_Name (1) /= '"')
                          and then (Include_Enum (Overloading)
                                    or not Is_Enumeration or not Visible_Identifiers.Current_Data.Is_Enumeration)
                          and then not Match_Any (Defining_Name_Image (First_Name), Allowed_Patterns (Overloading))
                        then
                           case Overloading_Report.Value is
                              when Compact =>
                                 Overload_Count := Overload_Count + 1;
                                 Overload_Last  := Visible_Identifiers.Current_Data.Elem;
                              when Detailed =>
                                 Report (Rule_Id,
                                         Rule_Context (Overloading),
                                         Get_Location (Name),
                                         '"' & Adjust_Image (To_Title (Full_Name))
                                         & """ overloads declaration at "
                                         & Image (Get_Location (Visible_Identifiers.Current_Data.Elem)));
                           end case;
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
                       '"' & Adjust_Image (To_Title (Full_Name))
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
                                                    Is_Enumeration => Is_Enumeration,
                                                    Family         => Declaration_Family (First_Name)));
            else
               Visible_Identifiers.Push ((Full_Name'Length,
                                          Full_Name,
                                          Short_Name'Length,
                                          First_Name,
                                          Is_Callable    => Callable_Name,
                                          Is_Enumeration => Is_Enumeration,
                                         Family          => Declaration_Family (First_Name)));
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
         Name      : Asis.Expression;
         Current   : Asis.Expression;
      begin
         for N : Asis.Name of Clause_Names (With_Clause) loop
            Current := N;
            loop
               case Expression_Kind (Current) is
                  when An_Identifier =>
                     Name := Current;
                  when A_Selected_Component =>
                     Name := Selector (Current);
                  when others =>
                     Failure ("Unexpected name in with clause", N);
               end case;
               declare
                  Short_Name : constant Wide_String := To_Upper (Name_Image (Name));
                  Full_Name  : constant Wide_String := Short_Name & Profile_Image (Name, With_Profile => False);
               begin
                  -- Bind names to scope 0
                  Visible_Identifiers.Push_Enclosing ((Full_Name'Length,
                                                       Full_Name,
                                                       Short_Name'Length,
                                                       Name,
                                                       Is_Callable    => Is_Callable_Construct (Name),
                                                       Is_Enumeration => False,
                                                      Family         => Declaration_Family (Name)));
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
   Framework.Variables.Register (Overloading_Report'Access,
                                 Variable_Name => Rule_Id & ".OVERLOADING_REPORT");
end Rules.Local_Hiding;
