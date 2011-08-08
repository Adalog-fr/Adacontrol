----------------------------------------------------------------------
--  Rules.Unnecessary_Use - Package body                            --
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
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;

package body Rules.Unnecessary_Use is
   use Framework, Utilities;

   -- Algorithm:
   -- We use a Scoped_Store to maintain a list of used packages. For each package, we
   -- keep the Full_Name_Image of the package, and the original name (for the purpose
   -- of the message).
   --
   -- When we encounter a use clause, we check if the package is already in some scope.
   -- if yes => use clause within scope of use clause.
   --
   -- When we encounter an identifier, we take the Full_Name_Image of the package that
   -- encloses *immediately* its declaration (see below for a proof of why it works).
   -- If it matches a package in store, that package is used, and removed from the store.
   --
   -- When we exit a scope, any remaining package for that scope has not been used.
   --
   -- Note that we do *not* consider whether the package name has actually been used in
   -- the naming of the identifier. i.e., package Pack will be considered as used in the
   -- following:
   --
   -- declare
   --    use Pack;
   -- begin
   --    Pack.A := Pack.B;
   -- end;
   --
   -- This is intentional, since we consider that a use clause can be used to document
   -- that a package is "used" at a certain point.
   --
   -- Note that the rule does its best for calls of operators made visible by use clauses,
   -- but it is very tricky in some cases.

   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Rule_Type  : Rule_Types;
   Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type Package_Info (Length_1, Length_2 : Positive) is
      record
         Elem            : Asis.Element;
         Name            : Wide_String (1..Length_1);
         Original_Name   : Wide_String (1..Length_2);
      end record;

   package Used_Packages is new Framework.Scope_Manager.Scoped_Store (Package_Info);

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): none");
      User_Message ("Control use clauses for packages, where no element of the package");
      User_Message ("is referenced in the scope of the use clause.");
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
      end if;

      Rule_Type  := Rule_Use_Type;
      Rule_Label := To_Unbounded_Wide_String (Label);
      Rule_Used  := True;
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

   ------------------------
   -- Process_Use_Clause --
   ------------------------

   procedure Process_Use_Clause (Clause : in Asis.Clause) is
      use Asis.Clauses, Asis.Elements, Asis.Expressions, Thick_Queries;

      function Build_Info (Name_Elem : Asis.Element) return Package_Info is
         use Asis;
         Good_Name_Elem : Asis.Element := Name_Elem;
      begin
         -- Get rid of selected names
         case Expression_Kind (Good_Name_Elem) is
            when A_Selected_Component =>
               Good_Name_Elem := Selector (Good_Name_Elem);
            when An_Identifier =>
               null;
            when others =>
               Utilities.Failure ("Not a name in use clause", Good_Name_Elem);
         end case;

         if Declaration_Kind (Corresponding_Name_Declaration (Good_Name_Elem))
           in A_Renaming_Declaration
         then
            -- Get rid of renamings (Text_IO!)
            Good_Name_Elem := A4G_Bugs.Corresponding_Base_Entity (Corresponding_Name_Declaration (Good_Name_Elem));

            -- We can get a selected component here...
            if Expression_Kind (Good_Name_Elem) = A_Selected_Component then
               Good_Name_Elem := Selector (Good_Name_Elem);
            end if;
         end if;

         declare
            Name_String     : constant Wide_String := To_Upper (Full_Name_Image (Good_Name_Elem));
            Original_String : constant Wide_String := Extended_Name_Image (Name_Elem);
         begin
            return (Length_1      => Name_String'Length,
                    Length_2      => Original_String'Length,
                    Elem          => Clause,
                    Name          => Name_String,
                    Original_Name => Original_String);
         end;
      end Build_Info;

   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Names : constant Asis.Name_List := Clause_Names (Clause);
      begin
         --
         for I in Names'Range loop
            declare
               use Framework.Scope_Manager, Framework.Reports, Ada.Strings.Wide_Unbounded;
               Info : constant Package_Info := Build_Info (Names (I));
            begin
               -- Check if already there
               Used_Packages.Reset (All_Scopes);
               while Used_Packages.Data_Available loop
                  if Used_Packages.Current_Data.Name = Info.Name then
                     Report (Rule_Id,
                             To_Wide_String (Rule_Label),
                             Rule_Type,
                             Get_Location (Info.Elem),
                             "use clause for """ & Info.Original_Name
                             & " in scope of use clause for same package at "
                             & Image (Get_Location (Used_Packages.Current_Data.Elem)));
                  end if;

                  Used_Packages.Next;
               end loop;

               -- Add it in any case
               Used_Packages.Push (Info);
            end;
         end loop;
      end;
   end Process_Use_Clause;


   ------------------------
   -- Process_Identifier --
   ------------------------

   -- We consider only identifiers that are declared *immediately* within a
   -- package specification.
   --
   -- Proof:
   -- 1) Use clauses apply only to packages
   -- 2) An entity can be declared inside another entity in a package spec, but
   --    then it is declared:
   --    2.a) in another package: it will be found recursively
   --    2.b) as a formal of a (protected) subprogram, but use clauses do not
   --         apply to formals
   --    2.c) as an entry of a task, or as a protected operation; but use clauses don't
   --         apply to tasks or protected objects. Therefore, any use of the entry's or
   --         protected operation name will be prefixed by the task or protected object
   --         name, and this task or protected object name will be identified as a use of
   --         something declared in the package.
   --    2.d) as an element of a generic unit, but it is impossible to refer to an element
   --         inside a generic (only in instances)
   -- Q.E.D.

   procedure Process_Identifier (Name : in Asis.Name) is
      use Thick_Queries, Asis.Expressions;

      function Enclosing_Package_Name (N : in Asis.Name) return Wide_String is
         -- If N is declared immediately within a package specification, returns
         -- the Full_Name_Image of the package
         -- Otherwise, returns ""
         use Asis, Asis.Declarations, Asis.Elements, Asis.Compilation_Units;

         E : Asis.Element := Corresponding_Name_Declaration (N);
         C : Asis.Compilation_Unit;
      begin
         if Is_Nil (E) then
            -- This should be:
            -- 1) A dispatching call
            --    There is nothing we can do in this case
            -- 2) an implicitely defined operation for which the
            --    implementation does not build an artificial declaration
            --    We have no way to access the declaration, but this declaration is
            --    at the same place as the type it operates on
            --    => use the type of the parameter to determine where the operation
            --       is declared

            -- Go up to the function call, but beware that the name of the
            -- function may be composite.
            E := Enclosing_Element (N);

            while Expression_Kind (E) = A_Selected_Component loop
               E := Enclosing_Element (E);
            end loop;

            if Asis.Statements.Is_Dispatching_Call (E) then
               -- There is no way of determining the location of the "root" declaration
               -- of a dispatching call
               -- *** THIS MAY CREATE FALSE DETECTION OF UNUSED PACKAGES ***
               return "";

            elsif Expression_Kind (E) = A_Function_Call then
               declare
                  Parameters : constant Asis.Element_List := Function_Call_Parameters (E);
               begin
                  E := A4G_Bugs.Corresponding_Expression_Type (Actual_Parameter (Parameters (1)));

                  -- Annoying cases:
                  -- A string litteral will return a nil element for E
                  -- A universal value will return a declaration, however there is
                  -- not much we can do with it. We recognize universal values by the
                  -- fact that the declaration has no enclosing element. If someone knows
                  -- a better way...
                  if (Is_Nil (E) or else Is_Nil (Enclosing_Element (E)))
                    and Parameters'Length > 1
                  then
                     E := A4G_Bugs.Corresponding_Expression_Type (Actual_Parameter (Parameters (2)));
                  end if;

                  if Is_Nil (E) or else Is_Nil (Enclosing_Element (E)) then
                     -- All operands universal or equivalent => give up
                     -- (anyway, it's a language defined operator, not
                     -- subject to use clauses)
                     return "";
                  end if;

                  -- Go to the full declaration if necessary (incomplete and private)
                  if Declaration_Kind (E) in
                    An_Incomplete_Type_Declaration .. A_Private_Extension_Declaration
                  then
                     E := Corresponding_Type_Declaration (E);
                  end if;

                  E := Corresponding_First_Subtype (E);
               end;
           elsif Element_Kind (E) = An_Association
              or Declaration_Kind (E) in A_Renaming_Declaration
            then
               -- This is an actual in an instantiation, or something similar where
               -- the function name appears, but it's not a call.
               -- There is nothing we can hook on, thus ignore.
               -- *** THIS MAY CREATE FALSE DETECTION OF UNUSED PACKAGES ***
               -- (but very unlikely).
               -- If you have a solution for this case, please mail to
               -- rosen@adalog.fr
               return "";
            elsif Declaration_Kind (E) in
              A_Formal_Procedure_Declaration .. A_Formal_Package_Declaration_With_Box
            then
               -- These are not visible outside the generic, so we don't care
               return "";
            else
               Failure ("Enclosing_Package_Name, unexpected nil_element", E);
            end if;
         end if;

         if Is_Nil (Enclosing_Element (E)) then
            -- Knowing that N is a name, this happens only in the case of
            -- a compilation unit. It can be use-visible only if it is a
            -- child unit => take the parent as the enclosing unit
            C := Corresponding_Parent_Declaration (Enclosing_Compilation_Unit (E));
            if Is_Nil (C) then
               -- This happens only if the package is Standard itself. This one is
               -- certainly not use-visible...
               return "";
            end if;
            E := Unit_Declaration (C);
         else
            E := Enclosing_Element (E);
         end if;

         if Element_Kind (E) = A_Definition then
            -- This is an enumeration_literal, or an implicitely declared name
            -- for an inherited operation and thus appears as enclosed in the
            -- corresponding type definition.
            -- The type definition is enclosed in a type declaration, whose
            -- enclosing element is the scope we are interested in.
            E := Enclosing_Element (Enclosing_Element (E));
         end if;

         if Declaration_Kind (E) = A_Package_Declaration then
            return To_Upper (Full_Name_Image (Names (E)(1)));
         else
            return "";
         end if;
      end Enclosing_Package_Name;

   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         use Framework.Scope_Manager;
         Enclosing_Name : constant Wide_String := Enclosing_Package_Name (Name);
      begin
         if Enclosing_Name = "" then
            -- Not declared immediately in a package specification
            return;
         end if;

         Used_Packages.Reset (All_Scopes);
         while Used_Packages.Data_Available loop
            declare
               use Framework.Reports, Ada.Strings.Wide_Unbounded;
               Info : constant Package_Info := Used_Packages.Current_Data;
            begin
               if Enclosing_Name = Info.Name then
                  if Used_Packages.Is_Current_Transmitted_From_Spec then
                     Report (Rule_Id,
                             To_Wide_String (Rule_Label),
                             Rule_Type,
                             Get_Location (Info.Elem),
                             "use clause for " & Info.Original_Name
                               & " can be moved to body");
                  end if;
                  Used_Packages.Delete_Current;
                  exit;
              else
                  Used_Packages.Next;
               end if;
            end;
         end loop;
      end;
   end Process_Identifier;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Instantiation : Asis.Declaration) is
      -- Process all names found as actuals.
      -- We really need to do that only for those actuals that are defaulted, but there is
      -- no way to tell. Note that this implies that parameters that are not defaulted will
      -- be processed twice, once here and once when the ruler will find them.
      -- What's the heck...
      use Asis, Asis.Declarations, Asis.Expressions, Asis.Elements;

      Actual : Asis.Element;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Associations : constant Asis.Association_List := Generic_Actual_Part (Instantiation,
                                                                               Normalized => True);
      begin
         for I in Associations'Range loop
            Actual := Actual_Parameter (Associations (I));
            case Expression_Kind (Actual) is
               when An_Identifier =>
                  Process_Identifier (Actual);

               when A_Selected_Component =>
                  -- We must process all names in the selected component
                  loop
                     Process_Identifier (Selector (Actual));
                     Actual := Prefix (Actual);
                     exit when Expression_Kind (Actual) /= A_Selected_Component;
                  end loop;
                  -- The first name must still be processed
                  Process_Identifier (Actual);

               when others =>
                  null;
            end case;
         end loop;
      end;
   end Process_Instantiation;

   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Process_Scope_Exit (Scope : in Asis.Element) is
      use Framework.Reports, Framework.Scope_Manager;
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Elements, Asis.Declarations;

      Is_Package_Spec : constant Boolean := Declaration_Kind (Scope) = A_Package_Declaration;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Used_Packages.Reset (Current_Scope_Only);
      while Used_Packages.Data_Available loop
         if Is_Package_Spec and then not Is_Nil (Corresponding_Body (Scope)) then
            -- It is a package spec with a body
            -- => delay messages until the end of the body
            Used_Packages.Next;

         else
            declare
               Info : constant Package_Info := Used_Packages.Current_Data;
               Child_Warning : constant Boolean
                 := (Is_Package_Spec or Used_Packages.Is_Current_Transmitted_From_Spec)
                 and Current_Depth = 1;
            begin
               Report (Rule_Id,
                       To_Wide_String (Rule_Label),
                       Rule_Type,
                       Get_Location (Info.Elem),
                       "unused: """ & Info.Original_Name
                         & Choose (Child_Warning,
                                   """ (possible usage in child units)",
                                   """")
                      );
               Used_Packages.Delete_Current;  -- Moves to next
            end;
         end if;
      end loop;

   end Process_Scope_Exit;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access);
end Rules.Unnecessary_Use;
