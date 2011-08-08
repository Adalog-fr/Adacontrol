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
   -- If it matches a package in store, that package is used, and marked as such.
   --
   -- When we exit a scope, any remaining package for that scope which is not marked
   -- has not been used.
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
   Context    : Basic_Rule_Context;

   type Package_Info (Length_1, Length_2 : Positive) is
      record
         Elem            : Asis.Element;
         Name            : Wide_String (1..Length_1);
         Original_Name   : Wide_String (1..Length_2);
         Used            : Boolean;
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

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Framework.Language;

   begin
      if  Parameter_Exists then
         Parameter_Error ("No parameter for rule " & Rule_Id);
      end if;

      if Rule_Used then
         Parameter_Error (Rule_Id & ": this rule can be specified only once");
      end if;

      Context   := Basic.New_Context (Rule_Type, Label);
      Rule_Used := True;
   end Add_Use;


   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := False;
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
      use Asis.Clauses, Thick_Queries;

      function Build_Info (Name_Elem : Asis.Element) return Package_Info is
         Name_String     : constant Wide_String := To_Upper (Full_Name_Image (Ultimate_Name (Name_Elem)));
         Original_String : constant Wide_String := Extended_Name_Image (Name_Elem);
      begin
         return (Length_1      => Name_String'Length,
                 Length_2      => Original_String'Length,
                 Elem          => Clause,
                 Name          => Name_String,
                 Original_Name => Original_String,
                 Used          => False);
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
               use Framework.Scope_Manager, Framework.Reports;
               Info : constant Package_Info := Build_Info (Names (I));
            begin
               -- Check if already there
               Used_Packages.Reset (All_Scopes);
               while Used_Packages.Data_Available loop
                  if Used_Packages.Current_Data.Name = Info.Name then
                     Report (Rule_Id,
                             Context,
                             Get_Location (Info.Elem),
                             "use clause for """ & Info.Original_Name
                             & """ in scope of use clause for same package at "
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
             or Declaration_Kind (E) in A_Generic_Instantiation
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
               Failure ("Enclosing_Package_Name, unexpected nil_element", N);
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
               use Framework.Reports;
               Info : Package_Info := Used_Packages.Current_Data;
            begin
               if Enclosing_Name = Info.Name then
                  if not Info.Used and Used_Packages.Current_Origin = Specification then
                     Report (Rule_Id,
                             Context,
                             Get_Location (Info.Elem),
                             "use clause for " & Info.Original_Name
                               & " can be moved to body");
                  end if;
                  Info.Used := True;
                  Used_Packages.Update_Current (Info);
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
      -- Names used in actuals that are explicitely provided are processed by the ruler as identifiers.
      -- Names that are part of default expressions follow the visibility rules at the place of
      -- the declaration of the generic, and therefore should not be processed here.
      -- However, names that correspond to defaulted associations for "is box" formal subprograms
      -- follow the visibility rules at the point of instantiation, and must therefore be processed
      -- here.
      use Asis, Asis.Declarations, Asis.Expressions, Asis.Elements;

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
            if Is_Defaulted_Association (Associations (I))
              and then Default_Kind (Enclosing_Element
                                     (Formal_Parameter
                                      (Associations (I)))) = A_Box_Default
            then
               Process_Identifier (Actual_Parameter (Associations (I)));
            end if;
         end loop;
      end;
   end Process_Instantiation;

   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Process_Scope_Exit (Scope : in Asis.Element) is
      use Framework.Reports, Framework.Scope_Manager;
      use Asis, Asis.Elements, Asis.Declarations;

      Is_Package_Spec : constant Boolean := Declaration_Kind (Scope) = A_Package_Declaration or
                                            Declaration_Kind (Scope) = A_Generic_Package_Declaration;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Used_Packages.Reset (Current_Scope_Only);
      while Used_Packages.Data_Available loop
         -- For a package spec with a body, delay messages until the end of the body
         if not Is_Package_Spec or else Is_Nil (Corresponding_Body (Scope)) then
            declare
               Info : constant Package_Info := Used_Packages.Current_Data;
               Child_Warning : constant Boolean
                 := (Is_Package_Spec or Used_Packages.Current_Origin = Specification)
                 and Current_Depth = 1;
            begin
               if not Info.Used and Used_Packages.Current_Origin /= Parent then
                  Report (Rule_Id,
                          Context,
                          Get_Location (Info.Elem),
                          "unused: """ & Info.Original_Name
                          & Choose (Child_Warning,
                                    """ (possible usage in child units)",
                                    """")
                         );
               end if;
            end;
         end if;

         Used_Packages.Next;
      end loop;

   end Process_Scope_Exit;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.Unnecessary_Use;
