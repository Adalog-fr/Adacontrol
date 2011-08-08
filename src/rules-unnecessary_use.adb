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
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

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
  Framework.Scope_Manager,
  Framework.Queries;
pragma Elaborate (Framework.Language);

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
   -- If it matches a package in store, that package is used, and marked depending on
   -- the kind of usage (from operator, from qualified name, or plain identifier).
   --
   -- When we exit a scope, any remaining package for that scope which is still marked
   -- as "Nothing" has not been used, otherwise the mark tells the kind of usage that
   -- governs the message.
   --
   -- Note that the rule does its best for calls of operators made visible by use clauses,
   -- but it is very tricky in some cases.

   type Subrules is (Unused, Qualified, Operator, Nested, Movable);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Subrules_Set is array (Subrules) of Boolean;
   Rule_Used : Subrules_Set := (others => False);
   Save_Used : Subrules_Set;
   Contexts  : array (Subrules) of Basic_Rule_Context;

   type User_Kind is (Nothing, Qualified_Name, Operator, Identifier);
   type Package_Info (Length_1, Length_2 : Positive) is
      record
         Elem            : Asis.Element;
         Name            : Wide_String (1..Length_1);
         Original_Name   : Wide_String (1..Length_2);
         User            : User_Kind;
      end record;

   package Used_Packages is new Framework.Scope_Manager.Scoped_Store (Package_Info);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags ("Parameter(s):");
      User_Message ("Control use clauses that can be removed, moved, or changed to use type.");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Framework.Language, Subrules_Flag_Utilities;

      S : Subrules;
   begin
      if  Parameter_Exists then
         while Parameter_Exists loop
            S := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (S) then
               Parameter_Error (Rule_Id, "subrule already specified");
            end if;
            Contexts (S)   := Basic.New_Context (Rule_Type, Label);
            Rule_Used (S) := True;
         end loop;
      elsif Rule_Used /= (Subrules => False) then
         Parameter_Error (Rule_Id, "rule already specified");
      else
         Contexts   := (others => Basic.New_Context (Rule_Type, Label));
         Rule_Used := (others => True);
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
            Rule_Used  := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      if Rule_Used /= (Subrules => False) then
         Used_Packages.Activate;
      end if;
   end Prepare;

   ------------------------
   -- Process_Use_Clause --
   ------------------------

   procedure Process_Use_Clause (Clause : in Asis.Clause) is
      use Asis.Clauses;

      function Build_Info (Name_Elem : Asis.Element) return Package_Info is
         use Thick_Queries;
         Name_String     : constant Wide_String := To_Upper (Full_Name_Image (Ultimate_Name (Name_Elem)));
         Original_String : constant Wide_String := Extended_Name_Image (Name_Elem);
      begin
         return (Length_1      => Name_String'Length,
                 Length_2      => Original_String'Length,
                 Elem          => Clause,
                 Name          => Name_String,
                 Original_Name => Original_String,
                 User          => Nothing);
      end Build_Info;

   begin
      if Rule_Used = (Subrules => False) then
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
                     if Rule_Used (Nested) then
                        Report (Rule_Id,
                                Contexts (Nested),
                                Get_Location (Info.Elem),
                                "use clause for """ & Info.Original_Name
                                & """ in scope of use clause for same package at "
                                & Image (Get_Location (Used_Packages.Current_Data.Elem)));
                     end if;
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
      use Asis, Asis.Elements;
      use Framework.Reports, Framework.Queries;

      function Is_Name_Prefixed_With (Pack_Name : Wide_String) return Boolean is
         use Asis.Expressions;
         use Thick_Queries;

         Name_Enclosing : constant Asis.Element := Enclosing_Element (Name);
      begin
         if Expression_Kind (Name_Enclosing) /= A_Selected_Component then
            return False;
         end if;
         if not Is_Equal (Name, Selector (Name_Enclosing)) then
            return False;
         end if;
         case Expression_Kind (Prefix (Name_Enclosing)) is
            when An_Identifier | A_Selected_Component =>
               return To_Upper (Full_Name_Image (Prefix (Name_Enclosing))) = Pack_Name;
            when others =>
               -- A_Function_Call, An_Indexed_Component...
               return False;
         end case;
      end Is_Name_Prefixed_With;

   begin  -- Process_Identifier
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         use Framework.Scope_Manager;
         Enclosing_Name : constant Wide_String := Enclosing_Package_Name (Rule_Id, Name);
      begin
         if Enclosing_Name = "" then
            -- Not declared immediately in a package specification
            return;
         end if;

         Used_Packages.Reset (All_Scopes);
         while Used_Packages.Data_Available loop
            declare
               Info : Package_Info := Used_Packages.Current_Data;
            begin
               if Enclosing_Name = Info.Name then
                  if Info.User = Nothing and Used_Packages.Current_Origin = Specification then
                     if Rule_Used (Movable) then
                        Report (Rule_Id,
                                Contexts (Movable),
                                Get_Location (Info.Elem),
                                "use clause can be moved to body: " & Info.Original_Name);
                     end if;
                  end if;
                  if Is_Name_Prefixed_With (Info.Name) then
                     Info.User := User_Kind'Max (Qualified_Name, Info.User);
                  elsif Expression_Kind (Name) = An_Operator_Symbol then
                     Info.User := User_Kind'Max (Operator, Info.User);
                  else
                     Info.User := Identifier;
                  end if;
                  Used_Packages.Update_Current (Info);
                  exit;
               end if;

               Used_Packages.Next;
            end;
         end loop;
      end;
   end Process_Identifier;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Instantiation : Asis.Declaration) is
      -- Names used in actuals that are explicitely provided are processed as regular identifiers.
      -- Names that are part of default expressions follow the visibility rules at the place of
      -- the declaration of the generic, and therefore should not be processed here.
      -- However, names that correspond to defaulted associations for "is box" formal subprograms
      -- follow the visibility rules at the point of instantiation, and must therefore be processed
      -- here.
      use Asis, Asis.Declarations, Asis.Expressions, Asis.Elements;
      use Framework.Reports;
   begin
      if Rule_Used = (Subrules => False) then
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
               if Is_Nil (Actual_Parameter (Associations (I))) then
                  A4G_Bugs.Trace_Bug ("Unnecessary_Use.Process_Instantiation: Nil box-defaulted parameter");
                  Uncheckable (Rule_Id,
                               False_Positive,
                               Get_Location (Instantiation),
                               "Default for parameter not considered");
               else
                  Process_Identifier (Actual_Parameter (Associations (I)));
               end if;
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
      if Rule_Used = (Subrules => False) then
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
               if Used_Packages.Current_Origin /= Parent then
                  case Info.User is
                     when Nothing =>
                        if Rule_Used (Unused) then
                           Report (Rule_Id,
                                   Contexts (Unused),
                                   Get_Location (Info.Elem),
                                   "unused: """ & Info.Original_Name
                                   & Choose (Child_Warning,
                                             """ (possible usage in child units)",
                                             """"));
                        end if;
                     when Qualified_Name =>
                        if Rule_Used (Qualified) then
                           Report (Rule_Id,
                                   Contexts (Qualified),
                                   Get_Location (Info.Elem),
                                   "all uses qualified: """ & Info.Original_Name
                                   & Choose (Child_Warning,
                                             """ (possible usage in child units)",
                                             """"));
                        end if;
                     when Operator =>
                        if Rule_Used (Operator) then
                           Report (Rule_Id,
                                   Contexts (Operator),
                                   Get_Location (Info.Elem),
                                   "only used for operators: """ & Info.Original_Name
                                   & Choose (Child_Warning,
                                             """ (possible usage in child units)",
                                             """"));
                        end if;
                     when Identifier =>
                        null;
                  end case;
               end if;
            end;
         end if;

         Used_Packages.Next;
      end loop;

   end Process_Scope_Exit;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB    => Help'Access,
                                     Add_Use_CB => Add_Use'Access,
                                     Command_CB => Command'Access,
                                     Prepare_CB => Prepare'Access);
end Rules.Unnecessary_Use;
