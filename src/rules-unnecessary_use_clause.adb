----------------------------------------------------------------------
--  Rules.Unnecessary_Use_Clause - Package body                     --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2008.         --
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
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Fixes,
  Framework.Language,
  Framework.Queries;
pragma Elaborate (Framework.Language);

package body Rules.Unnecessary_Use_Clause is
   use Framework, Framework.Control_Manager, Utilities;

   -- Algorithm:
   -- We use a Scoped_Store to maintain a list of used packages. For each package, we
   -- keep the Full_Name_Image of the package, and the original name (for the purpose
   -- of the message).
   --
   -- When we encounter a use clause, we check if the package is already in some scope.
   -- if yes => use clause within scope of use clause.
   --
   -- When we encounter an identifier, we take the Full_Name_Image of the package that
   -- encloses *immediately* its declaration (see below in Process_Identifier for a proof
   -- of why it works).
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
   Not_Used  : constant Subrules_Set := (others => False);
   Rule_Used : Subrules_Set := Not_Used;
   Save_Used : Subrules_Set;
   Ctl_Contexts  : array (Subrules) of Basic_Rule_Context;

   type User_Kind is (Nothing, Qualified_Name, Operator, Identifier);
   type Package_Info (Name_Length : Positive) is
      record
         Use_Clause      : Asis.Element;
         Position        : Asis.List_Index;
         Name            : Wide_String (1..Name_Length);  -- Full name image of ultimate identifier
         Original_Name   : Asis.Element;
         User            : User_Kind;
      end record;
   package Used_Packages is new Scope_Manager.Scoped_Store (Package_Info);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control use clauses that can be removed, moved, or changed to use type.");
      User_Message;
      Help_On_Flags ("Parameter(s):");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities;

      Subrule : Subrules;
   begin
      if  Parameter_Exists then
         while Parameter_Exists loop
            Subrule := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Subrule) then
               Parameter_Error (Rule_Id, "subrule already specified");
            end if;
            Ctl_Contexts (Subrule) := Basic.New_Context (Ctl_Kind, Ctl_Label);
            Rule_Used (Subrule)    := True;
         end loop;
      elsif Rule_Used /= (Subrules => False) then
         Parameter_Error (Rule_Id, "rule already specified");
      else
         Ctl_Contexts := (others => Basic.New_Context (Ctl_Kind, Ctl_Label));
         Rule_Used := (others => True);
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
      if Rule_Used /= (Subrules => False) then
         Used_Packages.Activate;
      end if;
   end Prepare;

   ------------------------
   -- Process_Use_Clause --
   ------------------------

   procedure Process_Use_Clause (Clause : in Asis.Clause) is
      use Asis.Clauses;
      use Thick_Queries;

   begin  -- Process_Use_Clause
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Names : constant Asis.Name_List := Clause_Names (Clause);
      begin
         --
         for I in Names'Range loop
            declare
               use Scope_Manager, Framework.Reports;
               Name_String : constant Wide_String := To_Upper (Full_Name_Image (Ultimate_Name (Names(I))));
            begin
               -- Check if already there
               Used_Packages.Reset (All_Scopes);
               while Used_Packages.Data_Available loop
                  if Used_Packages.Current_Data.Name = Name_String then
                     if Rule_Used (Nested) then
                        Report (Rule_Id,
                                Ctl_Contexts (Nested),
                                Get_Location (Clause),
                                "use clause for " & Extended_Name_Image (Names (I))
                                & " in scope of use clause for same package at "
                                & Image (Get_Location (Used_Packages.Current_Data.Use_Clause)));
                     end if;
                  end if;

                  Used_Packages.Next;
               end loop;

               -- Add it in any case
               Used_Packages.Push ((Name_Length   => Name_String'Length,
                                    Use_Clause    => Clause,
                                    Position      => I,
                                    Name          => Name_String,
                                    Original_Name => Names (I),
                                    User          => Nothing));
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
      use Framework.Reports, Framework.Queries, Thick_Queries;

      function Is_Name_Prefixed_With (Pack_Name : Wide_String) return Boolean is
         use Asis.Expressions;

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
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         use Scope_Manager;
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
                                Ctl_Contexts (Movable),
                                Get_Location (Info.Use_Clause),
                                "use clause for "
                                & Extended_Name_Image (Info.Original_Name)
                                & " can be moved to body");
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
   begin
      if Rule_Used = Not_Used then
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
      use Framework.Reports, Scope_Manager, Thick_Queries;
      use Asis, Asis.Elements, Asis.Declarations;

      Is_Package_Spec : Boolean := False;
      Is_Package_Body : Boolean := False;
   begin
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- For a unit spec with a (non-imported) body, delay messages until the end of the body
      case Declaration_Kind (Scope) is
         when A_Package_Declaration
            | A_Generic_Package_Declaration
            =>
            declare
               The_Body : constant Asis.Declaration := Corresponding_Body (Scope);
            begin
               if not Is_Nil (The_Body) and then Element_Kind (The_Body) /= A_Pragma then
                  return;
               end if;
            end;
            Is_Package_Spec := True;
         when A_Procedure_Declaration
            | A_Null_Procedure_Declaration
            | A_Function_Declaration
            | An_Expression_Function_Declaration   -- Ada 2012
            =>
            -- Nothing to report from these, since these kind of scopes cannot include
            -- use clauses
            return;
         when A_Generic_Procedure_Declaration
            | A_Generic_Function_Declaration
            =>
            -- But there can be use clauses in the generic formal part...
            declare
               The_Body : constant Asis.Declaration := Corresponding_Body (Scope);
            begin
               if not Is_Nil (The_Body) and then Element_Kind (The_Body) /= A_Pragma then
                  return;
               end if;
            end;
         when A_Package_Body_Declaration =>
            Is_Package_Body := True;
         when others =>
            null;
      end case;

      Used_Packages.Reset (Current_Scope_Only);
      while Used_Packages.Data_Available loop
         declare
            Info          : constant Package_Info := Used_Packages.Current_Data;
            Child_Warning : constant Boolean :=  Current_Depth = 1
                                                 and (Is_Package_Spec
                                                      or (Is_Package_Body
                                                          and Used_Packages.Current_Origin = Specification));
         begin
            if Used_Packages.Current_Origin /= Parent then
               case Info.User is
                  when Nothing =>
                     if Rule_Used (Unused) then
                        Report (Rule_Id,
                                Ctl_Contexts (Unused),
                                Get_Location (Info.Use_Clause),
                                "unused use clause for " & Extended_Name_Image (Info.Original_Name)
                                & Choose (Child_Warning,
                                          " (possible usage in child units)",
                                          ""));
                        Fixes.List_Remove (Info.Position, From => Info.Use_Clause);
                     end if;
                  when Qualified_Name =>
                     if Rule_Used (Qualified) then
                        Report (Rule_Id,
                                Ctl_Contexts (Qualified),
                                Get_Location (Info.Use_Clause),
                                "all uses of " & Extended_Name_Image (Info.Original_Name) & " are qualified"
                                 & Choose (Child_Warning,
                                           " (possible usage in child units)",
                                           ""));
                        Fixes.List_Remove (Info.Position, From => Info.Use_Clause);
                     end if;
                  when Operator =>
                     if Rule_Used (Operator) then
                        Report (Rule_Id,
                                Ctl_Contexts (Operator),
                                Get_Location (Info.Use_Clause),
                                "use clause for "
                                & Extended_Name_Image (Info.Original_Name)
                                & " only used for operators"
                                & Choose (Child_Warning,
                                          " (possible usage in child units)",
                                          ""));
                     end if;
                  when Identifier =>
                     null;
               end case;
            end if;
         end;

         Used_Packages.Next;
      end loop;

   end Process_Scope_Exit;

begin  -- Rules.Unnecessary_Use_Clause
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Unnecessary_Use_Clause;
