----------------------------------------------------------------------
--  Rules.Unnecessary_Use_Clause - Package body                     --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2008.           --
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

with    -- Ada
  Ada.Strings.Wide_Unbounded;

with   -- Asis
  Asis.Clauses,
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;


with   -- Adalog
  A4G_Bugs,
  Elements_Set,
  Scope_Manager,
  Thick_Queries,
  Utilities;


with   -- AdaControl
  Framework.Reports.Fixes,
  Framework.Language,
  Framework.Queries,
  Framework.Variables,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);

package body Rules.Unnecessary_Use_Clause is
   use Framework, Framework.Control_Manager, Framework.Variables.Shared_Types, Utilities, Elements_Set;

   -- Algorithm:
   -- We use a Scoped_Store to maintain a list of active use clauses. In case of multiple elements
   -- in the clause, there is one entry for each element.
   --
   -- When we encounter a use clause, we check if the clause is already in the scope of
   -- some other clause for the same package (including f.e. a use type clause in the scope
   -- of a use clause for the package where the type is declared).
   --
   -- When we encounter an identifier, we consider only those declared *immediately* within a
   -- package declaration (see below in Process_Identifier for a proof of why it works).
   -- We check all enclosing use clauses:
   --   - for a use package: if it matches the package in store, that package is used, and marked
   --     depending on the kind of usage (from operator, from primitive operation, from qualified name,
   --     or plain identifier).
   --   - for a use [all] type: we do the same, considering the package where the type is declared
   -- In addition, if the clause is marked "nothing" and its Current_Origin is Specification
   -- (which implies we are now in the corresponding body), the clause is marked as movable to the
   -- body. The message will be printed at scope exit, in order to not print it if the clause is unused.
   --
   -- When we exit a scope, any remaining package for that scope which is still marked
   -- as "Nothing" has not been used, otherwise the mark tells the kind of usage that
   -- governs the message.


   type Subrules is (Unused, Qualified, Operator, Primitive, Nested, Movable);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Subrules_Set is array (Subrules) of Boolean;
   Not_Used  : constant Subrules_Set := (others => False);
   Rule_Used : Subrules_Set := Not_Used;
   Save_Used : Subrules_Set;
   Ctl_Contexts  : array (Subrules) of Basic_Rule_Context;

                                       -- Elements used within the scope of a use [[all] type] clause:
   type User_Kind is (Nothing,         -- No element from package or no primitive of type (clause is useless)
                      Qualified_Name,  -- Only elements that use a fully qualified notation (clause is useless)
                      Operator,        -- Only primitive operators, use package and use all type changeable to use type
                      Primitive,       -- Only primitive operations, use package changeable to use all type
                      Identifier);     -- Other elements);
   type Used_Elem_Info (Name_Length : Positive; Spec_Length : Natural) is
      record
         Use_Clause     : Asis.Element;                       -- The original use clause
         Elem           : Asis.Element;                       -- The package or type name inside the use clause
         Position       : Asis.List_Index;                    -- Index of Elem inside the use clause
         Name           : Wide_String (1 .. Name_Length);     -- Full name image of ultimate identifier
         Spec_Name      : Wide_String (1 .. Spec_Length);     -- Full_Name_Image of package where the type is defined
                                                              --    "" if not defined in a package spec, or use package
         Op_Type_List   : Set := Empty_Set;                   -- List of all types used in the package
         Prim_Type_List : Set := Empty_Set;                   -- List of all types used in the package
         User           : User_Kind;                          -- How the package/type is used
         Spec_Unused    : Boolean := False;                   -- True after entering a body if User=Nothing from spec
      end record;

   -- Spec name is the name of the package specification where the type Elem is declared.
   -- It is empty if Elem is not a type (use package clause) or if the type is not declared
   -- immediately within a package spec (use type clause).
   package Used_Elements is new Scope_Manager.Scoped_Store (Used_Elem_Info);

   -- Useful subtype for case statements dealing only with use clauses:
   subtype Use_Clause_Kinds is Asis.Clause_Kinds range Asis.A_Use_Package_Clause .. Asis.A_Use_All_Type_Clause;

   Max_Replacements : aliased Natural_Type.Object := (Value => Natural'Last);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control use clauses that can be removed, moved, or changed to use [all] type.");
      User_Message;
      Help_On_Flags ("Parameter(s):");
      User_Message ("Variables:");
      Framework.Variables.Help_On_Variable (Rule_Id & ".Max_Replacements");
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
               if not Basic.Merge_Context (Ctl_Contexts (Subrule), Ctl_Kind, Ctl_Label) then
                  Parameter_Error (Rule_Id, "subrule already specified");
               end if;
            else
               Ctl_Contexts (Subrule) := Basic.New_Context (Ctl_Kind, Ctl_Label);
               Rule_Used (Subrule)    := True;
            end if;
         end loop;
      elsif Rule_Used /= (Subrules => False) then
         Parameter_Error (Rule_Id, "rule already specified");
      else
         Ctl_Contexts := (others => Basic.New_Context (Ctl_Kind, Ctl_Label));
         Rule_Used    := (others => True);
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
            Rule_Used        := Not_Used;
            Max_Replacements := (Value => Natural'Last);
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
         Used_Elements.Activate;
      end if;
   end Prepare;

   ---------------------
   -- Clause_And_Name --
   ---------------------

   function Clause_And_Name (Info : Used_Elem_Info) return Wide_String is
      use Asis, Asis.Elements;
      use Thick_Queries;
   begin
      case Use_Clause_Kinds (Clause_Kind (Info.Use_Clause)) is
         when A_Use_Package_Clause =>
            return """use"" clause for " & Extended_Name_Image (Info.Elem);
         when A_Use_Type_Clause =>
            return """use type"" clause for " & Extended_Name_Image (Info.Elem);
         when A_Use_All_Type_Clause =>
            return """use all type"" clause for " & Extended_Name_Image (Info.Elem);
      end case;
   end Clause_And_Name;

   -------------------------
   -- Is_Use_Type_Visible --
   -------------------------

   function Is_Use_Type_Visible (The_Type : Asis.Element; The_Callable : Asis.Element) return Boolean is
   -- Use [all] type clauses make a little bit more than primitive operations visible, see 8.4(8)
      use Asis, Asis.Elements, Asis.Expressions;
      use Thick_Queries;
      The_Call : Asis.Element := Enclosing_Element (The_Callable);
   begin
      while Expression_Kind (The_Call) in A_Selected_Component | An_Indexed_Component | An_Explicit_Dereference loop
         The_Call := Enclosing_Element (The_Call);
      end loop;

      -- If the name is the name of the called entity in a call that uses O.M notation, it is not use-type visible
      if (Expression_Kind (The_Call) = A_Function_Call
          or else Statement_Kind (The_Call) in A_Procedure_Call_Statement | An_Entry_Call_Statement)
        and then Is_Equal (Called_Simple_Name (The_Call), The_Callable)
      then
         if Is_Prefix_Notation (The_Call) then
            return False;
         end if;
      end if;

      if Attribute_Kind (Simple_Name (The_Type)) = A_Class_Attribute
        and then Is_Primitive_Of (Prefix (The_Type), The_Callable)
      then
         return True;
      elsif Is_Primitive_Of (The_Type, The_Callable) then
         return True;
      end if;

      return False;
   end Is_Use_Type_Visible;

   ------------------------
   -- Process_Use_Clause --
   ------------------------

   procedure Process_Use_Clause (Clause : in Asis.Clause) is
      use Asis, Asis.Clauses, Asis.Compilation_Units, Asis.Elements;
      use Thick_Queries;

      function Build_Info (The_Names : Asis.Element_List; Inx : Asis.List_Index) return Used_Elem_Info is
         Name_Elem : Asis.Element renames The_Names (Inx);
      begin
         case Use_Clause_Kinds (Clause_Kind (Clause)) is
            when A_Use_Package_Clause =>
               declare
                  Name_String : constant Wide_String := Full_Name_Image (Ultimate_Name (Name_Elem),
                                                                         With_Profile => True);
               begin
                  return (Name_Length    => Name_String'Length,
                          Spec_Length    => 0,
                          Use_Clause     => Clause,
                          Elem           => Name_Elem,
                          Position       => Inx,
                          Name           => Name_String,
                          Spec_Name      => "",
                          Op_Type_List   => Empty_Set,
                          Prim_Type_List => Empty_Set,
                          User           => Nothing,
                          Spec_Unused    => False);
               end;
            when A_Use_Type_Clause | A_Use_All_Type_Clause =>
               declare
                  use Asis.Expressions;
                  Type_Unit : constant Asis.Defining_Name := Enclosing_Program_Unit
                                                              (A4G_Bugs.Corresponding_First_Subtype
                                                               (Corresponding_Name_Declaration
                                                                (Simple_Name
                                                                 (Strip_Attributes (Name_Elem)))));

                  Name_String : constant Wide_String := Full_Name_Image (Ultimate_Name (Name_Elem),
                                                                         With_Profile => True);
                  Spec_String : constant Wide_String := Full_Name_Image (Ultimate_Name (Type_Unit),
                                                                         With_Profile => True);
               begin
                  if Declaration_Kind (Enclosing_Element (Type_Unit)) = A_Package_Declaration then
                     return (Name_Length    => Name_String'Length,
                             Spec_Length    => Spec_String'Length,
                             Use_Clause     => Clause,
                             Elem           => Name_Elem,
                             Position       => Inx,
                             Name           => Name_String,
                             Spec_Name      => Spec_String,
                             Op_Type_List   => Empty_Set,
                             Prim_Type_List => Empty_Set,
                             User           => Nothing,
                             Spec_Unused    => False);
                  else  -- We are necessarily in the scope of the type => no need for use [all] type clause
                     return (Name_Length    => Name_String'Length,
                             Spec_Length    => 0,
                             Use_Clause     => Clause,
                             Elem           => Name_Elem,
                             Position       => Inx,
                             Name           => Name_String,
                             Spec_Name      => "",
                             Op_Type_List   => Empty_Set,
                             Prim_Type_List => Empty_Set,
                             User           => Nothing,
                             Spec_Unused    => False);
                  end if;
               end;
         end case;
      end Build_Info;

   begin  -- Process_Use_Clause
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         use Asis.Expressions;
         use Framework.Reports.Fixes;
         Names     : constant Asis.Name_List        := Clause_Names (Clause);
         This_Unit : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Clause);
         Incr_Fix  : Incremental_Fix;
      begin
         for I in Names'Range loop
            declare
               use Scope_Manager, Framework.Locations, Framework.Reports;
               Info     : constant Used_Elem_Info := Build_Info (Names, I);
               Reported : Boolean := False;
            begin
               if Clause_Kind (Clause) in A_Use_Type_Clause | A_Use_All_Type_Clause
                 and then Info.Spec_Length = 0
               then
                  if Rule_Used (Nested) then
                     Report (Rule_Id,
                             Ctl_Contexts (Nested),
                             Get_Location (Names (I)),
                             "Nested: " & Clause_And_Name (Info)
                             & ", type not declared in package specification");
                     Fixes.List_Remove (Incr_Fix, I, From => Clause);
                  end if;

               -- Check if ancestor
               elsif Use_Clause_Kinds (Clause_Kind (Clause)) = A_Use_Package_Clause
                 and then Is_Compilation_Unit (Corresponding_Name_Declaration (Simple_Name (Names (I))))
                 and then Is_Ancestor (Definition_Compilation_Unit (Strip_Attributes (Names (I))),
                                       This_Unit,
                                       Strict => True)
               then
                  if Rule_Used (Nested) then
                     Report (Rule_Id,
                             Ctl_Contexts (Nested),
                             Get_Location (Names (I)),
                             "Nested: " & "use clause for " & Extended_Name_Image (Names (I))
                             & " in child unit "            & Unit_Full_Name (This_Unit));
                     Fixes.List_Remove (Incr_Fix, I, From => Clause);
                  end if;

               else
                  if Rule_Used (Nested) then
                     declare
                        Enclosing_PU : constant Asis.Defining_Name := Enclosing_Program_Unit (Clause);
                     begin
                        -- Checks if use clause inside the named package (or one of its nested units)
                        -- Enclosing_PU is nil if use clause in context clauses
                        if not Is_Nil (Enclosing_PU) then
                           declare
                              Enclosing_Name : constant Wide_String := Full_Name_Image (Enclosing_PU,
                                                                                        With_Profile => True);
                           begin
                              if Info.Name = Enclosing_Name then
                                 Report (Rule_Id,
                                         Ctl_Contexts (Nested),
                                         Get_Location (Names(I)),
                                         "Nested: use clause for " & Extended_Name_Image (Names (I))
                                         & " inside same package");
                                 Reported := True;
                                 Fixes.List_Remove (Incr_Fix, I, From => Clause);
                              elsif Starts_With (Enclosing_Name, Info.Name & '.') then
                                 Report (Rule_Id,
                                         Ctl_Contexts (Nested),
                                         Get_Location (Names(I)),
                                         "Nested: use clause for " & Extended_Name_Image (Names (I))
                                         & " inside nested unit " & Extended_Name_Image (Enclosing_PU));
                                 Reported := True;
                                 Fixes.List_Remove (Incr_Fix, I, From => Clause);
                              end if;
                           end;
                        end if;

                        -- Check if already there
                        if not Reported then
                           Used_Elements.Reset (All_Scopes);
                           while Used_Elements.Data_Available loop
                              if Used_Elements.Current_Data.Name = Info.Name then
                                 case Use_Clause_Kinds (Clause_Kind (Clause)) is
                                    when A_Use_Package_Clause =>
                                       Report (Rule_Id,
                                               Ctl_Contexts (Nested),
                                               Get_Location (Info.Elem),
                                               "Nested: " & Clause_And_Name (Info)
                                               & " in scope of use clause for same package at "
                                               & Image (Get_Location (Used_Elements.Current_Data.Elem)));
                                       Fixes.List_Remove (Incr_Fix, I, From => Clause);
                                       Reported := True;
                                    when A_Use_Type_Clause =>
                                       Report (Rule_Id,
                                               Ctl_Contexts (Nested),
                                               Get_Location (Info.Elem),
                                               "Nested: " & Clause_And_Name (Info)
                                               & " in scope of use type or use all type clause for same type at "
                                               & Image (Get_Location (Used_Elements.Current_Data.Elem)));
                                       Fixes.List_Remove (Incr_Fix, I, From => Clause);
                                       Reported := True;
                                    when A_Use_All_Type_Clause =>
                                       -- A use all type clause can make sense in the scope of a use type clause
                                       if Clause_Kind (Used_Elements.Current_Data.Use_Clause) /= A_Use_Type_Clause then
                                          Report (Rule_Id,
                                                  Ctl_Contexts (Nested),
                                                  Get_Location (Info.Elem),
                                                  "Nested: " & Clause_And_Name (Info)
                                                  & " in scope of use all type clause for same type at "
                                                  & Image (Get_Location (Used_Elements.Current_Data.Elem)));
                                          Fixes.List_Remove (Incr_Fix, I, From => Clause);
                                          Reported := True;
                                       end if;
                                 end case;

                              elsif Info.Spec_Length /= 0 and then Used_Elements.Current_Data.Name = Info.Spec_Name then
                                 Report (Rule_Id,
                                         Ctl_Contexts (Nested),
                                         Get_Location (Info.Elem),
                                         "Nested: " & Clause_And_Name (Info)
                                         & " in scope of use clause for " & Strip_Profile (Info.Spec_Name)
                                         & " at " & Image (Get_Location (Used_Elements.Current_Data.Elem)));
                                 Fixes.List_Remove (Incr_Fix, I, From => Clause);
                                 Reported := True;
                              end if;

                              Used_Elements.Next;
                           end loop;
                        end if;
                     end;
                  end if;

                  -- Add it in any case
                  -- Note that we DON'T add it in the other paths of the enclosing if statement, or if reported
                  -- from this path, because they are cases where the use clause doesn't add any visibility
                  -- (elements are directly visible anyway), therefore it is not useful to have further messages.
                  if not Reported then
                     Used_Elements.Push (Info);
                  end if;
               end if;
            end;
         end loop;
         Flush (Incr_Fix);
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
   --    Use type and use all type clauses have no effect to types not declared in a package, since those have no
   --    primitive operations
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
      use Asis, Asis.Elements, Asis.Expressions, Asis.Declarations;
      use Framework.Queries, Thick_Queries;

      function Is_Name_Prefixed_With (Pack_Name : Wide_String) return Boolean is
         Name_Enclosing  : constant Asis.Element := Enclosing_Element (Name);
         Ultimate_Prefix : Asis.Element;
      begin
         if Expression_Kind (Name_Enclosing) /= A_Selected_Component then
            return False;
         end if;
         if not Is_Equal (Name, Selector (Name_Enclosing)) then
            return False;
         end if;
         case Expression_Kind (Prefix (Name_Enclosing)) is
            when An_Identifier | A_Selected_Component =>
               -- Beware: the prefix can be a rename of Pack_Name!
               Ultimate_Prefix := Ultimate_Name (Prefix (Name_Enclosing));
               if Is_Nil (Ultimate_Prefix) then
                  -- This happens if the prefix is a renaming of a dereference, certainly not a package
                  return False;
               else
                  return Full_Name_Image (Ultimate_Prefix, With_Profile => True) = Pack_Name;
               end if;
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

         Used_Elements.Reset (All_Scopes);
         while Used_Elements.Data_Available loop
            declare
               Info      : Used_Elem_Info       := Used_Elements.Current_Data;
               Pack_Name : constant Wide_String := (if Clause_Kind (Info.Use_Clause) = A_Use_Package_Clause
                                                    then Info.Name
                                                    else Info.Spec_Name);
               Current_User : User_Kind := Info.User; -- Default: don't change it
            begin
               if Info.User /= Identifier         -- If already Identifier, no need to check lesser priority uses
                 and Pack_Name = Enclosing_Name   -- Entity declared in use-target package
               then
                  if Rule_Used (Movable)
                    and then (Info.User = Nothing and Used_Elements.Current_Origin = Specification)
                  then
                     Info.Spec_Unused := True;
                  end if;

                  if Is_Name_Prefixed_With (Pack_Name) then
                     case Use_Clause_Kinds (Clause_Kind (Info.Use_Clause)) is
                        when A_Use_Package_Clause =>
                           Current_User := Qualified_Name;
                        when A_Use_Type_Clause =>
                           if Is_Callable_Construct (Name)
                             and then Is_Use_Type_Visible (Info.Elem, Name)
                             and then Expression_Kind (Name) = An_Operator_Symbol
                           then
                              Current_User := Qualified_Name;
                           else
                              Current_User := Info.User;
                           end if;
                        when A_Use_All_Type_Clause =>
                           if Is_Callable_Construct (Name)
                             and then Is_Use_Type_Visible (Info.Elem, Name)
                           then
                              Current_User := Qualified_Name;
                           else
                              Current_User := Info.User;
                           end if;
                     end case;

                  elsif Clause_Kind (Info.Use_Clause) in A_Use_Type_Clause | A_Use_All_Type_Clause then
                     if Is_Callable_Construct (Name) and then Is_Use_Type_Visible (Info.Elem, Name) then
                        if Expression_Kind (Name) = An_Operator_Symbol then
                           Current_User := Operator;
                           Add (Info.Op_Type_List, Info.Elem);
                        else
                           Current_User := Primitive;
                        end if;
                     end if;

                  else  -- A_Use_Package_Clause
                     Current_User := Identifier; -- Default if not more specific found
                     if Is_Callable_Construct (Name) then
                        -- Check if primitive of some type declared in the package
                        for T : Asis.Declaration of Corresponding_Primitive_Types (Name) loop
                           if Is_Equal (Enclosing_Element (T),
                                        Corresponding_Name_Declaration (Simple_Name (Info.Elem)))
                           then
                              if Expression_Kind (Name) = An_Operator_Symbol
                                and then not Contains (Info.Prim_Type_List, Names (T) (1))
                              then
                                 Current_User := Operator;
                                 Add (Info.Op_Type_List, Names (T) (1));
                              else
                                 if Contains (Info.Op_Type_List, Names (T) (1)) then
                                    Delete (Info.Op_Type_List, Names (T) (1));
                                 end if;
                                 Current_User := Primitive;
                                 Add (Info.Prim_Type_List, Names (T) (1));
                              end if;
                           end if;
                        end loop;
                     end if;
                  end if;
                  Info.User := User_Kind'Max (Current_User, Info.User);
                  Used_Elements.Update_Current (Info);
               end if;
               Used_Elements.Next;
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
      -- Special case: generic formal instantiations are checked only for names that are actually given, and
      -- therefore not use the normalized form of Generic_Actual_Part.
      use Asis, Asis.Declarations, Asis.Expressions, Asis.Elements;
   begin
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      for Assoc : Asis.Association
        of Generic_Actual_Part (Instantiation,
                                Normalized => Declaration_Kind (Instantiation) /= A_Formal_Package_Declaration)
      loop
         if Is_Defaulted_Association (Assoc)
           and then Default_Kind (Enclosing_Element (Formal_Parameter (Assoc))) = A_Box_Default
         then
            Process_Identifier (Actual_Parameter (Assoc));
         end if;
      end loop;
   end Process_Instantiation;

   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Process_Scope_Exit (Scope : in Asis.Element) is
      use Framework.Locations, Framework.Reports, Framework.Reports.Fixes, Scope_Manager, Thick_Queries;
      use Asis, Asis.Elements, Asis.Declarations;

      Is_Package_Spec : Boolean := False;
      Is_Package_Body : Boolean := False;

      Insertions : Incremental_Fix;
      Deletions  : Incremental_Fix;
      -- Two different Incremental fix because deletions must be flushed for each clause,
      -- while all insertions are merged to minimize conflicts.
      Previous_From : Asis.Element := Nil_Element;
   begin  -- Process_Scope_Exit
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

      Used_Elements.Reset (Current_Scope_Only);
      while Used_Elements.Data_Available loop
         declare
            Info          : constant Used_Elem_Info := Used_Elements.Current_Data;
            Child_Warning : constant Boolean :=  Current_Depth = 1
                                                 and (Is_Package_Spec
                                                      or (Is_Package_Body
                                                          and Used_Elements.Current_Origin = Specification));
            use Ada.Strings.Wide_Unbounded;
         begin
            if Used_Elements.Current_Origin /= Parent then
               if Info.User /= Nothing and Info.Spec_Unused and Rule_Used (Movable) then
                  Report (Rule_Id,
                          Ctl_Contexts (Movable),
                          Get_Location (Info.Elem),
                          "Movable: " & Clause_And_Name (Info) & " can be moved to body");
               end if;

               case Info.User is
                  when Nothing =>
                     if Rule_Used (Unused) then
                        Report (Rule_Id,
                                Ctl_Contexts (Unused),
                                Get_Location (Info.Elem),
                                "Unused: " & Clause_And_Name (Info)
                                & Choose (Child_Warning,
                                  " (possible usage in child units)",
                                  ""));
                        if not Is_Equal (Info.Use_Clause, Previous_From) then
                           Fixes.Flush (Deletions);
                           Previous_From := Info.Use_Clause;
                        end if;
                        Fixes.List_Remove (Deletions, Info.Position, From => Info.Use_Clause);
                     end if;
                  when Qualified_Name =>
                     if Rule_Used (Qualified) then
                        Report (Rule_Id,
                                Ctl_Contexts (Qualified),
                                Get_Location (Info.Elem),
                                "Qualified: all uses of " & Extended_Name_Image (Info.Elem) & " are qualified"
                                & Choose (Child_Warning,
                                  " (possible usage in child units)",
                                  ""));
                        if not Is_Equal (Info.Use_Clause, Previous_From) then
                           Fixes.Flush (Deletions);
                           Previous_From := Info.Use_Clause;
                        end if;
                        Fixes.List_Remove (Deletions, Info.Position, From => Info.Use_Clause);
                     end if;
                  when Operator =>
                     if Rule_Used (Operator) and then Clause_Kind (Info.Use_Clause) /= A_Use_Type_Clause
                       and then Size (Info.Op_Type_List) <= Max_Replacements.Value
                     then
                        for T : Unbounded_Wide_String of Names_In_Set (Info.Op_Type_List) loop
                           Report (Rule_Id,
                                   Ctl_Contexts (Operator),
                                   Get_Location (Info.Elem),
                                   "Operator: " & Clause_And_Name (Info) & " used for operators of "
                                   & To_Wide_String (T)
                                   & (if Child_Warning then " (possible usage in child units)" else ""));
                           Fixes.Insert (Insertions,
                                         Line_Delimiter
                                            & Indentation_Of (Info.Use_Clause) & "use type "
                                            & To_Wide_String (T) & ";",
                                         After,
                                         Info.Use_Clause);
                        end loop;

                        if not Is_Equal (Info.Use_Clause, Previous_From) then
                           Fixes.Flush (Deletions);
                           Previous_From := Info.Use_Clause;
                        end if;
                        Fixes.List_Remove (Deletions, Info.Position, From => Info.Use_Clause);
                     end if;

                  when Primitive =>
                     if Rule_Used (Primitive) and then Clause_Kind (Info.Use_Clause) = A_Use_Package_Clause
                       and then Size (Info.Op_Type_List) + Size (Info.Prim_Type_List) <= Max_Replacements.Value
                     then
                        for T : Unbounded_Wide_String of Names_In_Set (Info.Op_Type_List) loop
                           Report (Rule_Id,
                                   Ctl_Contexts (Operator),
                                   Get_Location (Info.Elem),
                                   "Operator: " & Clause_And_Name (Info) & " used for operators of "
                                   & To_Wide_String (T)
                                   & (if Child_Warning then " (possible usage in child units)" else ""));
                           Fixes.Insert (Insertions, Line_Delimiter
                                         & Indentation_Of (Info.Use_Clause) & "use type "
                                         & To_Wide_String (T) & ";",
                                         After, Info.Use_Clause);
                        end loop;


                        for T : Unbounded_Wide_String of Names_In_Set (Info.Prim_Type_List) loop
                           Report (Rule_Id,
                                   Ctl_Contexts (Primitive),
                                   Get_Location (Info.Elem),
                                   "Primitive: " & Clause_And_Name (Info) & " used for primitive operations of "
                                   & To_Wide_String (T)
                                   & (if Child_Warning then " (possible usage in child units)" else ""));
                           Fixes.Insert (Insertions, Line_Delimiter
                                         & Indentation_Of (Info.Use_Clause)
                                         & "use all type "
                                         & To_Wide_String (T) & ";",
                                         After, Info.Use_Clause);
                        end loop;

                        if not Is_Equal (Info.Use_Clause, Previous_From) then
                           Fixes.Flush (Deletions);
                           Previous_From := Info.Use_Clause;
                        end if;
                        Fixes.List_Remove (Deletions, Info.Position, From => Info.Use_Clause);
                     end if;

                  when Identifier =>
                     null;
               end case;
            end if;
         end;

         Used_Elements.Next;
      end loop;
      Fixes.Flush (Deletions);
      Fixes.Flush (Insertions);

   end Process_Scope_Exit;

begin  -- Rules.Unnecessary_Use_Clause
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
   Framework.Variables.Register (Max_Replacements'Access,
                                 Variable_Name => Rule_Id & ".MAX_REPLACEMENTS");
end Rules.Unnecessary_Use_Clause;
