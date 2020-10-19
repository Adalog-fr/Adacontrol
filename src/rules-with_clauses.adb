----------------------------------------------------------------------
--  Rules.With_Clauses - Package body                               --
--                                                                  --
--  This software is (c) CSEE and Adalog 2004-2006.                 --
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
  Asis.Compilation_Units,
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
  Framework.Reports.Fixes,
  Framework.Language,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);

package body Rules.With_Clauses is
   use Framework, Framework.Control_Manager, Framework.Variables.Shared_Types;

   -- Algorithm
   --
   -- Multiple_Names is trivial (just checks that names count is 1).
   --
   -- A Scoped_Store (Withed_Units) holds the description of withed units. Since the Scoped_Store is saved
   -- and restored between spec and body, it allows checking for units mentionned twice, either in the same
   -- compilation unit, or between spec and body.
   --
   -- When an identifier is encountered, and it is declared in another compilation unit, the state of the
   -- unit is updated accordingly in Withed_Units.
   --
   -- As usual, things get subtle when you consider renamings (Text_IO!). A "with Text_IO" is useful if we
   -- encounter something declared in Ada.Text_IO. For this reason, we compare the name of the unit that declared
   -- an indentifier to the Ultimate_Name of withed units (stored in Withed_Units); we also keep the original name
   -- for use in the error message.
   --
   -- Use clauses are special: it is the only case where the name directly matters (not the name of the enclosing
   -- unit). Therefore, an identifier in a use clause should be compared against the original name, not the ultimate
   -- name.
   --
   -- Limited is trivial (just checks limited with)
   --
   -- Private is trivial (just checks private with)
   --
   -- Limited_Private is trivial (just checks limited private with)

   type Subrules is (Sr_Regular,        Sr_Limited,    Sr_Private, Sr_Limited_Private,
                     Sr_Multiple_Names, Sr_Reduceable, Sr_Inherited);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "Sr_");

   type Usage_Flags is array (Subrules) of Boolean;
   Not_Used : constant Usage_Flags := (others => False);

   Rule_Used    : Usage_Flags := Not_Used;
   Save_Used    : Usage_Flags;
   Ctl_Contexts : array (Subrules) of Basic_Rule_Context;

   type Usage is (Never_Used, Used_In_Separate, Used);
   type With_Info (U_Length, O_Length : Positive) is
      record
         With_Clause   : Asis.Clause;
         Position      : Asis.List_Index;               -- Index of name in the with clause
         Loc           : Locations.Location;            -- Location of name
         Unit_Name     : Wide_String (1 .. U_Length);   -- Ultimate name of unit
         Original_Name : Wide_String (1 .. O_Length);   -- Full name of unit from the with clause
         Is_Private    : Boolean;
         Status        : Usage;
      end record;
   function Equivalent_Info (Left, Right : With_Info) return Boolean is
   begin
      return Left.Unit_Name = Right.Unit_Name;
   end Equivalent_Info;
   package Withed_Units is new Scope_Manager.Scoped_Store (With_Info, Equivalent_Info);

   -- Rule variables
   Check_Private_With : aliased Switch_Type.Object := (Value => On);
   Ignore_Use_Clause  : aliased Switch_Type.Object := (Value => On);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrules_Flag_Utilities, Framework.Variables;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control ""with"" clauses that use multiple names, can be moved to a more reduced scope,");
      User_Message ("or are implicitely inherited from a parent unit");
      User_Message;
      Help_On_Flags ("Parameter(s):");
      User_Message;
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Check_Private_With");
      Help_On_Variable (Rule_Id & ".Ignore_Use_Clause");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities, Utilities;
      Subrule : Subrules;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Subrule := Get_Flag_Parameter (Allow_Any => False);

            if Rule_Used (Subrule) then
               if not Basic.Merge_Context (Ctl_Contexts (Subrule), Ctl_Kind, Ctl_Label) then
                  Parameter_Error (Rule_Id, "Rule already specified for " & Image (Subrule, Lower_Case));
               end if;
            else
               Ctl_Contexts (Subrule) := Basic.New_Context (Ctl_Kind, Ctl_Label);
               Rule_Used    (Subrule) := True;
            end if;
         end loop;

      else
         -- All usages
         if Rule_Used /= (Subrules => False) then
            Parameter_Error (Rule_Id, "already specified");
         end if;

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
            Rule_Used          := Not_Used;
            Check_Private_With := (Value => On);
            Ignore_Use_Clause  := (Value => On);
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
         Withed_Units.Activate;
      end if;
   end Prepare;

   ---------------
   -- Is_Within --
   ---------------

   function Is_Within (Expr : Asis.Expression; Clause : Asis.Clause_Kinds) return Boolean is
      use Asis, Asis.Elements;
      Current : Asis.Element := Expr;
   begin
      while Element_Kind (Current) = An_Expression loop
         Current := Enclosing_Element (Current);
      end loop;

      return Clause_Kind (Current) = Clause;
   end Is_Within;

   -------------------------
   -- Process_With_Clause --
   -------------------------

   procedure Process_With_Clause (Element : in Asis.Clause) is
      use Asis.Clauses, Asis.Elements;
      use Scope_Manager, Framework.Locations, Framework.Reports, Thick_Queries, Utilities;

      function Required_For_Other_Context_Clauses (Name : Asis.Name) return Boolean is
      -- Is Name used in a use clause from the same context clause?
         use Asis, Asis.Expressions;

         Name_Def : constant Asis.Defining_Name := Thick_Queries.First_Defining_Name (Name);
      begin
         for Clause : Asis.Element of Context_Clause_Elements (Enclosing_Compilation_Unit (Name),
                                                               Include_Pragmas =>  True)
         loop
            case Clause_Kind (Clause) is
               when A_Use_Package_Clause =>
                  for N : Asis.Name of Clause_Names (Clause) loop
                     if Is_Equal (First_Defining_Name (N), Name_Def) then
                        return True;
                     end if;
                  end loop;
               when A_Use_Type_Clause
                  | A_Use_All_Type_Clause
                  =>
                  for N : Asis.Name of Clause_Names (Clause) loop
                     -- Normally, a use type will use a selected name. The only way to have a
                     -- simple name is if there is already a use clause for the containing package,
                     -- and of course this use clause will be analyzed. We can thus safely ignore
                     -- simple names
                     if Expression_Kind (N) = A_Selected_Component
                       and then Is_Equal (First_Defining_Name (Prefix (N)), Name_Def)
                     then
                        return True;
                     end if;
                  end loop;
               when others =>    -- Including Not_A_Clause (pragma)
                  null;
            end case;

            case Pragma_Kind (Clause) is
               when An_Elaborate_Pragma
                  | An_Elaborate_All_Pragma
                  =>
                  for A : Asis.Association of Pragma_Argument_Associations (Clause) loop
                     if Is_Equal (First_Defining_Name (Actual_Parameter (A)), Name_Def) then
                        return True;
                     end if;
                  end loop;
               when others =>   -- Including Not_A_Pragma
                  null;
            end case;
         end loop;

         return False;
      end Required_For_Other_Context_Clauses;

      function With_Image (Cl : Asis.Clause) return Wide_String is
      begin
         if Has_Limited (Cl) then
            if Has_Private (Cl) then
               return "limited private with ";
            else
               return "limited with ";
            end if;
         elsif Has_Private (Cl) then
            return "private with ";
         else
            return "with ";
         end if;
      end With_Image;
   begin   -- Process_With_Clause
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Names         : constant Asis.Name_List := Clause_Names (Element);
         This_Unit     : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Element);
         Inserts_Fix   : Fixes.Incremental_Fix;
         Deletions_Fix : Fixes.Incremental_Fix;

         use Reports.Fixes;

      begin
         if Rule_Used (Sr_Multiple_Names) and Names'Length > 1 then
            Report (Rule_Id,
                    Ctl_Contexts (Sr_Multiple_Names),
                    Get_Location (Element),
                    "With clause uses multiple names");
            Fixes.Replace (Element, By => With_Image (Element) & Extended_Name_Image (Names (1)) & ';');
            for N : Asis.Name of Names (2 .. Names'Last) loop
               Fixes.Break (Inserts_Fix, After, Element);
               Fixes.Insert (Inserts_Fix, With_Image (Element) & Extended_Name_Image (N) & ';', After, Element);
            end loop;
            Fixes.Flush (Inserts_Fix);
         end if;

         if Rule_Used (Sr_Regular) then
            if not Has_Limited (Element) and not Has_Private (Element) then
               Report (Rule_Id,
                       Ctl_Contexts (Sr_Private),
                       Get_Location (Element),
                       "Regular with clause");
            end if;
         end if;
         if Rule_Used (Sr_Private) then
            if not Has_Limited (Element) and Has_Private (Element) then
               Report (Rule_Id,
                       Ctl_Contexts (Sr_Private),
                       Get_Location (Element),
                       "Private with clause");
            end if;
         end if;
         if Rule_Used (Sr_Limited) then
            if Has_Limited (Element) and not Has_Private (Element) then
               Report (Rule_Id,
                       Ctl_Contexts (Sr_Limited),
                       Get_Location (Element),
                       "Limited with clause");

            end if;
         end if;
         if Rule_Used (Sr_Limited_Private) then
            if Has_Limited (Element) and Has_Private (Element) then
               Report (Rule_Id,
                       Ctl_Contexts (Sr_Limited_Private),
                       Get_Location (Element),
                       "Limited private with clause");
            end if;
         end if;

         if not Rule_Used (Sr_Reduceable) and not Rule_Used (Sr_Inherited) then
            return;
         end if;

         for I in Names'Range loop
            declare
               U_Name    : constant Wide_String := To_Upper (Full_Name_Image (Ultimate_Name (Names (I))));
               type With_Status is (OK, Redundant, Required_For_Use);
               Status : With_Status := OK;
            begin
               -- Check if for ancestor unit
               if Is_Ancestor (Definition_Compilation_Unit (Names (I)), This_Unit, Strict => True) then
                  if Rule_Used (Sr_Reduceable) then
                     Report (Rule_Id,
                             Ctl_Contexts (Sr_Reduceable),
                             Get_Location (Names (I)),
                             "With clause for ancestor unit " & Extended_Name_Image (Names (I)));
                     Fixes.List_Remove (Deletions_Fix, I, From => Element);
                  end if;
                  Status := Redundant;
               else
                  -- Check if already there
                  Withed_Units.Reset (Unit_Scopes);
                  while Withed_Units.Data_Available loop
                     if U_Name = Withed_Units.Current_Data.Unit_Name then
                        -- Redundant, unless in a body, the other with is from the spec,
                        -- and some context use clause mentions this package
                        if Withed_Units.Current_Origin /= Specification
                          or else not Required_For_Other_Context_Clauses (Names (I))
                        then
                           if Rule_Used (Sr_Reduceable) then
                              Report (Rule_Id,
                                      Ctl_Contexts (Sr_Reduceable),
                                      Get_Location (Names (I)),
                                      "With clause for " & Extended_Name_Image (Names (I))
                                      & " redundant with clause at " & Image (Withed_Units.Current_Data.Loc));
                              Fixes.List_Remove (Deletions_Fix, I, From => Element);
                           end if;
                           Status := Redundant;
                           exit;
                        end if;

                        Status := Required_For_Use;
                     end if;
                     Withed_Units.Next;
                  end loop;
               end if;

               if Status /= Redundant then
                  declare
                     O_Name : constant Wide_String := Full_Name_Image (Names (I));
                  begin
                     Withed_Units.Push ((With_Clause   => Element,
                                         Position      => I,
                                         Loc           => Get_Location (Names (I)),
                                         U_Length      => U_Name'Length,
                                         O_Length      => O_Name'Length,
                                         Unit_Name     => U_Name,
                                         Original_Name => O_Name,
                                         Is_Private    => Has_Private (Element),
                                         Status        => Never_Used));
                  end;
               end if;
            end;
         end loop;
         Fixes.Flush (Deletions_Fix);
      end;
   end Process_With_Clause;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Element : in Asis.Expression) is
      use Asis.Compilation_Units, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Scope_Manager, Thick_Queries, Utilities;
      use all type Reports.Fixes.Insert_Place;

      Elem_Def      : Asis.Defining_Name;
      Elem_Def_Unit : Asis.Compilation_Unit;
      Is_From_Use   : Boolean;
   begin
      if not Rule_Used (Sr_Reduceable) and not Rule_Used (Sr_Inherited) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Within (Element, Asis.A_With_Clause) then
         -- Use of name within its own with clause doesn't count...
         return;
      end if;

      Is_From_Use := Is_Within (Element, Asis.A_Use_Package_Clause) ;
      if Is_From_Use and Ignore_Use_Clause.Value = On then
            -- Ignore names in use clauses
            return;
      end if;

      Elem_Def := Corresponding_Name_Definition (Element);
      if Is_Nil (Elem_Def) then
         -- Some predefined stuff...
         return;
      end if;

      Elem_Def_Unit := Enclosing_Compilation_Unit (Elem_Def);
      if Is_Equal (Elem_Def_Unit, Enclosing_Compilation_Unit (Element)) then
         -- This is a local element
         return;
      end if;

      declare
         function Defeat_Gnat_Trick (Name : Wide_String) return Wide_String is
            -- GNAT implements Integer_IO and brothers as (hidden) children of Ada.Text_IO, through
            -- special magic in the compiler. Unfortunately, this has the effect that Enclosing_Compilation_Unit
            -- reports them as their own compilation unit, rather than being included in Ada.Text_IO.
            -- Therefore, if the only use of Ada.Text_IO is for instantiating one of these packages, the rule
            -- will report that the "with Ada.Text_IO" is not necessary...
            --
            -- This function filters the names of the special generics that appear to be
            -- children of Ada.Text_IO, and reestablishes the truth.

            function Is_Magic_Unit_Of (IO_Unit : Wide_String) return Boolean is
            -- Lower bound of Name *is* 1, since it is obtained from Full_Name_Image:
               pragma Warnings (Off, "index for ""Name"" may assume lower bound of 1");
            begin
               if Name'Length <= IO_Unit'Length+1
                 or else Name (1 .. IO_Unit'Length) /= IO_Unit
                 or else Name (IO_Unit'Length+1) /= '.'
               then
                  return False;
               end if;

               declare
                  Rest : Wide_String renames Name (IO_Unit'Length + 2 .. Name'Last);
               begin
                  return    Rest = "ENUMERATION_IO"
                    or else Rest = "INTEGER_IO"
                    or else Rest = "MODULAR_IO"
                    or else Rest = "FLOAT_IO"
                    or else Rest = "FIXED_IO"
                    or else Rest = "DECIMAL_IO";
               end;
               pragma Warnings (On, "index for ""Name"" may assume lower bound of 1");
            end Is_Magic_Unit_Of;

         begin  -- Defeat_Gnat_Trick
            if Is_Magic_Unit_Of ("ADA.TEXT_IO") then
               return "ADA.TEXT_IO";
            elsif Is_Magic_Unit_Of ("ADA.WIDE_TEXT_IO") then
                  return "ADA.WIDE_TEXT_IO";
            elsif Is_Magic_Unit_Of ("ADA.WIDE_WIDE_TEXT_IO") then
               return "ADA.WIDE_WIDE_TEXT_IO";
            else
               return Name;
            end if;
         end Defeat_Gnat_Trick;

         U_Name : constant Wide_String := Defeat_Gnat_Trick (To_Upper
                                                             (Full_Name_Image
                                                              (Names (Unit_Declaration (Elem_Def_Unit)) (1))));
      begin
         Withed_Units.Reset (Unit_Scopes);
         while Withed_Units.Data_Available loop
            declare
               Info : With_Info := Withed_Units.Current_Data;
            begin
               if (Is_From_Use and To_Upper(Info.Original_Name) = U_Name)
                 or else Info.Unit_Name = U_Name
               then
                  case Info.Status is
                     when Never_Used | Used_In_Separate =>
                        case Withed_Units.Current_Origin is
                           when Specification =>
                              if Rule_Used (Sr_Reduceable) then
                                 Report (Rule_Id,
                                         Ctl_Contexts (Sr_Reduceable),
                                         Info.Loc,
                                         "With clause for "
                                           & Info.Original_Name
                                           & " can be moved to body"
                                           & Choose (Info.Is_Private, " (remove private)", ""));
                              end if;
                           when Parent =>
                              if Rule_Used (Sr_Inherited) then
                                 Report (Rule_Id,
                                         Ctl_Contexts (Sr_Inherited),
                                         Get_Location (Unit_Declaration (Enclosing_Compilation_Unit (Element))),
                                         "With clause for "
                                           & Info.Original_Name
                                           & " inherited from " & Image (Info.Loc));
                                 Fixes.Insert ("with " & Info.Original_Name & ';',
                                               Before,
                                               Unit_Declaration (Enclosing_Compilation_Unit (Element)),
                                               Full_Line => True);
                              end if;
                           when Same_Unit =>
                              if Rule_Used (Sr_Reduceable)
                                and then Check_Private_With.Value = On
                                and then In_Private_Part (Compilation_Unit_Scope)
                                and then not Info.Is_Private
                              then
                                 Report (Rule_Id,
                                         Ctl_Contexts (Sr_Reduceable),
                                         Info.Loc,
                                         "With clause for "
                                           & Info.Original_Name
                                         & " can be changed to private with");
                                 Fixes.Insert ("private ", Before, Info.With_Clause);
                              end if;
                        end case;
                        Info.Status := Used;
                        Withed_Units.Update_Current (Info);
                     when Used =>
                        null;
                  end case;

                  -- We have an explicit with in our unit
                  -- => no need to check for inheritance
                  return;
               end if;

               Withed_Units.Next;
            end;
         end loop;

         -- In the case of separate units, inheritance is seen if we have a
         -- with from outside the unit.
         -- Note that the loop will be exited immediately if we are not in a separate unit,
         -- therefore it is not useful (as far as optimization is concerned) to check whether
         -- we are in a separate unit
         if Rule_Used (Sr_Inherited) then
            Withed_Units.Continue (All_Scopes);
            while Withed_Units.Data_Available loop
               declare
                  Info : With_Info := Withed_Units.Current_Data;
               begin
                  if Info.Unit_Name = U_Name then
                     if Info.Status = Never_Used then
                        Report (Rule_Id,
                                Ctl_Contexts (Sr_Inherited),
                                Get_Location (Unit_Declaration (Enclosing_Compilation_Unit (Element))),
                                "With clause for "
                                  & Info.Original_Name
                                  & " inherited from " & Image (Info.Loc));
                        Fixes.Insert ("with " & Info.Original_Name & ';',
                                      Before,
                                      Unit_Declaration (Enclosing_Compilation_Unit (Element)),
                                      Full_Line => True);
                     end if;
                     Info.Status := Used_In_Separate;
                     Withed_Units.Update_Current (Info);
                     exit;
                  end if;
                  Withed_Units.Next;
               end;
            end loop;
         end if;
      end;
   end Process_Identifier;


   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Decl : in Asis.Declaration) is
   -- A with clause may serve only to provide (implicit) actuals for formal subprograms
   -- with a "<>" default. Since these subprograms do not appear in the text, they are
   -- not traversed, hence the need to handle them here. They are recognized because they
   -- are the only ones that are Is_Part_Of_Implicit (see ASIS standard).
   -- Note that subprograms with regular defaults do not depend on visibility, and thus
   -- need not be traversed, even when the default is used in the instantiation.
      use Asis.Declarations, Asis.Elements, Asis.Expressions;
   begin
      -- We check Rule_Used here to make sure that nothing is executed if the unit is inhibited
      -- However, we don't call Enter_Unit, because it will be called from Process_Identifier
      if not Rule_Used (Sr_Reduceable) and not Rule_Used (Sr_Inherited) then
         return;
      end if;

      for A : Asis.Association of Generic_Actual_Part (Decl, Normalized => True) loop
         if Is_Part_Of_Implicit (Actual_Parameter (A)) then
            Process_Identifier (Actual_Parameter (A));
         end if;
      end loop;
   end Process_Instantiation;


   -----------------------
   -- Process_Unit_Exit --
   -----------------------

   procedure Process_Unit_Exit (Unit : in Asis.Compilation_Unit) is
      use Framework.Reports, Scope_Manager, Utilities;
      use Asis, Asis.Elements, Asis.Declarations;

      Decl : constant Asis.Declaration := Unit_Declaration (Unit);
      Is_Spec : constant Boolean := Declaration_Kind (Decl) = A_Package_Declaration              or
                                    Declaration_Kind (Decl) = A_Procedure_Declaration            or
                                    Declaration_Kind (Decl) = A_Null_Procedure_Declaration       or
                                    Declaration_Kind (Decl) = A_Function_Declaration             or
                                    Declaration_Kind (Decl) = An_Expression_Function_Declaration or
                                    Declaration_Kind (Decl) in A_Generic_Declaration;
      Deletions_Fix   : Fixes.Incremental_Fix;
      Previous_Clause : Asis.Clause := Nil_Element;
   begin
      if not Rule_Used (Sr_Reduceable) and not Rule_Used (Sr_Inherited) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Withed_Units.Reset (Unit_Scopes);
      while Withed_Units.Data_Available and then Withed_Units.Current_Origin /= Parent loop
         -- For a package spec with a body, delay messages until the end of the body
         if not Is_Spec or else Is_Nil (Corresponding_Body (Decl)) then
            declare
               Info : With_Info := Withed_Units.Current_Data;
            begin
               if Rule_Used (Sr_Reduceable) then
                  case Info.Status is
                     when Never_Used =>
                        Report (Rule_Id,
                                Ctl_Contexts (Sr_Reduceable),
                                Info.Loc,
                                "Unnecessary with clause for """ & Info.Original_Name
                                  & Choose (Is_Spec or Withed_Units.Current_Origin = Specification,
                                            """ (possible use in child units)",
                                            """")
                               );
                        if not Is_Equal (Info.With_Clause, Previous_Clause) then
                           Fixes.Flush (Deletions_Fix);
                           Previous_Clause := Info.With_Clause;
                        end if;
                        Fixes.List_Remove (Deletions_Fix, Info.Position, From => Info.With_Clause);
                     when Used_In_Separate =>
                        Report (Rule_Id,
                                Ctl_Contexts (Sr_Reduceable),
                                Info.Loc,
                                "Unnecessary with clause for """ & Info.Original_Name
                                & """ (used in separate unit(s))");
                        if not Is_Equal (Info.With_Clause, Previous_Clause) then
                           Fixes.Flush (Deletions_Fix);
                           Previous_Clause := Info.With_Clause;
                        end if;
                        Fixes.List_Remove (Deletions_Fix, Info.Position, From => Info.With_Clause);
                     when Used =>
                        null;
                  end case;
               end if;

               -- Reset status for possible later child units
               Info.Status := Never_Used;
               Withed_Units.Update_Current (Info);
            end;
         end if;

         Withed_Units.Next;
      end loop;
      Fixes.Flush (Deletions_Fix);
   end Process_Unit_Exit;

begin  -- Rules.With_Clauses
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
   Framework.Variables.Register (Check_Private_With'Access,
                                 Variable_Name => Rule_Id & ".CHECK_PRIVATE_WITH");
   Framework.Variables.Register (Ignore_Use_Clause'Access,
                                 Variable_Name => Rule_Id & ".IGNORE_USE_CLAUSE");
end Rules.With_Clauses;
