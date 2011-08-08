----------------------------------------------------------------------
--  Rules.With_Clauses - Package body                               --
--                                                                  --
--  This  software  is  (c)  CSEE  and Adalog  2004-2006.  The  Ada --
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

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Asis
with
  Asis.Clauses,
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;
pragma Elaborate (Framework.Language);

package body Rules.With_Clauses is
   use Framework;

   type With_Usage is (Multiple_Names, Reduceable, Inherited);
   package With_Usage_Utilities is new Framework.Language.Flag_Utilities (With_Usage);

   type Usage_Flags is array (With_Usage) of Boolean;
   Rule_Used    : Usage_Flags := (others => False);
   Save_Used    : Usage_Flags;
   Rule_Context : array (With_Usage) of Basic_Rule_Context;

   type Usage is (Not_Used, Used_In_Separate, Used);
   type With_Info (U_Length, O_Length : Positive) is
      record
         Unit_Name     : Wide_String (1 .. U_Length);
         Original_Name : Wide_String (1 .. O_Length);
         Unit_Loc      : Location;
         Status        : Usage;
      end record;
   function Equivalent_Info (Left, Right : With_Info) return Boolean is
   begin
      return Left.Unit_Name = Right.Unit_Name;
   end Equivalent_Info;

   package Withed_Units is new Framework.Scope_Manager.Scoped_Store (With_Info, Equivalent_Info);

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

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): reduceable | inherited");
      User_Message ("Control ""with"" clauses that use multiple names, can be moved to a more reduced scope,");
      User_Message ("or are implicitely inherited from a parent unit");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language, With_Usage_Utilities;
      Usage : With_Usage;
   begin
      if Parameter_Exists then
         Usage := Get_Flag_Parameter (Allow_Any => False);

         if Rule_Used (Usage) then
            Parameter_Error (Rule_Id & "already specified for " & Image (Usage));
         end if;

         Rule_Context (Usage) := Basic.New_Context (Rule_Type, Label);
         Rule_Used    (Usage) := True;

      else
         -- All usages
         if Rule_Used /= (With_Usage => False) then
            Parameter_Error (Rule_Id & "already specified");
         end if;

         Rule_Context := (others => Basic.New_Context (Rule_Type, Label));
         Rule_Used    := (others => True);
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
            Rule_Used := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -------------------------
   -- Process_With_Clause --
   -------------------------

   procedure Process_With_Clause (Element : in Asis.Clause) is
      use Asis.Clauses;
      use Framework.Scope_Manager, Framework.Reports, Thick_Queries, Utilities;
   begin
      if not Rule_Used (Reduceable) and not Rule_Used (Multiple_Names) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Names : constant Asis.Name_List := Clause_Names (Element);
      begin
         if Rule_Used (Multiple_Names) and Names'Length > 1 then
            Report (Rule_Id,
                    Rule_Context (Multiple_Names),
                    Get_Location (Element),
                    "With clause uses multiple names");
         end if;

         if not Rule_Used (Reduceable) then
            return;
         end if;

         for I in Names'Range loop
            declare
               U_Name    : constant Wide_String := To_Upper (Full_Name_Image (Ultimate_Name (Names (I))));
               Redundant : Boolean := False;
            begin
               -- Check if already there
               Withed_Units.Reset (Unit_Scopes);
               while Withed_Units.Data_Available loop
                  if U_Name = Withed_Units.Current_Data.Unit_Name then
                     Report (Rule_Id,
                             Rule_Context (Reduceable),
                             Get_Location (Names (I)),
                             "With clause for " & Extended_Name_Image (Names (I))
                               & " redundant with clause at " & Image (Withed_Units.Current_Data.Unit_Loc));
                     Redundant := True;
                     exit;
                  end if;
                  Withed_Units.Next;
               end loop;

               if not Redundant then
                  declare
                     O_Name : constant Wide_String := Full_Name_Image (Names (I));
                  begin
                     Withed_Units.Push ((U_Length      => U_Name'Length,
                                         O_Length      => O_Name'Length,
                                         Unit_Name     => U_Name,
                                         Unit_Loc      => Get_Location (Names (I)),
                                         Original_Name => O_Name,
                                         Status        => Not_Used));
                  end;
               end if;
            end;
         end loop;
      end;
   end Process_With_Clause;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Element : in Asis.Expression) is
      use Asis.Compilation_Units, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Framework.Scope_Manager, Thick_Queries, Utilities;
      Elem_Def      : Asis.Defining_Name;
      Elem_Def_Unit : Asis.Compilation_Unit;
   begin
      if not Rule_Used (Reduceable) and not Rule_Used (Inherited) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Within (Element, Asis.A_With_Clause) then
         -- Use of name within its own with clause doesn't count...
         return;
      end if;

      if Is_Within (Element, Asis.A_Use_Package_Clause) then
         -- Ignore names in use clauses
         return;
      end if;

      Elem_Def := Corresponding_Name_Definition (Element);
      if Is_Nil (Elem_Def) then
         -- Some predefined stuff...
         return;
      else
         Elem_Def_Unit := Enclosing_Compilation_Unit (Elem_Def);
         if Is_Equal (Elem_Def_Unit, Enclosing_Compilation_Unit (Element)) then
            -- This is a local element
            return;
         end if;
      end if;

      declare
         U_Name : constant Wide_String := To_Upper (Full_Name_Image
                                                      (Names (Unit_Declaration (Elem_Def_Unit)) (1)));
      begin
         Withed_Units.Reset (Unit_Scopes);
         while Withed_Units.Data_Available loop
            declare
               Info : With_Info := Withed_Units.Current_Data;
            begin
               if Info.Unit_Name = U_Name then
                  case Info.Status is
                     when Not_Used | Used_In_Separate =>
                        case Withed_Units.Current_Origin is
                           when Specification =>
                              if Rule_Used (Reduceable) then
                                 Report (Rule_Id,
                                         Rule_Context (Reduceable),
                                         Info.Unit_Loc,
                                         "With clause for "
                                           & Info.Original_Name
                                           & " can be moved to body");
                              end if;
                           when Parent =>
                              if Rule_Used (Inherited) then
                                 Report (Rule_Id,
                                         Rule_Context (Inherited),
                                         Get_Location (Unit_Declaration (Enclosing_Compilation_Unit (Element))),
                                         "With clause for "
                                           & Info.Original_Name
                                           & " inherited from " & Image (Info.Unit_Loc));
                              end if;
                           when Same_Unit =>
                              null;
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
         if Rule_Used (Inherited) then
            Withed_Units.Continue (All_Scopes);
            while Withed_Units.Data_Available loop
               declare
                  Info : With_Info := Withed_Units.Current_Data;
               begin
                  if Info.Unit_Name = U_Name then
                     if Info.Status = Not_Used then
                        Report (Rule_Id,
                                Rule_Context (Inherited),
                                Get_Location (Unit_Declaration (Enclosing_Compilation_Unit (Element))),
                                "With clause for "
                                  & Info.Original_Name
                                  & " inherited from " & Image (Info.Unit_Loc));
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


   -----------------------
   -- Process_Unit_Exit --
   -----------------------

   procedure Process_Unit_Exit (Unit : in Asis.Compilation_Unit) is
      use Framework.Reports, Framework.Scope_Manager, Utilities;
      use Asis, Asis.Elements, Asis.Declarations;

      Decl : constant Asis.Declaration := Unit_Declaration (Unit);
      Is_Spec : constant Boolean := Declaration_Kind (Decl) = A_Package_Declaration   or
                                    Declaration_Kind (Decl) = A_Procedure_Declaration or
                                    Declaration_Kind (Decl) = A_Function_Declaration  or
                                    Declaration_Kind (Decl) in A_Generic_Declaration;
   begin
      if not Rule_Used (Reduceable) and not Rule_Used (Inherited) then
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
               if Rule_Used (Reduceable) then
                  case Info.Status is
                     when Not_Used =>
                        Report (Rule_Id,
                                Rule_Context (Reduceable),
                                Info.Unit_Loc,
                                "Unnecessary with clause for """ & Info.Original_Name
                                  & Choose (Is_Spec or Withed_Units.Current_Origin = Specification,
                                            """ (possible use in child units)",
                                            """")
                               );
                     when Used_In_Separate =>
                        Report (Rule_Id,
                                Rule_Context (Reduceable),
                                Info.Unit_Loc,
                                "Unnecessary with clause for """ & Info.Original_Name
                                  & """ (used in separate unit(s))");
                     when Used =>
                        null;
                  end case;
               end if;

               -- Reset status for possible later child units
               Info.Status := Not_Used;
               Withed_Units.Update_Current (Info);
            end;
         end if;

         Withed_Units.Next;
      end loop;
   end Process_Unit_Exit;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.With_Clauses;
