----------------------------------------------------------------------
--  Rules.Max_Statement_Nesting - Package body                      --
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

-- ASIS
with
  Asis.Elements;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Thick_Queries;
pragma Elaborate (Framework.Language);

package body Rules.Max_Statement_Nesting is

   use Asis, Framework;

   type Statement_Names is (Stmt_If, Stmt_Case, Stmt_Loop, Stmt_All);
   package Statement_Flag_Utilities is new Framework.Language.Flag_Utilities (Statement_Names, "STMT_");

   subtype Controlled_Statements is Asis.Statement_Kinds range An_If_Statement .. A_For_Loop_Statement;

   type Usage is array (Statement_Names) of Rule_Types_Set;
   Rule_Used : Usage := (others => (others => False));
   Save_Used : Usage;

   Labels : array (Statement_Names, Rule_Types) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Values : array (Statement_Names, Rule_Types) of Natural;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Statement_Flag_Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      Help_On_Flags ("Parameter 1:");
      User_Message  ("Parameter 2: nesting depth");
      User_Message  ("Control max nesting of compound statements");
   end Help;

   -------------
   -- Add_Use --
   -------------

    procedure Add_Use (Label     : in Wide_String;
                       Rule_Type : in Rule_Types) is
      use Framework.Language, Statement_Flag_Utilities, Ada.Strings.Wide_Unbounded;
      Stmt : Statement_Names;
      Val  : Integer;
    begin
      if not Parameter_Exists then
         Parameter_Error ("Two parameters required for rule " & Rule_Id);
      end if;

      Stmt := Get_Flag_Parameter (Allow_Any => False);
      if Rule_Used (Stmt) (Rule_Type) then
         Parameter_Error (Rule_Id & ": rule already specified for " & Rule_Types'Wide_Image (Rule_Type));
      end if;

      if not Parameter_Exists then
         Parameter_Error ("Two parameters required for rule " & Rule_Id);
      end if;

      Val := Get_Integer_Parameter;
      if Val >= 1 then
         Values (Stmt, Rule_Type) := Val;
      else
         Parameter_Error (Rule_Id & ": value must be >= 1");
      end if;

      Labels    (Stmt, Rule_Type):= To_Unbounded_Wide_String (Label);
      Rule_Used (Stmt)(Rule_Type):= True;
    end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => (others => False));
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => (others => False));
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -----------------------
   -- Process_Statement --
   -----------------------

   procedure Process_Statement (Stmt : in Asis.Statement) is
      use Ada.Strings.Wide_Unbounded, Asis.Elements, Framework.Reports, Thick_Queries;
      Unit_Name  : constant Asis.Defining_Name := Enclosing_Program_Unit (Stmt, Including_Accept => True);
      Elem       : Asis.Element                := Stmt;
      If_Count   : Natural := 0;
      Case_Count : Natural := 0;
      Loop_Count : Natural := 0;
      All_Count  : Natural := 0;
   begin
      if Rule_Used = (Statement_Names => (Rule_Types => False)) then
         return;
      end if;

      if Statement_Kind (Stmt) not in Controlled_Statements then
         return;
      end if;

      loop
         case Statement_Kind (Elem) is
            when An_If_Statement =>
               if Rule_Used (Stmt_If) /= (Rule_Types => False)
                 or else Rule_Used (Stmt_All) /= (Rule_Types => False)
               then
                  If_Count  := If_Count + 1;
                  All_Count := All_Count + 1;
               end if;
            when A_Case_Statement =>
               if Rule_Used (Stmt_Case) /= (Rule_Types => False)
                 or else Rule_Used (Stmt_All) /= (Rule_Types => False)
               then
                  Case_Count := Case_Count + 1;
                  All_Count  := All_Count + 1;
               end if;
            when A_Loop_Statement
               | A_While_Loop_Statement
               | A_For_Loop_Statement
               =>
               if Rule_Used (Stmt_Loop) /= (Rule_Types => False)
                 or else Rule_Used (Stmt_All) /= (Rule_Types => False)
               then
                  Loop_Count := Loop_Count + 1;
                  All_Count  := All_Count + 1;
               end if;
            when others =>
               null;
         end case;
         Elem := Enclosing_Element (Elem);
         exit when Is_Nil (Elem)
           or else not Is_Equal (Unit_Name, Enclosing_Program_Unit (Elem, Including_Accept => True));
      end loop;

      for T in Rule_Types loop
         if Rule_Used (Stmt_All)(T) and then All_Count > Values (Stmt_All, T) then
            Report (Rule_Id,
                    To_Wide_String (Labels (Stmt_All, T)),
                    T,
                    Get_Location (Stmt),
                    "All statements nesting deeper than" & Integer'WIDE_IMAGE(Values (Stmt_All, T)));
         end if;
      end loop;

      case Controlled_Statements (Statement_Kind (Stmt)) is
         when An_If_Statement =>
            if Rule_Used (Stmt_If) /= (Rule_Types => False)
              or else Rule_Used (Stmt_All) /= (Rule_Types => False)
            then
               for T in Rule_Types loop
                  if Rule_Used (Stmt_If)(T) and then If_Count > Values (Stmt_If, T) then
                     Report (Rule_Id,
                             To_Wide_String (Labels (Stmt_If, T)),
                             T,
                             Get_Location (Stmt),
                             "If nesting deeper than" & Integer'WIDE_IMAGE(Values (Stmt_If, T)));
                  end if;
               end loop;
            end if;
         when A_Case_Statement =>
            if Rule_Used (Stmt_Case) /= (Rule_Types => False)
              or else Rule_Used (Stmt_All) /= (Rule_Types => False)
            then
               for T in Rule_Types loop
                  if Rule_Used (Stmt_Case)(T) and then Case_Count > Values (Stmt_Case, T) then
                     Report (Rule_Id,
                             To_Wide_String (Labels (Stmt_Case, T)),
                             T,
                             Get_Location (Stmt),
                             "Case nesting deeper than" & Integer'WIDE_IMAGE(Values (Stmt_Case, T)));
                  end if;
               end loop;
            end if;
         when A_Loop_Statement
            | A_While_Loop_Statement
            | A_For_Loop_Statement
            =>
            if Rule_Used (Stmt_Loop) /= (Rule_Types => False)
              or else Rule_Used (Stmt_All) /= (Rule_Types => False)
            then
               for T in Rule_Types loop
                  if Rule_Used (Stmt_Loop)(T) and then Loop_Count > Values (Stmt_Loop, T) then
                     Report (Rule_Id,
                             To_Wide_String (Labels (Stmt_Loop, T)),
                             T,
                             Get_Location (Stmt),
                             "Loop nesting deeper than" & Integer'WIDE_IMAGE(Values (Stmt_Loop, T)));
                  end if;
               end loop;
            end if;
      end case;
   end Process_Statement;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.Max_Statement_Nesting;
