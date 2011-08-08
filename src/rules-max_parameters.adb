----------------------------------------------------------------------
--  Rules.Max_Parameters - Package body                             --
--                                                                  --
--  This software  is (c) SAGEM DS and  Adalog  2004-2006.  The Ada --
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
  Asis.Declarations,
  Asis.Elements;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Max_Parameters is
   use Framework, Thick_Queries;

   type Entity_Names is (E_Function,           E_Procedure,           E_Protected_Entry,
                         E_Protected_Function, E_Protected_Procedure, E_Task_Entry);
   package Entity_Flag_Utilities  is new Framework.Language.Flag_Utilities (Entity_Names, Prefix => "E_");

   type Usage is array (Entity_Names) of Rule_Types_Set;
   Rule_Used : Usage := (others => (others => False));
   Save_Used : Usage;

   Labels : array (Entity_Names, Rule_Types) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Values : array (Entity_Names, Rule_Types) of Biggest_Natural := (others => (others => 0));

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: "& Rule_Id);
      User_Message ("Parameter(1): maximum allowed number of parameters");
      Entity_Flag_Utilities.Help_On_Flags (Header => "Parameter(2..):",
                                           Footer => "(optional, default = all)");
      User_Message ("Controls the maximum allowed number of parameters for callable entities");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Framework.Language, Entity_Flag_Utilities, Ada.Strings.Wide_Unbounded;
      Stmt : Entity_Names;
      Stmt_List : array (1 .. Entity_Names'Pos (Entity_Names'Last) + 1) of Entity_Names;
      Stmt_Cnt  : Natural := 0;
      Max       : Biggest_Int;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least two parameters required");
      end if;

      if not Is_Integer_Parameter then
         Parameter_Error (Rule_Id, "missing max allowed value");
      end if;
      Max := Get_Integer_Parameter (Min => 0);

      if Parameter_Exists then
         while Parameter_Exists loop
            Stmt := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Stmt)(Rule_Type) then
               Parameter_Error (Rule_Id, "rule already specified for " & Rule_Types'Wide_Image (Rule_Type));
            end if;
            Stmt_Cnt := Stmt_Cnt + 1;
            Stmt_List (Stmt_Cnt) := Stmt;
         end loop;
      else
         -- no statement specified => applies to all
         for E in Entity_Names loop
            Stmt_Cnt := Stmt_Cnt + 1;
            Stmt_List (Stmt_Cnt) := E;
         end loop;
      end if;

      for I in Natural range 1 .. Stmt_Cnt loop
         Labels    (Stmt_List (I), Rule_Type):= To_Unbounded_Wide_String (Label);
         Rule_Used (Stmt_List (I))(Rule_Type):= True;
         Values (Stmt_List (I), Rule_Type)   := Max;
      end loop;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : in Framework.Rules_Manager.Rule_Action) is
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

   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Declaration : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Utilities;

      Good_Decl : Asis.Declaration := Declaration;

      procedure Do_Report (Rule_Type : Rule_Types; Entity : Entity_Names;  Loc : Location; Value : Biggest_Int) is
         use Ada.Strings.Wide_Unbounded;
         use Entity_Flag_Utilities, Framework.Reports;

         Good_Loc : Location := Loc;
      begin
         if Good_Loc = Null_Location then
            -- Formal parameter of an instantiation
            Good_Loc := Get_Location (Declaration);
         end if;
         Report (Rule_Id,
                 To_Wide_String (Labels (Entity, Rule_Type)),
                    Rule_Type,
                    Good_Loc,
                    "more than " & Biggest_Int_Img (Values (Entity, Rule_Type))
                       & " parameters in " & Image (Entity)
                       & " ("   & Biggest_Int_Img (Value) & ')');
         end Do_Report;

      E : Entity_Names;


   begin
      if Rule_Used = (Entity_Names => (Rule_Types => False)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Good_Decl) is
         when A_Procedure_Body_Declaration
            | A_Function_Body_Declaration
            | A_Procedure_Body_Stub
            | A_Function_Body_Stub
              =>
            if Is_Subunit (Good_Decl) or not Is_Nil (Corresponding_Declaration (Good_Decl)) then
               -- The check is performed on the specification or the stub, no need to repeat here
               return;
            end if;
         when others =>
            null;
      end case;

      case Declaration_Kind (Good_Decl) is
         when A_Procedure_Declaration =>
            if Definition_Kind (Enclosing_Element (Good_Decl)) = A_Protected_Definition then
               E := E_Protected_Procedure;
            else
               E := E_Procedure;
            end if;
         when A_Procedure_Instantiation =>
            E         := E_Procedure;
            Good_Decl := Corresponding_Declaration (Good_Decl);
         when A_Generic_Procedure_Declaration
            | A_Procedure_Body_Declaration
            | A_Procedure_Body_Stub
              =>
            E := E_Procedure;
         when A_Function_Declaration =>
            if Definition_Kind (Enclosing_Element (Good_Decl)) = A_Protected_Definition then
               E := E_Protected_Function;
            else
               E := E_Function;
            end if;
         when A_Function_Instantiation =>
            E         := E_Function;
            Good_Decl := Corresponding_Declaration (Good_Decl);
         when A_Generic_Function_Declaration
            | A_Function_Body_Declaration
            | A_Function_Body_Stub
              =>
            E := E_Function;
         when An_Entry_Declaration =>
            if Is_Task_Entry (Good_Decl) then
               E := E_Task_Entry;
            else
               E := E_Protected_Entry;
            end if;
         when others =>
            -- We don't call this procedure on entry bodies, since those always have
            -- a specification
            Failure ("not a callable entity");
      end case;

      declare
         Profile     : constant Parameter_Specification_List := Parameter_Profile (Good_Decl);
         Param_Count : Biggest_Natural := 0;
      begin
         for P in Profile'Range loop
            Param_Count := Param_Count + Names (Profile (P))'Length;
         end loop;

         -- Note that allowed values are at least 0, therefore the checks cannot fail
         -- if Profile'Length = 0
         if Rule_Used (E) (Check) and then Param_Count > Values (E, Check) then
            Do_Report (Check, E, Get_Location (Profile(1)), Param_Count);
         elsif Rule_Used (E) (Search) and then Param_Count > Values (E, Search) then
            Do_Report (Search, E, Get_Location (Profile(1)), Param_Count);
         end if;

         if Rule_Used (E) (Count) and then Param_Count > Values (E, Count) then
            Do_Report (Count, E, Get_Location (Profile(1)), Param_Count);
         end if;
      end;
   end Process_Declaration;

begin
   Rules_Manager.Register (Rule_Id,
                           Rules_Manager.Semantic,
                           Help_CB    => Help'Access,
                           Add_Use_CB => Add_Use'Access,
                           Command_CB => Command'Access);
end Rules.Max_Parameters;
