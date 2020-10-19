----------------------------------------------------------------------
--  Rules.Type_Initial_Values - Package body                        --
--                                                                  --
--  This software is (c) CSEE and Adalog 2004-2007.                 --
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

-- Ada
with
  Ada.Characters.Handling,
  Ada.Exceptions;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements;

-- Adalog
with
  Scope_Manager,
  String_Matching,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Symbol_Table;

package body Rules.Type_Initial_Values is
   use Framework, Framework.Control_Manager;

   type Pattern_Access is access String_Matching.Compiled_Pattern;
   Pattern   : Pattern_Access;

   type Constant_Found is (No_Constant, Non_Matching_Constant, Matching_Constant);
   package Constant_Table is new Framework.Symbol_Table.Data_Access (Constant_Found);

   Context   : Basic_Rule_Context;
   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control types without proper initialization constants");
      User_Message;
      User_Message ("Parameter(1): <pattern>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Characters.Handling, Ada.Exceptions;
      use Framework.Language, String_Matching;

   begin
      if Rule_Used then
         Parameter_Error (Rule_Id, "rule can be specified only once");
      end if;

      if Parameter_Exists then
         declare
            Ignore_Case : constant Boolean     := Get_Modifier (True_KW  => "CASE_INSENSITIVE",
                                                                False_KW => "CASE_SENSITIVE",
                                                                Default  => True);
            Pat_Str     : constant Wide_String := Get_String_Parameter;
         begin
            Pattern := new Compiled_Pattern'(Compile (Pat_Str, Ignore_Case));
         exception
            when Occur: Pattern_Error =>
               Parameter_Error (Rule_Id,
                                "Incorrect pattern: " & Pat_Str
                                & " (" & To_Wide_String (Exception_Message (Occur)) & ')');
         end;
      end if;

      Context   := Basic.New_Context (Ctl_Kind, Ctl_Label);
      Rule_Used := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Constant_Table.Clear;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   ------------------------------
   -- Process_Type_Declaration --
   ------------------------------

   procedure Process_Type_Declaration (Decl : in Asis.Declaration) is
      use Asis.Declarations;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Constant_Table.Store (Names (Decl)(1), No_Constant);
   end Process_Type_Declaration;

   ----------------------------------
   -- Process_Constant_Declaration --
   ----------------------------------

   procedure Process_Constant_Declaration (Decl : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Scope_Manager, String_Matching, Thick_Queries;

      Def        : Asis.Definition;
      Const_Type : Asis.Name;
      State      : Constant_Found;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Def := Object_Declaration_View (Decl);
      if Definition_Kind (Def) = An_Access_Definition
        or else Type_Kind (Def) in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition
      then
         -- constant of an anonymous access or array type
         return;
      end if;

      Const_Type := Subtype_Simple_Name (Def);
      if not Constant_Table.Is_Present (Const_Type)
        or else not Is_Equal (Constant_Table.Scope_Of (Const_Type), Current_Scope)
      then
         return;
      end if;

      State := Constant_Table.Fetch (Const_Type, Default => No_Constant);
      if State = Matching_Constant then
         return;
      end if;

      if Pattern = null then
         State := Matching_Constant;
      else
         State := Non_Matching_Constant;
         for N : Asis.Name of Names (Decl) loop
            if Match (Defining_Name_Image (N), Pattern.all) then
               State := Matching_Constant;
               exit;  -- One matching name is enough
            end if;
         end loop;
      end if;
      Constant_Table.Store (Const_Type, State);
   end Process_Constant_Declaration;

   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Check_One_Type (Entity : Asis.Defining_Name; State : in out Constant_Found) is
      use Asis.Declarations;
      use Framework.Locations, Framework.Reports, Framework.Symbol_Table;
   begin
      case State is
         when No_Constant =>
            Report (Rule_Id,
                    Context,
                    Get_Location (Entity),
                    "No initialization constant defined for type " & Defining_Name_Image (Entity));
         when Non_Matching_Constant =>
            Report (Rule_Id,
                    Context,
                    Get_Location (Entity),
                    "No initialization constant matches pattern for type "& Defining_Name_Image (Entity));
         when Matching_Constant =>
            null;
      end case;

      raise Delete_Current;
      -- Systematically delete current entity since:
      -- 1) we don't need it any more
      -- 2) if it is from a package spec, we don't want to have it when analyzing the corresponding
      --    body, or else the message would appear twice.
   end Check_One_Type;

   procedure Check_All_Types is new Constant_Table.On_Every_Entity_From_Scope (Check_One_Type);

   procedure Process_Scope_Exit (Scope : in Asis.Element) is
      pragma Unreferenced (Scope);
      use Framework.Symbol_Table;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_All_Types (Declaration_Scope);
   end Process_Scope_Exit;

begin  -- Rules.Type_Initial_Values
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Type_Initial_Values;
