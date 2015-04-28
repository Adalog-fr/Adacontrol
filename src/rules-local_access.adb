----------------------------------------------------------------------
--  Rules.Local_Access - Package body                               --
--                                                                  --
--  This  software  is  (c) Adalog  2004-2011.                      --
--  The Ada  Controller is free  software; you can  redistribute it --
--  and/or modify it under terms  of the GNU General Public License --
--  as published by the Free Software Foundation; either version 2, --
--  or  (at   your  option)  any  later  version.    This  unit  is --
--  distributed in the hope that it will be useful, but WITHOUT ANY --
--  WARRANTY; without even  the implied warranty of MERCHANTABILITY --
--  or  FITNESS FOR  A  PARTICULAR PURPOSE.   See  the GNU  General --
--  Public License  for more details.   You should have  received a --
--  copy of  the GNU General  Public License distributed  with this --
--  program; see file COPYING.  If  not, write to the Free Software --
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, --
--  USA.                                                            --
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
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Scope_Manager,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Local_Access is
   use Framework, Framework.Control_Manager;

   type Subrules is (K_Constant, K_Function, K_Procedure, K_Protected_Function, K_Protected_Procedure, K_Variable);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, "K_");

   type Usage_Flags is array (Subrules) of Boolean;
   Rule_Used : Usage_Flags := (Subrules => False);
   Save_Used : Usage_Flags;

   Contexts : array (Subrules) of Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control usage of non-local 'Access attribute (and similar ones)");
      User_Message;
      Help_On_Flags ("Parameter(s): ");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Ctl_Label : in Wide_String; Ctl_Kind  : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities, Utilities;
      Sr : Subrules;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Sr := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Sr) then
               Parameter_Error (Rule_Id, "Subrule already given: " & Image (Sr, Lower_Case));
            end if;
            Rule_Used (Sr) := True;
            Contexts (Sr)  := Basic.New_Context (Ctl_Kind, Ctl_Label);
         end loop;
      elsif Rule_Used /= (Subrules => False) then
         Parameter_Error (Rule_Id, "rule already given");
      else
         for S in Subrules loop
            Contexts (S) := Basic.New_Context (Ctl_Kind, Ctl_Label);
         end loop;
         Rule_Used := (Subrules => True);
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
            Rule_Used := (Subrules => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -----------------------
   -- Process_Attribute --
   -----------------------

   procedure Process_Attribute (Attr : Asis.Expression) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Scope_Manager, Utilities, Thick_Queries;

      Good_Prefix : Asis.Expression;
      Decl        : Asis.Declaration;

      procedure Do_Report (Sr : Subrules) is
         use Framework.Reports, Subrules_Flag_Utilities;
      begin
         if  Rule_Used (Sr) then
            Report (Rule_Id,
                    Contexts (Sr),
                    Get_Location (Good_Prefix),
                    ''' & Attribute_Name_Image (Attr) & " of non local " & Image (Sr, Lower_Case));
            end if;
      end Do_Report;

   begin   -- Process_Attribute
      if Rule_Used = (Subrules => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Attribute_Kind (Attr) /= An_Access_Attribute
        and Attribute_Kind (Attr) /= An_Unchecked_Access_Attribute
        and (Attribute_Kind (Attr) /= An_Implementation_Defined_Attribute
             or else To_Upper (Attribute_Name_Image (Attr)) /= "UNRESTRICTED_ACCESS")
      then
         return;
      end if;

      Good_Prefix := Prefix (Attr);
      -- Get rid of indexing and components to get to the real element

      On_Names: loop
         loop
            case Expression_Kind (Good_Prefix) is
               when An_Identifier =>
                  exit;
               when An_Indexed_Component =>
                  Good_Prefix := Prefix (Good_Prefix);
               when A_Selected_Component =>
                  if Declaration_Kind (Corresponding_Name_Declaration (Selector (Good_Prefix)))
                    /= A_Component_Declaration
                  then
                     Good_Prefix := Selector (Good_Prefix);
                     exit;
                  end if;
                  Good_Prefix := Prefix (Good_Prefix);
               when An_Explicit_Dereference =>
                  -- Dynamic objects have global scope
                  -- If the pointer has been created with non local 'Access, it will be caught at
                  -- the point where it was created
                  return;
               when A_Parenthesized_Expression =>
                  Good_Prefix := Expression_Parenthesized (Good_Prefix);
               when A_Type_Conversion | A_Qualified_Expression =>
                  Good_Prefix := Converted_Or_Qualified_Expression (Good_Prefix);
               when A_Function_Call =>
                  -- 'Access of (part of) the anonymous object returned by a function
                  -- report it as a constant, but considering that it is declared in the local scope
                  if not Is_Current_Scope_Global then
                     Do_Report (K_Constant);
                  end if;
                  return;
               when others =>
                  Failure ("Local_Access: Unexpected element kind", Good_Prefix);
            end case;
         end loop;

         if Static_Level (Good_Prefix) = Global_Level then
            return;
         end if;

         Decl := Corresponding_Name_Declaration (Good_Prefix);
         On_Declarations : loop
            case Declaration_Kind (Decl) is
               when A_Constant_Declaration =>
                  Do_Report (K_Constant);
                  return;

               when A_Variable_Declaration =>
                  Do_Report (K_Variable);
                  return;

               when A_Parameter_Specification | A_Formal_Object_Declaration =>
                  case Mode_Kind (Decl) is
                     when Not_A_Mode =>
                        Failure ("Bad mode in Local_Access");
                     when A_Default_In_Mode | An_In_Mode =>
                        Do_Report (K_Constant);
                     when An_Out_Mode | An_In_Out_Mode =>
                        Do_Report (K_Variable);
                  end case;
                  return;

               when A_Function_Declaration
                  | An_Expression_Function_Declaration   -- Ada 2012
                  =>
                  if Definition_Kind (Enclosing_Element (Corresponding_Name_Declaration (Good_Prefix)))
                    = A_Protected_Definition
                  then
                     Do_Report (K_Protected_Function);
                  else
                     Do_Report (K_Function);
                  end if;
                  return;

               when A_Function_Instantiation =>
                  -- cannot be protected
                  Do_Report (K_Function);
                  return;

               when A_Function_Body_Declaration
                  | A_Function_Body_Stub
                  =>
                  if Is_Subunit (Decl) then
                     Decl := Corresponding_Body_Stub (Decl);
                  end if;
                  Decl := Corresponding_Declaration (Decl);
                  if Is_Nil (Decl) then               -- Only when the function has no spec => cannot be protected
                     Do_Report (K_Function);
                     return;
                  end if;

               when A_Procedure_Declaration =>
                  if Definition_Kind (Enclosing_Element (Corresponding_Name_Declaration (Good_Prefix)))
                    = A_Protected_Definition
                  then
                     Do_Report (K_Protected_Procedure);
                  else
                     Do_Report (K_Procedure);
                  end if;
                  return;

               when A_Procedure_Instantiation =>
                  -- cannot be protected
                  Do_Report (K_Procedure);
                  return;

               when A_Procedure_Body_Declaration
                  | A_Procedure_Body_Stub
                  =>
                  if Is_Subunit (Decl) then
                     Decl := Corresponding_Body_Stub (Decl);
                  end if;
                  Decl := Corresponding_Declaration (Decl);
                  if Is_Nil (Decl) then
                     -- Only when the procedure has no spec => cannot be protected
                     Do_Report (K_Procedure);
                     return;
                  end if;

               when An_Ordinary_Type_Declaration =>
                  -- Prefix of 'Access is a type => self pointer
                  -- Not considered local for this rule
                  return;

               when An_Object_Renaming_Declaration
                  | A_Procedure_Renaming_Declaration
                  | A_Function_Renaming_Declaration
                  =>
                  Good_Prefix := Renamed_Entity (Decl);
                  exit On_Declarations;

               when others =>
                  Failure ("Unexpected declaration kind in Local_Access", Decl);
            end case;
         end loop On_Declarations;
      end loop On_Names;
   end Process_Attribute;

begin   -- Rules.Local_Access
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Use'Access,
                                     Command_CB     => Command'Access);
end Rules.Local_Access;
