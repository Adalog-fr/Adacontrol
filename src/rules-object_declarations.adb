----------------------------------------------------------------------
--  Rules.Object_Declarations - Package body                       --
--                                                                  --
--  This  software  is  (c)  CSEE  and Adalog  2004-2007.  The  Ada --
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

-- ASIS
with
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
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Object_Declarations is
   use Framework;

   type Subrules is (Min_Integer_Span);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Subrule_Set is array (Subrules) of Boolean;
   No_Rule : constant Subrule_Set := (others => False);
   Rule_Used : Subrule_Set := No_Rule;
   Save_Used : Subrule_Set;

   type Object_Kinds is (K_All, K_Variable, K_Constant);
   package Object_Kinds_Utilities is new Framework.Language.Modifier_Utilities (Object_Kinds, "K_");

   type Object_Context is new Basic_Rule_Context with
      record
         Min_Values : Thick_Queries.Biggest_Natural := 0;
      end record;
   Ctl_Contexts : array (Subrules, Object_Kinds, Control_Kinds) of Object_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Subrules_Flag_Utilities, Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags ("Parameter(1):");
      User_Message ("Parameter(2..)");
      User_Message ("   for Min_Integer_Span: [all|constant|variable] <value>");
      User_Message ("Control allowed forms of object declarations");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Subrules_Flag_Utilities, Object_Kinds_Utilities, Thick_Queries, Framework.Language;
      Subrule : Subrules;
      Ok      : Object_Kinds;
      Vc      : Object_Context;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "missing subrule name");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);

      case Subrule is
         when Min_Integer_Span =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "missing number of allowed values");
            end if;
            loop
               Ok := Get_Modifier (Required => False);
               Vc := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Get_Integer_Parameter (Min => 1));
               if Ok = K_All or Ok = K_Constant then
                  if Ctl_Contexts (Subrule, K_Constant, Ctl_Kind).Min_Values /= 0 then
                     Parameter_Error (Rule_Id, "rule already given for constants");
                  end if;
                  Ctl_Contexts (Subrule, K_Constant, Ctl_Kind) := Vc;
               end if;
               if Ok = K_All or Ok = K_Variable then
                  if Ctl_Contexts (Subrule, K_Variable, Ctl_Kind).Min_Values /= 0 then
                     Parameter_Error (Rule_Id, "rule already given for variables");
                  end if;
                  Ctl_Contexts (Subrule, K_Variable, Ctl_Kind) := Vc;
               end if;
               exit when not Parameter_Exists;
            end loop;
      end case;
      Rule_Used (Subrule) := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := No_Rule;
            for Sr in Subrules loop
               for Ok in Object_Kinds loop
                  for Rt in Control_Kinds loop
                     Ctl_Contexts (Sr, Ok, Rt).Min_Values := 0;
                  end loop;
               end loop;
            end loop;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Decl : in Asis.Declaration) is
      use Framework.Reports, Thick_Queries, Utilities;
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;

      Val      : Extended_Biggest_Natural;
      Def      : Asis.Definition;
      St_Name  : Asis.Expression;
      Type_Def : Asis.Declaration;
      Obj_Kind : Object_Kinds;
   begin
      if Rule_Used = No_Rule then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Check we have an object of an integer type
      Def := Object_Declaration_View (Decl);
      if Definition_Kind (Def) /= A_Subtype_Indication then
         -- anonymous array
         return;
      end if;
      St_Name := Subtype_Simple_Name (Def);
      if Expression_Kind (St_Name) = An_Attribute_Reference then
         case A4G_Bugs.Attribute_Kind (St_Name) is
            when A_Base_Attribute =>
               -- for the purpose of checking if it is an integer type, the prefix will do as well
               St_Name := Prefix (St_Name);
            when A_Class_Attribute =>
               -- Certainly not an integer type...
               return;
            when others =>
               Failure ("Bad attribute", St_Name);
         end case;
      end if;
      Type_Def := Type_Declaration_View (Corresponding_Name_Declaration (St_Name));
      if Type_Kind (Type_Def) not in A_Signed_Integer_Type_Definition .. A_Modular_Type_Definition then
         return;
      end if;

      if Declaration_Kind (Decl) = A_Constant_Declaration then
         Obj_Kind := K_Constant;
      else
         Obj_Kind := K_Variable;
      end if;

      -- Check values

      Val := Discrete_Constraining_Lengths (Decl) (1);

      if Val = Not_Static then
         return;
      end if;

      -- Note: Unspecified values of Range/Obj_Kind/Control contain 0, and Val is >= 0
      --       No problem in the following tests
      if Val < Ctl_Contexts (Min_Integer_Span, Obj_Kind, Check).Min_Values  then
         Report (Rule_Id,
                 Ctl_Contexts (Min_Integer_Span, Obj_Kind, Check),
                 Get_Location (Decl),
                 "integer object declaration has too few values ("
                 & Biggest_Int_Img (Val)
                 & ')');
      elsif Val < Ctl_Contexts (Min_Integer_Span, Obj_Kind, Search).Min_Values  then
         Report (Rule_Id,
                 Ctl_Contexts (Min_Integer_Span, Obj_Kind, Search),
                 Get_Location (Decl),
                 "integer object declaration has too few values ("
                 & Biggest_Int_Img (Val)
                 & ')');
      end if;

      if Val < Ctl_Contexts (Min_Integer_Span, Obj_Kind, Count).Min_Values  then
         Report (Rule_Id,
                 Ctl_Contexts (Min_Integer_Span, Obj_Kind, Count),
                 Get_Location (Decl),
                 "");
      end if;
   end Process_Declaration;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Object_Declarations;
