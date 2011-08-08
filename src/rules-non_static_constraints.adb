----------------------------------------------------------------------
--  Rules.Non_Static_Constraints - Package body                     --
--                                                                  --
--  This software  is (c) Adalog  2004-2005. The Ada  Controller is --
--  free software;  you can redistribute it and/or  modify it under --
--  terms of  the GNU  General Public License  as published  by the --
--  Free Software Foundation; either version 2, or (at your option) --
--  any later version.   This unit is distributed in  the hope that --
--  it will be  useful, but WITHOUT ANY WARRANTY;  without even the --
--  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
--  PURPOSE.  See the GNU  General Public License for more details. --
--  You  should have  received a  copy  of the  GNU General  Public --
--  License distributed  with this  program; see file  COPYING.  If --
--  not, write to  the Free Software Foundation, 59  Temple Place - --
--  Suite 330, Boston, MA 02111-1307, USA.                          --
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
  Asis.Definitions,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework,
  Framework.Reports,
  Framework.Rules_Manager,
  Framework.Language;


pragma Elaborate (Framework.Language);

package body Rules.Non_Static_Constraints is
   use Framework;

   -- Algorithm:
   --   Proceed with Process_Constrained_Array_Definition, Process_Discriminant or
   --   Process_Index_Constraint whether matching a constrained array definition, a
   --   discriminant constraint or an index constraint.
   --
   --   For each constraint or definition, we first retrieve the ranges.
   --   We then process differently on the constraint list we retrieved to check if
   --   any of the constraints cannot be statically determined.
   --   Reports are shown when matching a non-static constraint.
   --
   -- Note:
   --   When matching a constrained array definition or an index constraint, we
   --   first retrieve the ranges in different ways, but checking the "static-ity"
   --   is done in a common way.
   --

   type Available_Keyword is (K_Index, K_Discriminant);

   package Keyword_Flag_Utilities is new Framework.Language.Flag_Utilities (Available_Keyword, "K_");

   type Usage_Flags is array (Available_Keyword) of Boolean;

   Rule_Used  : Usage_Flags := (others => False);
   Save_Used  : Usage_Flags;
   Usage      : array (Available_Keyword) of Basic_Rule_Context;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
      use Keyword_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags (Header => "Parameter 1:", Footer => "(optional)");
      User_Message ("Control that index and discriminant constraints use only static expressions");
   end Help;


   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Framework.Language;
      use Keyword_Flag_Utilities;

      procedure Add_One (Key : Available_Keyword) is
      begin
         if Rule_Used (Key) then
            Parameter_Error ("Rule " & Rule_Id &
                             " can be specified only once for ""index""" &
                             " and once for ""discriminant"".");
         end if;

         Rule_Used (Key) := True;
         Usage (Key)     := Basic.New_Context (Rule_Use_Type, Label);
      end Add_One;

   begin
      if Parameter_Exists then
         loop
            Add_One (Get_Flag_Parameter (Allow_Any => False));
            exit when not Parameter_Exists;
         end loop;
      else
         for K in Available_Keyword loop
            Add_One (K);
         end loop;
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


   ------------------------
   -- Check_Static_Index --
   ------------------------

   procedure Check_Static_Index (Elem : Asis.Element; Constraint_List : Asis.Element_List) is
      use Framework.Reports, Thick_Queries;
   begin
      for I in Constraint_List'Range loop
         if Discrete_Constraining_Lengths (Constraint_List (I))(1) = Non_Static then
            Report (Rule_Id,
                    Usage (K_Index),
                    Get_Location (Elem),
                    "array definition non-statically constrained");
         end if;
      end loop;
   end Check_Static_Index;


   ------------------------------------------
   -- Process_Constrained_Array_Definition --
   ------------------------------------------

   procedure Process_Constrained_Array_Definition (Elem : Asis.Type_Definition) is
      use Asis.Definitions;
   begin
      if not Rule_Used (K_Index) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_Static_Index (Elem, Discrete_Subtype_Definitions (Elem));
   end Process_Constrained_Array_Definition;


   -------------------------------------
   -- Process_Discriminant_Constraint --
   -------------------------------------

   procedure Process_Discriminant_Constraint (Elem : Asis.Constraint) is
      use Asis.Definitions, Asis.Expressions;
      use Framework.Reports, Thick_Queries;
   begin
      if not Rule_Used (K_Discriminant) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Constraint_List : constant Asis.Discriminant_Association_List := Discriminant_Associations (Elem);
      begin
         for I in Constraint_List'Range loop
            if Static_Expression_Value_Image (Discriminant_Expression (Constraint_List (I))) = "" then
               Report (Rule_Id,
                       Usage (K_Discriminant),
                       Get_Location (Elem),
                       "discriminant non-statically constrained");
            end if;
         end loop;
      end;
   end Process_Discriminant_Constraint;


   ----------------------------
   -- Process_Discrete_Range --
   ----------------------------

   procedure Process_Index_Constraint (Elem : Asis.Discrete_Range) is
      use Asis.Definitions;
   begin
      if not Rule_Used (K_Index) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_Static_Index (Elem, Discrete_Ranges (Elem));
   end Process_Index_Constraint;


begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.Non_Static_Constraints;
