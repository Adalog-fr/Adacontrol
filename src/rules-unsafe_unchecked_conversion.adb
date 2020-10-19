----------------------------------------------------------------------
--  Rules.Unsafe_Unchecked_Conversion - Package body                --
--                                                                  --
--  This software is (c) SAGEM DS and Adalog 2004-2006.             --
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

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

package body Rules.Unsafe_Unchecked_Conversion is
   use Framework, Framework.Control_Manager;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;
   Context   : Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control unsafe usage of Unchecked_Conversion");
      User_Message;
      User_Message ("Parameter(s): none");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;

   begin
      if Rule_Used then
         Parameter_Error (Rule_Id, "rule already specified");
      end if;

      if Parameter_Exists then
         Parameter_Error (Rule_Id, "no parameter for rule");
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
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   --------------------------
   -- Process_Instantation --
   --------------------------

   procedure Process_Instantiation (Instantiation : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Thick_Queries, Utilities;

      Source, Target : Asis.Expression;
      S_Size, T_Size : Biggest_Int;
      Assocs : Asis.Association_List (1..2);

      Not_Specified : constant Biggest_Int := -1;

      function Size_Value (Type_Name : Asis.Expression) return Biggest_Int is
         use Asis.Elements;
         Expr : Asis.Expression;
      begin
         Expr := Attribute_Clause_Expression (A_Size_Attribute, Type_Name);
         if Is_Nil (Expr) then
            return Not_Specified;
         end if;

         declare
            Val : constant Extended_Biggest_Natural := Discrete_Static_Expression_Value (Expr);
         begin
            if Val = Not_Static then
               Uncheckable (Rule_Id,
                            False_Positive,
                            Get_Location (Type_Name),
                            "unable to evaluate size clause value for " & Name_Image (Type_Name));
               return Not_Specified;
            else
               return Val;
            end if;
         end;
      end Size_Value;

      Reported : Boolean := False;
   begin  -- Process_Instantiation
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Name_Image : constant Wide_String := To_Upper (Full_Name_Image
                                                        (Ultimate_Name
                                                         (Generic_Unit_Name (Instantiation))));
      begin
         if Name_Image not in "ADA.UNCHECKED_CONVERSION" | "UNCHECKED_CONVERSION" then
            -- In Gnat, Unchecked_Conversion is not a renaming of Ada.Unchecked_Conversion
            return;
         end if;
      end;

      Assocs := Generic_Actual_Part (Instantiation);
      Source := Simple_Name (Actual_Parameter (Assocs (1)));
      Target := Simple_Name (Actual_Parameter (Assocs (2)));

      if Is_Class_Wide_Subtype (Source) then
         Report (Rule_Id,
                 Context,
                 Get_Location (Source),
                 "class-wide type given for Source");
         Reported := True;
      end if;
      if Is_Class_Wide_Subtype (Target) then
         Report (Rule_Id,
                 Context,
                 Get_Location (Target),
                 "class-wide type given for Target");
         Reported := True;
      end if;

      S_Size := Size_Value (Source);
      T_Size := Size_Value (Target);

      if S_Size = Not_Specified then
         Report (Rule_Id,
                 Context,
                 Get_Location (Source),
                 "no size clause given for Source");
         Reported := True;
      end if;
      if T_Size = Not_Specified then
         Report (Rule_Id,
                 Context,
                 Get_Location (Target),
                 "no size clause given for Target");
         Reported := True;
      end if;

      if Reported then
         return;
      end if;

      -- Here, S_Size and T_Size are known
      if S_Size /= T_Size then
         Report (Rule_Id,
                 Context,
                 Get_Location (Source),
                 "Source size (" & Biggest_Int_Img (S_Size) & ") /= Target size (" & Biggest_Int_Img (T_Size) & ')');
      end if;

   end Process_Instantiation;

begin  -- Rules.Unsafe_Unchecked_Conversion
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Unsafe_Unchecked_Conversion;
