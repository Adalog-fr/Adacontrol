----------------------------------------------------------------------
--  Rules.Array_Declarations - Package body                         --
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
  Asis.Definitions,
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

package body Rules.Array_Declarations is

   use Asis, Framework, Thick_Queries;

   type Array_Declaration_Names is (First, Max_Length);
   package Array_Declaration_Flag_Utilities  is new Framework.Language.Flag_Utilities (Array_Declaration_Names);

   type Usage is array (Array_Declaration_Names) of Rule_Types_Set;
   Rule_Used : Usage := (others => (others => False));
   Save_Used : Usage;

   Labels : array (Array_Declaration_Names, Rule_Types) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Values : array (Array_Declaration_Names, Rule_Types) of Biggest_Natural := (others => (others => 0));

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: "& Rule_Id);
      Array_Declaration_Flag_Utilities.Help_On_Flags (Header => "Parameter(1):");
      User_Message ("Parameter(2): for first      : required value of the lower bound");
      User_Message ("              for max_length : maximum allowed length of the array");
      User_Message ("Controls various sizes related to array types or objects declarations");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Framework.Language, Array_Declaration_Flag_Utilities, Ada.Strings.Wide_Unbounded;
      Stmt : Array_Declaration_Names;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "two parameters required");
      end if;

      Stmt := Get_Flag_Parameter (Allow_Any => False);
      if Rule_Used (Stmt) (Rule_Type) then
         Parameter_Error (Rule_Id, "rule already specified for " & Rule_Types'Wide_Image (Rule_Type));
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "two parameters required");
      end if;

      case Stmt is
         when First =>
            Values (Stmt, Rule_Type) := Get_Integer_Parameter;
         when Max_Length =>
            Values (Stmt, Rule_Type) := Get_Integer_Parameter (Min => 1);
      end case;

      Labels    (Stmt, Rule_Type):= To_Unbounded_Wide_String (Label);
      Rule_Used (Stmt)(Rule_Type):= True;
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

   ------------------------------
   -- Process_Array_Definition --
   ------------------------------

   procedure Process_Array_Definition (Definition : Asis.Definition) is
      use Ada.Strings.Wide_Unbounded;
      use Asis.Elements;
      use Framework.Reports;

      function Get_Bound_Location (Dim : Positive) return Location is
         use Asis.Definitions;
         use Utilities;
      begin
         if Definition_Kind (Definition) = A_Constraint then
            case Discrete_Range_Kind (Discrete_Ranges (Definition)(Dim)) is
               when A_Discrete_Subtype_Indication =>
                  return Get_Location
                    (Asis.Definitions.Subtype_Mark            --## rule line off Use_Subtype_Simple_Name
                       (Discrete_Ranges (Definition)(Dim)));  --   we don't want the selector
               when A_Discrete_Range_Attribute_Reference =>
                  return Get_Location (Range_Attribute (Discrete_Ranges (Definition)(Dim)));
               when A_Discrete_Simple_Expression_Range =>
                  return Get_Location (Lower_Bound (Discrete_Ranges (Definition)(Dim)));
               when Not_A_Discrete_Range =>
                  Failure ("not a discrete range");
            end case;
         else
            -- Array
            case Type_Kind (Definition) is
            when A_Constrained_Array_Definition =>
               return Get_Location (Discrete_Subtype_Definitions (Definition)(Dim));
            when An_Unconstrained_Array_Definition =>
               return Get_Location (Index_Subtype_Definitions (Definition)(Dim));
            when others =>
               Failure ("not an array definition");
            end case;
         end if;
      end Get_Bound_Location;

      procedure Process_First is
         Bounds : constant Asis.Element_List := Discrete_Constraining_Bounds (Definition);
      begin
         for B in Bounds'Range loop
            if B rem 2 = 1 then
               declare
                  Image : constant Wide_String := Static_Expression_Value_Image (Bounds (B));
                  Val   : Biggest_Int;
               begin
                  if Image /= "" then
                     Val := Biggest_Int'Wide_Value (Image);
                     if  Rule_Used (First) (Check) and Rule_Used (First) (Search) then
                        if Val /= Values (First, Check) and Val /= Values (First, Search) then
                           Report (Rule_Id,
                                   To_Wide_String (Labels (First, Check)),
                                   Check,
                                   Get_Bound_Location ((B+1)/2),
                                   "lower bound of array is not" & Biggest_Int'Wide_Image (Values (First, Check))
                                   & " or" & Biggest_Int'Wide_Image (Values (First, Search))
                                   & " (" & Biggest_Int'Wide_Image (Val) & ')');
                        elsif Val /= Values (First, Search) then
                           Report (Rule_Id,
                                   To_Wide_String (Labels (First, Search)),
                                   Search,
                                   Get_Bound_Location ((B+1)/2),
                                   "lower bound of array is not" & Biggest_Int'Wide_Image (Values (First, Search))
                                   & " (" & Biggest_Int'Wide_Image (Val) & ')');
                        end if;

                     elsif Rule_Used (First) (Check) and then Val /= Values (First, Check) then
                        Report (Rule_Id,
                                To_Wide_String (Labels (First, Check)),
                                Check,
                                Get_Bound_Location ((B+1)/2),
                                "lower bound of array is not" & Biggest_Int'Wide_Image (Values (First, Check))
                                & " (" & Biggest_Int'Wide_Image (Val) & ')');

                     elsif Rule_Used (First) (Search) and then Val /= Values (First, Search) then
                        Report (Rule_Id,
                                To_Wide_String (Labels (First, Search)),
                                Search,
                                Get_Bound_Location ((B+1)/2),
                                "lower bound of array is not" & Biggest_Int'Wide_Image (Values (First, Search))
                                & " (" & Biggest_Int'Wide_Image (Val) & ')');
                     end if;

                     if Rule_Used (First) (Count) and then Val /= Values (First, Count) then
                        Report (Rule_Id,
                                To_Wide_String (Labels (First, Count)),
                                Count,
                                Get_Bound_Location ((B+1)/2),
                                "");
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end Process_First;

      procedure Process_Max_Length is
         Lengths : constant Extended_Biggest_Natural_List := Discrete_Constraining_Lengths (Definition);
      begin
         for L in Lengths'Range loop
            if Lengths (L) /= Not_Static then
               if Rule_Used (Max_Length) (Check) and then Lengths (L) > Values (Max_Length, Check) then
                  Report (Rule_Id,
                          To_Wide_String (Labels (Max_Length, Check)),
                          Check,
                          Get_Bound_Location (L),
                          "array dimension is bigger than" & Biggest_Int'Wide_Image (Values (Max_Length, Check))
                          & " (" & Biggest_Int'Wide_Image (Lengths (L)) & ')');

               elsif Rule_Used (Max_Length) (Search) and then Lengths (L) > Values (Max_Length, Search) then
                  Report (Rule_Id,
                          To_Wide_String (Labels (Max_Length, Search)),
                          Search,
                          Get_Bound_Location (L),
                          "array dimension is bigger than" & Biggest_Int'Wide_Image (Values (Max_Length, Search))
                          & " (" & Biggest_Int'Wide_Image (Lengths (L)) & ')');
               end if;

               if Rule_Used (Max_Length) (Count) and then Lengths (L) > Values (Max_Length, Count) then
                  Report (Rule_Id,
                          To_Wide_String (Labels (Max_Length, Count)),
                          Count,
                          Get_Bound_Location (L),
                          "");
               end if;
            end if;
         end loop;
      end Process_Max_Length;

   begin
      if Rule_Used = (Array_Declaration_Names => (Rule_Types => False)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

       if Rule_Used (First) /= (Rule_Types => False) then
          Process_First;
       end if;

      if Rule_Used (Max_Length) /= (Rule_Types => False)
        and then Type_Kind (Definition) /= An_Unconstrained_Array_Definition
      then
         Process_Max_Length;
       end if;
   end Process_Array_Definition;

begin
   Rules_Manager.Register_Semantic (Rule_Id,
                                    Help    => Help'Access,
                                    Add_Use => Add_Use'Access,
                                    Command => Command'Access);
end Rules.Array_Declarations;
