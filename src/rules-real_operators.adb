----------------------------------------------------------------------
--  Rules.Real_Operators - Package body                             --
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

-- Asis
with
  Asis,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Real_Operators is
   use Framework;

   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Rule_Type  : Rule_Types;
   Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): None");
      User_Message ("Control occurrences of = or /= operators with real types");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if Parameter_Exists then
         Parameter_Error ("No parameter for rule " & Rule_Id);
      end if;

      if Rule_Used then
         Parameter_Error (Rule_Id & ": this rule can be specified only once");
      else
         Rule_Type  := Rule_Use_Type;
         Rule_Label := To_Unbounded_Wide_String (Label);
         Rule_Used  := True;
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
            Rule_Used := False;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ---------------------------
   -- Process_Function_Call --
   ---------------------------

   procedure Process_Function_Call (Call : in Asis.Expression) is
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Elements,
          Asis.Expressions, Framework.Reports, Thick_Queries;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Ruler calls us for all operators.  Check type of operator.
      case Operator_Kind (Prefix (Call)) is
         when An_Equal_Operator
           | A_Not_Equal_Operator
           =>
            -- Now check the context in which the operator is used and report
            -- errors according to the following rules

            -- 1) if    the left  parameter is not universal, print the message
            --    according to it
            --
            -- 2) elsif the right parameter is not universal, print the message
            --    according to it
            --
            -- 3) else we must be in a context like: if 0.0 = 1.0 theni ....

            declare
               Parsed_First_Parameter : Boolean := False;
               F : constant Asis.Association_List := Function_Call_Parameters (Call);
            begin
               Parameter_Loop: for I in F'RANGE loop
                  declare
                     P : constant Asis.Expression  := Actual_Parameter (F (I));
                     T : constant Asis.Declaration := Ultimate_Expression_Type (P);
                  begin
                     case Type_Kind (T) is
                        when A_Root_Type_Definition =>
                           case Root_Type_Kind (T) is
                              when A_Root_Real_Definition => -- 3.4.1(8)
                                 Report
                                 (Rule_Id,
                                  To_Wide_String (Rule_Label),
                                  Rule_Type,
                                  Get_Location (Call),
                                  "equality or inequality with Root Real !!!");
                              when A_Universal_Real_Definition => -- 3.4.1(6)
                                 if Parsed_First_Parameter then
                                    Report
                                    (Rule_Id,
                                     To_Wide_String (Rule_Label),
                                     Rule_Type,
                                     Get_Location (Call),
                                     "equality or inequality with two Universal Real constants !!!");
                                 else
                                    Parsed_First_Parameter := True;
                                 end if;
                              when others =>
                                 null;
                           end case;
                        when A_Floating_Point_Definition => -- 3.5.7(2)
                           Report
                           (Rule_Id,
                            To_Wide_String (Rule_Label),
                            Rule_Type,
                            Get_Location (Call),
                            "equality or inequality with Floating Point");
                            exit Parameter_Loop;
                        when An_Ordinary_Fixed_Point_Definition => -- 3.5.9(3)
                           Report
                           (Rule_Id,
                            To_Wide_String (Rule_Label),
                            Rule_Type,
                            Get_Location (Call),
                            "equality or inequality with Binary Fixed Point");
                            exit Parameter_Loop;
                         when A_Decimal_Fixed_Point_Definition => -- 3.5.9(4)
                           Report
                           (Rule_Id,
                            To_Wide_String (Rule_Label),
                            Rule_Type,
                            Get_Location (Call),
                            "equality or inequality with Decimal Fixed Point");
                            exit Parameter_Loop;
                       when others =>
                           null;
                     end case;
                  end;
               end loop Parameter_Loop;
            end;

         when others =>
            -- Including Not_An_Operator
            null;
      end case;
   end Process_Function_Call;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access);
end Rules.Real_Operators;
