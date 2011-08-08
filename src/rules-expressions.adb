----------------------------------------------------------------------
--  Rules.Expressions - Package body                                --
--                                                                  --
--  This software  is (c) SAGEM DS and  Adalog  2004-2005.  The Ada --
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
pragma Elaborate (Framework.Language);

package body Rules.Expressions is
   use Framework;

   type Expression_Names is (E_And,           E_And_Then,      E_Array_Others, E_Or, E_Or_Else,
                             E_Real_Equality, E_Record_Others, E_Slice,        E_Xor);

   package Usage_Flags_Utilities is new Framework.Language.Flag_Utilities (Expression_Names, "E_");
   use Usage_Flags_Utilities;

   type Usage_Flags is array (Expression_Names) of Boolean;
   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;
   Usage     : array (Expression_Names) of Basic_Rule_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Help_On_Flags (Header => "Parameter (s):");
      User_Message ("Control occurrences of Ada expressions");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language;
      Expr : Expression_Names;

   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "At least one parameter required");
      end if;

      while Parameter_Exists loop
         Expr := Get_Flag_Parameter (Allow_Any => False);
         if Rule_Used (Expr) then
            Parameter_Error (Rule_Id, "Expression already given: " & Image (Expr));
         end if;

         Rule_Used (Expr) := True;
         Usage (Expr)     := Basic.New_Context (Rule_Type, Label);
      end loop;
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

   ---------------
   -- Do_Report --
   ---------------

   procedure Do_Report (Expr : Expression_Names; Loc : Location) is
      use Framework.Reports;
   begin
      if not Rule_Used (Expr) then
         return;
      end if;

      Report (Rule_Id,
              Usage (Expr),
              Loc,
              "use of expression """ & Image (Expr) & '"');
   end Do_Report;


   ---------------------------
   -- Process_Function_Call --
   ---------------------------

   procedure Process_Function_Call (Call : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Thick_Queries;
   begin
      case Operator_Kind (Prefix (Call)) is
         when An_Equal_Operator
           | A_Not_Equal_Operator
              =>
            if not Rule_Used (E_Real_Equality) then
               return;
            end if;

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
            Parameter_Loop:
               for I in F'Range loop
                  declare
                     P : constant Asis.Expression := Actual_Parameter (F (I));
                     T : constant Asis.Definition := Ultimate_Expression_Type (P);
                  begin
                     case Type_Kind (T) is
                        when A_Root_Type_Definition =>
                           case Root_Type_Kind (T) is
                              when A_Root_Real_Definition => -- 3.4.1(8)
                                 Report
                                 (Rule_Id,
                                  Usage (E_Real_Equality),
                                  Get_Location (Prefix (Call)),
                                  "equality or inequality with Root Real !!!");
                              when A_Universal_Real_Definition => -- 3.4.1(6)
                                 if Parsed_First_Parameter then
                                    Report
                                    (Rule_Id,
                                     Usage (E_Real_Equality),
                                     Get_Location (Prefix (Call)),
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
                            Usage (E_Real_Equality),
                            Get_Location (Prefix (Call)),
                            "equality or inequality with Floating Point");
                            exit Parameter_Loop;
                        when An_Ordinary_Fixed_Point_Definition => -- 3.5.9(3)
                           Report
                           (Rule_Id,
                            Usage (E_Real_Equality),
                            Get_Location (Prefix (Call)),
                            "equality or inequality with Ordinary Fixed Point");
                            exit Parameter_Loop;
                         when A_Decimal_Fixed_Point_Definition => -- 3.5.9(4)
                           Report
                           (Rule_Id,
                            Usage (E_Real_Equality),
                            Get_Location (Prefix (Call)),
                            "equality or inequality with Decimal Fixed Point");
                            exit Parameter_Loop;
                       when others =>
                           null;
                     end case;
                  end;
               end loop Parameter_Loop;
            end;

         when An_And_Operator =>
            Do_Report (E_And, Get_Location (Prefix (Call)));

         when An_Or_Operator =>
            Do_Report (E_Or, Get_Location (Prefix (Call)));

         when An_Xor_Operator =>
            Do_Report (E_Xor, Get_Location (Prefix (Call)));

         when others =>
            -- Including Not_An_Operator
            null;
      end case;
   end Process_Function_Call;

   ------------------------
   -- Process_Expression --
   ------------------------

   procedure Process_Expression (Expression : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;
   begin
      if Rule_Used = (Expression_Names => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Expression_Kind (Expression) is
         when A_Function_Call =>
            Process_Function_Call (Expression);

         when A_Slice =>
            Do_Report (E_Slice, Get_Location (Slice_Range (Expression)));

         when An_And_Then_Short_Circuit =>
            Do_Report (E_And_Then, Get_Next_Word_Location (Short_Circuit_Operation_Left_Expression(Expression)));

         when An_Or_Else_Short_Circuit =>
            Do_Report (E_Or_Else, Get_Next_Word_Location (Short_Circuit_Operation_Left_Expression(Expression)));

         when A_Named_Array_Aggregate
            | A_Positional_Array_Aggregate
              =>
            declare
               Assocs  : constant Asis.Association_List := Array_Component_Associations (Expression);
               Choices : constant Asis.Expression_List  := Array_Component_Choices (Assocs (Assocs'Last));
            begin
               if Is_Nil (Choices) then
                  return;
               end if;
               if Definition_Kind (Choices (Choices'First)) = An_Others_Choice then
                  Do_Report (E_Array_Others, Get_Location (Choices (Choices'First)));
               end if;
            end;

         when A_Record_Aggregate
            | An_Extension_Aggregate
              =>
            declare
               Assocs  : constant Asis.Association_List := Record_Component_Associations (Expression);
            begin
               if Is_Nil (Assocs) then
                  -- (null record)
                  return;
               end if;

               declare
                  Choices : constant Asis.Expression_List  := Record_Component_Choices (Assocs (Assocs'Last));
               begin
                  if Is_Nil (Choices) then
                     return;
                  end if;
                  if Definition_Kind (Choices (Choices'First)) = An_Others_Choice then
                     Do_Report (E_Record_Others, Get_Location (Choices (Choices'First)));
                  end if;
               end;
            end;

         when others =>
            null;
      end case;
   end Process_Expression;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.Expressions;
