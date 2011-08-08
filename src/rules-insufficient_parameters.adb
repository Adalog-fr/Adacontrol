----------------------------------------------------------------------
--  Rules.Insufficient_Parameters - Package body                    --
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

package body Rules.Insufficient_Parameters is
   use Framework;

   Rule_Used : Rule_Types_Set := (others => False);
   Save_Used : Rule_Types_Set;

   Rule_Counts : array (Rule_Types) of Natural;
   Rule_Labels : array (Rule_Types) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type Insufficients_Context is new Root_Context with
      record
         Insufficients : Rule_Types_Set;
      end record;

   Insufficient_Types : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(1): Allowed number of ""insufficient"" parameters");
      User_Message ("Parameter(2..N): Enumeration type names whose values are insufficient");
      User_Message ("Control calls where absence of named notation can lead to confusion");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;
   begin
      if Rule_Used (Rule_Type) then
         Parameter_Error (Rule_Id, "rule can be specified only once for each of check, search and count");
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "number of ""insufficient"" parameters required");
      end if;
      Rule_Counts (Rule_Type) := Get_Integer_Parameter (Min => 0);

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
            Value  : Rule_Types_Set := (others => False);
         begin
            Value (Rule_Type) := True;
            Associate (Insufficient_Types,
                       Entity,
                       Insufficients_Context'(Insufficients => Value));
         exception
            when Already_In_Store =>
               Value := Insufficients_Context (Association (Insufficient_Types, Entity)).Insufficients;
               Value (Rule_Type) := True;
               Update (Insufficient_Types, Insufficients_Context'(Insufficients => Value));
         end;
      end loop;

      Rule_Labels (Rule_Type) := To_Unbounded_Wide_String (Label);
      Rule_Used   (Rule_Type) := True;
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
            Clear (Insufficient_Types);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Insufficient_Types);
   end Prepare;

   ---------------------
   -- Is_Insufficient --
   ---------------------

   function Is_Insufficient (Expr : Asis.Expression) return Rule_Types_Set is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Thick_Queries, Utilities;
      Pref : Asis.Expression;
   begin
      case Expression_Kind (Expr) is
         when Not_An_Expression
           | An_Operator_Symbol
           =>
           Failure ("Unexpected expression");

         when An_Integer_Literal
           | A_Real_Literal
           | A_Null_Literal
           =>
            return (others => True);

         when An_Attribute_Reference =>
            return (others => False);

         when A_String_Literal
           | A_Character_Literal
           | An_Identifier
           | An_Explicit_Dereference
           | An_Indexed_Component
           | A_Slice
           | A_Record_Aggregate
           | An_Extension_Aggregate
           | A_Positional_Array_Aggregate
           | A_Named_Array_Aggregate
           | An_Allocation_From_Subtype
           | An_Allocation_From_Qualified_Expression
           =>
           return (others => False);

         when A_Selected_Component =>
            return Is_Insufficient (Selector (Expr));

         when An_Enumeration_Literal =>
            declare
               C : constant Root_Context'Class := Matching_Context (Insufficient_Types,
                                                                    Names (A4G_Bugs.Corresponding_Expression_Type
                                                                           (Expr)) (1));
            begin
               if C = No_Matching_Context then
                  return (others => False);
               else
                  return Insufficients_Context (C).Insufficients;
               end if;
            end;

         when A_Function_Call =>
            Pref := Prefix (Expr);
            if Expression_Kind (Pref) = A_Selected_Component then
               Pref := Selector (Pref);
            end if;
            if Expression_Kind (Pref) = An_Operator_Symbol then
               declare
                  Params : constant Asis.Association_List := Actual_Parameters (Expr);
               begin
                  case Operator_Kind (Pref) is
                     when Not_An_Operator =>
                        Failure ("not an operator");
                     when A_Unary_Plus_Operator
                       | A_Unary_Minus_Operator
                       | An_Abs_Operator
                       | A_Not_Operator
                       =>
                        -- Unary operators
                        return Is_Insufficient (Actual_Parameter (Params (1)));
                     when others =>
                        -- Binary operators
                        return Is_Insufficient (Actual_Parameter (Params (1)))
                           and Is_Insufficient (Actual_Parameter (Params (2)));
                  end case;
               end;
            else
               return (others => False);
            end if;

         when An_And_Then_Short_Circuit
           | An_Or_Else_Short_Circuit
           =>
            return Is_Insufficient (Short_Circuit_Operation_Left_Expression (Expr))
               and Is_Insufficient (Short_Circuit_Operation_Right_Expression (Expr));

         when An_In_Range_Membership_Test
           | A_Not_In_Range_Membership_Test
           | An_In_Type_Membership_Test
           | A_Not_In_Type_Membership_Test
           =>
            return (others => False); --TBSL

         when A_Parenthesized_Expression =>
            return Is_Insufficient (Expression_Parenthesized (Expr));

         when A_Type_Conversion
           | A_Qualified_Expression
           =>
            return Is_Insufficient (Converted_Or_Qualified_Expression (Expr));

      end case;
   end Is_Insufficient;


   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Element : in Asis.Element) is
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Thick_Queries, Utilities;
   begin
      if Rule_Used = (Rule_Types => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Expression_Kind (Element) = A_Function_Call and then not Is_Prefix_Call (Element) then
         -- Infix operator => cannot use named notation
         return;
      end if;

      declare
         Parameters       : constant Asis.Association_List := Actual_Parameters (Element);
         Nb_Insufficients : array (Rule_Types) of Natural := (others => 0);
         Insufficiencies  : Rule_Types_Set;
      begin
         for I in Parameters'Range loop
            if Is_Nil (Formal_Parameter (Parameters (I))) then
               -- Positional notation
               Insufficiencies := Is_Insufficient (Actual_Parameter (Parameters (I)));
               for T in Rule_Types loop
                  if Insufficiencies (T) then
                     Nb_Insufficients (T) := Nb_Insufficients (T) + 1;
                  end if;
               end loop;
            else
               -- Named notation, no more positional behind
               exit;
            end if;
         end loop;

         if Rule_Used (Check) and then Nb_Insufficients (Check) > Rule_Counts (Check) then
            Report (Rule_Id,
                    To_Wide_String (Rule_Labels (Check)),
                    Check,
                    Get_Location (Element),
                    "call has "
                    & Integer_Img (Nb_Insufficients (Check))
                    & " ""insufficient"" parameters (maximum "
                    & Integer_Img (Rule_Counts (Check))
                    & ')');
         elsif Rule_Used (Search) and then Nb_Insufficients (Search) > Rule_Counts (Search) then
            Report (Rule_Id,
                    To_Wide_String (Rule_Labels (Search)),
                    Search,
                    Get_Location (Element),
                    "call has "
                    & Integer_Img (Nb_Insufficients (Search))
                    & " ""insufficient"" parameters (maximum "
                    & Integer_Img (Rule_Counts (Search))
                    & ')');
         end if;

         if Rule_Used (Count) and then Nb_Insufficients (Count) > Rule_Counts (Count) then
            Report (Rule_Id,
                    To_Wide_String (Rule_Labels (Count)),
                    Count,
                    Get_Location (Element),
                    "");
         end if;
      end;

   end Process_Call;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access,
                                              Prepare => Prepare'Access);
end Rules.Insufficient_Parameters;
