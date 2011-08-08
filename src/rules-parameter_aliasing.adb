----------------------------------------------------------------------
--  Rules.Parameter_Aliasing - Package body                         --
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
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Parameter_Aliasing is
   use Framework, Utilities;

   --  Algorithm:
   --  The heart of the algorithm is the Split procedure. It takes the expression corresponding to
   --  an [in] out parameter, and splits it into the true variable on one side, a string that represents
   --  the various selectors and/or indexings applied to the variable on the other side.
   --  Split is applied between any pair of [in] out parameters (conveniently called Left and Right)
   --  When applied to the Left parameter, any indexing in the string of selectors/indexing is replaced by
   --  '1'.
   --  When applied to the Right parameter, any indexing in the string of selectors/indexing is replaced by
   --  '2' if we are searching for "certain" aliasing, and by '1' if we are searching for "possible" or
   --  "unlikely" aliasing.
   --  There is aliasing if the variables are the same and the strings are the same, or one is identical to
   --  the beginning of the other one.
   --
   --  The technique for replacing the indexing is actually "assume the best" for "certain" (we assume that
   --  indexings are different) and "assume the worst" for "possible" and "unlikely" (we assume that indexings
   --  are the same).
   --
   --  We attempt however to diagnose simple cases of static indexing. If *all* indexings for both Left and
   --  Right are integer litterals or enumeration litterals (we know that as a result of Split), we split
   --  the variables again, but this time we replace all indexings by the value of the index. This way,
   --  the strings will differ if the indexings are statically different.
   --
   --  The situation is somewhat complicated by access types. We keep track of the rightmost dereference.
   --  For "certain" and "possible", we assume the best, i.e. that dereferences designate different objects,
   --  and therefore compare only the part before the dereference.
   --  For "unlikely", we assume the worst (that the dereferences allways designate the same object).
   --  Currently, we don't take into account the type of the dereferenced object. There is still room for
   --  improvements...


   -- Order of declaration is important:
   type Rule_Detail is (Certain, Possible, Unlikely);
   type Usage is array (Rule_Detail) of Boolean;
   Rule_Used  : Usage := (others => False);
   Save_Used  : Usage;
   Rule_Type  : array (Rule_Detail) of Rule_Types;
   Rule_Label : array (Rule_Detail) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter 1: certain | possible | unlikely (optional, default=certain)");
      User_Message ("Control subprogram or entry calls where the same variable is given");
      User_Message ("for more than one [in] out parameter.");
      User_Message ("This rule can detect non-straightforward aliasing cases, see doc for details");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

      function Get_Detail_Parameter is new Get_Flag_Parameter (Flags     => Rule_Detail,
                                                               Allow_Any => False);
      Detail  : Rule_Detail := Certain;
   begin
      if Parameter_Exists then
         Detail := Get_Detail_Parameter;
      end if;

      if Rule_Used (Detail) then
         Parameter_Error ("Rule " & Rule_Id & " can be called only once for ""Certain"","
                            & " once for ""Possible"","
                            & " and once for ""Unlikely""");
      end if;

      if Parameter_Exists then
         Parameter_Error ("Only one parameter for rule " & Rule_Id);
      end if;

      Rule_Type  (Detail) := Rule_Use_Type;
      Rule_Label (Detail) := To_Unbounded_Wide_String (Label);
      Rule_Used  (Detail) := True;
   end Add_Use;


   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := (others => False);
            Rule_Label := (others => Null_Unbounded_Wide_String);
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
      -- If weaker checks have been specified, force them for stronger ones
   begin
      if Rule_Used (Unlikely) and not Rule_Used (Possible) then
         Rule_Used  (Possible) := True;
         Rule_Type  (Possible) := Rule_Type  (Unlikely);
         Rule_Label (Possible) := Rule_Label (Unlikely);
      end if;
      if Rule_Used (Possible) and not Rule_Used (Certain) then
         Rule_Used  (Certain) := True;
         Rule_Type  (Certain) := Rule_Type  (Possible);
         Rule_Label (Certain) := Rule_Label (Possible);
      end if;
   end Prepare;


   ------------------
   -- Process_Call --
   ------------------

   type Parameters_Table is array (Asis.List_Index range <>) of Asis.List_Index;

   -- NB:
   -- Some of the algorithms in this procedure are a bit convoluted, because we avoid
   -- using normalized formals and actuals list, which are UNIMPLEMENTED in some versions
   -- of ASIS-for-Gnat.
   -- Some rewriting might be in order when the problem goes away...

   procedure Process_Call (Call : in Asis.Statement) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Reports, Ada.Strings.Wide_Unbounded;

      function Are_Aliased (Left, Right : Asis.Expression;
                            Detail      : Rule_Detail) return Boolean
      is
         -- Determines if there is aliasing (Certain, Possible or Unlikely) between Left and Right.
         -- Left and Right are the actuals to an [in] out parameter, they are therefore
         -- variables, and they can't be defaulted parameters.
         --
         -- Case 1: None of the expressions includes (explicit or implicit) dereferences
         --    There is aliasing if both are exactly the same, or one is a subcomponent of
         --    the other.
         --    - Certain if there are no indexed components
         --    - Possible otherwise
         -- Case 2: At least one of the expressions includes (explicit or implicit) dereferences
         --    The "true" full variable (not considering subcomponents) is the target of the
         --    rightmost dereference.
         --    - There is aliasing if everything appearing left of this rightmost dereference
         --      is identical in Left and Right
         --      - Certain if there are no indexed components nor function calls
         --      - Possible otherwise
         --    - Otherwise, aliasing is Unlikely.

         type Inx_State is (None, Static, Dynamic);

         L_Variable  : Asis.Definition;
         L_Selectors : Unbounded_Wide_String;
         L_Deref     : Natural;
         L_Inx_Found : Inx_State;
         R_Variable  : Asis.Definition;
         R_Selectors : Unbounded_Wide_String;
         R_Deref     : Natural;
         R_Inx_Found : Inx_State;

         procedure Split (Name       : in     Asis.Expression;
                          Variable   :    out Asis.Definition;
                          Selectors  :    out Unbounded_Wide_String;
                          Last_Deref :    out Natural;
                          Inx_Found  :    out Inx_State;
                          Indicator  : in     Wide_Character;
                          Static_Inx : in     Boolean)
         is
            -- Given the original Name (possibly cleaned-up from a view conversion):
            -- Returns in Variable the true variable declaration (after following
            --   possible renamings)
            --
            -- Returns in Selectors the string of all selectors. A '.' is added in the end to
            --   avoid the matching of fields where one is identical to the beginning of the other.
            --   However, if there are any indexed components, the string is truncated at the
            --   selected component. Indicator is an arbitrary character so that indexed components
            --   from Left and Right are not equal. Assuming Indicator is '1':
            --   V.X.Y    => "X.Y."
            --   V.X(3).Y => "X(1)"
            --   V        => ""
            --   If Static is True, the Name is assumed to contain only litterals for the indexing
            --   of arrays, and the actual value is used in place of the indicator.
            --
            -- Returns in Last_Deref the position of the last character of the ".all"
            -- corresponding to the right-most dereference if any, or 0.
            -- Returns True in Static_Inx if some indexing were found, but they are all integer or
            -- enumeration litterals; returns False otherwise.
            use Thick_Queries;

            function Build_Indicator (Expr : Asis.Expression) return Wide_String is
               -- Returns the indicator for indexed expressions and slices
               -- If the expression is an enumeration or integer litteral, we can use a
               -- (normalized) representation as indicator; this will enable us to not report
               -- aliasing between X(1) and X(2).
               -- For anything else, return the provided Indicator, prepended with a '_' to
               -- distinguish from an allowed value.
               -- This function can be made more clever in the future if we can recognize more
               -- cases of static expressions.
               Good_Expr: Asis.Expression;
            begin
               if Expression_Kind (Expr) = A_Selected_Component then
                  Good_Expr := Selector (Expr);
               else
                  Good_Expr := Expr;
               end if;

               if Static_Inx then
                  case Expression_Kind (Good_Expr) is
                     when An_Integer_Literal =>
                        -- We make a round-trip through Value/Image below for the case of the naughty
                        -- user who wrote something like P(Tab (10#1#), Tab (1)).
                        -- The indicators must be the same!
                        return Asis_Integer'Wide_Image (Asis_Integer'Wide_Value (Value_Image (Good_Expr)));
                     when An_Enumeration_Literal =>
                        return To_Upper (Name_Image (Good_Expr));
                     when others =>
                        Failure ("Non static index in static Build_Indicator");
                  end case;
               else
                  case Expression_Kind (Good_Expr) is
                     when An_Integer_Literal
                       | An_Enumeration_Literal =>
                        if Inx_Found /= Dynamic then
                           Inx_Found := Static;
                        end if;
                     when others =>
                        Inx_Found := Dynamic;
                  end case;
                  return (1 => Indicator);
               end if;
            end Build_Indicator;

            procedure Add_Selector (Sel : Wide_String) is
            begin
               Selectors := Sel & Selectors;
               if Last_Deref /= 0 then
                  Last_Deref := Last_Deref + Sel'Length;
               end if;
            end Add_Selector;

            E          : Asis.Element := Name;
            Temp_Sel   : Unbounded_Wide_String;
            Temp_Deref : Natural;
            Temp_Found : Inx_State;
            Variable_Enclosing : Asis.Element;
         begin   -- Split
            Last_Deref := 0;
            Inx_Found  := None;

            Selectors := Null_Unbounded_Wide_String;
            loop
               case Expression_Kind (E) is
                  when An_Identifier =>
                     exit;

                  when A_Selected_Component =>
                     case Declaration_Kind (Corresponding_Name_Declaration (Selector (E))) is
                        when A_Component_Declaration | A_Discriminant_Specification =>
                           -- It's a record field, a protected type field...
                           Add_Selector (To_Upper (Name_Image (Selector (E))) & '.');
                           E := Prefix (E);
                        when A_Variable_Declaration | An_Object_Renaming_Declaration =>
                           -- Its a Pack.Var selector
                           E := Selector (E);
                           exit;
                        when others =>
                           Failure ("Wrong selected component", E);
                     end case;

                  when An_Indexed_Component =>
                     Add_Selector (")");
                     declare
                        Indexers : constant Asis.Expression_List := Index_Expressions (E);
                     begin
                        Add_Selector (Build_Indicator (Indexers (Indexers'Last)));
                        for I in reverse Indexers'First .. Indexers'Last - 1 loop
                           Add_Selector (Build_Indicator (Indexers (I)) & ',');
                        end loop;
                     end;
                     Add_Selector ("(");
                     E := Prefix (E);

                  when A_Slice =>
                     -- Well, it could be the whole object as well...
                     -- Simply ignore the slice
                     -- (Too complicated to check for static matching)
                     E := Prefix (E);

                  when A_Function_Call =>
                     --  a Function_Call can appear only as the first
                     --  element, and if it returns an access value,
                     --  or a composite object used for one of its
                     --  access subcomponents.
                     Add_Selector("_CALL_" & Indicator & '.');
                     E := Prefix (E);
                     if Expression_Kind (E) = A_Selected_Component then
                        E := Selector (E);
                     end if;
                     exit;

                  when An_Explicit_Dereference =>
                     -- "all." will be added below, since the prefix is necessarily
                     -- of an access type
                     E := Prefix (E);

                  when A_Type_Conversion =>
                     E := Converted_Or_Qualified_Expression (E);

                  when others =>
                     Failure ("Wrong variable name", E);
               end case;

               -- Add a "all." if the *type* is an access type
               -- This allows explicit and implicit dereferences to match
               if Expression_Type_Kind (E) = An_Access_Type_Definition then
                  Add_Selector ("all.");
                  if Last_Deref = 0 then
                     Last_Deref := 3;  -- Points to the last character of "all"
                  end if;
               end if;
            end loop;

            -- Return the "true" definion of Variable, after following all renamings
            -- But the renaming can be a complicated expression like:
            -- A : T renames Rec.X.Y(3);
            Variable := Corresponding_Name_Definition (E);
            loop
               Variable_Enclosing := Enclosing_Element (Variable);
               exit when Declaration_Kind (Variable_Enclosing) not in A_Renaming_Declaration;
               Split (Name       => Renamed_Entity (Variable_Enclosing),
                      Variable   => Variable,
                      Selectors  => Temp_Sel,
                      Last_Deref => Temp_Deref,
                      Inx_Found  => Temp_Found,
                      Indicator  => Indicator,
                      Static_Inx => Static_Inx);
               Add_Selector (To_Wide_String (Temp_Sel));
               if Last_Deref = 0 then
                  Last_Deref := Temp_Deref;
               end if;
               Inx_Found := Inx_State'Max (Inx_Found, Temp_Found);
            end loop;
         end Split;

         R_Indicator : Wide_Character;
      begin   -- Are_Aliased
         Split (Left, L_Variable, L_Selectors, L_Deref, L_Inx_Found,
                Indicator  => '1',
                Static_Inx => False);

         case Detail is
            when Certain =>
               R_Indicator := '2';
            when Possible | Unlikely =>
               -- Use the same indicator as for Left
               -- => all indexings and function calls will match
               R_Indicator := '1';
         end case;
         Split (Right, R_Variable, R_Selectors, R_Deref, R_Inx_Found,
                Indicator  => R_Indicator,
                Static_Inx => False);

         if L_Inx_Found = Static and R_Inx_Found = Static then
            -- Both are indexed, and only with static indices
            -- => Resplit with the actual values of indices
            Split (Left,  L_Variable, L_Selectors, L_Deref, L_Inx_Found,
                   Indicator  => '1',
                   Static_Inx => True);
            Split (Right, R_Variable, R_Selectors, R_Deref, R_Inx_Found,
                   Indicator  => R_Indicator,
                   Static_Inx => True);
         end if;

         declare
            -- X_Head is the part of the selectors up to and including the last ".all"
            -- X_Tail is the remaining of the string
            L_Head : constant Wide_String := Slice (L_Selectors, 1, L_Deref);
            L_Tail : constant Wide_String := Slice (L_Selectors, L_Deref+1, Length (L_Selectors));
            R_Head : constant Wide_String := Slice (R_Selectors, 1, R_Deref);
            R_Tail : constant Wide_String := Slice (R_Selectors, R_Deref+1, Length (R_Selectors));
         begin
            if Is_Equal (L_Variable, R_Variable) and L_Head = R_Head then
               if L_Tail'Length  > R_Tail'Length then
                  return L_Tail (L_Tail'First .. L_Tail'First + R_Tail'Length - 1) = R_Tail;
               else
                  return R_Tail (R_Tail'First .. R_Tail'First + L_Tail'Length - 1) = L_Tail;
               end if;
            else
               case Detail is
                  when Certain | Possible =>
                     return False;
                  when Unlikely =>
                     return L_Head /= "" or R_Head /= "";
               end case;
            end if;
         end;
      end Are_Aliased;

   begin
      if Rule_Used = (Rule_Detail => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Dispatching_Call (Call) then
         -- Improvement needed here, but it's quite difficult
         return;
      end if;

      declare
         use Thick_Queries;
         Actuals : constant Asis.Association_List := Call_Statement_Parameters (Call);
         To_Check_Parameters : Parameters_Table (Actuals'Range);

         function Association_Image (Position : List_Index) return Wide_String is
            -- Image of a parameter association
            -- Calls the correct function depending on whether Name is a Defining_Name or a
            -- plain identifier.
            -- This kludge is needed because currently the function Formal_Name is
            -- inconsistent, depending on whether the actual association is positionnal or named
            use Asis.Text, Ada.Strings, Ada.Strings.Wide_Fixed;

            Name : constant Asis.Name := Formal_Name (Call, Position);
         begin
            if Element_Kind (Name) = A_Defining_Name then
               return '"' & Defining_Name_Image (Name) & " => "
                      & Trim (Element_Image (Actual_Parameter (Actuals (Position))), Both) & '"';
            else
               return '"' & Name_Image (Name) & " => "
                      & Trim (Element_Image (Actual_Parameter (Actuals (Position))), Both) & '"';
            end if;
         end Association_Image;

         Mode    : Mode_Kinds;
         TCP_Top : Asis_Natural := To_Check_Parameters'First - 1;

         pragma Warnings (Off, To_Check_Parameters);
         -- GNAT warns that To_Check_Parameters may be used before it has a value,
         -- but the algorithm ensures that this does not happen
      begin
         for I in Actuals'Range loop
            Mode := Mode_Kind (Enclosing_Element (Formal_Name (Call, I)));

            if Mode in An_Out_Mode .. An_In_Out_Mode then
               for J in To_Check_Parameters'First .. TCP_Top loop
                  for Detail in Rule_Detail loop
                     if Rule_Used (Detail) and then
                       Are_Aliased (Actual_Parameter (Actuals (To_Check_Parameters (J))),
                                    Actual_Parameter (Actuals (I)),
                                    Detail)
                     then
                        Report (Rule_Id,
                                To_Wide_String (Rule_Label (Detail)),
                                Rule_Type (Detail),
                                Get_Location (Call),
                                Choose (Detail = Certain,
                                        "Certain",
                                        Choose (Detail = Possible,
                                                "Possible",
                                                "Unlikely"))
                                  & " aliasing between parameters "
                                  & Association_Image (To_Check_Parameters (J))
                                  & " and "
                                  & Association_Image (I)
                               );

                        -- If we found a stronger aliasing, don't check weaker ones
                        exit;
                     end if;
                  end loop;
               end loop;

               TCP_Top := TCP_Top + 1;
               To_Check_Parameters (TCP_Top) := I;
            end if;
         end loop;
      end;
   end Process_Call;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access,
                                     Prepare => Prepare'Access);
end Rules.Parameter_Aliasing;
