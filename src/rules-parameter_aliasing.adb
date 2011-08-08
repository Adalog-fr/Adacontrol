----------------------------------------------------------------------
--  Rules.Parameter_Aliasing - Package body                         --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Code Cheker  is free software;  you can redistribute  it and/or --
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

   type Rule_Detail is (Certain, Possible, Unlikely);

   Rule_Used  : array (Rule_Detail) of Boolean := (others => False);
   Rule_Type  : array (Rule_Detail) of Rule_Types;
   Rule_Label : array (Rule_Detail) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter (optional): certain (default) | possible | unlikely");
      User_Message ("This rule can be used to check/search for subprogram or entry calls where");
      User_Message ("the same variable is given for more than one [in] out parameter.");
      User_Message ("This rule can detect non-straightforward aliasing cases, see doc for details");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

      Detail : Rule_Detail := Certain;
   begin
      if Parameter_Exists then
         begin
            Detail := Rule_Detail'Wide_Value (Get_String_Parameter);
         exception
            when Constraint_Error =>
               Parameter_Error ("Only ""Certain"",  ""Possible"" or ""Unlikely"" allowed"
                                  & " as parameter for rule " & Rule_Id);
         end;
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
   -- Prepare --
   -------------

   procedure Prepare is
      -- If weaker checks have not been specified, force them for stronger ones
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

         L_Variable  : Asis.Definition;
         L_Selectors : Unbounded_Wide_String;
         L_Deref     : Natural;
         R_Variable  : Asis.Definition;
         R_Selectors : Unbounded_Wide_String;
         R_Deref     : Natural;

         procedure Split (Name       : in     Asis.Expression;
                          Variable   : in out Asis.Definition;
                          Selectors  :    out Unbounded_Wide_String;
                          Last_Deref :    out Natural;
                          Indicator  : in     Wide_Character)
         is
            -- Given the original Name (possibly cleaned-up from a type conversion):
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
            --
            -- Returns in Last_Deref the position of the last character of the ".all"
            -- corresponding to the right-most dereference if any, or 0.
            use Thick_Queries;

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
         begin
            Last_Deref := 0;

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
                           E         := Prefix (E);
                        when A_Variable_Declaration | An_Object_Renaming_Declaration =>
                           -- Its a Pack.Var selector
                           E := Selector (E);
                           exit;
                        when others =>
                           Trace ("Decl", Corresponding_Name_Declaration (Selector (E)));
                           Failure ("Wrong selected component", E);
                     end case;

                  when An_Indexed_Component | A_Slice =>
                     Add_Selector ('(' & Indicator & ')');
                     E := Prefix (E);

                  when A_Function_Call =>
                     --  a Function_Call can appear only as the first
                     --  element, and if it returns an access value,
                     --  or a composite object used for one of its
                     --  access subcomponents.
                     Add_Selector("_CALL_." & Indicator);
                     E         := Prefix (E);

                  when An_Explicit_Dereference =>
                     -- ".all" will be added below, since the prefix is necessarily
                     -- of an access type
                     E := Prefix (E);

                  when A_Type_Conversion =>
                     E := Converted_Or_Qualified_Expression (E);

                  when others =>
                     Failure ("Wrong variable name", E);
               end case;

               -- Add a ".all" if the *type* is an access type
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
            while Declaration_Kind (Enclosing_Element (Variable)) in A_Renaming_Declaration loop
               Split (Name       => Renamed_Entity (Enclosing_Element(Variable)),
                      Variable   => Variable,
                      Selectors  => Temp_Sel,
                      Last_Deref => Temp_Deref,
                      Indicator  => Indicator);
               Add_Selector (To_Wide_String (Temp_Sel));
               if Last_Deref = 0 then
                  Last_Deref := Temp_Deref;
               end if;
            end loop;
         end Split;

      begin   -- Are_Aliased
         Split (Left, L_Variable, L_Selectors, L_Deref, '1');

         case Detail is
            when Certain =>
               Split (Right, R_Variable, R_Selectors, R_Deref, '2');
            when Possible | Unlikely =>
               -- Use the same indicator as for Left
               -- => all indexings and function calls will match
               Split (Right, R_Variable, R_Selectors, R_Deref, '1');
         end case;

         declare
            -- X_Head is the part up to and including the last ".all"
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

      declare
         use Thick_Queries;
         Actuals : constant Asis.Association_List             := Call_Statement_Parameters (Call);
         Formals : constant Asis.Parameter_Specification_List := Called_Profile (Call);
         To_Check_Parameters : Parameters_Table (Actuals'Range);

         function Formal_Name (Actual : List_Index) return Asis.Expression is
            -- Retrieves the name of the formal corresponding to the Actual given
            I_A     : Asis_Natural;
            I_F     : Asis_Natural;
            FP      : constant Asis.Element := Formal_Parameter (Actuals (Actual));
         begin
            Trace ("-------------------------------");
            if Is_Nil (FP) then
               -- Parameter given in positional notation
               I_F := Formals'First;
               I_A := 0;
               loop
                  declare
                     These_Names : constant Asis.Element_List :=  Names (Formals (I_F));
                  begin
                     Trace ("These_Names'Length" & Asis_Natural'Wide_Image (These_Names'Length));
                     I_A := I_A + These_Names'Length;
                     if I_A >= Actual then
                        return These_Names (These_Names'Length - (I_A - Actual));
                     end if;
                  end;
                  I_F := I_F + 1;
               end loop;
            else
               return Formal_Parameter (Actuals (Actual));
            end if;
         exception
            when Constraint_Error =>
               Trace ("I_F=" & Asis_Natural'Wide_Image (I_F));
               Trace ("I_A=" & Asis_Natural'Wide_Image (I_A));
               Trace ("Actual=" & Asis_Natural'Wide_Image (Actual));
               raise;
         end Formal_Name;

         function Association_Image (Position : List_Index) return Wide_String is
            -- Image of a parameter association
            -- Calls the correct function depending on whether Name is a Defining_Name or a
            -- plain identifier.
            -- This kludge is needed because currently the function Formal_Name below is
            -- inconsistent, depending on whether the actual association is positionnal or named
            use Asis.Text, Ada.Strings, Ada.Strings.Wide_Fixed;

            Name : constant Asis.Name := Formal_Name (Position);
         begin
            Trace ("actual", Actual_Parameter (Actuals (Position)));
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
      begin
         for I in Actuals'Range loop
            if Is_Nil (Formal_Parameter (Actuals (I))) then
               -- Parameter given in positional notation
               -- Formal_Name returns the name from the formal declaration
               Mode := Mode_Kind (Enclosing_Element (Formal_Name (I)));
            else
               -- Parameter given in named notation
               Mode := Mode_Kind (Corresponding_Name_Declaration
                                    (Formal_Parameter (Actuals (I))));
            end if;

            if Mode in An_Out_Mode .. An_In_Out_Mode then
               for J in To_Check_Parameters'First .. TCP_Top loop
                  for Detail in Rule_Detail loop
                     if Rule_Used (Detail) and then
                       Are_Aliased (Actual_Parameter (Actuals (I)),
                                    Actual_Parameter (Actuals (To_Check_Parameters (J))),
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

                        -- If we found a weaker aliasing, don't check stronger ones
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
                                     Prepare => Prepare'Access,
                                     Add_Use => Add_Use'Access);
end Rules.Parameter_Aliasing;
