----------------------------------------------------------------------
--  Rules.Multiple_Assignments - Package body                       --
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
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  Binary_Map,
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Multiple_Assignments is
   use Ada.Strings.Wide_Unbounded;
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- Since this rule is about sequences of assignments, it is plugged on any construct
   -- that can contain statements. The sequence of statements is scanned for consecutive
   -- assignments.
   --
   -- A map holds information for all LHS (Left Hand Side) of a sequence of assignments.
   -- The name (key to the map) is made of the full name of the variable, plus selectors
   -- (for record components) or the value of static indices (for arrays). If anything
   -- not static is encountered, processing is stopped by raising Dynamic_LHS.
   -- The information associated to a LHS contains:
   --    - the total number of subcomponents this LHS can have
   --    - the number of subcomponents currently assigned.
   --
   -- The parent of a LHS is the expression with one less selector (or indexing) than
   -- the current LHS. It is also considered a LHS.
   --
   -- The first LHS (the one to the left of ":=") is marked as "full" assignment, others
   -- (during recursion) are marked as "component".
   --
   -- The Repeated subrule is triggered if the LHS or any of its parents is already in the
   -- map.
   --
   -- The Groupable subrule is triggered on various comparisons between the total number of
   -- subcomponents and the number of assigned subcomponents. The difficulty is that, for a
   -- subcomponent that is itself of a composite type, it has to be counted as "assigned" not
   -- only if it has been assigned in full, but also if it should have been assigned in full
   -- (i.e. if the rule would trigger on the subcomponent). And since it depends on the parameters
   -- of the control, it has to be recomputed dynamically for each control.
   --
   -- The processing of a "full" LHS simply increments its parent's count of (full) subcomponents.
   -- A "component" LHS is chained to its parent through Child/Brother links, and when evaluating
   -- the parent, the number of assigned components is recomputed by following this chain.
   --
   -- At the end of a sequence of assignments, all LHS in the map are traversed, and
   -- messages are issued according to the values assigned/total subcomponents.

   type Subrules is (Repeated, Groupable);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Criteria is (Crit_Given, Crit_Missing, Crit_Ratio, Crit_Total);
   package Criteria_Utilities is new Framework.Language.Modifier_Utilities (Criteria, Prefix => "Crit_");

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   -- Data for subrule Repeated:
   Repeated_Used    : Boolean := False;
   Repeated_Context : Basic_Rule_Context;

   -- Data for subrule Groupable
   subtype Percentage is Asis.ASIS_Natural range 0 .. 100;
   type Rule_Context is new Basic_Rule_Context with
      record
         Given   : Thick_Queries.Biggest_Natural;
         Missing : Thick_Queries.Biggest_Natural;
         Ratio   : Percentage;
         Total   : Thick_Queries.Biggest_Natural;
      end record;

   package Context_Queue is new Linear_Queue (Rule_Context);
   Groupable_Contexts : Context_Queue.Queue;

   type Coverage_Kind is (Full, Component);
   type LHS_Descriptor (Coverage : Coverage_Kind := Full) is
      record
         Loc : Location;
         case Coverage is
            when Full =>
               null;
            when Component =>
               Subcomp_Total      : Thick_Queries.Extended_Biggest_Natural;
               Full_Subcomp_Count : Thick_Queries.Biggest_Natural;
               First_Child        : Unbounded_Wide_String;
               Brother            : Unbounded_Wide_String;
         end case;
      end record;
   package LHS_Map is new Binary_Map (Unbounded_Wide_String, LHS_Descriptor);


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Criteria_Utilities, Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control repeated assignments in a sequence to a same variable, or sequences of assignments");
      User_Message ("to components of a structured variable that could be replaced by an aggregate");
      User_Message;
      Help_On_Flags ("Parameter(1):");
      User_Message;
      User_Message ("For groupable:");
      User_Message ("Parameter(2..n): <criterion> <value>");
      Help_On_Modifiers (Header => "<criterion>:");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Context_Queue, Criteria_Utilities, Subrules_Flag_Utilities, Framework.Language, Thick_Queries;
      Given   : Biggest_Natural := 0;
      Missing : Biggest_Natural := Biggest_Natural'Last;
      Ratio   : Percentage      := 0;
      Total   : Biggest_Natural := 0;
      Crit    : Criteria;
      Subrule : Subrules;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "subrule expected");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);
      case Subrule is
         when Repeated =>
            if Parameter_Exists then
               Parameter_Error (Rule_Id, "No parameter for subrule ""repeated""");
            end if;
            if Repeated_Used then
               Parameter_Error (Rule_Id, "subrule ""repeated"" already specified");
            end if;

            Repeated_Used    := True;
            Repeated_Context := Basic.New_Context (Ctl_Kind, Ctl_Label);

         when Groupable =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "criterion of acceptable component assignments required");
            end if;

            while Parameter_Exists loop
               Crit := Get_Modifier (Required => True);
               case Crit is
                  when Crit_Given =>
                     Given := Get_Integer_Parameter (Min => 1);
                  when Crit_Missing =>
                     Missing := Get_Integer_Parameter (Min => 0);
                  when Crit_Ratio =>
                     Ratio := Get_Integer_Parameter (Min => 1, Max => 100);
                  when Crit_Total =>
                     Total := Get_Integer_Parameter (Min => 1);
               end case;
            end loop;
            Append (Groupable_Contexts, (Basic.New_Context (Ctl_Kind, Ctl_Label) with Given, Missing, Ratio, Total));
      end case;
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
            Rule_Used     := False;
            Repeated_Used := False;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   ----------------------------------
   -- Process_Statement_Container  --
   ----------------------------------

   procedure Process_Statement_Container (Element : in Asis.Element) is
      use LHS_Map, Thick_Queries;
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;

      LHS_Infos   : LHS_Map.Map;
      Dynamic_LHS : exception;
      -- Raised when the LHS is not statically determinable, the whole assignment
      -- is abandonned.
      Reason  : Unbounded_Wide_String;


      function Total_Fields (Struct : Asis.Element) return Extended_Biggest_Natural is
         use Utilities;
         use Asis.Declarations, Asis.Definitions;

         Def : Asis.Definition;
      begin
         case Element_Kind (Struct) is
            when An_Expression =>
               Def := Thick_Queries.Corresponding_Expression_Type_Definition (Struct);
            when A_Definition =>
               Def := Struct;
            when others =>
               Failure ("Total_Fields: unexpected element", Struct);
         end case;

         -- Among other things, this loop unwinds subtypes and visible derivations
         -- We cannot use Ultimate_Expression_Type because we do not want to unwind
         -- private type declarations and record extensions.
         loop
            case Definition_Kind (Def) is
               when A_Type_Definition =>
                  case Type_Kind (Def) is
                     when An_Unconstrained_Array_Definition
                        | A_Constrained_Array_Definition
                        =>
                        declare
                           Lengths : constant Extended_Biggest_Natural_List := Discrete_Constraining_Lengths (Struct);
                           Result  : Biggest_Natural := 1;
                        begin
                           for I in Lengths'Range loop
                              if Lengths (I) = Not_Static then
                                 return Not_Static;
                              end if;
                              Result := Result * Lengths (I);
                           end loop;
                           return Result;
                        end;
                     when A_Record_Type_Definition
                        | A_Tagged_Record_Type_Definition
                        =>
                        Def := Asis.Definitions.Record_Definition (Def);
                     when A_Derived_Type_Definition =>
                        Def := Parent_Subtype_Indication (Def);
                     when A_Derived_Record_Extension_Definition =>
                        declare
                           Parent_Fields    : constant Extended_Biggest_Natural
                             := Total_Fields (Parent_Subtype_Indication (Def));
                           Extension_Fields : constant Extended_Biggest_Natural
                             := Total_Fields (Asis.Definitions.Record_Definition (Def));
                        begin
                           if Parent_Fields = Not_Static or Extension_Fields = Not_Static then
                              return Not_Static;
                           end if;
                           return Parent_Fields + Extension_Fields;
                        end;
                     when An_Interface_Type_Definition => --2005
                        -- Interfaces have no data...
                        return 0;
                     when others =>
                        Failure ("Total_Fields: unexpected type kind " & Type_Kinds'Wide_Image (Type_Kind (Def)), Def);
                  end case;

               when A_Record_Definition =>
                  declare
                     -- The Record_Type_Definition is in A_Type_Definition which is in A_Type_Declaration...
                     Decl       : constant Asis.Declaration := Enclosing_Element (Enclosing_Element (Def));
                     Components : constant Asis.Record_Component_List := Record_Components (Def);
                     Result     : Biggest_Natural := 0;
                  begin
                     if Definition_Kind (Components (Components'Last)) = A_Variant_Part then
                        return Not_Static;
                     end if;
                     for I in Components'Range loop
                        if Definition_Kind (Components (I)) /= A_Null_Component then
                           Result := Result + Names (Components (I))'Length;
                        end if;
                     end loop;
                     if not Is_Nil (Discriminant_Part (Decl)) then
                        declare
                           Discrs : constant Asis.Discriminant_Specification_List
                             := Discriminants (Discriminant_Part (Decl));
                        begin
                           for I in Discrs'Range loop
                              Result := Result + Names (Discrs (I))'Length;
                           end loop;
                        end;
                     end if;
                     return Result;
                  end;

               when A_Protected_Definition =>
                  declare
                     Decl       : constant Asis.Declaration := Enclosing_Element (Def);
                     Components : constant Asis.Declarative_Item_List := Private_Part_Items (Def);
                     Result     : Biggest_Natural := 0;
                  begin
                     for I in Components'Range loop
                        if Declaration_Kind (Components (I)) = A_Component_Declaration then
                           Result := Result + Names (Components (I))'Length;
                        end if;
                     end loop;
                     if Declaration_Kind (Decl) = A_Protected_Type_Declaration  -- no discriminants for single PO
                       and then not Is_Nil (Discriminant_Part (Decl))
                     then
                        declare
                           Discrs : constant Asis.Discriminant_Specification_List
                             := Discriminants (Discriminant_Part (Decl));
                        begin
                           for I in Discrs'Range loop
                              Result := Result + Names (Discrs (I))'Length;
                           end loop;
                        end;
                     end if;
                     return Result;
                  end;

               when A_Subtype_Indication =>
                  Def := Type_Declaration_View (Corresponding_Name_Declaration (Subtype_Simple_Name (Def)));

               when A_Private_Type_Definition
                  | A_Tagged_Private_Type_Definition
                  | A_Private_Extension_Definition =>
                  return Not_Static;

               when A_Null_Record_Definition =>
                  return 0;

               when A_Formal_Type_Definition =>
                  return Not_Static;

               when others =>
                  Failure ("Total_Fields: unexpected definition", Def);
            end case;
         end loop;
      end Total_Fields;

      function Exceeds_Thresholds (Value         : LHS_Descriptor;
                                   Limits        : Rule_Context;
                                   With_Messages : Boolean := False) return Boolean

      is
         use Utilities;

         Subcomp_Count : Thick_Queries.Biggest_Natural := Value.Full_Subcomp_Count;
         Subcomp_Descr : LHS_Descriptor;
         Subcomp_Key   : Unbounded_Wide_String;
         Matched       : Boolean := True;

      begin
         if Value.Coverage = Full then
            return True;  -- Since aggregate required
         end if;

         Reason := Null_Unbounded_Wide_String;

         Subcomp_Key := Value.First_Child;
         while Subcomp_Key /= Null_Unbounded_Wide_String loop
            Subcomp_Descr := Fetch (LHS_Infos, Subcomp_Key);
            if Exceeds_Thresholds (Subcomp_Descr, Limits, With_Messages => False) then
               Subcomp_Count := Subcomp_Count + 1;
            end if;
            Subcomp_Key := Subcomp_Descr.Brother;
         end loop;

         --
         -- Given
         --
         if Limits.Given > 0 then
            if Subcomp_Count < Limits.Given then
               Matched := False;
            elsif With_Messages then
               Append (Reason, ", given: " & Biggest_Int_Img (Subcomp_Count)
                       & " (>=" & Biggest_Int_Img (Limits.Given) & ')');
            end if;
         end if;

         --
         -- Missing
         --
         if Limits.Missing < Biggest_Int'Last then
            if Value.Subcomp_Total = Not_Static
              or else Value.Subcomp_Total - Subcomp_Count > Limits.Missing
            then
               Matched := False;
            elsif With_Messages then
               Append (Reason, ", missing: " & Biggest_Int_Img (Value.Subcomp_Total - Subcomp_Count)
                       & " (<=" & Biggest_Int_Img (Limits.Missing) & ')');
            end if;
         end if;

         --
         -- Ratio
         --
         if Limits.Ratio > 0 then
            if Value.Subcomp_Total = Not_Static
              or else Subcomp_Count * 100 / Value.Subcomp_Total < Biggest_Int (Limits.Ratio)
            then
               Matched := False;
            elsif With_Messages then
               Append (Reason, ", ratio: " & Biggest_Int_Img (Subcomp_Count * 100 / Value.Subcomp_Total)
                       & " (>=" & ASIS_Integer_Img (Limits.Ratio) & ')');
            end if;
         end if;

         --
         -- Total
         --
         if Limits.Total > 0 then
            if Value.Subcomp_Total = Not_Static
              or else Value.Subcomp_Total > Limits.Total
            then
               Matched := False;
            elsif With_Messages then
               Append (Reason, ", total: " & Biggest_Int_Img (Value.Subcomp_Total)
                       & " (<=" & Biggest_Int_Img (Limits.Total) & ')');
            end if;
         end if;

         return Matched;
      end Exceeds_Thresholds;

      procedure Process_Assignment (LHS      : in Asis.Expression;
                                    Coverage : in Coverage_Kind;
                                    Key      : out Unbounded_Wide_String)
      is
         use Asis.Declarations;
         use Framework.Reports, Utilities;
         Target       : Asis.Expression := LHS;
         Target_Descr : LHS_Descriptor;
         Parent       : Asis.Expression;
         Parent_Key   : Unbounded_Wide_String;
         Parent_Descr : LHS_Descriptor;
      begin
         loop
            case Expression_Kind (Target) is
               when A_Selected_Component =>
                  if Declaration_Kind (Corresponding_Name_Declaration (Selector (Target)))
                    in A_Discriminant_Specification .. A_Component_Declaration
                  then
                     Parent := Prefix (Target);
                     if Is_Access_Expression (Parent) then
                        -- Implicit dereference
                        raise Dynamic_LHS;
                     end if;
                     Process_Assignment (Parent, Component, Key);
                     Parent_Key := Key;
                     Append (Key, '.' & To_Upper (Name_Image (Selector (Target))));
                     exit;
                  end if;

                  -- Not a record component (Fully named variable from a package, f.e.)
                  Target := Selector (Target);

               when An_Indexed_Component =>
                  Parent := Prefix (Target);
                  if Is_Access_Expression (Parent) then
                     -- Implicit dereference
                     raise Dynamic_LHS;
                  end if;
                  Process_Assignment (Parent, Component, Key);
                  Parent_Key := Key;
                  declare
                     Indices : constant Asis.Expression_List := Index_Expressions (Target);
                  begin
                     Append (Key, '(');
                     for I in Indices'Range loop
                        declare
                           Value : constant Wide_String := Static_Expression_Value_Image (Indices (I));
                        begin
                           if Value = "" then
                              raise Dynamic_LHS;
                           end if;
                           Append (Key, Value & ',');
                        end;
                     end loop;
                     Replace_Element (Key, Length (Key), ')');
                  end;
                  exit;
               when A_Slice  =>
                  -- Strictly speaking, we should handle that as as many LHS as elements
                  -- in the slice (provided it is static of course). But this could make
                  -- thousands of entries...
                  -- Not worth the complication, just consider it dynamic in all cases.
                  raise Dynamic_LHS;
               when A_Type_Conversion =>
                  Target := Converted_Or_Qualified_Expression (Target);
               when An_Explicit_Dereference
                  | A_Function_Call
                    =>
                  raise Dynamic_LHS;
               when others =>
                  if Declaration_Kind (Corresponding_Name_Declaration (Target))
                    /= An_Object_Renaming_Declaration
                  then
                     -- A truly global assignment, not groupable
                     Key := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Target)));
                     Parent := Nil_Element;
                     exit;
                  end if;

                  -- A renaming: start over with the renamed expression
                  Target := Renamed_Entity (Corresponding_Name_Declaration (Target));
            end case;
         end loop;

         if Is_Present (LHS_Infos, Key) then
            -- Variable already assigned
            Target_Descr := Fetch (LHS_Infos, Key);
            if Repeated_Used
              and then (Coverage = Full or Target_Descr.Coverage = Full)
            then
               Report (Rule_Id,
                       Repeated_Context,
                       Get_Location (LHS),
                       "variable already assigned in same group at " & Image (Target_Descr.Loc));
            end if;
            return;
         end if;

         -- Only "new" (not already assigned) components past this point
         case Coverage is
            when Full =>
               Target_Descr := (Coverage => Full, Loc => Get_Location (LHS));
            when Component =>
               Target_Descr := (Coverage           => Component,
                                Loc                => Get_Location (LHS),
                                Subcomp_Total      => Total_Fields (LHS),
                                Full_Subcomp_Count => 0,  -- will be incremented by full children
                                First_Child        => Null_Unbounded_Wide_String,
                                Brother            => Null_Unbounded_Wide_String);
         end case;

         if Is_Nil (Parent) or else Is_Limited (Parent) then
            Add (LHS_Infos, Key, Target_Descr);
            return;
         end if;

         -- True field, not already seen: Increment parent count if full child, chain otherwise
         Parent_Descr := Fetch (LHS_Infos, Parent_Key);
         if Parent_Descr.Coverage = Full then  -- Parent previously assigned in full
            if Repeated_Used then
               Report (Rule_Id,
                       Repeated_Context,
                       Get_Location (LHS),
                       "variable already assigned in same group at " & Image (Parent_Descr.Loc));
            end if;
         else
            Parent_Descr.Loc := Get_Location (LHS);
            case Coverage is
               when Full =>
                  Parent_Descr.Full_Subcomp_Count := Parent_Descr.Full_Subcomp_Count + 1;
               when Component =>
                  if Parent_Descr.First_Child /= Null_Unbounded_Wide_String then
                     Target_Descr.Brother := Parent_Descr.First_Child;
                  end if;
                  Parent_Descr.First_Child := Key;
            end case;
            Add (LHS_Infos, Parent_Key, Parent_Descr);
         end if;

         Add (LHS_Infos, Key, Target_Descr);
      end Process_Assignment;

      procedure Report_One (Key : Unbounded_Wide_String; Value : in out LHS_Descriptor) is
         use Context_Queue, Framework.Reports, Utilities;
         Current : Cursor;
         Context : Rule_Context;
      begin
         if Value.Coverage = Full   --A global assignment
           or else Value.Subcomp_Total = 0  --A null record
         then
            return;
         end if;

         -- We do not use Assert for the following sanity check, to avoid evaluating the error string
         -- (would raise Constraint_Error if Value.Total = Not_Static)
         if Value.Subcomp_Total /= Not_Static and then Value.Full_Subcomp_Count > Value.Subcomp_Total then
            Failure ("More assigned fields than possible for " & To_Wide_String (Key) & ": "
                     & Biggest_Int_Img (Value.Full_Subcomp_Count) & "/" & Biggest_Int_Img (Value.Subcomp_Total));
         end if;

         Current := First (Groupable_Contexts);
         while Has_Element (Current) loop
            Context := Fetch (Current);

            if Exceeds_Thresholds (Value, Limits => Context, With_Messages => True)  then
               Report (Rule_Id,
                       Context,
                       Value.Loc,
                       "too many assignments to components of "
                       & To_Title (To_Wide_String (Key))
                       & To_Wide_String (Reason));
            end if;

            Current := Next (Current);
         end loop;
      end Report_One;

      procedure Do_Report is new LHS_Map.Iterate (Report_One);

   begin   -- Process_Statement_Container
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Stmts   : constant Asis.Statement_List := Thick_Queries.Statements (Element);
         Ignored : Unbounded_Wide_String;
      begin
         if Stmts'Length = 0 then
            -- Package body without statements
            return;
         end if;

         for I in Stmts'Range loop
            case Statement_Kind (Stmts (I)) is
               when An_Assignment_Statement =>
                  begin
                     Process_Assignment (LHS      => Assignment_Variable_Name (Stmts (I)),
                                         Coverage => Full,
                                         Key      => Ignored);
                  exception
                     when Dynamic_LHS =>
                        null;
                  end;
               when A_Null_Statement =>
                  -- This one is harmless...
                  null;
               when others =>
                  Do_Report (LHS_Infos);
                  Clear (LHS_Infos);
            end case;
         end loop;

         if Statement_Kind (Stmts (Stmts'Last)) = An_Assignment_Statement
           or else Statement_Kind (Stmts (Stmts'Last)) = A_Null_Statement
         then
            Do_Report (LHS_Infos);
            Clear (LHS_Infos);
         end if;
      end;
   end Process_Statement_Container;

begin  -- Rules.Multiple_Assignments
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Multiple_Assignments;
