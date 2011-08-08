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
  A4G_Bugs,
  Binary_Map,
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Multiple_Assignments is
   use Ada.Strings.Wide_Unbounded;
   use Framework;

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
   -- The processing of an LHS increments its parent's count of subcomponents (recursively,
   -- of course) unless the parent is limited.
   --
   -- The first LHS (the one to the left of ":=") is marked as "full" assignment, others
   -- (during recursion) are marked as "component".
   --
   -- At the end of a sequence of assignments, all LHS in the map are traversed, and
   -- messages are issued according to the values assigned/total subcomponents.

   type Subrules is (Repeated, Groupable);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Criteria is (Crit_Given, Crit_Missing, Crit_Ratio);
   package Criteria_Utilities is new Framework.Language.Modifier_Utilities (Criteria, Prefix => "Crit_");

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   -- Data for subrule Repeated:
   Repeated_Used    : Boolean := False;
   Repeated_Context : Basic_Rule_Context;

   -- Data for subrule Groupable

   subtype Percentage is Natural range 0 .. 100;
   type Rule_Context is new Basic_Rule_Context with
      record
         Given   : Thick_Queries.Biggest_Natural;
         Missing : Thick_Queries.Biggest_Natural;
         Ratio   : Percentage;
      end record;

   package Context_Queue is new Linear_Queue (Rule_Context);
   Groupable_Contexts : Context_Queue.Queue;

   type LHS_Descriptor (Is_Full : Boolean := False) is
      record
         Loc : Location;
         case Is_Full is
            when True =>
               null;
            when False =>
               Total       : Thick_Queries.Extended_Biggest_Natural;
               Nb_Subcompo : Thick_Queries.Biggest_Natural;
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
      Help_On_Flags ("Parameter (1) :");
      User_Message ("For groupable:");
      User_Message ("Parameter(2..n): <criterion> <value>");
      Help_On_Modifiers (Header => "<criterion> :");
      User_Message ("Control repeated assignments in a sequence to a same variable, or sequences of assignments");
      User_Message ("to components of a structured variable that could be replaced by an aggregate");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Context_Queue, Criteria_Utilities, Subrules_Flag_Utilities, Framework.Language, Thick_Queries;
      Given   : Biggest_Natural := 0;
      Missing : Biggest_Natural := Biggest_Natural'Last;
      Ratio   : Percentage      := 0;
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
               end case;
            end loop;
            Append (Groupable_Contexts, (Basic.New_Context (Ctl_Kind, Ctl_Label) with Given, Missing, Ratio));
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
      use Asis, Asis.Elements, Asis.Statements;

      LHS_Infos   : LHS_Map.Map;
      Dynamic_LHS : exception;
      -- Raised when the LHS is not statically determinable, the whole assignment
      -- is abandonned.

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
               Failure ("unexpected element in Total_Fields", Struct);
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
                           Parent_Fields : constant Extended_Biggest_Natural
                             := Total_Fields (Parent_Subtype_Indication (Def));
                           Extension_Fields : constant Extended_Biggest_Natural
                             := Total_Fields (Asis.Definitions.Record_Definition (Def));
                        begin
                           if Parent_Fields = Not_Static or Extension_Fields = Not_Static then
                              return Not_Static;
                           end if;
                           return Parent_Fields + Extension_Fields;
                        end;
                     when others =>
                        Failure ("unexpected type kind in Total_Fields", Def);
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
                        Result := Result + Names (Components (I))'Length;
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

               when A_Subtype_Indication =>
                  Def := Type_Declaration_View (A4G_Bugs.Corresponding_Name_Declaration
                                                (Subtype_Simple_Name (Def)));
               when A_Private_Type_Definition
                  | A_Tagged_Private_Type_Definition
                  | A_Private_Extension_Definition =>
                  return Not_Static;

               when A_Null_Record_Definition =>
                  return 0;

               when A_Formal_Type_Definition =>
                  return Not_Static;

               when others =>
                  Failure ("unexpected definition in Total_Fields", Def);
            end case;
         end loop;
      end Total_Fields;

      type Coverage_Kind is (Full, Component);
      procedure Process_Assignment (LHS      : in Asis.Expression;
                                    Coverage : in Coverage_Kind;
                                    Key      : out Unbounded_Wide_String)
      is
         use Asis.Expressions;
         use Framework.Reports, Utilities;
         Target     : Asis.Expression := LHS;
         Parent     : Asis.Expression;
         Parent_Key : Unbounded_Wide_String;
         Count      : LHS_Descriptor;
      begin
         loop
            case Expression_Kind (Target) is
               when A_Selected_Component =>
                  if Declaration_Kind (A4G_Bugs.Corresponding_Name_Declaration (Selector (Target)))
                    in A_Discriminant_Specification .. A_Component_Declaration
                  then
                     Parent := Prefix (Target);
                     if Is_Access_Expression (Parent) then
                        -- Implicit dereference
                        raise Dynamic_LHS;
                     end if;
                     Process_Assignment (Parent, Component, Key);
                     Parent_Key := Key;
                     Append (Key, '.' & To_Upper (A4G_Bugs.Name_Image (Selector (Target))));
                     exit;
                  end if;

                  -- Not a record component
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
                     for I in Indices'Range loop
                        declare
                           Value : constant Wide_String := Static_Expression_Value_Image (Indices (I));
                        begin
                           if Value = "" then
                              raise Dynamic_LHS;
                           end if;
                           Append (Key, '.' & Value);
                        end;
                     end loop;
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
                  if Declaration_Kind (A4G_Bugs.Corresponding_Name_Declaration (Target))
                    /= An_Object_Renaming_Declaration
                  then
                     -- A truly global assignment, not groupable
                     Key := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Target)));
                     Parent := Nil_Element;
                     exit;
                  end if;

                  -- A renaming: start over with the renamed expression
                  Target := A4G_Bugs.Renamed_Entity (A4G_Bugs.Corresponding_Name_Declaration (Target));
            end case;
         end loop;

         if Is_Present (LHS_Infos, Key) then
            -- Variable already assigned
            Count := Fetch (LHS_Infos, Key);
            if Repeated_Used
              and then (Coverage = Full or Count.Is_Full)
            then
               Report (Rule_Id,
                       Repeated_Context,
                       Get_Location (LHS),
                       "variable already assigned in same group at " & Image (Count.Loc));
            end if;
            return;
         end if;

         case Coverage is
            when Full =>
               Add (LHS_Infos, Key, (Is_Full => True, Loc => Get_Location (LHS)));
            when Component =>
               Add (LHS_Infos, Key, (Is_Full      => False,
                                     Loc          => Get_Location (LHS),
                                     Total        => Total_Fields (LHS),
                                     Nb_Subcompo  => 0)); -- will be incremented by child
         end case;

         if Is_Nil (Parent) or else Is_Limited (Parent) then
            return;
         end if;

         -- True field, not already seen: Increment parent count
         Count := Fetch (LHS_Infos, Parent_Key);
         if Count.Is_Full then
            if Repeated_Used then
               Report (Rule_Id,
                       Repeated_Context,
                       Get_Location (LHS),
                       "variable already assigned in same group at " & Image (Count.Loc));
            end if;
         else
            Count.Nb_Subcompo := Count.Nb_Subcompo + 1;
            Count.Loc         := Get_Location (LHS);
            Add (LHS_Infos, Parent_Key, Count);
         end if;
      end Process_Assignment;

      procedure Report_One (Key : Unbounded_Wide_String; Value : in out LHS_Descriptor) is
         use Context_Queue, Framework.Reports, Utilities;
         Current : Cursor := First (Groupable_Contexts);
         Context : Rule_Context;
         Reason  : Unbounded_Wide_String;
         Matched : Boolean;
      begin
         if Value.Is_Full      --A global assignment
           or else Value.Total = 0  --A null record
         then
            return;
         end if;

         -- We do not use Assert for the following sanity check, to avoid evaluating the error string
         -- (would raise Constraint_Error if Value.Total = Not_Static)
         if Value.Total /= Not_Static and then Value.Nb_Subcompo > Value.Total then
            Failure ("More assigned fields than possible for " & To_Wide_String (Key) & ": "
                     & Biggest_Int_Img (Value.Nb_Subcompo) & "/" & Biggest_Int_Img (Value.Total));
         end if;

         while Has_Element (Current) loop
            Matched := True;
            Reason  := Null_Unbounded_Wide_String;
            Context := Fetch (Current);

            if Context.Given > 0 then
               if Value.Nb_Subcompo >= Context.Given then
                  Append (Reason, ", given: " & Biggest_Int_Img (Value.Nb_Subcompo)
                          & " (>=" & Biggest_Int_Img (Context.Given) & ')');
               else
                  Matched := False;
               end if;
            end if;

            if Value.Total = Not_Static then
               if Context.Missing < Biggest_Int'Last or Context.Ratio > 0 then
                  Matched := False;
               end if;
            else
               if Context.Missing < Biggest_Int'Last then
                  if Value.Total - Value.Nb_Subcompo <= Context.Missing then
                     Append (Reason, ", missing: " & Biggest_Int_Img (Value.Total - Value.Nb_Subcompo)
                             & " (<=" & Biggest_Int_Img (Context.Missing) & ')');
                  else
                     Matched := False;
                  end if;
               end if;

               if Context.Ratio > 0 then
                  if Value.Nb_Subcompo * 100 / Value.Total >= Biggest_Int (Context.Ratio) then
                     Append (Reason, ", ratio: " & Biggest_Int_Img (Value.Nb_Subcompo * 100 / Value.Total)
                             & " (>=" & Integer_Img (Context.Ratio) & ')');
                  else
                     Matched := False;
                  end if;
               end if;
            end if;

            if Matched then
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
                     Process_Assignment (Assignment_Variable_Name (Stmts (I)), Full, Ignored);
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

         if Statement_Kind (Stmts (Stmts'Last)) = An_Assignment_Statement then
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
