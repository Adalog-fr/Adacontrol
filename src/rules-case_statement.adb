----------------------------------------------------------------------
--  Rules.Case_Statement - Package body                             --
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
  Asis.Elements,
  Asis.Statements;

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

package body Rules.Case_Statement is

   use Asis, Framework, Thick_Queries;

   type Case_Statement_Names is (Min_Others_Span, Min_Paths, Max_Range_Span, Max_Values);
   package Case_Statement_Flag_Utilities  is new Framework.Language.Flag_Utilities (Case_Statement_Names);

   type Usage is array (Case_Statement_Names) of Rule_Types_Set;
   Rule_Used : Usage := (others => (others => False));
   Save_Used : Usage;

   Labels : array (Case_Statement_Names, Rule_Types) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Values : array (Case_Statement_Names, Rule_Types) of Biggest_Natural := (others => (others => 0));

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: "& Rule_Id);
      Case_Statement_Flag_Utilities.Help_On_Flags (Header => "Parameter(1):");
      User_Message ("Parameter(2): for min_others_span: minimum number of values allowed for others");
      User_Message ("              for min_paths      : minimum number of 'when' allowed in the case statement");
      User_Message ("              for max_range_span : maximum number of values allowed in a choice given as a range");
      User_Message ("              for max_values     : maximum number of values allowed in the selector's subtype");
      User_Message ("Controls various sizes related to the case statement");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Framework.Language, Case_Statement_Flag_Utilities, Ada.Strings.Wide_Unbounded;
      Stmt : Case_Statement_Names;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id & ": Two parameters required");
      end if;

      Stmt := Get_Flag_Parameter (Allow_Any => False);
      if Rule_Used (Stmt) (Rule_Type) then
         Parameter_Error (Rule_Id & ": rule already specified for " & Rule_Types'Wide_Image (Rule_Type));
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id & ": Two parameters required");
      end if;

      case Stmt is
         when Min_Others_Span | Min_Paths | Max_Values =>
            Values (Stmt, Rule_Type) := Get_Integer_Parameter (Min => 1);
         when Max_Range_Span =>
            Values (Stmt, Rule_Type) := Get_Integer_Parameter (Min => 0);
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

   ----------------------------
   -- Process_Case_Statement --
   ----------------------------

   procedure Process_Case_Statement (Statement : in Asis.Statement) is
      use Ada.Strings.Wide_Unbounded;
      use Asis.Declarations, Asis.Elements, Asis.Statements, Framework.Reports;

      Non_Evaluable : exception;

      -- Compute the number of cases covered by all case alternatives
      -- (including discrete ranges) excluding the "when others" alternative
      function Count_Non_Others_Choices (Case_Paths : in Path_List) return Biggest_Natural is
         use Utilities;
         Count : Biggest_Natural := 0;
      begin
         -- We know that the last path is for "when others":
         for CP in List_Index range Case_Paths'First .. Case_Paths'Last - 1 loop
            declare
               Path_Elements : constant Element_List := Case_Statement_Alternative_Choices (Case_Paths (CP));
               Temp          : Extended_Biggest_Natural;
            begin
               for PE in Path_Elements'Range loop
                  if Definition_Kind (Path_Elements (PE)) = A_Discrete_Range then
                     Temp := Discrete_Constraining_Lengths (Path_Elements (PE))(1);
                     if Temp = Not_Static then
                        -- it IS static, but the evaluator cannot evaluate it...
                        Uncheckable (Rule_Id,
                                     False_Negative,
                                     Get_Location (Path_Elements (PE)),
                                     "Could not evaluate bounds of expression");
                        raise Non_Evaluable;
                     end if;
                     Count := Count + Temp;

                  elsif Element_Kind (Path_Elements (PE)) = An_Expression then
                     Count := Count + 1;

                  else
                     Failure ("Unexpected path kind:", Path_Elements (PE));
                  end if;
               end loop;
            end;
         end loop;

         return Count;
      end Count_Non_Others_Choices;

      procedure Process_Min_Others_Range is
         Case_Paths   : constant Path_List := Statement_Paths (Statement);
         Subtype_Span : Biggest_Int;
         Others_Span  : Biggest_Int;
      begin
         -- Don't waste time if there is no "when others" choice (must be last)
         if Definition_Kind (Case_Statement_Alternative_Choices
                             (Case_Paths (Case_Paths'Last))(1)) /= An_Others_Choice
         then
            return;
         end if;

         Subtype_Span := Discrete_Constraining_Lengths (A4G_Bugs.Corresponding_Expression_Type
                                                        (Case_Expression (Statement)))(1);
         if Subtype_Span = Not_Static then
            Subtype_Span := Discrete_Constraining_Lengths (Corresponding_First_Subtype
                                                           (A4G_Bugs.Corresponding_Expression_Type
                                                            (Case_Expression (Statement))))(1);
            if Subtype_Span = Not_Static then
               -- Hmmm... this one IS static, so there is something we can't evaluate
               -- give up
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Case_Expression (Statement)),
                            "Could not evaluate bounds of expression");
               return;
            end if;
         end if;

         Others_Span  := Subtype_Span - Count_Non_Others_Choices (Case_Paths);
         if Rule_Used (Min_Others_Span)(Check) and then Others_Span < Values (Min_Others_Span, Check) then
               Report (Rule_Id,
                       To_Wide_String (Labels (Min_Others_Span, Check)),
                       Check,
                       Get_Location (Case_Paths (Case_Paths'Last)),
                       "too few values covered by ""others"" in case statement ("
                       & Biggest_Int'Wide_Image (Others_Span)
                       & ')');
         elsif Rule_Used (Min_Others_Span)(Search) and then Others_Span < Values (Min_Others_Span, Search) then
               Report (Rule_Id,
                       To_Wide_String (Labels (Min_Others_Span, Search)),
                       Search,
                       Get_Location (Case_Paths (Case_Paths'Last)),
                       "too few values covered by ""others"" in case statement ("
                       & Biggest_Int'Wide_Image (Others_Span)
                       & ')');
         end if;

         if Rule_Used (Min_Others_Span)(Count) and then Others_Span < Values (Min_Others_Span, Count) then
               Report (Rule_Id,
                       To_Wide_String (Labels (Min_Others_Span, Count)),
                       Count,
                       Get_Location (Case_Paths (Case_Paths'Last)),
                       "");
         end if;

      exception
         when Non_Evaluable =>
            return;
      end Process_Min_Others_Range;

      --
      -- max_values is the number of values covered by the sub type
      -- of the case selector
      --
      procedure Process_Max_Values is
         Subtype_Span : Biggest_Int;
      begin
         Subtype_Span := Discrete_Constraining_Lengths (A4G_Bugs.Corresponding_Expression_Type
                                                        (Case_Expression (Statement))) (1);

         -- check if value if above or not
         if Rule_Used (Max_Values)(Check) and then Subtype_Span > Values (Max_Values, Check) then
            Report (Rule_Id,
                    To_Wide_String (Labels (Max_Values, Check)),
                    Check,
                    Get_Location (Statement),
                       "too many values for subtype of selector in case statement ("
                       & Biggest_Int'Wide_Image (Subtype_Span)
                       & ')');
         elsif Rule_Used (Max_Values)(Search) and then Subtype_Span > Values (Max_Values, Search) then
            Report (Rule_Id,
                    To_Wide_String (Labels (Max_Values, Search)),
                    Search,
                    Get_Location (Statement),
                       "too many values for subtype of selector in case statement ("
                       & Biggest_Int'Wide_Image (Subtype_Span)
                       & ')');
         end if;

         if Rule_Used (Max_Values)(Count) and then Subtype_Span > Values (Max_Values, Count) then
            Report (Rule_Id,
                    To_Wide_String (Labels (Max_Values, Count)),
                    Count,
                    Get_Location (Statement),
                    "");
         end if;

      exception
         when Non_Evaluable =>
            return;
     end Process_Max_Values;

      --
      -- min_paths is the minimum value of "when" in the case
      --
      procedure Process_Min_Paths is
         Nbr_Of_Paths : constant Biggest_Int := Statement_Paths (Statement)'Length;
      begin
         if Rule_Used (Min_Paths)(Check) and then Nbr_Of_Paths < Values (Min_Paths, Check) then
            Report (Rule_Id,
                    To_Wide_String (Labels (Min_Paths, Check)),
                    Check,
                    Get_Location (Statement),
                       "too few paths in case statement ("
                       & Biggest_Int'Wide_Image (Nbr_Of_Paths)
                       & ')');
         elsif Rule_Used (Min_Paths)(Search) and then Nbr_Of_Paths < Values (Min_Paths, Search) then
            Report (Rule_Id,
                    To_Wide_String (Labels (Min_Paths, Search)),
                    Search,
                    Get_Location (Statement),
                       "too few paths in case statement ("
                       & Biggest_Int'Wide_Image (Nbr_Of_Paths)
                       & ')');
         end if;

         if Rule_Used (Min_Paths)(Count) and then Nbr_Of_Paths < Values (Min_Paths, Count) then
            Report (Rule_Id,
                    To_Wide_String (Labels (Min_Paths, Count)),
                    Count,
                    Get_Location (Statement),
                    "");
         end if;
      end Process_Min_Paths;

   begin
      if Rule_Used = (Case_Statement_Names => (Rule_Types => False)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

       if Rule_Used (Max_Values) /= (Rule_Types => False) then
          Process_Max_Values;
       end if;

       if Rule_Used (Min_Paths) /= (Rule_Types => False) then
         Process_Min_Paths;
       end if;

      if Rule_Used (Min_Others_Span) /= (Rule_Types => False) then
         Process_Min_Others_Range;
      end if;
   end Process_Case_Statement;

   ------------------
   -- Process_Path --
   ------------------

   procedure Process_Path (Path : Asis.Path) is
      use Asis.Elements, Asis.Statements;
      use Framework.Reports, Utilities;
      use Ada.Strings.Wide_Unbounded;

      Choices : constant Asis.Element_List := Case_Statement_Alternative_Choices (Path);
      Nb_Val  : Extended_Biggest_Natural;
   begin
      if Rule_Used (Max_Range_Span) = (Rule_Types => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      for C in Choices'Range loop
         case Definition_Kind (Choices (C)) is
            when Not_A_Definition -- An_Expression
               | An_Others_Choice
                 =>
               null;
            when A_Discrete_Range =>
               Nb_Val := Discrete_Constraining_Lengths (Choices (C))(1);
               if Nb_Val = Not_Static then
                  -- This was supposed to be static, but for some reason we can't evaluate it
                  -- Give up
                  Uncheckable (Rule_Id,
                               False_Negative,
                               Get_Location (Choices(C)),
                               "Could not evaluate discrete range");
                  return;
               end if;

               if Rule_Used (Max_Range_Span) (Check) and then Nb_Val > Values (Max_Range_Span, Check) then
                  Report (Rule_Id,
                          To_Wide_String (Labels (Max_Range_Span, Check)),
                          Check,
                          Get_Location (Choices (C)),
                          "too many values in choice range ("
                            & Biggest_Int'Wide_Image (Nb_Val)
                            & ')');
               elsif Rule_Used (Max_Range_Span) (Search) and then Nb_Val > Values (Max_Range_Span, Search) then
                  Report (Rule_Id,
                    To_Wide_String (Labels (Max_Range_Span, Search)),
                    Search,
                    Get_Location (Choices (C)),
                          "too many values in choice range ("
                       & Biggest_Int'Wide_Image (Nb_Val)
                       & ')');
               end if;

               if Rule_Used (Max_Range_Span) (Count) and then Nb_Val > Values (Max_Range_Span, Count) then
                  Report (Rule_Id,
                          To_Wide_String (Labels (Max_Range_Span, Count)),
                          Count,
                          Get_Location (Choices (C)),
                          "");
               end if;

            when others =>
               Failure ("Wrong definition in case path");
         end case;
      end loop;
   end Process_Path;

begin
   Rules_Manager.Register_Semantic (Rule_Id,
                                    Help    => Help'Access,
                                    Add_Use => Add_Use'Access,
                                    Command => Command'Access);
end Rules.Case_Statement;
