----------------------------------------------------------------------
--  Rules.Parameter_Aliasing - Package body                         --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
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

-- Ada
with
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
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Parameter_Aliasing is
   use Framework, Utilities;

   --  Algorithm:
   --  Simply determine the "proximity" between each pair of [in] out parameters.
   --  See Thick_Queries.Variables_Proximity for the definition of proximity.

   subtype Rule_Detail is Thick_Queries.Result_Confidence;

   package Detail_Flags_Utilities is new Framework.Language.Flag_Utilities (Rule_Detail);
   use Detail_Flags_Utilities;

   type Usage is array (Rule_Detail) of Boolean;
   Rule_Used  : Usage := (others => False);
   Save_Used  : Usage;
   Ctl_Kinds  : array (Rule_Detail) of Control_Kinds;
   Ctl_Labels : array (Rule_Detail) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   With_In    : array (Rule_Detail) of Boolean;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control subprogram or entry calls where the same variable is given");
      User_Message  ("for more than one [in] out parameter.");
      User_Message  ("If ""with_in"" is given, consider also in parameters");
      User_Message;
      Help_On_Flags (Header => "Parameter(1): [with_in]",
                     Footer => "(optional, default=certain)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language, Thick_Queries;

      Detail  : Rule_Detail;
      In_Flag : Boolean;
   begin
      if Parameter_Exists then
         In_Flag := Get_Modifier ("WITH_IN");
         Detail  := Get_Flag_Parameter (Allow_Any => False);
      else
         In_Flag := False;
         Detail  := Certain;
      end if;

      if Rule_Used (Detail) then
         Parameter_Error (Rule_Id,
                          "rule can be called only once for ""Certain"","
                          & " once for ""Possible"","
                          & " and once for ""Unlikely""");
      end if;

      if Parameter_Exists then
         Parameter_Error (Rule_Id, "only one parameter allowed");
      end if;

      Ctl_Kinds  (Detail) := Ctl_Kind;
      Ctl_Labels (Detail) := To_Unbounded_Wide_String (Ctl_Label);
      Rule_Used  (Detail) := True;
      With_In    (Detail) := In_Flag;
   end Add_Control;


   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := (others => False);
            Ctl_Labels := (others => Null_Unbounded_Wide_String);
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
      use Thick_Queries;
   begin
      -- If weaker checks have been specified, force them for stronger ones
      if Rule_Used (Unlikely) and not Rule_Used (Possible) then
         Rule_Used  (Possible) := True;
         Ctl_Kinds  (Possible) := Ctl_Kinds  (Unlikely);
         Ctl_Labels (Possible) := Ctl_Labels (Unlikely);
         With_In    (Possible) := With_In    (Unlikely);
      end if;
      if Rule_Used (Possible) and not Rule_Used (Certain) then
         Rule_Used  (Certain) := True;
         Ctl_Kinds  (Certain) := Ctl_Kinds  (Possible);
         Ctl_Labels (Certain) := Ctl_Labels (Possible);
         With_In    (Certain) := With_In    (Possible);
      end if;
   end Prepare;


   ------------------
   -- Process_Call --
   ------------------

   type Parameters_Descr is
      record
         Mode : Asis.Mode_Kinds;
         Expr : Asis.Expression;
      end record;

   type Parameters_Table is array (Asis.List_Index range <>) of Parameters_Descr;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries, Framework.Locations, Framework.Reports, Ada.Strings.Wide_Unbounded;

      function Are_Dangerous_Modes (Left, Right : Parameters_Descr; With_In_Mode : Boolean) return Boolean is
         Cat : Type_Categories;
      begin
         if Left.Mode in An_Out_Mode .. An_In_Out_Mode and Right.Mode in An_Out_Mode .. An_In_Out_Mode then
            return True;
         end if;

         if With_In_Mode then
            if Left.Mode in An_Out_Mode .. An_In_Out_Mode then
               Cat := Type_Category (Right.Expr, Follow_Derived => True, Privacy => Follow_User_Private);
               return Cat not in Scalar_Types | An_Access_Type; -- Not a Pass by copy type
            elsif Right.Mode in An_Out_Mode .. An_In_Out_Mode then
               Cat := Type_Category (Left.Expr, Follow_Derived => True, Privacy => Follow_User_Private);
               return Cat not in Scalar_Types | An_Access_Type; -- Not a Pass by copy type
            end if;
         end if;

         return False;
      end Are_Dangerous_Modes;

   begin  -- Process_Call
      if Rule_Used = (Rule_Detail => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Expression_Kind (Called_Simple_Name (Call)) = An_Attribute_Reference then
         -- These ('Read and 'Write) are known to not have parameters that allow aliasing
         -- Moreover, the rest of the algorithm wouldn't work since parameters of
         -- attributes SP have no "name"
         return;
      end if;

      declare
         Actuals : constant Asis.Association_List := Actual_Parameters (Call);
         To_Check_Parameters : Parameters_Table (Actuals'Range);

         function Association_Image (Position : List_Index) return Wide_String is
            -- Image of a parameter association
            -- Calls the correct function depending on whether Name is a Defining_Name or a
            -- plain identifier.
            -- This kludge is needed because currently the function Formal_Name is
            -- inconsistent, depending on whether the actual association is positionnal or named
            use Asis.Declarations, Asis.Text;

            Name : constant Asis.Name := Formal_Name (Call, Position);
         begin
            if Element_Kind (Name) = A_Defining_Name then
               return '"' & Defining_Name_Image (Name) & " => "
                 & Trim_All (Element_Image (Actual_Parameter (Actuals (Position)))) & '"';
            else
               return '"' & Name_Image (Name) & " => "
                 & Trim_All (Element_Image (Actual_Parameter (Actuals (Position)))) & '"';
            end if;
         end Association_Image;

         Param_Proximity : Proximity;
      begin
         if Actuals'Length <= 1 then
            -- 0 or 1 parameter => no possible aliasing
            return;
         end if;

         if Expression_Kind (Call) = A_Function_Call and then Is_Nil (Corresponding_Called_Function (Call)) then
            -- A predefined operator... Anyway, all parameters are "in"
            return;
         end if;

         if Is_Dispatching_Call (Call) then
            -- We can avoid the Uncheckable if there is at most one parameter which is modified.
            -- (provided with_in was not used)
            if With_In /= (Rule_Detail => False) then
               Uncheckable (Rule_Id, False_Negative, Get_Location (Call), "Dispatching call");
               return;
            end if;

            declare
               E  : Asis.Expression;
               Nb : Natural := 0;
            begin
               for Act : Asis.Association of Actuals loop
                  E := Actual_Parameter (Act);
                  if Expression_Kind (E) = A_Type_Conversion then
                     E := Converted_Or_Qualified_Expression (E);
                  end if;

                  if Expression_Usage_Kind (E) /= Read then
                     Nb := Nb + 1;
                  end if;

               end loop;

               if Nb > 1 then
                  Uncheckable (Rule_Id, False_Negative, Get_Location (Call), "Dispatching call");
               end if;
            end;

            return;
         end if;

         for I in Actuals'Range loop
            To_Check_Parameters (I) := (Mode_Kind (Enclosing_Element (Formal_Name (Call, I))),
                                        Actual_Parameter (Actuals (I)));
            for J in List_Index range To_Check_Parameters'First .. I - 1 loop
               Param_Proximity := Variables_Proximity (To_Check_Parameters (J).Expr,
                                                       To_Check_Parameters (I).Expr);
               if Rule_Used (Param_Proximity.Confidence)
                 and then Param_Proximity.Overlap /= None
                 and then Are_Dangerous_Modes (To_Check_Parameters (I),
                                               To_Check_Parameters (J),
                                               With_In (Param_Proximity.Confidence))
               then
                  Report (Rule_Id,
                    To_Wide_String (Ctl_Labels (Param_Proximity.Confidence)),
                    Ctl_Kinds (Param_Proximity.Confidence),
                    Get_Location (Call),
                    (case Param_Proximity.Confidence is
                        when Certain  => "Certain",
                        when Possible => "Possible",
                        when Unlikely => "Unlikely")
                    & " aliasing between parameters "
                    & Association_Image (J)
                    & " and "
                    & Association_Image (I)
                   );
               end if;
            end loop;
         end loop;
      end;
   end Process_Call;

begin  -- Rules.Parameter_Aliasing
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Parameter_Aliasing;
